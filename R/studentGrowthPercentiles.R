`studentGrowthPercentiles` <-
function(panel.data,         ## REQUIRED
         sgp.labels,         ## REQUIRED
         panel.data.vnames,
         grade.progression,
         num.prior,
         subset.grade,
         percentile.cuts,
         growth.levels, 
         use.my.knots.boundaries,
         use.my.coefficient.matrices,
         calculate.confidence.intervals,
         print.other.gp=FALSE,
         print.sgp.order=FALSE, 
         calculate.sgps=TRUE, 
         rq.method="br",
         knot.cut.percentiles=c(0.2,0.4,0.6,0.8),
         exact.grade.progression.sequence=FALSE,
         drop.nonsequential.grade.progression.variables=TRUE,
         convert.0and100=TRUE,
         sgp.quantiles="Percentiles",
         percuts.digits=0,
         isotonize=TRUE,
         convert.using.loss.hoss=TRUE,
         goodness.of.fit=TRUE,
         print.time.taken=TRUE) {

	started.at <- proc.time()
	started.date <- date()

	##########################################################
	###
	### Internal utility functions
	###
	##########################################################

	rbind.all <- function(.list, ...){
		if(length(.list)==1) return(.list[[1]])
		Recall(c(list(rbind(.list[[1]], .list[[2]], ...)), .list[-(1:2)]), ...)
	}

	.smooth.isotonize.row <- function(x, iso=isotonize) {
        	x[which(is.na(x))] <- approx(x, xout=which(is.na(x)))$y
        	if (iso) return(sort(x))
        	else return(x)
	}

	.create.path <- function(labels) {
		toupper(sub('\\.+$', '', do.call(paste, c(labels[c(2,1,3)], sep="."))))
	}

	.create.knots.boundaries <- function(data, by.grade) {
		tmp.list <- list()
		num.panels <- (dim(data)[2]-2)/2
		if (by.grade) {
			tmp.stack <- data.frame(GRADE=as.vector(sapply(data[,3:(3+num.panels-1), with=FALSE], as.character)), 
				SCORE=as.vector(sapply(data[,(3+num.panels):(3+2*num.panels-1), with=FALSE], as.numeric))) 
		} else {
			tmp.stack <- data.frame(GRADE=rep(tmp.gp, each=dim(data)[1]), 
				SCORE=as.vector(sapply(data[,(3+num.panels):(3+2*num.panels-1), with=FALSE], as.numeric)))
		}
 
		for (i in seq_along(tmp.gp)) {
		tmp.list[[3*i-2]] <- round(as.vector(quantile(subset(tmp.stack, tmp.stack[["GRADE"]]==tmp.gp[i])[,2], probs=knot.cut.percentiles, na.rm=TRUE)), digits=3)
		tmp.list[[3*i-1]] <- round(as.vector(extendrange(subset(tmp.stack, tmp.stack[["GRADE"]]==tmp.gp[i])[,2], f=0.1)), digits=3)  
		tmp.list[[3*i]] <- round(as.vector(extendrange(subset(tmp.stack, tmp.stack[["GRADE"]]==tmp.gp[i])[,2], f=0.0)), digits=3)  
		}
		names(tmp.list) <- paste(rep(c("knots_", "boundaries_", "loss.hoss_"), length(tmp.gp)), rep(tmp.gp, each=3), sep="")
		return(tmp.list) 
	}

	.get.data.table <- function(ss.data) {
		num.panels <- (dim(ss.data)[2]-1)/2
		num.predictors <- seq_along(tmp.gp)
		names(ss.data) <- NA
		names(ss.data)[c(1, (1+num.panels-max(num.predictors)+1):(1+num.panels), (1+2*num.panels-max(num.predictors)+1):(1+2*num.panels))] <- 
		c("ORIGINAL.ID", GD, SS)
		data.table(ID=seq(dim(ss.data)[1]), ss.data, key="ID")
	}

	.unget.data.table <- function(my.data, my.lookup) {
		key(my.data) <- "ID"; ORIGINAL.ID <- NULL
		my.data[["ID"]] <- my.lookup[my.data[["ID"]], ORIGINAL.ID]
		return(as.data.frame(my.data))
	}

	.get.panel.data <- function(data, k, by.grade) {
		str1 <- paste(" & !is.na(", tail(SS, 1), ")", sep="")
		str2 <- paste(" & ", tail(GD, 1), "== \"", tmp.last, "\"", sep="")
		str3 <- tail(SS, 1)
		for (i in 2:(k+1)) {
			str1 <- paste(str1, " & !is.na(", rev(SS)[i], ")", sep="")
			str2 <- paste(str2, " & ", rev(GD)[i], "== \"", rev(tmp.gp)[i], "\"", sep="")
			str3 <- paste(rev(SS)[i], ", ", str3, sep="")
		}
		if (by.grade) {
			eval(parse(text=paste("return(subset(data,", substring(str1, 3), str2, ", select=c(ID, ", str3 ,")))", sep="")))
		} else {
			eval(parse(text=paste("return(subset(data,", substring(str1, 3), ", select=c(ID, ", str3 ,")))", sep="")))
		}
	}

    .create.coefficient.matrices <- function(data, k, by.grade) {
      tmp.data <- .get.panel.data(data, k, by.grade)
      mod <- character()
      s4Ks <- "Knots=list("
      s4Bs <- "Boundaries=list("
      for (i in rev(tmp.gp)[2:(k+1)]) {
        .check.knots.boundaries(knot.names, i)
        knt <- paste("Knots_Boundaries[['", tmp.path.knots.boundaries, "']][['knots_", i, "']]", sep="")
        bnd <- paste("Knots_Boundaries[['", tmp.path.knots.boundaries, "']][['boundaries_", i, "']]", sep="")
        mod <- paste(mod, " + bs(SS", i, ", knots=", knt, ", Boundary.knots=", bnd, ")", sep="")
        s4Ks <- paste(s4Ks, "knots_", i, "=", knt, ",", sep="")
        s4Bs <- paste(s4Bs, "boundaries_", i, "=", bnd, ",", sep="")
      }
      tmp.mtx <- eval(parse(text=paste("rq(SS", tmp.last, " ~ ", substring(mod,4), ", tau=taus, data=tmp.data, method=rq.method)[['coefficients']]", sep="")))
      eval(parse(text=paste("new('splineMatrix', tmp.mtx, ", substring(s4Ks, 1, nchar(s4Ks)-1), "), ", substring(s4Bs, 1, nchar(s4Bs)-1), "), ", "Date=Sys.time())", sep="")))
    }

	.check.knots.boundaries <- function(names, grade) {
		tmp <- do.call(rbind, strsplit(names, "_"))
		if (!grade %in% tmp[tmp[,1]=="knots", 2]) stop(paste("knots_", grade, " not found in Knot_Boundaries.", sep=""))
		if (!grade %in% tmp[tmp[,1]=="boundaries", 2]) stop(paste("boundaries_", grade, " not found in Knot_Boundaries.", sep=""))                           
		}

	.check.my.coefficient.matrices <- function(names, grade, order) {
		tmp <- do.call(rbind, strsplit(names, "_"))
		if (!grade %in% tmp[,2]) stop(paste("Coefficient matrix associated with grade ", grade, " not found.", sep=""))
		if (!order %in% tmp[tmp[,2]==grade,3]) stop(paste("Coefficient matrix associated with grade ", grade, " order ", order, " not found.", sep=""))
	}

	.get.max.matrix.order <- function(names, grade) {
		tmp <- do.call(rbind, strsplit(names, "_"))
		max(as.numeric(tmp[tmp[,2]==grade,3]), na.rm=TRUE)
	}

	.create_taus <- function(sgp.quantiles) {
		if (is.character(sgp.quantiles)) {
			taus <- switch(sgp.quantiles,
				PERCENTILES = (1:100-0.5)/100)
		}

		if (is.numeric(sgp.quantiles)) {
			taus <- sgp.quantiles
		}
		return(taus)
	}

	.get.percentile.predictions <- function(data, order) {
		.check.my.coefficient.matrices(matrix.names, tmp.last, order)
		mod <- character()
		tmp.mtx <- eval(parse(text=paste("Coefficient_Matrices[['", tmp.path.coefficient.matrices, "']][['qrmatrix_", tmp.last, "_", j, "']]", sep="")))
		for (k in 1:order) {
			int <- "cbind(rep(1, dim(data)[1]),"
			knt <- paste("tmp.mtx@Knots[['knots_", rev(tmp.gp)[k+1], "']]", sep="")
			bnd <- paste("tmp.mtx@Boundaries[['boundaries_", rev(tmp.gp)[k+1], "']]", sep="")
			mod <- paste(mod, ", bs(data$SS", rev(tmp.gp)[k+1], ", knots=", knt, ", Boundary.knots=", bnd, ")", sep="")
		}	
		tmp <- eval(parse(text=paste(int, substring(mod, 2), ") %*% tmp.mtx", sep="")))
		return(round(t(apply(tmp, 1, function(x) .smooth.isotonize.row(x))), digits=5))
	}

	.get.quantiles <- function(data1, data2) {
		tmp <- data1 < data2
		tmp <- cbind(tmp, FALSE)
		tmp <- apply(tmp, 1, function(x) which.min(x)-1)
		if (convert.0and100) {
			tmp[tmp==0] <- 1
			tmp[tmp==100] <- 99
		}
		return(as.integer(tmp))
	}

	.get.percentile.cuts <- function(data1) {
		tmp <- round(data1[ , percentile.cuts+1], digits=percuts.digits)
		if (convert.using.loss.hoss) {
			bnd <- Knots_Boundaries[[tmp.path.knots.boundaries]][[paste("loss.hoss_", tmp.last, sep="")]]
			tmp[tmp < bnd[1]] <- bnd[1]
			tmp[tmp > bnd[2]] <- bnd[2]
		}
		colnames(tmp) <- paste("PERCENTILE_CUT_", percentile.cuts, sep="")
		return(tmp)
} 

	.goodness.of.fit <- function(data1) {
		.cell.color <- function(x){
			tmp.cell.color <- character(length(x))
			my.reds <- c("#FFFFFF", "#FEF1E1", "#FBD9CA", "#F9C1B4", "#F7A99E", "#F59188", "#F27972", "#F0615C", "#EE4946", "#EC3130", "#EA1A1A")
			tmp.diff <- abs(x - 10)
			tmp.cell.color[tmp.diff < 1] <- my.reds[1]
			for (i in 1:9){
			tmp.cell.color[tmp.diff >= i & tmp.diff < i+1] <- my.reds[i+1]
			}
			tmp.cell.color[tmp.diff >= 10] <- my.reds[11]
			return(tmp.cell.color)
		}

		.quantcut <- function (x, q = seq(0, 1, by = 0.25), na.rm = TRUE, ...) { ### From the quantcut package (thanks!!)
			quant <- quantile(x, q, na.rm = na.rm)
			dups <- duplicated(quant)
			if (any(dups)) {
				flag <- x %in% unique(quant[dups])
				retval <- ifelse(flag, paste("[", as.character(x), "]", sep = ""), NA)
				uniqs <- unique(quant)
				reposition <- function(cut) {
					flag <- x >= cut
					if (sum(flag) == 0) return(cut) else return(min(x[flag], na.rm = na.rm))
				}
				newquant <- sapply(uniqs, reposition)
				retval[!flag] <- as.character(cut(x[!flag], breaks = newquant, 
				include.lowest = TRUE, ...))
				levs <- unique(retval[order(x)])
				retval <- factor(retval, levels = levs)
				mkpairs <- function(x) sapply(x, function(y) if (length(y) == 2) y[c(2, 2)] else y[2:3])
				pairs <- mkpairs(strsplit(levs, "[^0-9+\\.\\-]+"))
				rownames(pairs) <- c("lower.bound", "upper.bound")
				colnames(pairs) <- levs
				closed.lower <- rep(FALSE, ncol(pairs))
				closed.upper <- rep(TRUE, ncol(pairs))
				closed.lower[1] <- TRUE
				for (i in 2:ncol(pairs)) if (pairs[1, i] == pairs[1, i - 1] && pairs[1, i] == pairs[2, i - 1]) closed.lower[i] <- FALSE
				for (i in 1:(ncol(pairs) - 1)) if (pairs[2, i] == pairs[1, i + 1] && pairs[2, i] == pairs[2, i + 1]) closed.upper[i] <- FALSE
				levs <- ifelse(pairs[1, ] == pairs[2, ], pairs[1, ], paste(ifelse(closed.lower, "[", "("), pairs[1, ], ",", pairs[2, ], ifelse(closed.upper, "]", ")"), sep = ""))
				levels(retval) <- levs
			}
			else retval <- cut(x, quant, include.lowest = TRUE, ...)
			return(retval)
		} ## END .quantcut function


		if (convert.0and100) {
			my.percentile.labels <- paste(c(1,1:9*10), "to", seq(9,99,10))
		} else {
			my.percentile.labels <- paste(0:9*10, "to", c(seq(9,89,10),100))
		}

		.sgp.fit <- function (score, sgp) {
			gfittable <- prop.table(table(.quantcut(score, q=0:10/10, right=FALSE, dig.lab=3),
			cut(sgp, c(-1, 9.5, 19.5, 29.5, 39.5, 49.5, 59.5, 69.5, 79.5, 89.5, 100.5),
			labels=my.percentile.labels)), 1)*100
			return(gfittable)
		}

		names(data1) <- c("PRIOR_SS", "SGP"); PRIOR_SS <- SGP <- NULL
		tmp.table <- .sgp.fit(data1[,PRIOR_SS], data1[,SGP])
		tmp.cuts <- .quantcut(data1[,PRIOR_SS], 0:10/10)
		tmp.colors <- .cell.color(as.vector(tmp.table))
		tmp.list <- list()

		for (i in levels(tmp.cuts)) {
			tmp.list[[i]] <- quantile(data1$SGP[tmp.cuts==i], probs=ppoints(1:500))
		}
		tmp.ppoints <- do.call(c, tmp.list)

		layout.vp <- viewport(layout = grid.layout(2, 2, widths = unit(c(4.75, 3.5), rep("inches", 2)),
		heights = unit(c(0.75, 3.5), rep("inches", 2))), name="layout")
		components <- vpList(viewport(layout.pos.row=1, layout.pos.col=1:2, name="title"),
		viewport(layout.pos.row=2, layout.pos.col=1, xscale=c(-3,12), yscale=c(0,13), name="table"),
		viewport(layout.pos.row=2, layout.pos.col=2, xscale=c(-25,110), yscale=c(-8,130), name="qq"))

		grobs <- gTree(childrenvp=layout.vp,
			name=paste(sgp.labels$my.subject, ".", sgp.labels$my.year, ".GRADE.", tmp.last, sep=""), 
			children=gList(gTree(vp="layout",
				childrenvp=components,
				name=paste("CHILDREN.", sgp.labels$my.subject, ".", sgp.labels$my.year, ".GRADE.", tmp.last, sep=""), 
				children=gList(
					rectGrob(gp=gpar(fill="grey95"), vp="title"),
					textGrob(x=0.5, y=0.65, "Student Growth Percentile Goodness-of-Fit Descriptives", gp=gpar(cex=1.25), vp="title"),
					textGrob(x=0.5, y=0.4, paste(sgp.labels$my.year, " ", sgp.labels$my.subject, ", Grade ", tmp.last, sep=""), vp="title"),
					rectGrob(vp="table"),
					rectGrob(x=rep(1:10,each=10), y=rep(10:1,10), width=1, height=1, default.units="native", 
						gp=gpar(col="black", fill=tmp.colors), vp="table"),
					textGrob(x=0.35, y=10:1, paste(c("1st", "2nd", "3rd", paste(4:10, "th", sep="")), dimnames(tmp.table)[[1]], sep="/"), just="right", 
						gp=gpar(cex=0.7), default.units="native", vp="table"),
					textGrob(x=-2.5, y=5.5, "Prior Scale Score Decile/Range", gp=gpar(cex=0.8), default.units="native", rot=90, vp="table"),
					textGrob(x=1:10, y=10.8, dimnames(tmp.table)[[2]], gp=gpar(cex=0.7), default.units="native", rot=45, just="left", vp="table"),
					textGrob(x=5.75, y=12.5, "Student Growth Percentile Range", gp=gpar(cex=0.8), default.units="native", vp="table"),
					textGrob(x=rep(1:10,each=10), y=rep(10:1,10), formatC(as.vector(tmp.table), format="f", digits=2), default.units="native", 
						gp=gpar(cex=0.7), vp="table"),

					rectGrob(vp="qq"),
					linesGrob(tmp.ppoints, rep(ppoints(1:500)*100, length(levels(tmp.cuts))), gp=gpar(lwd=0.35), default.units="native", vp="qq"),
					linesGrob(c(0,100), c(0,100), gp=gpar(lwd=0.75, col="red"), default.units="native", vp="qq"),
					linesGrob(x=c(-3,-3,103,103,-3), y=c(-3,103,103,-3,-3), default.units="native", vp="qq"),
					polylineGrob(x=rep(c(-6,-3), 11), y=rep(0:10*10, each=2), id=rep(1:11, each=2), default.units="native", vp="qq"),
					textGrob(x=-7, y=0:10*10, 0:10*10, default.units="native", gp=gpar(cex=0.7), just="right", vp="qq"),
					polylineGrob(x=rep(0:10*10, each=2), y=rep(c(103,106), 11), id=rep(1:11, each=2), default.units="native", vp="qq"),
					textGrob(x=0:10*10, y=109, 0:10*10, default.units="native", gp=gpar(cex=0.7), vp="qq"),
					textGrob(x=45, y=123, "QQ-Plot: Student Growth Percentiles", default.units="native", vp="qq"),
					textGrob(x=50, y=115, "Theoretical SGP Distribution", default.units="native", gp=gpar(cex=0.7), vp="qq"),
					textGrob(x=-17, y=50, "Empirical SGP Distribution", default.units="native", gp=gpar(cex=0.7), rot=90, vp="qq")))))
	} 

	.csem.score.simulator <- function(scale_scores, grade, content_area, year, state, distribution="Normal", round=1) {
		GRADE <- CONTENT_AREA <- YEAR <- NULL ## To avoid R CMD check warnings
		min.max <- stateData[[state]][["Achievement"]][["Knots_Boundaries"]][[content_area]][[paste("loss.hoss_", grade, sep="")]]
		if ("YEAR" %in% names(stateData[[state]][["Assessment_Program_Information"]][["CSEM"]])) {
			CSEM_Data <- subset(stateData[[state]][["Assessment_Program_Information"]][["CSEM"]], GRADE==grade & CONTENT_AREA==content_area & YEAR==year)
		} else {
			CSEM_Data <- subset(stateData[[state]][["Assessment_Program_Information"]][["CSEM"]], GRADE==grade & CONTENT_AREA==content_area)
		}
		CSEM_Data <- stateData[[state]][["Assessment_Program_Information"]][["CSEM"]][
			stateData[[state]][["Assessment_Program_Information"]][["CSEM"]][["GRADE"]]==grade & 
			stateData[[state]][["Assessment_Program_Information"]][["CSEM"]][["CONTENT_AREA"]]==content_area,]
		CSEM_Function <- splinefun(CSEM_Data[["SCALE_SCORE"]], CSEM_Data[["SCALE_SCORE_CSEM"]], method="natural")
		tmp.scale <- CSEM_Function(scale_scores)
		tmp.shape <- tan((pi/2)*((min.max[1]+min.max[2]) - 2*scale_scores)/(min.max[2]-min.max[1]))
		if(distribution=="Skew-Normal") {
			require(sn) 
			tmp.score <- round_any(as.numeric(rsn(length(scale_scores), location=scale_scores, scale=tmp.scale, shape=tmp.shape)), round)
		}
		if(distribution=="Normal") {
			tmp.score <- round_any(as.numeric(rnorm(length(scale_scores), mean=scale_scores, sd=tmp.scale)), round)
		}
		tmp.score[tmp.score < min.max[1]] <- min.max[1]
		tmp.score[tmp.score > min.max[2]] <- min.max[2]
		return(tmp.score)
	}


	############################################################################
	###
	### Data Preparation & Checks
	###
	############################################################################

	if (missing(panel.data)) {
		stop("User must supply student achievement data for student growth percentile calculations. NOTE: data is now supplied to function using panel.data argument. See help page for details.")
	}
	if (!(is.matrix(panel.data) | is.list(panel.data))) {
		stop("Supplied panel.data not of a supported class. See help for details of supported classes")
	}
	if (class(panel.data) %in% c("list", "SGP") & !("Panel_Data" %in% names(panel.data))) {
		stop("Supplied panel.data missing Panel_Data")
	}
	if (class(panel.data) %in% c("list", "SGP")) {
		if (!is.data.frame(panel.data[["Panel_Data"]])) {
			stop("Supplied panel.data$Panel_Data is not a data.frame")   
	}}

	if (!missing(sgp.labels)) {
		if (!is.list(sgp.labels)) {
			stop("Please specify an appropriate list of SGP function labels (sgp.labels). See help page for details.")
	}}
	if (!identical(names(sgp.labels), c("my.year", "my.subject")) &
		!identical(names(sgp.labels), c("my.year", "my.subject", "my.extra.label"))) {
		stop("Please specify an appropriate list for sgp.labels. See help page for details.")
	}
	sgp.labels <- lapply(sgp.labels, toupper)
	tmp.path <- .create.path(sgp.labels)

	if (!missing(growth.levels)) {
		tmp.growth.levels <- list()
		if (!is.list(growth.levels) & !is.character(growth.levels)) {
			message("growth.levels must be supplied as a list or character abbreviation. See help page for details. studentGrowthPercentiles will be calculated without augmented growth.levels")
		}
		if (is.list(growth.levels)) {
			if (!identical(names(growth.levels), c("my.cuts", "my.levels"))) {
				message("Please specify an appropriate list for growth.levels. See help page for details. Student growth percentiles will be calculated without augmented growth.levels")
			} else {
				tmp.growth.levels <- growth.levels
			} 
		}
		if (is.character(growth.levels)) {
			if (!growth.levels %in% names(stateData)) {
				message("Growth Level are currently not specified for the state indicated. Please contact the SGP package administrator to have your state's data included in the package. Student growth percentiles will be calculated without augmented growth.levels")
			} else {
			tmp.growth.levels[["my.cuts"]] <- stateData[[growth.levels]][["Growth"]][["Cutscores"]][["Cuts"]]
			tmp.growth.levels[["my.levels"]] <- stateData[[growth.levels]][["Growth"]][["Levels"]]
			}
		}
	}
	if (!missing(use.my.knots.boundaries)) {
		if (!is.list(use.my.knots.boundaries) & !is.character(use.my.knots.boundaries)) {
			stop("use.my.knots.boundaries must be supplied as a list or character abbreviation. See help page for details.")
		}
		if (is.list(use.my.knots.boundaries)) {
			if (!(class(panel.data) %in% c("list", "SGP"))) {
				stop("use.my.knots.boundaries is only appropriate when panel data is of class list or SGP. See help page for details.")
			}
			if (!identical(names(use.my.knots.boundaries), c("my.year", "my.subject")) & 
				!identical(names(use.my.knots.boundaries), c("my.year", "my.subject", "my.extra.label"))) {
					stop("Please specify an appropriate list for use.my.knots.boundaries. See help page for details.")
			}
			tmp.path.knots.boundaries <- .create.path(use.my.knots.boundaries)
			if (is.null(panel.data[["Knots_Boundaries"]]) | is.null(panel.data[["Knots_Boundaries"]][[tmp.path.knots.boundaries]])) {
				stop("Knots and Boundaries indicated by use.my.knots.boundaries are not included.")
			}
		}
		if (is.character(use.my.knots.boundaries)) {
			if (!use.my.knots.boundaries %in% names(stateData)) {
				message(paste("Knots and Boundaries are currently not implemented for the state indicated (", use.my.knots.boundaries, "). Knots and boundaries will be calculated from the data. Please contact the SGP package administrator to have your Knots and Boundaries included in the package", sep=""))
			}
     		tmp.path.knots.boundaries <- tmp.path    
		}
	} else {
     		tmp.path.knots.boundaries <- tmp.path
	}

	if (!missing(use.my.coefficient.matrices)) {
		if (!(class(panel.data) %in% c("list", "SGP"))) {
			stop("use.my.coefficient.matrices is only appropriate when panel data is of class list or SGP. See help page for details.")
		}
		if (!is.list(use.my.coefficient.matrices)) {
			stop("Please specify an appropriate list for use.my.coefficient.matrices. See help page for details.")
		}
		if (!identical(names(use.my.coefficient.matrices), c("my.year", "my.subject")) & 
			!identical(names(use.my.coefficient.matrices), c("my.year", "my.subject", "my.extra.label"))) {
				stop("Please specify an appropriate list for use.my.coefficient.matrices. See help page for details.")
		}
		tmp.path.coefficient.matrices <- .create.path(use.my.coefficient.matrices)
		if (is.null(panel.data[["Coefficient_Matrices"]]) | is.null(panel.data[["Coefficient_Matrices"]][[tmp.path.coefficient.matrices]])) {
			stop("Coefficient matrices indicated by use.my.coefficient.matrices are not included.")
		}
	} else {
		tmp.path.coefficient.matrices <- tmp.path
	}

	if (is.character(sgp.quantiles)) {
		sgp.quantiles <- toupper(sgp.quantiles)
		if (sgp.quantiles != "PERCENTILES") {
			stop("Character options for sgp.quantiles include only Percentiles at this time. Other options available by specifying a numeric quantity. See help page for details.")
	}} 
	if (is.numeric(sgp.quantiles)) {
		if (!(all(sgp.quantiles > 0 & sgp.quantiles < 1))) {
			stop("Specify sgp.quantiles as as a vector of probabilities between 0 and 1.")
	}}
	if (!missing(percentile.cuts)) {
		if (sgp.quantiles != "PERCENTILES") {
			stop("percentile.cuts only appropriate for growth percentiles. Set sgp.quantiles to Percentiles to produce requested percentile.cuts.")
		}
		if (!all(percentile.cuts %in% 0:100)) {
			stop("Specified percentile.cuts must be integers between 0 and 100.")
	}}
	if (!calculate.sgps & goodness.of.fit) {
		warning("Goodness-of-Fit tables only produced when calculating SGPs.")
	}
	if (!missing(calculate.confidence.intervals)) {
		csem.tf <- TRUE
		if (!is.list(calculate.confidence.intervals)) {
			message("Please specify an appropriate list for calculate.confidence.intervals. See help page for details. SGPs will be calculated without confidence intervals.")
			csem.tf <- FALSE
		}
		if (!(names(calculate.confidence.intervals)[1] %in% c("state", "variable") &
			c("confidence.quantiles", "simulation.iterations", "distribution", "round") %in% names(calculate.confidence.intervals))) {
			message("Please specify an appropriate list for calculate.confidence.intervals including state/csem variable, confidence.quantiles, simulation.iterations, distribution and round. See help page for details. SGPs will be calculated without confidence intervals.")
			csem.tf <- FALSE
		}
	} else {
		csem.tf <- FALSE
	}

	### Create object to store the studentGrowthPercentiles objects

	tmp.objects <- c("Coefficient_Matrices", "Goodness_of_Fit", "Knots_Boundaries", "Panel_Data", "SGPercentiles", "SGProjections", "Simulated_SGPs") 

	for (i in tmp.objects) {
		assign(i, list())
		if (class(panel.data) %in% c("SGP", "list")) {
			if (!is.null(panel.data[[i]])) {
				assign(i, panel.data[[i]])
		}}
	}


	### Create Panel_Data based upon class of input data

	if (is.matrix(panel.data)) {
		Panel_Data <- panel.data <- as.data.frame(panel.data, stringsAsFactors=FALSE)
	}
	if (identical(class(panel.data), "list")) {
        	if (!identical(class(panel.data[["Panel_Data"]]), "data.frame")) {
			Panel_Data <- as.data.frame(panel.data[["Panel_Data"]], stringsAsFactors=FALSE)
	}}
	if (identical(class(panel.data), "data.frame")) {
		Panel_Data <- panel.data
	}
	if (class(panel.data) %in% c("list", "SGP")) {
		Panel_Data <- panel.data[["Panel_Data"]]
	} 

	### Create ss.data from Panel_Data

	if (!missing(panel.data.vnames)) {
		ss.data <- subset(Panel_Data, select=panel.data.vnames)
	} else {
		ss.data <- Panel_Data
	}
	if (dim(ss.data)[2] %% 2 != 1) {
		stop(paste("Number of columns of supplied panel data (", dim(ss.data)[2], ") does not conform to data requirements. See help page for details."))
	}

	num.panels <- (dim(ss.data)[2]-1)/2

	### Rename variables in ss.data based upon grade progression

	if (!missing(grade.progression)) {
		tmp.gp <- grade.progression
		by.grade <- TRUE
		if (length(grade.progression) > num.panels) {
			stop("Supplied grade.progression exceeds number of panels in provided data.")
	}}
	if (!missing(subset.grade) & missing(grade.progression)) {
		tmp.gp <- (subset.grade-num.panels+1):subset.grade
		by.grade <- TRUE
	}
	if (missing(subset.grade) & missing(grade.progression)) {
		tmp.gp <- 1:num.panels
		by.grade <- FALSE
	}
	if (!missing(num.prior)) {
		if (length(num.prior) > 1 | !((num.prior-round(num.prior)) < .Machine$double.eps^0.5) | num.prior <= 0) {
			stop("Specified num.prior not positive integer(s)")
		}
	if (num.prior > length(tmp.gp)-1) {
		stop("Specified num.prior exceeds number of panels of data supplied")
	} else {
		tmp.gp <- tail(tmp.gp, num.prior+1)
	}} else {
		num.prior <- length(tmp.gp)-1
	}

	tmp.last <- tail(tmp.gp, 1)
	GD <- paste("GD", tmp.gp, sep="")
	SS <- paste("SS", tmp.gp, sep="")
	names(ss.data) <- NA
	if (any(diff(tmp.gp)!=1) & drop.nonsequential.grade.progression.variables) {
		ss.data <- ss.data[,c(1, (num.panels+1)-rev(c(1, cumsum(rev(diff(tmp.gp)))+1)-1), (2*num.panels+1)-rev(c(1, cumsum(rev(diff(tmp.gp)))+1)-1))]
                num.panels <- (dim(ss.data)[2]-1)/2
	}
	names(ss.data)[c(1, (1+num.panels-num.prior):(1+num.panels), (1+2*num.panels-num.prior):(1+2*num.panels))] <- c("ID", GD, SS)
	ss.data <- ss.data[,which(!is.na(names(ss.data)))]
        num.panels <- (dim(ss.data)[2]-1)/2
        ss.data[,(2+num.panels):(1+2*num.panels)] <- sapply(ss.data[,(2+num.panels):(1+2*num.panels)], as.numeric)
	ss.data <- .get.data.table(ss.data)


	### Create Knots and Boundaries if requested (uses only grades in tmp.gp)

	if (missing(use.my.knots.boundaries)) {
		tmp.knots <- c(Knots_Boundaries[[tmp.path.knots.boundaries]], .create.knots.boundaries(ss.data, by.grade))
		Knots_Boundaries[[tmp.path.knots.boundaries]] <- tmp.knots[!duplicated(names(tmp.knots))]
	} else {
		if (is.character(use.my.knots.boundaries)) {
			if (!is.null(stateData[[use.my.knots.boundaries]][["Achievement"]][["Knots_Boundaries"]])) {
				Knots_Boundaries[[tmp.path.knots.boundaries]] <- stateData[[use.my.knots.boundaries]][["Achievement"]][["Knots_Boundaries"]][[sgp.labels$my.subject]]
			} else {
				tmp.knots <- c(Knots_Boundaries[[tmp.path.knots.boundaries]], .create.knots.boundaries(ss.data, by.grade))
					Knots_Boundaries[[tmp.path.knots.boundaries]] <- tmp.knots[!duplicated(names(tmp.knots))]
			}
		}
	}
	knot.names <- names(Knots_Boundaries[[tmp.path.knots.boundaries]])

	### QR Calculations: coefficient matrices are saved/read into/from panel.data[["Coefficient_Matrices"]]

	if (missing(use.my.coefficient.matrices)) {
		taus <- .create_taus(sgp.quantiles)
		if (exact.grade.progression.sequence) {
			coefficient.matrix.priors <- num.prior
		} else {
			coefficient.matrix.priors <- seq(num.prior)
		}
		for (k in coefficient.matrix.priors) {
			Coefficient_Matrices[[tmp.path.coefficient.matrices]][[paste("qrmatrix_", tmp.last, "_", k, sep="")]] <- 
			.create.coefficient.matrices(ss.data, k, by.grade) 
		}
	}
	matrix.names <- names(Coefficient_Matrices[[tmp.path.coefficient.matrices]])

	### Calculate growth percentiles (if requested),  percentile cuts (if requested), and simulated confidence intervals (if requested)

	if (calculate.sgps) {
		max.order <- .get.max.matrix.order(matrix.names, tmp.last)
		if (max.order < num.prior) {
			stop("Number of priors requested exceeds maximum order of supplied coefficient matrices")
		}
		if (max.order > num.prior) {
			max.order <- num.prior
			message(paste("Maximum coefficient matrix order (max.order=", max.order, ") exceeds that of specified number of priors, 
				(num.prior=", num.prior, "). Only matrices of order up to num.prior=", num.prior, " will be used."))
		}
		if (exact.grade.progression.sequence) {
			tmp.quantiles <- tmp.percentile.cuts <- tmp.csem.quantiles <- list(); orders <- max.order
			if (goodness.of.fit) { # either switch goodness.of.fit to false or change creation of prior.ss
				message(paste("Goodness of Fit plots will not be produced when exact.grade.progression.sequence = TRUE."))
				goodness.of.fit <- FALSE
			}
		} else {
			tmp.quantiles <- tmp.percentile.cuts <- tmp.csem.quantiles <- list(); orders <- 1:max.order
		}

		for (j in orders) {
			tmp.data <- .get.panel.data(ss.data, j, by.grade)
			tmp.predictions <- .get.percentile.predictions(tmp.data, j)
			tmp.quantiles[[j]] <- data.table(ID=tmp.data[["ID"]], ORDER=j, SGP=.get.quantiles(tmp.predictions, tmp.data[[tail(SS,1)]]))
			if (csem.tf) {
				for (k in seq(calculate.confidence.intervals$simulation.iterations)) { 
					set.seed(k)
					if (k==1) {
						tmp.csem.quantiles[[j]] <- data.frame(ID=tmp.data[["ID"]],
						SGP_SIM_1=.get.quantiles(tmp.predictions, .csem.score.simulator(
							tmp.data[[tail(SS,1)]],
							tmp.last,
							sgp.labels$my.subject,
							sgp.labels$my.year,
							calculate.confidence.intervals$state,
							calculate.confidence.intervals$distribution,
							calculate.confidence.intervals$round)))
					} else {
						tmp.csem.quantiles[[j]] <- cbind(tmp.csem.quantiles[[j]], 
							.get.quantiles(tmp.predictions, .csem.score.simulator(
								tmp.data[[tail(SS,1)]],
								tmp.last,
								sgp.labels$my.subject,
								sgp.labels$my.year,
								calculate.confidence.intervals$state,
								calculate.confidence.intervals$distribution,
								calculate.confidence.intervals$round)))
								names(tmp.csem.quantiles[[j]])[k+1]<-paste("SGP_SIM", k, sep="_")
					}
				} ## END k loop
			} ## END CSEM analysis

			if (!missing(percentile.cuts)) {
				tmp.percentile.cuts[[j]] <- data.table(ID=tmp.data[["ID"]], .get.percentile.cuts(tmp.predictions))
			}
			if (goodness.of.fit & j==1) prior.ss <- tmp.data[, tail(head(SS, -1),1), with=FALSE]
		} ## END j loop

		quantile.data <- data.table(rbind.all(tmp.quantiles), key="ID")

		if (print.other.gp) {
			quantile.data <- data.table(reshape(quantile.data, idvar="ID", timevar="ORDER", direction="wide"),
				SGP=quantile.data[c(which(!duplicated(quantile.data))[-1]-1, nrow(quantile.data))][["SGP"]])
		} else {
			if (print.sgp.order) {
				quantile.data <- quantile.data[c(which(!duplicated(quantile.data))[-1]-1, nrow(quantile.data))]
			} else {
				quantile.data <- quantile.data[c(which(!duplicated(quantile.data))[-1]-1, nrow(quantile.data)), c("ID", "SGP"), with=FALSE]
			}
		}

		if (!missing(growth.levels)) {
			quantile.data <- data.table(quantile.data, SGP_LEVEL=factor(findInterval(quantile.data[["SGP"]], tmp.growth.levels[["my.cuts"]]), 
				labels=tmp.growth.levels[["my.levels"]]))
		}

		if (!missing(calculate.confidence.intervals)) {
			simulation.data <- data.table(rbind.all(tmp.csem.quantiles), key="ID")
			simulation.data <- simulation.data[c(which(!duplicated(simulation.data))[-1]-1, nrow(simulation.data))]

			if (!is.null(calculate.confidence.intervals$confidence.quantiles)) {
				tmp.cq <- round(t(apply(simulation.data[, -1, with=FALSE], 1, quantile, probs = calculate.confidence.intervals$confidence.quantiles)))
					colnames(tmp.cq) <- paste("SGP_", calculate.confidence.intervals$confidence.quantiles, "_CONFIDENCE_BOUND", sep="")
					quantile.data <- cbind(quantile.data, tmp.cq)
			}
			Simulated_SGPs[[tmp.path]] <- rbind.fill(Simulated_SGPs[[tmp.path]], .unget.data.table(simulation.data, ss.data)) 
		}

		if (!missing(percentile.cuts)){
			cuts.best <- data.table(rbind.all(tmp.percentile.cuts), key="ID")
			cuts.best <- cuts.best[c(which(!duplicated(cuts.best))[-1]-1, nrow(cuts.best))][,-1, with=FALSE]
			quantile.data <- cbind(quantile.data, cuts.best)
		}

		SGPercentiles[[tmp.path]] <- rbind.fill(.unget.data.table(quantile.data, ss.data), SGPercentiles[[tmp.path]]) 

		if (goodness.of.fit) {
			Goodness_of_Fit[[tmp.path]][[paste("GRADE_", tmp.last, sep="")]] <- .goodness.of.fit(data.table(prior.ss, quantile.data[, "SGP", with=FALSE])) 
		}
	} ## End if calculate.sgps

	### Start/Finish Message & Return SGP Object

	if (print.time.taken) {
	        message(paste("\tStarted studentGrowthPercentiles", started.date))
		message(paste("\tSubject: ", sgp.labels$my.subject, ", Year: ", sgp.labels$my.year, ", Grade Progression: ", paste(tmp.gp, collapse=", "), " ", sgp.labels$my.extra.label, sep=""))
		message(paste("\tFinished SGP Student Growth Percentile Analysis", date(), "in", timetaken(started.at), "\n")) 
	}

	list(Coefficient_Matrices=Coefficient_Matrices,
		Cutscores=panel.data[["Cutscores"]], 
		Goodness_of_Fit=Goodness_of_Fit, 
		Knots_Boundaries=Knots_Boundaries,
		Panel_Data=Panel_Data, 
		SGPercentiles=SGPercentiles,
		SGProjections=panel.data[["SGProjections"]],
		Simulated_SGPs=Simulated_SGPs)

} ## END studentGrowthPercentiles Function
