`studentGrowthProjections` <-
function(panel.data,	## REQUIRED
	sgp.labels,	## REQUIRED  
	grade.progression,	## REQUIRED  
	max.forward.progression.years,
	max.forward.progression.grade,
	max.order.for.progression,
	use.my.knots.boundaries,
	use.my.coefficient.matrices,
	panel.data.vnames,
	performance.level.cutscores,
	chunk.size=5000,
	convert.0and100=TRUE,
	projection.unit="YEAR",
	percentile.trajectory.values=NULL,
	isotonize=TRUE,
	projcuts.digits=0,
	print.time.taken=TRUE) {

	started.at=proc.time()
	started.date <- date()

	##########################################################
	###
	### Utility functions
	###
	##########################################################

	.smooth.bound.iso.row <- function(x, grade, iso=isotonize, missing.taus, na.replace) {
		x[which(is.na(x))] <- approx(x, xout=which(is.na(x)))$y
		bnd <- panel.data[["Knots_Boundaries"]][[tmp.path.knots.boundaries]][[paste("loss.hoss_", grade, sep="")]]
		x[x < bnd[1]] <- bnd[1] ; x [x > bnd[2]] <- bnd[2]
		if (!iso) return(round(x, digits=5)) # Results are the same whether NAs present or not...
		if (iso & missing.taus) {
			x <- round(sort(x), digits=5)
			count <- 1
			na.row <- rep(NA,100)
			for (r in 1:length(na.replace)) if (na.replace[r]) {na.row[r] <- x[count]; count<-count+1}
			return(na.row)
		}	else return(round(sort(x), digits=5))
	}

	.create.path <- function(labels) {
		toupper(sub('\\.+$', '', do.call(paste, c(labels[c(2,1,3)], sep="."))))
	}

	.get.max.matrix.order <- function(names, grade) {
		tmp <- do.call(rbind, strsplit(names, "_"))
		tmp.vec <- vector("numeric", length(grade))
		for (i in seq_along(grade)) {
			tmp.vec[i] <- max(as.numeric(tmp[tmp[,2]==grade[i],3]), na.rm=TRUE)
		}
		return(tmp.vec)
	}

	.get.max.matrix.grade <- function(names, grade) {
		tmp <- do.call(rbind, strsplit(names, "_"))
		max(as.numeric(tmp[,2]), na.rm=TRUE)
	}

	.get.panel.data <- function(data, k, by.grade, subset.tf) {
		if (missing(subset.tf)) {
			str1 <- paste("!is.na(", tail(SS, 1), ")", sep="")
		} else {
			str1 <- paste("subset.tf & !is.na(", tail(SS, 1), ")", sep="")
		}
		str2 <- paste(" & ", tail(GD, 1), "==", tmp.last, sep="")
		str3 <- tail(SS, 1)

		if (k >= 2) {
			for (i in 2:k) {
				str1 <- paste(str1, " & !is.na(", rev(SS)[i], ")", sep="")
				str2 <- paste(str2, " & ", rev(GD)[i], "==", rev(grade.progression)[i], sep="")
				str3 <- paste(rev(SS)[i], ", ", str3, sep="")
		}}
		if (by.grade) {
			eval(parse(text=paste("return(subset(data,", str1, str2, ", select=c(ID, ", str3 ,")))", sep="")))
		} else {
			eval(parse(text=paste("return(subset(data,", str1, ", select=c(ID, ", str3 ,")))", sep="")))
		}
	}

	.get.data.table <- function(ss.data) {
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

	.year.increment <- function(year, increment) {
		paste(as.numeric(unlist(strsplit(as.character(year), "_")))+increment, collapse="_")	
	}

	.check.my.coefficient.matrices <- function(names, grade, order) {
		tmp <- do.call(rbind, strsplit(names, "_"))
		if (!grade %in% as.numeric(tmp[,2])) stop(paste("Coefficient matrix associated with grade ", grade, " not found.", sep=""))
		if (!order %in% as.numeric(tmp[tmp[,2]==grade,3])) stop(paste("Coefficient matrix associated with grade ", grade, "order ", order, " not found.", sep=""))
	}

	.get.grade.projection.sequence.priors <- function(grade.progression, grade.projection.sequence, max.order.tf) {
		tmp.list <- vector("list", length(grade.progression))
		for (i in 1:length(grade.progression)) {
			tmp.list[[i]] <- .get.max.matrix.order(matrix.names, grade.projection.sequence)
			tmp.list[[i]] <- pmin(tmp.list[[i]], seq(i, length.out=length(grade.projection.sequence)))
			if (!max.order.tf) {
				tmp.list[[i]] <- pmin(tmp.list[[i]], rep(max.order.for.progression, length(grade.projection.sequence)))
			}
		}
		return(rev(tmp.list[!duplicated(tmp.list)]))
	}

	.get.percentile.trajectories <- function(ss.data) { 
		tmp.percentile.trajectories <- vector("list", length(grade.projection.sequence.priors))
		completed.ids <- NULL

		for (i in seq_along(grade.projection.sequence.priors)) {
			tmp.gp <- grade.progression
			tmp.data <- .get.panel.data(ss.data, grade.projection.sequence.priors[[i]][1], by.grade, subset.tf=!(ss.data[["ID"]] %in% completed.ids))
			if (!empty(tmp.data)) {
				completed.ids <- c(tmp.data[["ID"]], completed.ids)
				num.rows <- dim(tmp.data)[1]
				tmp.data <- as.data.frame(sapply(tmp.data, rep, each=100))
				missing.taus=FALSE; na.replace=NULL # put these outside of j loop so that stay's true/non-null if only SOME of coef matrices are non-normal dimensions.
				for (j in seq_along(grade.projection.sequence.priors[[i]])) {
					mod <- character()
					int <- "cbind(rep(1, 100*num.rows),"
					for (k in 1:grade.projection.sequence.priors[[i]][j]) {
						knt <- paste("panel.data[['Knots_Boundaries']][['", tmp.path.knots.boundaries, "']][['knots_", rev(tmp.gp)[k], "']]", sep="")
						bnd <- paste("panel.data[['Knots_Boundaries']][['", tmp.path.knots.boundaries, "']][['boundaries_", rev(tmp.gp)[k], "']]", sep="")
						mod <- paste(mod, ", bs(tmp.data$SS", rev(tmp.gp)[k], ", knots=", knt, ", Boundary.knots=", bnd, ")", sep="")
					}
					mat <- paste("panel.data[['Coefficient_Matrices']][['", tmp.path.coefficient.matrices, "']][['qrmatrix_", grade.projection.sequence[j], "_", k, "']]", sep="")
					.check.my.coefficient.matrices(matrix.names, grade.projection.sequence[j], k)
					tmp.scores <- eval(parse(text=paste(int, substring(mod, 2), ")", sep="")))
					tmp.matrix <- eval(parse(text=mat))
					if (dim(tmp.matrix)[2] != 100) {
						tau.num <- ceiling(as.numeric(substr(colnames(tmp.matrix), 6, nchar(colnames(tmp.matrix))))*100)
						na.replace <- 1:100 %in% tau.num
						na.mtx <- matrix(NA, nrow=nrow(tmp.matrix), ncol=100)
						for (r in 1:length(na.replace)) if (na.replace[r]) na.mtx[,r] <- tmp.matrix[,r]
						tmp.matrix <- na.mtx
						missing.taus=TRUE
					}
					num.chunks <- floor(num.rows/chunk.size)
					chunk.list <- vector("list", num.chunks+1)
					for (chunk in 0:num.chunks) {
						lower.index <- chunk*chunk.size
						upper.index <- min((chunk+1)*chunk.size, num.rows)
						quantile.list <- vector("list", 100)
						for (m in 1:100) {
							quantile.list[[m]] <-  tmp.scores[m+lower.index:(upper.index-1)*100,] %*% tmp.matrix[,m] 
						}
						chunk.list[[chunk+1]] <- apply(matrix(do.call(c, quantile.list), ncol=100), 1, 
							function(x) .smooth.bound.iso.row(x, grade.projection.sequence[j], missing.taus=missing.taus, na.replace=na.replace)) #ADDED args for cases w/o 100 columns...
					}
					tmp.data <- data.frame(tmp.data, as.vector(do.call(cbind, chunk.list)))
					names(tmp.data)[length(names(tmp.data))] <- paste("SS", grade.projection.sequence[j], sep="")
					tmp.gp <- c(tmp.gp, grade.projection.sequence[j])
				} ## END j loop
				tmp.percentile.trajectories[[i]] <- tmp.data[,-(1:grade.projection.sequence.priors[[i]][1]+1)]
			} ## END if statement
		} ## END i loop
		do.call(rbind, tmp.percentile.trajectories)
	} ## END function

	.sgp.targets <- function(data, cut, convert.0and100) {
		tmp <- which.min(c(data < cut, FALSE))
		tmp[tmp==101] <- 100
		if (convert.0and100) {tmp[tmp==0] <- 1; tmp[tmp==100] <- 99}
		return(as.integer(tmp))
	}

	.get.trajectories.and.cuts <- function(percentile.trajectories, trajectories.tf, cuts.tf, projection.unit=projection.unit) {
		if (trajectories.tf) {
			tmp.traj <- round(percentile.trajectories[seq(dim(percentile.trajectories)[1]) %% 100 %in% ((percentile.trajectory.values+1) %% 100), 
					colnames(percentile.trajectories)], digits=projcuts.digits)
			tmp.traj <- reshape(data.table(tmp.traj, CUT=rep(percentile.trajectory.values, dim(tmp.traj)[1]/length(percentile.trajectory.values))), 
				idvar="ID", timevar="CUT", direction="wide")
			if (projection.unit=="GRADE") {
				tmp.vec <- expand.grid("P", percentile.trajectory.values, "_PROJ_GRADE_", grade.projection.sequence)
			} else {
				tmp.vec <- expand.grid("P", percentile.trajectory.values, "_PROJ_YEAR_", seq_along(grade.projection.sequence))
			}
			tmp.vec <- tmp.vec[order(tmp.vec$Var2),]
			names(tmp.traj) <- c("ID", do.call(paste, c(tmp.vec, sep="")))
			key(tmp.traj) <- "ID"
			if (!cuts.tf) return(tmp.traj)
		}
		if (cuts.tf) {
			percentile.trajectories[["ID"]] <- as.integer(percentile.trajectories[["ID"]]) 
			percentile.trajectories <- data.table(percentile.trajectories, key="ID")

			k <- 1
			cuts.arg <- names.arg <- character(sum(sapply(tmp.cutscores[[grep(sgp.labels$my.subject, names(tmp.cutscores))[1]]][paste("GRADE_", grade.projection.sequence, sep="")], length)))
			for (i in seq_along(grade.projection.sequence)) {
				tmp.year.increment <- paste(sgp.labels$my.subject, .year.increment(sgp.labels$my.year, i), sep=".")
				if (tmp.year.increment %in% names(tmp.cutscores)) {
					tmp.cutscores.iter <- length(tmp.cutscores[[tmp.year.increment]][[paste("GRADE_", grade.projection.sequence[i], sep="")]])
				} else {
					tmp.cutscores.iter <- length(tmp.cutscores[[sgp.labels$my.subject]][[paste("GRADE_", grade.projection.sequence[i], sep="")]])
				}

				for (j in seq(tmp.cutscores.iter)) {
					if (tmp.year.increment %in% names(tmp.cutscores)) {
						tmp <- tmp.cutscores[[tmp.year.increment]][[paste("GRADE_", grade.projection.sequence[i], sep="")]][j]
					} else {
						tmp <- tmp.cutscores[[sgp.labels$my.subject]][[paste("GRADE_", grade.projection.sequence[i], sep="")]][j]
					}
					cuts.arg[k] <- paste(".sgp.targets(SS", grade.projection.sequence[i], ", ", tmp, ", ", convert.0and100, ")", sep="")
					if (projection.unit=="GRADE") {
						names.arg[k] <- paste("LEVEL_", j, "_SGP_TARGET_GRADE_", grade.projection.sequence[i], sep="")
					} else {
						names.arg[k] <- paste("LEVEL_", j, "_SGP_TARGET_YEAR_", i, sep="")
					}
					k <- k+1
				}
			}
			arg <- paste("list(", paste(cuts.arg, collapse=", "), ")", sep="")

			tmp.cuts <- eval(parse(text=paste("percentile.trajectories[,", arg, ", by=ID]", sep="")))
			names(tmp.cuts) <- c("ID", names.arg)
			key(tmp.cuts) <- "ID"
			if (!trajectories.tf) {
				return(tmp.cuts)
			} else {
				return(merge(tmp.cuts, tmp.traj))
			}
		}
	}

	############################################################################
	###
	### Data Preparation & Checks
	###
	############################################################################

	if (missing(panel.data)) {
		stop("User must supply student achievement data for student growth percentile calculations. See help page for details.")
	}

	if (!(class(panel.data) %in% c("list", "SGP"))) {
		stop("Supplied panel.data not of a supported class. See help for details of supported classes")
	} else {
		if (!(all(c("Panel_Data", "Coefficient_Matrices", "Knots_Boundaries") %in% names(panel.data)))) {
			stop("Supplied panel.data missing Panel_Data, Coefficient_Matrices, and/or Knots_Boundaries. See help page for details")
		}
		if (!identical(class(panel.data[["Panel_Data"]]), "data.frame")) {
			stop("Supplied panel.data$Panel_Data is not a data.frame")	 
	}}

	if (missing(sgp.labels)) {
		stop("User must supply a list of SGP function labels (sgp.labels). See help page for details.")
	} else {
		if (!is.list(sgp.labels)) {
			stop("Please specify an appropriate list of SGP function labels (sgp.labels). See help page for details.")
		}
		if (!identical(names(sgp.labels), c("my.year", "my.subject")) & 
			!identical(names(sgp.labels), c("my.year", "my.subject", "my.extra.label"))) {
			stop("Please specify an appropriate list for sgp.labels. See help page for details.")
			}
		sgp.labels <- lapply(sgp.labels, toupper)
		tmp.path <- .create.path(sgp.labels)
	}

	if (missing(grade.progression)) {
		stop("User must supply a grade progression from which projections/trajectories will be derived. See help page for details.")
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
					stop("Knots and Boundaries are currently not implemented for the state indicated. Please contact the SGP package administrator to have your Knots and Boundaries included in the package")
		}
     		tmp.path.knots.boundaries <- tmp.path    
		}
	} else {
     		tmp.path.knots.boundaries <- tmp.path
	}

	if (!missing(use.my.coefficient.matrices)) {
		if (!is.list(use.my.coefficient.matrices)) {
			stop("Please specify an appropriate list for use.my.coefficient.matrices. See help page for details.")
		}
		if (!identical(names(use.my.coefficient.matrices), c("my.year", "my.subject")) &
			!identical(names(use.my.coefficient.matrices), c("my.year", "my.subject", "my.extra.label"))) {
			stop("Please specify an appropriate list for use.my.coefficient.matrices. See help page for details.")
			}
			tmp.path.coefficient.matrices <- .create.path(use.my.coefficient.matrices)
			if (is.null(panel.data[["Coefficient_Matrices"]]) | is.null(panel.data[["Coefficient_Matrices"]][[tmp.path.coefficient.matrices]])) {
				stop("Coefficient matrices indicated by argument use.my.coefficient.matrices are not included.")
		}} else {
			tmp.path.coefficient.matrices <- tmp.path
		} 

	if (!missing(performance.level.cutscores)) {
		if (is.character(performance.level.cutscores)) {
			if (!(performance.level.cutscores %in% names(stateData))) {
				message("\nTo use state cutscores, supply an appropriate two letter state abbreviation. \nRequested state may not be included. See help page for details.\n\n")
				tf.cutscores <- FALSE
			}
			if (is.null(names(stateData[[performance.level.cutscores]][["Achievement"]][["Cutscores"]]))) {
				message("\nCutscores are currently not implemented for the state indicated. \nPlease contact the SGP package administrator to have your cutscores included in the package.\n\n")
				tf.cutscores <- FALSE
			} else {
				tmp.cutscores <- stateData[[performance.level.cutscores]][["Achievement"]][["Cutscores"]]
				tf.cutscores <- TRUE
		}}
		if (is.list(performance.level.cutscores)) {
			if (any(names(performance.level.cutscores) %in% sgp.labels$my.subject)) {
				tmp.cutscores <- performance.level.cutscores[[sgp.labels$my.subject]]
				tf.cutscores <- TRUE
			} else {
				stop("\nList of cutscores provided in performance.level.cutscores must include a subject name that matches my.subject in sgp.labels (CASE SENSITIVE). See help page for details.\n\n")
				tf.cutscores <- FALSE
	}}} else {
		tf.cutscores <- FALSE
	}

	if (!(toupper(projection.unit)=="YEAR" | toupper(projection.unit)=="GRADE")) {
		stop("Projection unit must be specified as either YEAR or GRADE. See help page for details.")
	}

	if (is.null(percentile.trajectory.values) & !tf.cutscores) {
		stop("Either percentile trajectories and/or performance level cutscores must be supplied for the analyses.")
	}


	########################################################
	###
	### Calculate Student Growth Projections/Trajectories
	###
	########################################################

	tmp.objects <- c("SGProjections", "Cutscores") 

	for (i in tmp.objects) {
		if (!is.null(panel.data[[i]])) {
			assign(i, panel.data[[i]])
		} else {
			assign(i, list())
		}
	} 

	if (tf.cutscores) {
		Cutscores <- tmp.cutscores
	}

	### Create ss.data from Panel_Data and rename variables in based upon grade.progression

	if (!missing(panel.data.vnames)) {
		ss.data <- subset(panel.data[["Panel_Data"]], select=panel.data.vnames)
	} else {
		ss.data <- panel.data[["Panel_Data"]]
	}
	if (dim(ss.data)[2] %% 2 != 1) {
		stop(paste("Number of columns of supplied panel data (", dim(ss.data)[2], ") does not conform to data requirements. See help page for details."))
	}

	num.panels <- (dim(ss.data)[2]-1)/2

	if (length(grade.progression) > num.panels) {
		stop(paste("Supplied grade progression, grade.progression=c(", paste(grade.progression, collapse=","), "), exceeds number of panels (", num.panels, ") in provided data.", sep=""))
	}

	tmp.last <- tail(grade.progression, 1)
	by.grade <- TRUE ## Set to use studentGrowthPercentile functions. Currently, only works for TRUE in this function
	num.predictors <- 1:length(grade.progression)
	GD <- paste("GD", grade.progression, sep="")
	SS <- paste("SS", grade.progression, sep="")
	ss.data <- .get.data.table(ss.data)

	### Get Knots_Boudaries and Coefficient_Matrices names

	knot.names <- names(panel.data[["Knots_Boundaries"]][[tmp.path.knots.boundaries]])
	matrix.names <- names(panel.data[["Coefficient_Matrices"]][[tmp.path.coefficient.matrices]])

	### Calculate growth projections/trajectories 

	max.grade <- .get.max.matrix.grade(matrix.names)
	if (!missing(max.forward.progression.grade)) max.grade <- max.forward.progression.grade

	if (tmp.last+1 > max.grade) {
		stop("Supplied grade.progression and coefficient matrices do not allow projection. See help page for details.")
	}

	if (!missing(max.forward.progression.years)) {
		grade.projection.sequence <- (tmp.last+1):min(max.grade, tmp.last+1+max.forward.progression.years)
	} else {
		grade.projection.sequence <- (tmp.last+1):max.grade
	}

	grade.projection.sequence.priors <- .get.grade.projection.sequence.priors(grade.progression, grade.projection.sequence, max.order.tf=missing(max.order.for.progression)) 

	percentile.trajectories <- .get.percentile.trajectories(ss.data)


	### Select specific percentile trajectories and calculate cutscores

	if (tf.cutscores) {
		tmp.cutscore.grades <- as.numeric(unlist(strsplit(names(tmp.cutscores[[1]]), "_"))[seq(2,length(unlist(strsplit(names(tmp.cutscores[[1]]), "_"))), by=2)])
		if (!all(grade.projection.sequence %in% tmp.cutscore.grades)) {
			stop("Cutscores provided do not include cutscores for grades in projection.")
	}} 

	trajectories.and.cuts <- .get.trajectories.and.cuts(percentile.trajectories, !is.null(percentile.trajectory.values), tf.cutscores, toupper(projection.unit))

	if (is.null(SGProjections[[tmp.path]])) SGProjections[[tmp.path]] <- .unget.data.table(as.data.table(trajectories.and.cuts), ss.data)
	else SGProjections[[tmp.path]] <- rbind.fill(SGProjections[[tmp.path]], .unget.data.table(as.data.table(trajectories.and.cuts), ss.data))

	### Announce Completion & Return SGP Object

	if (print.time.taken) {
	        message(paste("\tStarted studentGrowthProjections", started.date))
		message(paste("\tSubject: ", sgp.labels$my.subject, ", Year: ", sgp.labels$my.year, ", Grade Progression: ", paste(grade.progression, collapse=", "), " ", sgp.labels$my.extra.label, sep="")) 
		message(paste("\tFinished studentGrowthProjections", date(), "in", timetaken(started.at), "\n"))
	} 

	list(Coefficient_Matrices=panel.data[["Coefficient_Matrices"]],
		Cutscores=Cutscores,
		Goodness_of_Fit=panel.data[["Goodness_of_Fit"]], 
		Knots_Boundaries=panel.data[["Knots_Boundaries"]], 
		Panel_Data=panel.data[["Panel_Data"]],
		SGPercentiles=panel.data[["SGPercentiles"]],
		SGProjections=SGProjections,
		Simulated_SGPs=panel.data[["Simulated_SGPs"]])

} ## END studentGrowthProjections Function
