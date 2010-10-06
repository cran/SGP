`studentGrowthPercentiles` <-
function(panel.data,                                                    ## REQUIRED
         sgp.labels,                                                    ## REQUIRED
         panel.data.vnames,                                             ## OPTIONAL
         grade.progression,                                             ## OPTIONAL
         num.prior,                                                     ## OPTIONAL
         subset.grade,                                                  ## OPTIONAL
         percentile.cuts,                                               ## OPTIONAL
         use.my.knots.boundaries,                                       ## OPTIONAL
         use.my.coefficient.matrices,                                   ## OPTIONAL
         print.other.gp,                                                ## OPTIONAL
         calculate.sgps=TRUE,                                           ## OPTIONAL
         rq.method="br",                                                ## OPTIONAL
         knot.cut.percentiles=c(0.2,0.4,0.6,0.8),                       ## OPTIONAL
         convert.0and100=TRUE,                                          ## OPTIONAL
         sgp.quantiles="Percentiles",                                   ## OPTIONAL
         percuts.digits=0,                                              ## OPTIONAL
         isotonize=TRUE,                                                ## OPTIONAL
         convert.using.loss.hoss=TRUE,                                  ## OPTIONAL
         goodness.of.fit=TRUE) {                                        ## OPTIONAL


##########################################################
###
### Internal utility functions
###
##########################################################

merge.all <- function(.list, ...){
	if(length(.list)==1) return(.list[[1]])
	Recall(c(list(merge(.list[[1]], .list[[2]], ...)), .list[-(1:2)]), ...)
}

.smooth.isotonize.row <- function(x, iso=isotonize) {
                                  x[which(is.na(x))] <- approx(x, xout=which(is.na(x)))$y
                                  if (iso) return(sort(x))
                                  else return(x)
}

.return.best.sgp <- function(x) {
                            as.numeric(tail(x[!is.na(x)], 1))
}

.return.best.percuts <- function(x, numpercentilecuts) {
                                    num.blocks <- (length(x))/numpercentilecuts
                                    block.start <- 1 + numpercentilecuts*0:(num.blocks-1)
                                    nonempty.blocks <- !is.na(x[block.start])
                                    best.block.start <- max(block.start[nonempty.blocks])
                                    as.numeric(x[best.block.start + 0:(numpercentilecuts-1)])
}

.create.knots.boundaries <- function(data, by.grade) {
                        tmp.list <- vector('list', 3*length(tmp.gp))
                        if (by.grade) {
                         tmp.stack <- data.frame(GRADE=stack(lapply(data[,2:(2+num.panels-1)], as.character))[,1], SCORE=stack(data[,(2+num.panels):(2+2*num.panels-1)])[,1]) 
                        } else {
                         tmp.stack <- data.frame(GRADE=rep(tmp.gp, each=dim(data)[1]), SCORE=stack(data[,(2+2*num.panels-1-length(tmp.gp)+1):(2+2*num.panels-1)])[,1])
                        }
                        for (i in seq_along(tmp.gp)) {
                         tmp.list[[3*i-2]] <- round(as.vector(quantile(subset(tmp.stack, tmp.stack$GRADE==tmp.gp[i])[,2], probs=knot.cut.percentiles, na.rm=T)), digits=3)
                         tmp.list[[3*i-1]] <- round(as.vector(extendrange(subset(tmp.stack, tmp.stack$GRADE==tmp.gp[i])[,2], f=0.1)), digits=3)  
                         tmp.list[[3*i]] <- round(as.vector(extendrange(subset(tmp.stack, tmp.stack$GRADE==tmp.gp[i])[,2], f=0.0)), digits=3)  
                        }
                        names(tmp.list) <- paste(rep(c("knots_", "boundaries_", "loss.hoss_"), length(tmp.gp)), rep(tmp.gp, each=3), sep="")
                        return(tmp.list) 
}

.get.panel.data <- function(data, k, by.grade) {
               str1 <- paste(" & !is.na(", tail(SS, 1), ")", sep="")
               str2 <- paste(" & ", tail(GD, 1), "==", tmp.last, sep="")
               str3 <- tail(SS, 1)

               for (i in 2:(k+1)) {
                    str1 <- paste(str1, " & !is.na(", rev(SS)[i], ")", sep="")
                    str2 <- paste(str2, " & ", rev(GD)[i], "==", rev(tmp.gp)[i], sep="")
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
    for (i in rev(tmp.gp)[2:(k+1)]) {
         .check.knots.boundaries(knot.names, i)
         knt <- paste("Knots_Boundaries$", tmp.path.knots.boundaries, "$knots_", i, sep="")
         bnd <- paste("Knots_Boundaries$", tmp.path.knots.boundaries, "$boundaries_", i, sep="")
         mod <- paste(mod, " + bs(SS", i, ", knots=", knt, ", Boundary.knots=", bnd, ")", sep="")
    }
    eval(parse(text=paste("rq(SS", tmp.last, " ~ ", substring(mod,4), ", tau=taus, data=tmp.data, method=rq.method)$coefficients", sep="")))
}

.check.knots.boundaries <- function(names, grade) {
                                     tmp <- do.call(rbind, strsplit(names, "_"))
                                     if (!grade %in% as.numeric(tmp[tmp[,1]=="knots", 2])) stop(paste("knots_", grade, " not found in Knot_Boundaries.", sep=""))
                                     if (!grade %in% as.numeric(tmp[tmp[,1]=="boundaries", 2])) stop(paste("boundaries_", grade, " not found in Knot_Boundaries.", sep=""))                           
}

.check.my.coefficient.matrices <- function(names, grade, order) {
                                     tmp <- do.call(rbind, strsplit(names, "_"))
                                     if (!grade %in% as.numeric(tmp[,2])) stop(paste("Coefficient matrix associated with grade ", grade, " not found.", sep=""))
                                     if (!order %in% as.numeric(tmp[tmp[,2]==grade,3])) stop(paste("Coefficient matrix associated with grade ", grade, "order ", order, " not found.", sep=""))
}

.get.max.matrix.order <- function(names, grade) {
                            tmp <- do.call(rbind, strsplit(names, "_"))
                            max(as.numeric(tmp[tmp[,2]==grade,3]), na.rm=TRUE)
}

.create_taus <- function(sgp.quantiles) {
      if (is.character(sgp.quantiles)) {
         taus <- switch(sgp.quantiles,
                        PERCENTILES = (1:100-0.5)/100,
                        DECILES     = 1:10/10,
                        QUINTILES   = 1:5/5,
                        QUARTILES   = 1:4/4)
      }

      if (is.numeric(sgp.quantiles)) {
         taus <- sgp.quantiles
      }
      return(taus)
}

.get.percentile.predictions <- function(data) {
     mod <- character()
     for (k in 1:j) {
         int <- "cbind(rep(1, dim(data)[1]),"
         knt <- paste("Knots_Boundaries$", tmp.path.knots.boundaries, "$knots_", rev(tmp.gp)[k+1], sep="")
         bnd <- paste("Knots_Boundaries$", tmp.path.knots.boundaries, "$boundaries_", rev(tmp.gp)[k+1], sep="")
         mod <- paste(mod, ", bs(data$SS", rev(tmp.gp)[k+1], ", knots=", knt, ", Boundary.knots=", bnd, ")", sep="")
         mat <- paste(") %*% Coefficient_Matrices$", tmp.path.coefficient.matrices, "$qrmatrix_", tmp.last, "_", k, sep="")
         .check.my.coefficient.matrices(matrix.names, tmp.last, k)
     }
     tmp <- eval(parse(text=paste(int, substring(mod, 2), mat, sep="")))
     return(round(t(apply(tmp, 1, function(x) .smooth.isotonize.row(x))), digits=5))
}

.get.quantiles <- function(data1, data2) {
      tmp <- data1 < data2[,tail(SS,1)]
      tmp <- cbind(tmp, FALSE)
      tmp <- apply(tmp, 1, function(x) which.min(x)-1)
      if (convert.0and100) {
              tmp[tmp==0] <- 1
              tmp[tmp==100] <- 99
      }
      tmp <- data.frame(data2$ID, tmp)
      names(tmp) <- c("ID", paste("SGP_", j, sep=""))
      return(tmp)
}

.get.percentile.cuts <- function(data1, data2) {
     tmp <- data1[ , percentile.cuts+1]
        if (convert.using.loss.hoss) {
           bnd <- Knots_Boundaries[[tmp.path.knots.boundaries]][[paste("loss.hoss_", tmp.last, sep="")]]
           tmp[tmp < bnd[1]] <- bnd[1]
           tmp[tmp > bnd[2]] <- bnd[2]
        }
     tmp <- data.frame(data2$ID, tmp)
     names(tmp) <- c("ID", paste("PERCUT_", j, "_", percentile.cuts, sep=""))
     return(tmp)
} 

.goodness.of.fit <- function(ss.data, sgp.data) {

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

   if (convert.0and100) {
       my.percentile.labels <- paste(c(1,1:9*10), "to", seq(9,99,10))
   } else {
      my.percentile.labels <- paste(0:9*10, "to", c(seq(9,89,10),100))
   }

   .sgp.fit <- function (score, sgp) {
                   gfittable <- prop.table(table(quantcut(round(score, digits=percuts.digits), q=0:10/10, right=FALSE),
                                    cut(sgp, c(-1, 9.5, 19.5, 29.5, 39.5, 49.5, 59.5, 69.5, 79.5, 89.5, 100.5),
                                    labels=my.percentile.labels)), 1)*100
                   return(gfittable)
   }

   tmp.merge <- merge(ss.data, sgp.data, by.x=1, by.y=1)
   tmp.table <- .sgp.fit(tmp.merge[,2], tmp.merge[,3])
   tmp.cuts <- quantcut(tmp.merge[,2], 0:10/10)
   tmp.colors <- .cell.color(as.vector(tmp.table))
   tmp.list <- vector("list", length(levels(tmp.cuts)))
   for (i in levels(tmp.cuts)) {
   tmp.list[[i]] <- quantile(tmp.merge$SGP[tmp.cuts==i], probs=ppoints(1:500))
   }
   tmp.ppoints <- do.call(c, tmp.list)


   layout.vp <- viewport(layout = grid.layout(2, 2, widths = unit(c(4.75, 3.5), rep("inches", 2)),
                         heights = unit(c(0.75, 3.5), rep("inches", 2))), name="layout")
   components <- vpList(viewport(layout.pos.row=1, layout.pos.col=1:2, name="title"),
                        viewport(layout.pos.row=2, layout.pos.col=1, xscale=c(-3,12), yscale=c(0,13), name="table"),
                        viewport(layout.pos.row=2, layout.pos.col=2, xscale=c(-25,110), yscale=c(-8,130), name="qq"))

   grobs <- gTree(childrenvp=layout.vp,
                  children=gList(gTree(vp="layout",
                  childrenvp=components,
                  children=gList(
                  rectGrob(gp=gpar(fill="grey95"), vp="title"),
                  textGrob(x=0.5, y=0.65, "Student Growth Percentile Goodness-of-Fit Descriptives", gp=gpar(cex=1.25), vp="title"),
                  textGrob(x=0.5, y=0.4, paste(sgp.labels$my.year, " ", sgp.labels$my.subject, ", Grade ", tmp.last, sep=""), vp="title"),

                  rectGrob(vp="table"),
                  rectGrob(x=rep(1:10,each=10), y=rep(10:1,10),
                      width=1, height=1, default.units="native",
                      gp=gpar(col="black", fill=tmp.colors),
                      vp="table"),
                  textGrob(x=0.35, y=10:1, paste(c("1st", "2nd", "3rd", paste(4:10, "th", sep="")), dimnames(tmp.table)[[1]], sep="/"), just="right", gp=gpar(cex=0.7), default.units="native", vp="table"),
                  textGrob(x=-2.5, y=5.5, "Prior Scale Score Decile/Range", gp=gpar(cex=0.8), default.units="native", rot=90, vp="table"),
                  textGrob(x=1:10, y=10.8, dimnames(tmp.table)[[2]], gp=gpar(cex=0.7), default.units="native", rot=45, just="left", vp="table"),
                  textGrob(x=5.75, y=12.5, "Student Growth Percentile Range", gp=gpar(cex=0.8), default.units="native", vp="table"),
                  textGrob(x=rep(1:10,each=10), y=rep(10:1,10), formatC(as.vector(tmp.table), format="f", digits=2), default.units="native", gp=gpar(cex=0.7), vp="table"),

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


############################################################################
###
### Data Preparation & Checks
###
############################################################################

if (missing(panel.data)) {
   stop("User must supply student achievement data for student growth percentile calculations. NOTE: data is now supplied to function using panel.data argument. See help page for details.")
}
if (!(class(panel.data) %in% c("matrix", "data.frame", "list", "SGP"))) {
     stop("Supplied panel.data not of a supported class. See help for details of supported classes")
}
if (class(panel.data) %in% c("list", "SGP") & !("Panel_Data" %in% names(panel.data))) {
     stop("Supplied panel.data missing Panel_Data")
}
if (class(panel.data) %in% c("list", "SGP")) {
     if (!class(panel.data$Panel_Data) == "data.frame") {
        stop("Supplied panel.data$Panel_Data is not a data.frame")   
     }
}
if (!missing(sgp.labels)) {
     if (!is.list(sgp.labels)) {
     stop("Please specify an appropriate list of SGP function labels (sgp.labels). See help page for details.")
}}
if (!identical(names(sgp.labels), c("my.year", "my.subject"))) {
     stop("Please specify an appropriate list for sgp.labels. See help page for details.")
}
if (!missing(use.my.knots.boundaries)) {
     if (!(class(panel.data) %in% c("list", "SGP"))) {
     stop("use.my.knots.boundaries is only appropriate when panel data is of class list or SGP. See help page for details.")
     }
     if (!is.list(use.my.knots.boundaries)) {
          stop("Please specify an appropriate list for use.my.knots.boundaries. See help page for details.")
     }
     if (!identical(names(use.my.knots.boundaries), c("my.year", "my.subject"))) {
          stop("Please specify an appropriate list for use.my.knots.boundaries. See help page for details.")
     }
     tmp.path.knots.boundaries <- toupper(paste(use.my.knots.boundaries$my.subject, ".", use.my.knots.boundaries$my.year, sep=""))
     if (is.null(panel.data[["Knots_Boundaries"]]) | is.null(panel.data[["Knots_Boundaries"]][[tmp.path.knots.boundaries]])) {
          stop("Knots and Boundaries indicated by use.my.knots.boundaries are not included.")
     }
}
if (!missing(use.my.coefficient.matrices)) {
     if (!(class(panel.data) %in% c("list", "SGP"))) {
     stop("use.my.coefficient.matrices is only appropriate when panel data is of class list or SGP. See help page for details.")
     }
     if (!is.list(use.my.coefficient.matrices)) {
          stop("Please specify an appropriate list for use.my.coefficient.matrices. See help page for details.")
     }
     if (!all(names(use.my.coefficient.matrices) %in% c("my.year", "my.subject"))) {
          stop("Please specify an appropriate list for use.my.coefficient.matrices. See help page for details.")
     }
     tmp.path.coefficient.matrices <- toupper(paste(use.my.coefficient.matrices$my.subject, ".", use.my.coefficient.matrices$my.year, sep=""))
     if (is.null(panel.data[["Coefficient_Matrices"]]) | is.null(panel.data[["Coefficient_Matrices"]][[tmp.path.coefficient.matrices]])) {
          stop("Coefficient matrices indicated by use.my.coefficient.matrices are not included.")
     }
}
if (is.character(sgp.quantiles)) {
     sgp.quantiles <- toupper(sgp.quantiles)
     if (!(sgp.quantiles %in% c("PERCENTILES", "DECILES", "QUINTILES", "QUARTILES"))) {
          stop("Character options for sgp.quantiles include Percentiles, Deciles, Quintiles, Quartiles. Other options available by specifying a numeric quantity. See help page for details.")
}} 
if (is.numeric(sgp.quantiles)) {
     if (!(all(sgp.quantiles > 0) & all(sgp.quantiles < 1))) {
          stop("Specify sgp.quantiles as as a vector of probabilities between 0 and 1.")
}}
if (!missing(percentile.cuts)) {
     if (sgp.quantiles != "PERCENTILES") {
          stop("percentile.cuts only appropriate for growth percentiles. Set sgp.quantiles to Percentiles to produce requested percentile.cuts.")
     }
     if (!all(percentile.cuts %in% 0:100)) {
          stop("Specified percentile.cuts must be integers between 0 and 100.")
}}


### Create object to store the studentGrowthPercentiles objects

sgp.labels <- lapply(sgp.labels, toupper)
tmp.path <- paste(sgp.labels$my.subject, sgp.labels$my.year, sep=".")
if (missing(use.my.knots.boundaries)) {
   tmp.path.knots.boundaries <- tmp.path
}
if (missing(use.my.coefficient.matrices)) {
   tmp.path.coefficient.matrices <- tmp.path
}
tmp.objects <- c("Coefficient_Matrices", "Goodness_of_Fit", "Knots_Boundaries", "Panel_Data", "SGPercentiles") 

for (i in tmp.objects) {
   assign(i, list())
   if (class(panel.data) %in% c("SGP", "list")) {
     if (!is.null(panel.data[[i]])) {
         assign(i, panel.data[[i]])
   }}
}


### Create Panel_Data based upon class of input data

if (class(panel.data) == "matrix") {
     Panel_Data <- as.data.frame(panel.data, stringsAsFactors=FALSE)
}
if (class(panel.data) == "list" & class(panel.data$Panel_Data) != "data.frame") {
     Panel_Data <- as.data.frame(panel.data$Panel_Data, stringsAsFactors=FALSE)
}
if (class(panel.data) == "data.frame") {
     Panel_Data <- panel.data
}
if (class(panel.data) %in% c("list", "SGP")) {
         Panel_Data <- panel.data$Panel_Data
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
names(ss.data)[c(1, (1+num.panels-num.prior):(1+num.panels), (1+2*num.panels-num.prior):(1+2*num.panels))] <- c("ID", GD, SS)


### Create Knots and Boundaries if requested (uses only grades in tmp.gp)

if (missing(use.my.knots.boundaries)) {
     tmp.list <- .create.knots.boundaries(ss.data, by.grade)
     Knots_Boundaries[[tmp.path.knots.boundaries]][names(tmp.list)] <- tmp.list
}

knot.names <- names(Knots_Boundaries[[tmp.path.knots.boundaries]])


### QR Calculations: coefficient matrices are saved/read into/from panel.data$Coefficient_Matrices

if (missing(use.my.coefficient.matrices)) {
      taus <- .create_taus(sgp.quantiles)
      for (k in seq(num.prior)) {
             tmp.matrix <- .create.coefficient.matrices(ss.data, k, by.grade)
             Coefficient_Matrices[[tmp.path.coefficient.matrices]][[paste("qrmatrix_", tmp.last, "_", k, sep="")]] <- tmp.matrix 
      }
}

matrix.names <- names(Coefficient_Matrices[[tmp.path.coefficient.matrices]])


### Calculate growth percentiles (if requested) and percentile cuts (if requested)

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
    tmp.quantiles <- tmp.percentile.cuts <- vector("list", max.order)

    for (j in 1:max.order) {
         tmp.data <- .get.panel.data(ss.data, j, by.grade)
         tmp.predictions <- .get.percentile.predictions(tmp.data) 
         tmp.quantiles[[j]] <- .get.quantiles(tmp.predictions, tmp.data)
         if (!missing(percentile.cuts)) {
            tmp.percentile.cuts[[j]] <- .get.percentile.cuts(tmp.predictions, tmp.data)
         }
    }

    quantile.data <- merge.all(tmp.quantiles, all=TRUE)
}


### Merge in percentile cut (if requested)

if (!missing(percentile.cuts)) {
    cuts.data <- merge.all(tmp.percentile.cuts, all=TRUE)
}


### Calculate highest order growth quantile and percentile cuts (if requested)

if (calculate.sgps) {
    tmp.best <- apply(quantile.data, 1, .return.best.sgp)
    if (missing(print.other.gp)) {
       quantile.data <- data.frame(ID=quantile.data$ID, SGP=tmp.best, stringsAsFactors=FALSE)
    } else {
       quantile.data <- data.frame(quantile.data, SGP=tmp.best, stringsAsFactors=FALSE)
    }

    if (!missing(percentile.cuts)){
       cuts.best <- t(apply(cuts.data[,-1], 1, .return.best.percuts, numpercentilecuts=length(percentile.cuts)))
       cuts.best <- round(cuts.best, digits=percuts.digits)
       colnames(cuts.best) <- paste("CUT_", percentile.cuts, sep="")
       quantile.data <- data.frame(quantile.data, cuts.best, stringsAsFactors=FALSE)
    }

    SGPercentiles[[tmp.path]] <- rbind(SGPercentiles[[tmp.path]], quantile.data)
}


### Perform goodness-of-fit analyses (if requested)

if (goodness.of.fit) {
    tmp.figure <- .goodness.of.fit(ss.data[,c("ID", tail(head(SS, -1),1))], quantile.data[,c("ID", "SGP")])
    Goodness_of_Fit[[tmp.path]][[paste("grade_", tmp.last, sep="")]] <- tmp.figure 
}


### Return SGP Object

list(Coefficient_Matrices=Coefficient_Matrices, 
     Goodness_of_Fit=Goodness_of_Fit, 
     Knots_Boundaries=Knots_Boundaries,
     Panel_Data=Panel_Data, 
     SGPercentiles=SGPercentiles)

} ## END studentGrowthPercentile Function
