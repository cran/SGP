`analyzeSGP` <- 
function(sgp_object,
	state,
	years,
	content_areas,
	grades,
	sgp.config,
	sgp.percentiles=TRUE, 
	sgp.projections=TRUE,
	sgp.projections.lagged=TRUE,
	simulate.sgps=TRUE,
	goodness.of.fit.print=TRUE) {

        started.at <- proc.time()
        message(paste("Started analyzeSGP", date()))

	## Function to return sgp.config based upon a supplied year and content_area

	.get.config <- function(content_area, year, grades) {
                tmp.data <- sgp_object[["Student"]][J("VALID_CASE", content_area), c("YEAR", "GRADE"), with=FALSE]
		.sgp.panel.years <- sort(unique(tmp.data$YEAR))[1:which(sort(unique(tmp.data$YEAR)) == year)]
                .sgp.content.areas <- rep(content_area, length(.sgp.panel.years))
                .sgp.grade.sequences <- lapply(grades, function(x) tail(seq(min(tmp.data$GRADE, na.rm=TRUE), x), length(.sgp.panel.years)))
                list(sgp.content.areas=.sgp.content.areas, sgp.panel.years=.sgp.panel.years, sgp.grade.sequences=.sgp.grade.sequences) 
	}

	.analyzeSGP_Internal <- function(sgp.iter,
		reduced_data=sgp_object[["Student"]]["VALID_CASE", c("ID", "CONTENT_AREA", "YEAR", "GRADE", "SCALE_SCORE"), with=FALSE], 
		sgp.percentiles_Internal=sgp.percentiles,
		sgp.projections_Internal=sgp.projections,
		sgp.projections.lagged_Internal=sgp.projections.lagged) {

			tmp_sgp_object <- list()
			key(reduced_data) <- c("CONTENT_AREA", "YEAR", "ID")
			tmp_sgp_object[["Panel_Data"]] <- 
				as.data.frame(reshape(reduced_data[J(sgp.iter[["sgp.content.areas"]], sgp.iter[["sgp.panel.years"]]), mult="all"],
					idvar="ID",
					timevar="YEAR",
					drop="CONTENT_AREA",
					direction="wide"))

			if (sgp.percentiles_Internal) {
				sgp.vnames <- c("ID", paste("GRADE", sgp.iter[["sgp.panel.years"]], sep="."), 
					paste("SCALE_SCORE", sgp.iter[["sgp.panel.years"]], sep="."))
				if (simulate.sgps) {
					for (k in sgp.iter[["sgp.grade.sequences"]]) {
						tmp_sgp_object <- studentGrowthPercentiles(
							panel.data=tmp_sgp_object,
							sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)),
							use.my.knots.boundaries=state,
							growth.levels=state,
							panel.data.vnames=sgp.vnames,
							grade.progression=k,
							calculate.confidence.intervals=list(state=state,  
								confidence.quantiles=NULL,
								simulation.iterations=100, 
								distribution="Skew-Normal", round=1))
					} ## END k loop
				} else {
					for (k in sgp.iter[["sgp.grade.sequences"]]) {
						tmp_sgp_object <- studentGrowthPercentiles(
							panel.data=tmp_sgp_object,
							sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)),
							use.my.knots.boundaries=state,
							growth.levels=state,
							panel.data.vnames=sgp.vnames,
							grade.progression=k)
					} ## END k loop
				} 
			} ## END if sgp.percentiles_Internal

			if (sgp.projections_Internal) {
				sgp.vnames <- c("ID", paste("GRADE", sgp.iter[["sgp.panel.years"]], sep="."), 
					paste("SCALE_SCORE", sgp.iter[["sgp.panel.years"]], sep="."))

				for (k in lapply(sgp.iter[["sgp.grade.sequences"]], function(x) head(x, -1))) {
					tmp_sgp_object <- studentGrowthProjections(
						panel.data=tmp_sgp_object,
						sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)),
						use.my.coefficient.matrices=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
						use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
						performance.level.cutscores=state,
						percentile.trajectory.values=c(35, 50, 65),
						panel.data.vnames=sgp.vnames,
						grade.progression=k)
				}
			} ## END if sgp.projections_Internal

			if (sgp.projections.lagged_Internal) {
				sgp.vnames <- c("ID", paste("GRADE", head(sgp.iter[["sgp.panel.years"]], -1), sep="."), 
					paste("SCALE_SCORE", head(sgp.iter[["sgp.panel.years"]], -1), sep="."))

				for (k in lapply(sgp.iter[["sgp.grade.sequences"]], function(x) head(x, -1))) {
					tmp_sgp_object <- studentGrowthProjections(
						panel.data=tmp_sgp_object,
						sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1), 
							my.extra.label="LAGGED"),
						use.my.coefficient.matrices=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
						use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
						performance.level.cutscores=state,
						percentile.trajectory.values=c(35, 50, 65),
						panel.data.vnames=sgp.vnames,
						grade.progression=k)
				}
			} ## END sgp.projections.lagged_Internal
	return(tmp_sgp_object)
	} ## END .analyzeSGP_Internal

	.mergeSGP <- function(list_1, list_2) {
		for (j in c("Coefficient_Matrices", "Cutscores", "Goodness_of_Fit", "Knots_Boundaries", "SGPercentiles", "SGProjections", "Simulated_SGPs")) {

			i <- match(names(list_2[[j]]), names(list_1[[j]]))
			i <- is.na(i)
			if (any(i)) 
				list_1[[j]][names(list_2[[j]])[which(i)]] <- list_2[[j]][which(i)]
		}
		list_1
	}

	gof.print <- function(sgp_object) {
		if (length(sgp_object[["SGP"]][["Goodness_of_Fit"]]) > 0) {
			for (i in names(sgp_object[["SGP"]][["Goodness_of_Fit"]])) {
				dir.create(paste("Goodness_of_Fit/", i, sep=""), recursive=TRUE)
				for (j in names(sgp_object[["SGP"]][["Goodness_of_Fit"]][[i]])) {
					pdf(file=paste("Goodness_of_Fit/", i, "/", j, ".pdf", sep=""), width=8.5, height=4.5)
						grid.draw(sgp_object[["SGP"]][["Goodness_of_Fit"]][[i]][[j]])
				dev.off()
				}
			}
		} else {
			message("Mo Goodness of Fit tables available to print. No tables will be produced.")
		}
	}

        ## If missing sgp.config then determine year(s), content_area(s), and grade(s) if not explicitely provided

	if (missing(sgp.config)) {
		sgp.config <- tmp.years <- tmp.grades <- list()
		if (missing(content_areas)) {
			content_areas <- unique(sgp_object[["Student"]]["VALID_CASE"]$CONTENT_AREA)
		}
		if (missing(years)) {
			for (i in content_areas) {
				tmp.years[[i]] <- sort(tail(unique(sgp_object[["Student"]][J("VALID_CASE", content_areas)]$YEAR), -2), decreasing=TRUE)
			}
		} else {
			for (i in content_areas) { 
				tmp.years[[i]] <- years
			}
		}
		if (missing(grades)) {
			for (i in content_areas) {
				for (j in tmp.years[[i]]) {
					tmp.grades[[paste(i,j,sep=".")]] <- tail(sort(unique(sgp_object[["Student"]][J("VALID_CASE", i, j)]$GRADE)), -1)
				}
			}
		} else {
			for (i in content_areas) {
				for (j in tmp.years[[i]]) {
					tmp.grades[[paste(i,j,sep=".")]] <- grades
				}
			}
		}
		for (i in content_areas) {
			for (j in tmp.years[[i]]) {
				sgp.config[[paste(i,j,sep=".")]] <- .get.config(i,j,tmp.grades[[paste(i,j,sep=".")]])
			}
		}
	}

	## studentGrowthPercentiles & studentGrowthProjections

	if (sgp.percentiles | sgp.projections | sgp.projections.lagged) {
		sgp.iter <- NULL ## To prevent R CMD check warning
		sgp_object[["SGP"]] <- foreach(sgp.iter=iter(sgp.config), .packages="SGP", .combine=".mergeSGP", .inorder=FALSE) %dopar% {
			return(.analyzeSGP_Internal(sgp.iter))
		}
	} ## END if

	if (goodness.of.fit.print) gof.print(sgp_object)

	message(paste("Finished analyzeSGP", date(), "in", timetaken(started.at), "\n"))
	return(sgp_object)
} ## END analyzeSGP Function
