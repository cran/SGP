`analyzeSGP` <- 
function(sgp_object=sgpData_LONG,
	state="DEMO",
	years,
	content_areas,
	grades,
	sgp.config,
	sgp.percentiles=TRUE, 
	sgp.projections=TRUE,
	sgp.projections.lagged=TRUE,
	simulate.sgps=FALSE) {
	## Function to return sgp.config based upon a supplied year and content_area

	.get.config <- function(content_area, year, grades) {
                tmp.data <- sgp_object[["Student"]][J("VALID_CASE", content_area), c("YEAR", "GRADE"), with=FALSE]
		.sgp.panel.years <- seq(min(tmp.data$YEAR, na.rm=TRUE), year)
                .sgp.content.areas <- rep(content_area, length(.sgp.panel.years))
                .sgp.grade.sequences <- lapply(grades, function(x) tail(seq(min(tmp.data$GRADE, na.rm=TRUE), x), length(.sgp.panel.years)))
                list(sgp.content.areas=.sgp.content.areas, sgp.panel.years=.sgp.panel.years, sgp.grade.sequences=.sgp.grade.sequences) 
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
                for (sgp.iter in sgp.config) {
			sgp_object[["SGP"]][["Panel_Data"]] <- 
				as.data.frame(reshape(sgp_object[["Student"]][J("VALID_CASE", sgp.iter$sgp.content.areas, as.integer(sgp.iter$sgp.panel.years)), mult="all"],
					idvar="ID",
					timevar="YEAR",
					drop=names(sgp_object[["Student"]])[!names(sgp_object[["Student"]]) %in% c("ID", "YEAR", "GRADE", "SCALE_SCORE")],
					direction="wide"))

			if (sgp.percentiles) {
				sgp.vnames <- c("ID", paste("GRADE", sgp.iter$sgp.panel.years, sep="."), 
					paste("SCALE_SCORE", sgp.iter$sgp.panel.years, sep="."))
				if (simulate.sgps) {
					for (k in sgp.iter$sgp.grade.sequences) {
						sgp_object[["SGP"]] <- studentGrowthPercentiles(panel.data=sgp_object[["SGP"]],
							sgp.labels=list(my.year=tail(sgp.iter$sgp.panel.years, 1), my.subject=tail(sgp.iter$sgp.content.areas, 1)),
							use.my.knots.boundaries=state,
							panel.data.vnames=sgp.vnames,
							grade.progression=k,
							calculate.confidence.intervals=list(state=state,  
								confidence.quantiles=NULL,
								simulation.iterations=100, 
								distribution="Normal", round=1))
					} ## END k loop
				} else {
					for (k in sgp.iter$sgp.grade.sequences) {
						sgp_object[["SGP"]] <- studentGrowthPercentiles(panel.data=sgp_object[["SGP"]],
							sgp.labels=list(my.year=tail(sgp.iter$sgp.panel.years, 1), my.subject=tail(sgp.iter$sgp.content.areas, 1)),
							use.my.knots.boundaries=state,
							panel.data.vnames=sgp.vnames,
							grade.progression=k)
					} ## END k loop
				} 
			} ## END sgp.percentiles

			if (sgp.projections) {
				sgp.vnames <- c("ID", paste("GRADE", sgp.iter$sgp.panel.years, sep="."), 
					paste("SCALE_SCORE", sgp.iter$sgp.panel.years, sep="."))

				for (k in lapply(sgp.iter$sgp.grade.sequences, function(x) head(x, -1))) {
					sgp_object[["SGP"]] <- studentGrowthProjections(panel.data=sgp_object[["SGP"]],
						sgp.labels=list(my.year=tail(sgp.iter$sgp.panel.years, 1), my.subject=tail(sgp.iter$sgp.content.areas, 1)),
						use.my.coefficient.matrices=list(my.year=tail(sgp.iter$sgp.panel.years, 1), my.subject=tail(sgp.iter$sgp.content.areas, 1)), 
						use.my.knots.boundaries=list(my.year=tail(sgp.iter$sgp.panel.years, 1), my.subject=tail(sgp.iter$sgp.content.areas, 1)), 
						performance.level.cutscores=state,
						percentile.trajectory.values=c(35, 50, 65),
						panel.data.vnames=sgp.vnames,
						grade.progression=k)
				}
			} ## END sgp.projections
			if (sgp.projections.lagged) {
				sgp.vnames <- c("ID", paste("GRADE", head(sgp.iter$sgp.panel.years, -1), sep="."), 
					paste("SCALE_SCORE", head(sgp.iter$sgp.panel.years, -1), sep="."))

				for (k in lapply(sgp.iter$sgp.grade.sequences, function(x) head(x, -1))) {
					sgp_object[["SGP"]] <- studentGrowthProjections(panel.data=sgp_object[["SGP"]],
						sgp.labels=list(my.year=tail(sgp.iter$sgp.panel.years, 1), my.subject=tail(sgp.iter$sgp.content.areas, 1), 
							my.extra.label="LAGGED"),
						use.my.coefficient.matrices=list(my.year=tail(sgp.iter$sgp.panel.years, 1), my.subject=tail(sgp.iter$sgp.content.areas, 1)), 
						use.my.knots.boundaries=list(my.year=tail(sgp.iter$sgp.panel.years, 1), my.subject=tail(sgp.iter$sgp.content.areas, 1)), 
						performance.level.cutscores=state,
						percentile.trajectory.values=c(35, 50, 65),
						panel.data.vnames=sgp.vnames,
						grade.progression=k)
				}
			} ## END sgp.projections.lagged
		} ## END sgp.iter loop
	} ## END if
return(sgp_object)
} ## END analyzeSGP function

