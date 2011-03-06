`abcSGP` <- 
function(sgp_object=sgpData_LONG,
	state="DEMO",
	years,
	content_areas,
	grades,
	sgp.config,
	sgp.percentiles=TRUE, 
	sgp.projections=TRUE,
	sgp.projections.lagged=TRUE,
	simulate.sgps=TRUE,
	sgp.summaries=list(MEDIAN_SGP="median_na(SGP)",
		MEDIAN_SGP_COUNT="num_non_missing(SGP)",
		PERCENT_AT_ABOVE_PROFICIENT="percent_in_category(ACHIEVEMENT_LEVEL, list(c(1,4)), list(1:5))",
		PERCENT_AT_ABOVE_PROFICIENT_COUNT="num_non_missing(ACHIEVEMENT_LEVEL)"),
	summary.groups=list(institution=c("STATE", "SCHOOL_NUMBER"),
		content="CONTENT_AREA",
		time="YEAR",
		institution_level="GRADE",
		demographic=c("GENDER", "ETHNICITY", "FREE_REDUCED_LUNCH_STATUS", "ELL_STATUS", "CATCH_KEEP_UP"),
		institution_inclusion=list(STATE="OCTOBER_ENROLLMENT_STATUS", SCHOOL_NUMBER="OCTOBER_ENROLLMENT_STATUS")),
	confidence.interval.groups=list(institution="SCHOOL_NUMBER",
		content="CONTENT_AREA",
		time="YEAR",
		institution_level= NULL,
		demographic=NULL,
		institution_inclusion=list(STATE=NULL, SCHOOL_NUMBER="OCTOBER_ENROLLMENT_STATUS"))) {

	## Prepare SGP data
	TMP_Data <- prepareSGP(sgp_object)
	cat("\nFinished with prepareSGP\n\n")

	## Function to return sgp.config based upon a supplied year and content_area - NOTE: Changed sgp_object (as in analyzeSGP) to TMP_Data in abcSGP
	.get.config <- function(content_area, year, grades) {
                tmp.data <- TMP_Data[["Student"]][J("VALID_CASE", content_area), c("YEAR", "GRADE"), with=FALSE]
		.sgp.panel.years <- seq(min(tmp.data$YEAR, na.rm=TRUE), year)
                .sgp.content.areas <- rep(content_area, length(.sgp.panel.years))
                .sgp.grade.sequences <- lapply(grades, function(x) tail(seq(min(tmp.data$GRADE, na.rm=TRUE), x), length(.sgp.panel.years)))
                list(sgp.content.areas=.sgp.content.areas, sgp.panel.years=.sgp.panel.years, sgp.grade.sequences=.sgp.grade.sequences) 
	}

        ## If missing sgp.config then determine year(s), content_area(s), and grade(s) if not explicitely provided
	if (missing(sgp.config)) {
		sgp.config <- tmp.years <- tmp.grades <- list()
		if (missing(content_areas)) {
			content_areas <- unique(TMP_Data[["Student"]]["VALID_CASE"]$CONTENT_AREA)
		}
		if (missing(years)) {
			for (i in content_areas) {
				tmp.years[[i]] <- sort(tail(unique(TMP_Data[["Student"]][J("VALID_CASE", content_areas)]$YEAR), -2), decreasing=TRUE)
			}
		} else {
			for (i in content_areas) { 
				tmp.years[[i]] <- years
			}
		}
		if (missing(grades)) {
			for (i in content_areas) {
				for (j in tmp.years[[i]]) {
					tmp.grades[[paste(i,j,sep=".")]] <- tail(sort(unique(TMP_Data[["Student"]][J("VALID_CASE", i, j)]$GRADE)), -1)
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

	## Get function call as object
	CALL <- match.call()

	if (missing(content_areas)) {
			content_areas <- unique(TMP_Data[["Student"]]["VALID_CASE"]$CONTENT_AREA)
		}
	if (missing(years)) {
		for (i in content_areas) {
			years <- sort(tail(unique(TMP_Data[["Student"]][J("VALID_CASE", content_areas)]$YEAR), -2), decreasing=TRUE)
		}
	}

	## Get the arguments with defaults for CALL object (can be set to NULL by user)
	CALL$state <- state
	CALL$sgp.percentiles <- sgp.percentiles
	CALL$sgp.projections <- sgp.projections
	CALL$sgp.projections.lagged <- sgp.projections.lagged
	CALL$simulate.sgps <- simulate.sgps
	CALL$sgp.summaries <- sgp.summaries
	CALL$summary.groups <- summary.groups
	CALL$confidence.interval.groups <- confidence.interval.groups

	## Check for consistency between simulate.sgps and confidence.interval.groups
	if (!simulate.sgps & !is.null(confidence.interval.groups)) {
		message("SIMULATED SGPs ARE REQUIRED TO COMPUTE CONFIDENCE INTERVALS. simulate.sgps WILL BE SET TO TRUE AND DEFAULT VALUES COMPUTED.")
		CALL$simulate.sgps <- TRUE
	}

	## run remaining SGP functions
	TMP_Data <- analyzeSGP(sgp_object=TMP_Data,
		state=CALL$state,
		sgp.config=sgp.config,
		sgp.percentiles=CALL$sgp.percentiles,
		sgp.projections=CALL$sgp.projections,
		sgp.projections.lagged=CALL$sgp.projections.lagged,
		simulate.sgps=CALL$simulate.sgps)
	cat("\nFinished with analyzeSGP\n\n")
	
	TMP_Data <- combineSGP(sgp_object=TMP_Data)
	cat("Finished with combineSGP\n\n")

	TMP_Data <- summarizeSGP(sgp_object=TMP_Data,
		state=CALL$state,
		years=years, 
		content_areas=content_areas, 
		sgp.summaries=CALL$sgp.summaries, 
		summary.groups=CALL$summary.groups, 
		confidence.interval.groups=CALL$confidence.interval.groups)

	### To be done: reportSGP(TMP_Data)

return(TMP_Data)
}

