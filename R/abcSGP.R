`abcSGP` <- 
function(sgp_object,
	state,
	steps=c("prepareSGP", "analyzeSGP", "combineSGP", "summarizeSGP", "reportSGP"),
	years,
	content_areas,
	grades,
	sgp.percentiles=TRUE, 
	sgp.projections=TRUE,
	sgp.projections.lagged=TRUE,
	simulate.sgps=TRUE,
	save.intermediate.results=FALSE,
	sgp.summaries=list(MEDIAN_SGP="median_na(SGP)",
		MEDIAN_SGP_TARGET="median_na(SGP_TARGET)",
		PERCENT_CATCHING_UP_KEEPING_UP="percent_in_category(CATCH_UP_KEEP_UP_STATUS, list(c('Catch Up: Yes', 'Keep Up: Yes')), list(c('Catch Up: Yes', 'Catch Up: No', 'Keep Up: Yes', 'Keep Up: No')))",
		MEDIAN_SGP_COUNT="num_non_missing(SGP)",
		PERCENT_AT_ABOVE_PROFICIENT="percent_in_category(ACHIEVEMENT_LEVEL, list(c('Proficient', 'Advanced')), list(c('Unsatisfactory', 'Partially Proficient', 'Proficient', 'Advanced')))",
		PERCENT_AT_ABOVE_PROFICIENT_COUNT="num_non_missing(ACHIEVEMENT_LEVEL)"),
	summary.groups=list(institution=c("STATE", "DISTRICT_NUMBER", "SCHOOL_NUMBER"),
		content="CONTENT_AREA",
		time="YEAR",
		institution_level="GRADE",
		demographic=c("GENDER", "ETHNICITY", "FREE_REDUCED_LUNCH_STATUS", "ELL_STATUS", "IEP_STATUS", "GIFTED_AND_TALENTED_PROGRAM_STATUS", "CATCH_UP_KEEP_UP_STATUS_INITIAL"),
		institution_inclusion=list(STATE="STATE_ENROLLMENT_STATUS", DISTRICT_NUMBER="DISTRICT_ENROLLMENT_STATUS", SCHOOL_NUMBER="SCHOOL_ENROLLMENT_STATUS")),
	confidence.interval.groups=list(institution="SCHOOL_NUMBER",
		content="CONTENT_AREA",
		time="YEAR",
		institution_level=NULL,
		demographic=NULL,
		institution_inclusion=list(STATE=NULL, DISTRICT_NUMBER=NULL, SCHOOL_NUMBER="SCHOOL_ENROLLMENT_STATUS"))) {

        started.at <- proc.time()
	message(paste("Started abcSGP", date()), "\n")

        ### Check for consistency between simulate.sgps and confidence.interval.groups ###

	if (!is.null(confidence.interval.groups) & !simulate.sgps) {
                message("Simulated SGPs are required to compute confidence intervals. simulate.sgps will be set to true.")
                simulate.sgps <- TRUE
        }


	### prepareSGP ###

	if ("prepareSGP" %in% steps) {
		abcSGP_TMP_Data <- prepareSGP(sgp_object)
	        if (save.intermediate.results) save(abcSGP_TMP_Data, file="abcSGP_TMP_Data.Rdata")
	}


        ### Calculate Relevant Quantities ###

        if (missing(content_areas)) {
                content_areas <- unique(abcSGP_TMP_Data[["Student"]]["VALID_CASE"]$CONTENT_AREA)
        }
        if (missing(years)) {
                for (i in content_areas) {
                        years <- sort(tail(unique(abcSGP_TMP_Data[["Student"]][J("VALID_CASE", content_areas)]$YEAR), -2), decreasing=TRUE)
                }
        }


	### analyzeSGP ###

	if ("analyzeSGP" %in% steps) {
		abcSGP_TMP_Data <- analyzeSGP(
			sgp_object=abcSGP_TMP_Data,
			state=state,
			content_areas=content_areas,
			years=years,
			sgp.percentiles=sgp.percentiles,
			sgp.projections=sgp.projections,
			sgp.projections.lagged=sgp.projections.lagged,
			simulate.sgps=simulate.sgps)

                if (save.intermediate.results) save(abcSGP_TMP_Data, file="abcSGP_TMP_Data.Rdata")
	}


	### combineSGP ###

	if ("combineSGP" %in% steps) {
		abcSGP_TMP_Data <- combineSGP(
			sgp_object=abcSGP_TMP_Data,
			state=state,
			sgp.percentiles=sgp.percentiles,
			sgp.projections.lagged=sgp.projections.lagged)

                if (save.intermediate.results) save(abcSGP_TMP_Data, file="abcSGP_TMP_Data.Rdata")
	}


	### summarizeSGP ###

	if ("summarizeSGP" %in% steps) {
		abcSGP_TMP_Data <- summarizeSGP(
			sgp_object=abcSGP_TMP_Data,
			state=state,
			years=years, 
			content_areas=content_areas, 
			sgp.summaries=sgp.summaries, 
			summary.groups=summary.groups, 
			confidence.interval.groups=confidence.interval.groups)

                if (save.intermediate.results) save(abcSGP_TMP_Data, file="abcSGP_TMP_Data.Rdata")
	}


	### reportSGP (To be done) ###

#	if ("reportSGP" %in% steps) {
#
#	}

        message(paste("Finished abcSGP", date(), "in", timetaken(started.at), "\n"))
	return(abcSGP_TMP_Data)
} ## END abcSGP Function

