`abcSGP` <- 
function(sgp_object,
	state,
	steps=c("prepareSGP", "analyzeSGP", "combineSGP", "summarizeSGP", "visualizeSGP"),
	years,
	content_areas,
	grades,
	sgp.percentiles=TRUE, 
	sgp.projections=TRUE,
	sgp.projections.lagged=TRUE,
	sgp.percentiles.baseline=TRUE,
	sgp.projections.baseline=TRUE,
	sgp.projections.lagged.baseline=TRUE,
	simulate.sgps=TRUE,
	parallel.config=NULL,
	save.intermediate.results=FALSE,
	sgp.summaries=list(MEDIAN_SGP="median_na(SGP)",
		MEDIAN_SGP_TARGET="median_na(SGP_TARGET)",
		PERCENT_CATCHING_UP_KEEPING_UP="percent_in_category(CATCH_UP_KEEP_UP_STATUS, list(c('Catch Up: Yes', 'Keep Up: Yes')), list(c('Catch Up: Yes', 'Catch Up: No', 'Keep Up: Yes', 'Keep Up: No')))",
		MEDIAN_SGP_COUNT="num_non_missing(SGP)",
		PERCENT_AT_ABOVE_PROFICIENT="percent_in_category(ACHIEVEMENT_LEVEL, list(c('Proficient', 'Advanced')), list(c('Unsatisfactory', 'Partially Proficient', 'Proficient', 'Advanced')))",
		PERCENT_AT_ABOVE_PROFICIENT_COUNT="num_non_missing(ACHIEVEMENT_LEVEL)",
                PERCENT_AT_ABOVE_PROFICIENT_PRIOR="percent_in_category(ACHIEVEMENT_LEVEL_PRIOR, list(c('Proficient', 'Advanced')), list(c('Unsatisfactory', 'Partially Proficient', 'Proficient', 'Advanced')))",
                PERCENT_AT_ABOVE_PROFICIENT_PRIOR_COUNT="num_non_missing(ACHIEVEMENT_LEVEL_PRIOR)"),
	summary.groups=list(institution=c("STATE", "DISTRICT_NUMBER", "SCHOOL_NUMBER"),
		content="CONTENT_AREA",
		time="YEAR",
		institution_level="GRADE",
		demographic=c("GENDER", "ETHNICITY", "FREE_REDUCED_LUNCH_STATUS", "ELL_STATUS", "IEP_STATUS", "GIFTED_AND_TALENTED_PROGRAM_STATUS", "CATCH_UP_KEEP_UP_STATUS_INITIAL"),
		institution_inclusion=list(STATE="STATE_ENROLLMENT_STATUS", DISTRICT_NUMBER="DISTRICT_ENROLLMENT_STATUS", SCHOOL_NUMBER="SCHOOL_ENROLLMENT_STATUS")),
        confidence.interval.groups=list(TYPE="Bootstrap",
                VARIABLES=c("SGP"),
                QUANTILES=c(0.025, 0.975),
                GROUPS=list(institution="SCHOOL_NUMBER",
                content="CONTENT_AREA",
                time="YEAR",
                institution_level= NULL,
                demographic=NULL,
                institution_inclusion=list(STATE=NULL, DISTRICT_NUMBER=NULL, SCHOOL_NUMBER="SCHOOL_ENROLLMENT_STATUS"))),
	plot.types=c("bubblePlot", "studentGrowthPlot", "growthAchievementPlot")) {

        started.at <- proc.time()
	message(paste("Started abcSGP", date()), "\n")

	### Create state (if missing) from sgp_object (if possible)

	if (missing(state)) {
		tmp.name <- gsub("_", " ", deparse(substitute(sgp_object)))
		if (any(sapply(c(state.name, "Demonstration"), function(x) regexpr(x, tmp.name)))==1) {
			state <- c(state.abb, "DEMO")[which(sapply(c(state.name, "Demonstration"), function(x) regexpr(x, tmp.name))==1)]
		}
	}

        ### Check for consistency between simulate.sgps and confidence.interval.groups ###

	if ("summarizeSGP" %in% plot.types & !is.null(confidence.interval.groups) & !simulate.sgps) {
                message("Simulated SGPs are required to compute confidence intervals. simulate.sgps will be set to true.")
                simulate.sgps <- TRUE
        }


	### prepareSGP ###

	if ("prepareSGP" %in% steps) {
		sgp_object <- prepareSGP(sgp_object)
	        if (save.intermediate.results) save(sgp_object, file="sgp_object.Rdata")
	}


        ### Calculate Relevant Quantities ###

        if (missing(content_areas)) {
                content_areas <- unique(sgp_object@Data["VALID_CASE"]$CONTENT_AREA)
        }
        if (missing(years)) {
                for (i in content_areas) {
                        years <- sort(tail(unique(sgp_object@Data[J("VALID_CASE", content_areas)]$YEAR), -2), decreasing=TRUE)
                }
        }


	### analyzeSGP ###

	if ("analyzeSGP" %in% steps) {
		sgp_object <- analyzeSGP(
			sgp_object=sgp_object,
			state=state,
			content_areas=content_areas,
			years=years,
			sgp.percentiles=sgp.percentiles,
			sgp.projections=sgp.projections,
			sgp.projections.lagged=sgp.projections.lagged,
			sgp.percentiles.baseline=sgp.percentiles.baseline,
			sgp.projections.baseline=sgp.projections.baseline,
			sgp.projections.lagged.baseline=sgp.projections.lagged.baseline,
			simulate.sgps=simulate.sgps,
			parallel.config=parallel.config)

                if (save.intermediate.results) save(sgp_object, file="sgp_object.Rdata")
	}


	### combineSGP ###

	if ("combineSGP" %in% steps) {
		sgp_object <- combineSGP(
			sgp_object=sgp_object,
			state=state,
			years=years,
			content_areas=content_areas,
			sgp.percentiles=sgp.percentiles,
			sgp.projections.lagged=sgp.projections.lagged,
			sgp.projections.lagged.baseline=sgp.projections.lagged.baseline)

                if (save.intermediate.results) save(sgp_object, file="sgp_object.Rdata")
	}


	### summarizeSGP ###

	if ("summarizeSGP" %in% steps) {
		sgp_object <- summarizeSGP(
			sgp_object=sgp_object,
			state=state,
			years=years, 
			content_areas=content_areas, 
			sgp.summaries=sgp.summaries, 
			summary.groups=summary.groups, 
			confidence.interval.groups=confidence.interval.groups)

                if (save.intermediate.results) save(sgp_object, file="sgp_object.Rdata")
	}


	### visualizeSGP ###

	if ("visualizeSGP" %in% steps) {

		visualizeSGP(
			sgp_object=sgp_object,
			plot.types=plot.types,
			state=state,
			bPlot.years=years,
			sgPlot.years=years,
			sgPlot.demo.report=TRUE,
			gaPlot.years=years,
			bPlot.content_areas=content_areas,
			gaPlot.content_areas=content_areas)
	}

        message(paste("Finished abcSGP", date(), "in", timetaken(started.at), "\n"))
	return(sgp_object)
} ## END abcSGP Function

