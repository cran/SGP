`summarizeSGP` <- function(
	sgp_object,
	state=NULL,
	years=NULL,
	content_areas=NULL,
	sgp.summaries=NULL,
	summary.groups=NULL,
	confidence.interval.groups=NULL,
	produce.all.summary.tables=FALSE,
	summarizeSGP.baseline=NULL,
	projection.years.for.target=NULL,
	save.old.summaries=FALSE,
	highest.level.summary.grouping="STATE",
	parallel.config=NULL) {

	started.at <- proc.time()
	messageSGP(paste("\nStarted summarizeSGP", prettyDate()), "\n")
	messageSGP(match.call())

	### Set variables to NULL to prevent R CMD check warnings
	SIM_NUM <- tmp.simulation.dt <- variable <- WEIGHT <- ENROLLMENT_STATUS <- names.type <- names.sgp <- names.output <- BY_GROWTH_ONLY <- VALID_CASE <- YEAR_WITHIN <- NULL
	tmp.simulation.dt <- variable <- WEIGHT <- ENROLLMENT_STATUS <- names.type <- names.sgp <- names.output <- BY_GROWTH_ONLY <- VALID_CASE <- YEAR_WITHIN <- DUP_COUNT <- NULL
	CONTENT_AREA <- YEAR <- BASELINE <- NULL


	### Create state (if NULL) from sgp_object (if possible)
	if (is.null(state)) {
		tmp.name <- toupper(gsub("_", " ", deparse(substitute(sgp_object))))
		state <- getStateAbbreviation(tmp.name, "summarizeSGP")
	}

	### Create specific variables
	if (!is.null(SGP::SGPstateData[[state]][["SGP_Configuration"]][["highest.level.summary.grouping"]])) {
		highest.level.summary.grouping <- SGP::SGPstateData[[state]][["SGP_Configuration"]][["highest.level.summary.grouping"]]
	}

	sgp.data.names <- names(sgp_object@Data)
	sgp_key <- getKey(sgp_object)

	### Export/Overwrite old summaries
	if (!is.null(sgp_object@Summary)) {
		if (save.old.summaries) {
			tmp.index <- grep("YEAR", names(sgp_object@Summary[[highest.level.summary.grouping]]))[1]
			tmp.year <- tail(sort(sgp_object@Summary[[highest.level.summary.grouping]][[tmp.index]][['YEAR']]), 1)
			tmp.state.name <- gsub(" ", "_", getStateAbbreviation(state, type="name"))
			tmp.file.name <- paste(tmp.state.name, "SGP_Summary", tmp.year, sep="_")
			assign(tmp.file.name, sgp_object@Summary)
			if (!"Data" %in% list.files()) dir.create("Data", showWarnings=FALSE)
			messageSGP(paste("\tNOTE: Saving @Summary slot to", paste0("Data/", tmp.file.name, ".Rdata and then deleting @Summary slot.")))
			save(list=tmp.file.name, file=file.path("Data", paste(tmp.file.name, "Rdata", sep=".")))
			sgp_object@Summary <- NULL
		} else {
			messageSGP("\tNOTE: Deleting @Summary slot")
			sgp_object@Summary <- NULL
		}
	}


	### Establish project.years.for.target
	if (!is.null(SGP::SGPstateData[[state]][['SGP_Configuration']][['projection.years.for.target']])) {
		projection.years.for.target <- max(SGP::SGPstateData[[state]][['SGP_Configuration']][['max.sgp.target.years.forward']])
	}
	if (is.null(projection.years.for.target)) {
		if (length(tmp.targets <- grep("CATCH_UP_KEEP_UP_STATUS_[123456789]", names(sgp_object@Data), value=TRUE))==0) {
			projection.years.for.target <- NA
		} else {
			projection.years.for.target <- max(sapply(strsplit(gsub("_YEAR", "", tmp.targets), "_"), tail, 1), na.rm=TRUE)
		}
	}

	### define summarizeSGP.baseline
	if (is.null(summarizeSGP.baseline)) {
		summarizeSGP.baseline <- FALSE ## Default to cohort referenced is not set by user
		if (SGP::SGPstateData[[state]][["Growth"]][["System_Type"]] == "Cohort Referenced") summarizeSGP.baseline <- FALSE
		if (SGP::SGPstateData[[state]][["Growth"]][["System_Type"]] == "Baseline Referenced" & "SGP_BASELINE" %in% sgp.data.names) summarizeSGP.baseline <- TRUE
		if (SGP::SGPstateData[[state]][["Growth"]][["System_Type"]] == "Cohort and Baseline Referenced" & "SGP_BASELINE" %in% sgp.data.names) summarizeSGP.baseline <- TRUE
	}

	if (summarizeSGP.baseline) {
		my.sgp <- intersect(sgp.data.names, c("SGP", "SGP_BASELINE", "SGP_SIMEX", "SGP_SIMEX_RANKED", "SGP_SIMEX_BASELINE", "SGP_SIMEX_BASELINE_RANKED", "SGP_EQUATED"))
	} else {
		my.sgp <- intersect(sgp.data.names, c("SGP", "SGP_SIMEX", "SGP_SIMEX_RANKED", "SGP_EQUATED"))
	}

	my.sgp.target <- paste("SGP_TARGET", projection.years.for.target, "YEAR", sep="_")
	my.cuku.target <- paste("CATCH_UP_KEEP_UP_STATUS", projection.years.for.target, "YEAR", sep="_")
	my.sgp.target.musu <- paste("SGP_TARGET_MOVE_UP_STAY_UP", projection.years.for.target, "YEAR", sep="_")
	my.musu.target <- paste("MOVE_UP_STAY_UP_STATUS", projection.years.for.target, "YEAR", sep="_")
	my.sgp.target.baseline <- paste("SGP_TARGET_BASELINE", projection.years.for.target, "YEAR", sep="_")
	my.cuku.target.baseline <- paste("CATCH_UP_KEEP_UP_STATUS_BASELINE", projection.years.for.target, "YEAR", sep="_")
	my.sgp.target.musu.baseline <- paste("SGP_TARGET_BASELINE_MOVE_UP_STAY_UP", projection.years.for.target, "YEAR", sep="_")
	my.musu.target.baseline <- paste("MOVE_UP_STAY_UP_STATUS_BASELINE", projection.years.for.target, "YEAR", sep="_")

	if (!my.sgp.target %in% sgp.data.names) my.sgp.target <- NULL
	if (!my.cuku.target %in% sgp.data.names) my.cuku.target <- NULL
	if (!my.sgp.target.musu %in% sgp.data.names) my.sgp.target.musu <- NULL
	if (!my.musu.target %in% sgp.data.names) my.musu.target <- NULL
	if (!my.sgp.target.baseline %in% sgp.data.names) my.sgp.target.baseline <- NULL
	if (!my.cuku.target.baseline %in% sgp.data.names) my.cuku.target.baseline <- NULL
	if (!my.sgp.target.musu.baseline %in% sgp.data.names) my.sgp.target.musu.baseline <- NULL
	if (!my.musu.target.baseline %in% sgp.data.names) my.musu.target.baseline <- NULL

	if (missing(sgp_object)) {
		stop("User must supply an SGP object containing a @Data slot with long data. See documentation for details.")
	}


	###
	## Utility Functions
	###

	"%w/o%" <- function(x, y) x[!x %in% y]

	getFromNames <- function(x) {
		tmp.names <- sgp_object@Names[!is.na(sgp_object@Names$names.type),]
		return(tmp.names[tmp.names$names.type==x, "names.sgp"])
	}

	group.format <- function(my.group, add.missing=TRUE) {
		if (is.null(my.group) & add.missing) {
			return(c(""))
		}
		if (is.null(my.group) & !add.missing) {
			return(NULL)
		}
		if (!is.null(my.group) & add.missing) {
			return(c("", unlist(lapply(my.group, function(x) paste0(", ", x)))))
		}
		if (!is.null(my.group) & !add.missing) {
			return(unlist(lapply(my.group, function(x) paste0(", ", x))))
		}
	}

	combineSims <- function(sgp_object) {
		tmp.list <- list()
		for (i in names(sgp_object@SGP[["Simulated_SGPs"]])) {
			if (length(grep("BASELINE", i) > 0)) tmp.baseline <- "BASELINE" else tmp.baseline <- "COHORT"
			columns.to.omit <- -(grep("ID|GRADE|YEAR_WITHIN", names(sgp_object@SGP[["Simulated_SGPs"]][[i]])))
			tmp.list[[i]] <- data.table(
				ID=sgp_object@SGP[["Simulated_SGPs"]][[i]][["ID"]],
				GRADE=sgp_object@SGP[["Simulated_SGPs"]][[i]][["GRADE"]],
				SGP_SIM=c(as.matrix(sgp_object@SGP[["Simulated_SGPs"]][[i]][,columns.to.omit, with=FALSE])))[,
					CONTENT_AREA:=unlist(strsplit(i, "[.]"))[1]][,
					YEAR:=unlist(strsplit(i, "[.]"))[2]][,
					BASELINE:=tmp.baseline]
			if ("YEAR_WITHIN" %in% names(sgp_object@SGP[["Simulated_SGPs"]][[i]])) {
				tmp.list[[i]][,YEAR_WITHIN:=rep(sgp_object@SGP[["Simulated_SGPs"]][[i]][["YEAR_WITHIN"]], each=length(grep("SGP_SIM", names(sgp_object@SGP[["Simulated_SGPs"]][[i]]))))]
			}
		}
		return(data.table(rbindlist(tmp.list), VALID_CASE="VALID_CASE", key=sgp_key))
	}

	summarizeSGP.config <- function(sgp_object, config.type) {

		if (config.type=="sgp.summaries") {
			if (!is.null(SGP::SGPstateData[[state]][["Assessment_Program_Information"]][["Assessment_Transition"]])) {
				tmp.achievement.levels <- SGP::SGPstateData[[state]][["Assessment_Program_Information"]][["Assessment_Transition"]][
                         grep("Achievement_Levels", names(SGP::SGPstateData[[state]][["Assessment_Program_Information"]][["Assessment_Transition"]]))]
				all.achievement.levels <- unlist(lapply(tmp.achievement.levels, function(x) x[['Labels']]), use.names=FALSE)
				proficient.achievement.levels <- all.achievement.levels[unlist(lapply(tmp.achievement.levels, function(x) x[['Proficient']]=="Proficient"))]
			} else {
				all.achievement.levels <- SGP::SGPstateData[[state]][["Achievement"]][["Levels"]][[1]][!is.na(SGP::SGPstateData[[state]][["Achievement"]][["Levels"]][[2]])]
				proficient.achievement.levels <- SGP::SGPstateData[[state]][["Achievement"]][["Levels"]][[1]][!is.na(SGP::SGPstateData[[state]][["Achievement"]][["Levels"]][[2]]) &
					SGP::SGPstateData[[state]][["Achievement"]][["Levels"]][[2]]=="Proficient"]
			}

			get.expression <- function(character.vector) {
				if (length(character.vector)==0) {
					return(NULL)
				} else {
					paste0("c(", paste0("'", paste(character.vector, collapse="', '"), "'"), ")")
				}
			}

			tmp.sgp.summaries <- list(
				MEAN_SGP="mean_na(SGP, WEIGHT)",
				MEDIAN_SGP="median_na(SGP, WEIGHT)",
				MEDIAN_SGP_COUNT="num_non_missing(SGP)",
				PERCENT_AT_ABOVE_PROFICIENT=paste0("percent_in_category(ACHIEVEMENT_LEVEL, ",
					get.expression(proficient.achievement.levels), ", ", get.expression(all.achievement.levels), ")"),
				PERCENT_AT_ABOVE_PROFICIENT_COUNT="num_non_missing(ACHIEVEMENT_LEVEL)",
				MEAN_SGP_STANDARD_ERROR="sgp_standard_error(SGP)",
				MEDIAN_SGP_STANDARD_ERROR="sgp_standard_error(SGP, 1.253)")

				if (summarizeSGP.baseline) {
					tmp.sgp.summaries <- c(
						tmp.sgp.summaries,
						MEAN_SGP_BASELINE="mean_na(SGP_BASELINE, WEIGHT)",
						MEDIAN_SGP_BASELINE="median_na(SGP_BASELINE, WEIGHT)",
						MEAN_SGP_BASELINE_STANDARD_ERROR="sgp_standard_error(SGP_BASELINE)",
						MEDIAN_SGP_BASELINE_STANDARD_ERROR="sgp_standard_error(SGP_BASELINE, 1.253)"
					)
				}

				if ("ACHIEVEMENT_LEVEL_PRIOR" %in% sgp.data.names) {
					tmp.sgp.summaries <- c(
						tmp.sgp.summaries,
						PERCENT_AT_ABOVE_PROFICIENT_PRIOR=paste0("percent_in_category(ACHIEVEMENT_LEVEL_PRIOR, ",
							get.expression(proficient.achievement.levels), ", ", get.expression(all.achievement.levels), ")"),
						PERCENT_AT_ABOVE_PROFICIENT_PRIOR_COUNT="num_non_missing(ACHIEVEMENT_LEVEL_PRIOR)"
					)
				}

				if ("SCALE_SCORE_PRIOR_STANDARDIZED" %in% sgp.data.names) {
					tmp.sgp.summaries <- c(
						tmp.sgp.summaries,
						MEAN_SCALE_SCORE_PRIOR_STANDARDIZED="mean_na(SCALE_SCORE_PRIOR_STANDARDIZED, WEIGHT)",
						SD_SCALE_SCORE_PRIOR_STANDARDIZED="sd_na(SCALE_SCORE_PRIOR_STANDARDIZED)"
					)
				}

				if ("SGP_SIMEX" %in% sgp.data.names) {
					tmp.sgp.summaries <- c(
						tmp.sgp.summaries,
						MEDIAN_SGP_SIMEX="median_na(SGP_SIMEX, WEIGHT)",
						MEAN_SGP_SIMEX="mean_na(SGP_SIMEX, WEIGHT)",
						MEAN_SGP_SIMEX_STANDARD_ERROR="sgp_standard_error(SGP_SIMEX)",
						MEDIAN_SGP_SIMEX_STANDARD_ERROR="sgp_standard_error(SGP_SIMEX, 1.253)"
					)
				}

				if ("SGP_SIMEX_RANKED" %in% sgp.data.names) {
					tmp.sgp.summaries <- c(
						tmp.sgp.summaries,
						MEDIAN_SGP_SIMEX_RANKED="median_na(SGP_SIMEX_RANKED, WEIGHT)",
						MEAN_SGP_SIMEX_RANKED="mean_na(SGP_SIMEX_RANKED, WEIGHT)",
						MEAN_SGP_SIMEX_RANKED_STANDARD_ERROR="sgp_standard_error(SGP_SIMEX_RANKED)",
						MEDIAN_SGP_SIMEX_RANKED_STANDARD_ERROR="sgp_standard_error(SGP_SIMEX_RANKED, 1.253)"
					)
				}

				if ("SGP_SIMEX_BASELINE" %in% sgp.data.names) {
					tmp.sgp.summaries <- c(
						tmp.sgp.summaries,
						MEDIAN_SGP_SIMEX_BASELINE="median_na(SGP_SIMEX_BASELINE, WEIGHT)",
						MEAN_SGP_SIMEX_BASELINE="mean_na(SGP_SIMEX_BASELINE, WEIGHT)",
						MEAN_SGP_SIMEX_BASELINE_STANDARD_ERROR="sgp_standard_error(SGP_SIMEX_BASELINE)",
						MEDIAN_SGP_SIMEX_BASELINE_STANDARD_ERROR="sgp_standard_error(SGP_SIMEX_BASELINE, 1.253)"
					)
				}

				if ("SGP_SIMEX_BASELINE_RANKED" %in% sgp.data.names) {
					tmp.sgp.summaries <- c(
						tmp.sgp.summaries,
						MEDIAN_SGP_SIMEX_BASELINE_RANKED="median_na(SGP_SIMEX_BASELINE_RANKED, WEIGHT)",
						MEAN_SGP_SIMEX_BASELINE_RANKED="mean_na(SGP_SIMEX_BASELINE_RANKED, WEIGHT)",
						MEAN_SGP_SIMEX_BASELINE_RANKED_STANDARD_ERROR="sgp_standard_error(SGP_SIMEX_BASELINE_RANKED)",
						MEDIAN_SGP_SIMEX_BASELINE_RANKED_STANDARD_ERROR="sgp_standard_error(SGP_SIMEX_BASELINE_RANKED, 1.253)"
					)
				}

				if (!is.null(my.sgp.target)) {
					tmp.sgp.summaries <- c(
						tmp.sgp.summaries,
						M1=paste0("median_na(", my.sgp.target, ", WEIGHT)"),
						M2=paste0("num_non_missing(", my.sgp.target, ")"),
						PERCENT_CATCHING_UP_KEEPING_UP=paste0("percent_in_category(", my.cuku.target, ", c('Catch Up: Yes', 'Keep Up: Yes'), c('Catch Up: Yes', 'Catch Up: No', 'Keep Up: Yes', 'Keep Up: No'))")
					)
					names(tmp.sgp.summaries)[sapply(c("M1", "M2"), function(x) which(names(tmp.sgp.summaries)==x))] <-
						c(paste("MEDIAN", my.sgp.target, sep="_"), paste("MEDIAN", my.sgp.target, "COUNT", sep="_"))
					names(tmp.sgp.summaries)[which(names(tmp.sgp.summaries)=="PERCENT_CATCHING_UP_KEEPING_UP")] <- paste("PERCENT_CATCHING_UP_KEEPING_UP", projection.years.for.target, "YEAR", sep="_")
				}

				if (!is.null(my.sgp.target.baseline)) {
					tmp.sgp.summaries <- c(
						tmp.sgp.summaries,
						M1=paste0("median_na(", my.sgp.target.baseline, ", WEIGHT)"),
						M2=paste0("num_non_missing(", my.sgp.target.baseline, ")"),
						PERCENT_CATCHING_UP_KEEPING_UP_BASELINE=paste0("percent_in_category(", my.cuku.target.baseline, ", c('Catch Up: Yes', 'Keep Up: Yes'), c('Catch Up: Yes', 'Catch Up: No', 'Keep Up: Yes', 'Keep Up: No'))")
					)
					names(tmp.sgp.summaries)[sapply(c("M1", "M2"), function(x) which(names(tmp.sgp.summaries)==x))] <-
						c(paste("MEDIAN", my.sgp.target.baseline, sep="_"), paste("MEDIAN", my.sgp.target.baseline, "COUNT", sep="_"))
					names(tmp.sgp.summaries)[which(names(tmp.sgp.summaries)=="PERCENT_CATCHING_UP_KEEPING_UP_BASELINE")] <- paste("PERCENT_CATCHING_UP_KEEPING_UP_BASELINE", projection.years.for.target, "YEAR", sep="_")
				}

				if (!is.null(my.sgp.target.musu)) {
					tmp.sgp.summaries <- c(
						tmp.sgp.summaries,
						M1=paste0("median_na(", my.sgp.target.musu, ", WEIGHT)"),
						M2=paste0("num_non_missing(", my.sgp.target.musu, ")"),
						PERCENT_MOVING_UP_STAYING_UP=paste0("percent_in_category(", my.musu.target, ", c('Move Up: Yes', 'Stay Up: Yes'), c('Move Up: Yes', 'Move Up: No', 'Stay Up: Yes', 'Stay Up: No'))")
					)
					names(tmp.sgp.summaries)[sapply(c("M1", "M2"), function(x) which(names(tmp.sgp.summaries)==x))] <-
						c(paste("MEDIAN", my.sgp.target.musu, sep="_"), paste("MEDIAN", my.sgp.target.musu, "COUNT", sep="_"))
					names(tmp.sgp.summaries)[which(names(tmp.sgp.summaries)=="PERCENT_MOVING_UP_STAYING_UP")] <- paste("PERCENT_MOVING_UP_STAYING_UP", projection.years.for.target, "YEAR", sep="_")
				}

				if (!is.null(my.sgp.target.musu.baseline)) {
					tmp.sgp.summaries <- c(
						tmp.sgp.summaries,
						M1=paste0("median_na(", my.sgp.target.musu.baseline, ", WEIGHT)"),
						M2=paste0("num_non_missing(", my.sgp.target.musu.baseline, ")"),
						PERCENT_MOVING_UP_STAYING_UP_BASELINE=paste0("percent_in_category(", my.musu.target.baseline, ", c('Move Up: Yes', 'Stay Up: Yes'), c('Move Up: Yes', 'Move Up: No', 'Stay Up: Yes', 'Stay Up: No'))")
					)
					names(tmp.sgp.summaries)[sapply(c("M1", "M2"), function(x) which(names(tmp.sgp.summaries)==x))] <-
						c(paste("MEDIAN", my.sgp.target.musu.baseline, sep="_"), paste("MEDIAN", my.sgp.target.musu.baseline, "COUNT", sep="_"))
					names(tmp.sgp.summaries)[which(names(tmp.sgp.summaries)=="PERCENT_MOVING_UP_STAYING_UP_BASELINE")] <- paste("PERCENT_MOVING_UP_STAYING_UP_BASELINE", projection.years.for.target, "YEAR", sep="_")
				}

			return(tmp.sgp.summaries)
		}

		if (config.type=="summary.groups") {
			tmp.summary.groups <- list(
				institution=c(highest.level.summary.grouping, getFromNames("institution")),
				institution_type=getFromNames("institution_type"),
				content=getFromNames("content"),
				time=getFromNames("time"),
				institution_level=getFromNames("institution_level"),
				demographic=intersect(c(getFromNames("demographic"), "CATCH_UP_KEEP_UP_STATUS", "MOVE_UP_STAY_UP_STATUS", "ACHIEVEMENT_LEVEL_PRIOR", "HIGH_NEED_STATUS"), sgp.data.names),
				institution_multiple_membership=get.multiple.membership(sgp_object))

			tmp.summary.groups[["institution_inclusion"]] <- vector(mode="list", length=length(tmp.summary.groups[["institution"]]))
			names(tmp.summary.groups[["institution_inclusion"]]) <- tmp.summary.groups[["institution"]]
			for (i in tmp.summary.groups[["institution"]]) {
				tmp.split <- paste(c(unlist(strsplit(i, "_"))[!unlist(strsplit(i, "_"))=="NUMBER"], "ENROLLMENT_STATUS"), collapse="_")
				if (tmp.split %in% getFromNames("institution_inclusion")) {
					tmp.summary.groups[["institution_inclusion"]][[i]] <- tmp.split
				}
				tmp.summary.groups[["growth_only_summary"]][[i]] <- "BY_GROWTH_ONLY"
			}
			tmp.summary.groups[["institution_inclusion"]] <- as.list(tmp.summary.groups[["institution_inclusion"]])
			tmp.summary.groups[["growth_only_summary"]] <- as.list(tmp.summary.groups[["growth_only_summary"]])

			return(tmp.summary.groups)
		}

		if (config.type=="confidence.interval.groups") {
			if (is.null(names(sgp_object@SGP[['Simulated_SGPs']]))) tmp.type <- c("Bootstrap_CI", "Bootstrap_SE") else tmp.type <- c("Bootstrap_CI", "Bootstrap_SE", "CSEM")
			tmp.confidence.interval.groups <- list(
				TYPE=tmp.type,
				VARIABLES=my.sgp,
				QUANTILES=c(0.025, 0.975),
				GROUPS=list(
					institution=c("SCHOOL_NUMBER", "STATE, INSTRUCTOR_NUMBER"),
					institution_type="EMH_LEVEL",
					content="CONTENT_AREA",
					time="YEAR",
					institution_level=NULL,
					demographic=NULL,
					institution_multiple_membership=get.multiple.membership(sgp_object)))

			tmp.confidence.interval.groups[["GROUPS"]][["institution_inclusion"]] <- vector(mode="list", length=length(tmp.confidence.interval.groups[["GROUPS"]][["institution"]]))
			names(tmp.confidence.interval.groups[["GROUPS"]][["institution_inclusion"]]) <- tmp.confidence.interval.groups[["GROUPS"]][["institution"]]
			for (i in tmp.confidence.interval.groups[["GROUPS"]][["institution"]]) {
				tmp.split <- paste(c(unlist(strsplit(i, "_"))[!unlist(strsplit(i, "_"))=="NUMBER"], "ENROLLMENT_STATUS"), collapse="_")
				if (tmp.split %in% getFromNames("institution_inclusion")) {
					tmp.confidence.interval.groups[["GROUPS"]][["institution_inclusion"]][[i]] <- tmp.split
				}
				tmp.confidence.interval.groups[["GROUPS"]][["growth_only_summary"]][[i]] <- "BY_GROWTH_ONLY"
			}
			tmp.confidence.interval.groups[["GROUPS"]][["institution_inclusion"]] <- as.list(tmp.confidence.interval.groups[["GROUPS"]][["institution_inclusion"]])
			tmp.confidence.interval.groups[["GROUPS"]][["growth_only_summary"]] <- as.list(tmp.confidence.interval.groups[["GROUPS"]][["growth_only_summary"]])

			return(tmp.confidence.interval.groups)
		}
	} ### END summarizeSGP.config

	get.multiple.membership <- function(sgp_object) {
		tmp.names <- list()
		if (is.null(sgp_object@Data_Supplementary)) {
			tmp.names <- NULL
		} else {
			for (i in seq_along(sgp_object@Data_Supplementary)) {
				tmp.variable.names <- names(sgp_object@Data_Supplementary)[i]
				tmp.weights <- grep("WEIGHT", names(sgp_object@Data_Supplementary[[i]]), value=TRUE)
				tmp.inclusion <- grep("ENROLLMENT_STATUS", names(sgp_object@Data_Supplementary[[i]]), value=TRUE)

				if (length(tmp.weights) == 0) {
					tmp.weights <- NULL
				}
				if (length(tmp.weights) > 1) {
					stop(paste("\tNOTE: Number of 'WEIGHT' variables in the '@Data_Supplementary' table", tmp.variable.names, "exceeds 1. Only 1 'WEIGHT' variable allowed."))
				}

				if (length(tmp.inclusion) == 0) {
					tmp.inclusion <- NULL
				}
				if (length(tmp.inclusion) > 1) {
					stop(paste("\tNOTE: Number of 'ENROLLMENT_STATUS' variables in the '@Data_Supplementary' table", tmp.variable.names, "exceeds 1. Only 1 'ENROLLMENT_STATUS' variable allowed."))
				}

				tmp.names[[i]] <- list(VARIABLE.NAME=tmp.variable.names, WEIGHTS=tmp.weights, ENROLLMENT_STATUS=tmp.inclusion)
			}
		}
		return(tmp.names)
	} ### END get.multiple.membership

	summarizeSGP_INTERNAL <- function(i) {

		tmp.summary <- list()

		### Create summary tables
		sgp.groups <- do.call(paste0, expand.grid(i,
			group.format(summary.groups[["institution_type"]]),
			group.format(summary.groups[["content"]]),
			group.format(summary.groups[["time"]]),
			group.format(summary.groups[["institution_level"]]),
			group.format(summary.groups[["demographic"]]),
			group.format(summary.groups[["institution_inclusion"]][[i]]),
			group.format(summary.groups[["growth_only_summary"]][[i]])))

		if (!produce.all.summary.tables) {
			sgp.groups <- intersect(sgp.groups, selected.summary.tables)
			if (length(sgp.groups)==0) return(NULL)
		}

		if (!is.null(confidence.interval.groups[["GROUPS"]]) & i %in% confidence.interval.groups[["GROUPS"]][["institution"]]) {
			ci.groups <- do.call(paste0, expand.grid(i,
				group.format(confidence.interval.groups[["GROUPS"]][["institution_type"]]),
				group.format(confidence.interval.groups[["GROUPS"]][["content"]]),
				group.format(confidence.interval.groups[["GROUPS"]][["time"]]),
				group.format(confidence.interval.groups[["GROUPS"]][["institution_level"]]),
				group.format(confidence.interval.groups[["GROUPS"]][["demographic"]]),
				group.format(confidence.interval.groups[["GROUPS"]][["institution_inclusion"]][[i]]),
				group.format(confidence.interval.groups[["GROUPS"]][["growth_only_summary"]][[i]])))

			if (!produce.all.summary.tables) ci.groups <- intersect(ci.groups, selected.summary.tables)
		}

		if (!is.null(confidence.interval.groups[["GROUPS"]]) & i %in% confidence.interval.groups[["GROUPS"]][["institution"]]) {
			summary.iter <- lapply(1:length(sgp.groups), function(x) c(sgp.groups[x], sgp.groups[x] %in% ci.groups))
		} else summary.iter <- lapply(1:length(sgp.groups), function(x) c(sgp.groups[x], FALSE))

		db.path = file.path(tempdir(), "TMP_Summary_Data.sqlite")

		## if NULL parallel.config
		if (is.null(parallel.config)) {
			for (s in seq_along(summary.iter)) {
				tmp.summary[[s]] <- sgpSummary(summary.iter[[s]][1], eval(parse(text=summary.iter[[s]][2])),
					tmp.simulation.dt, state, sgp.summaries, confidence.interval.groups, my.sgp, sgp_key, variables.for.summaries, sim.info, db.path)
			}
		}

		j <- k <- NULL ## To prevent R CMD check warnings

		if (identical(parallel.config[["BACKEND"]], "FOREACH")) {
			if (!is.null(confidence.interval.groups[["GROUPS"]]) & i %in% confidence.interval.groups[["GROUPS"]][["institution"]]) {
				k.iter <- iter(sgp.groups %in% ci.groups)
			} else	k.iter <- iter(rep(FALSE, length(sgp.groups)))

			tmp.summary <- foreach(j = iter(sgp.groups), k = k.iter,
				.export=c("tmp.simulation.dt", "state", "sgp.summaries", "confidence.interval.groups", "my.sgp", "sgp_key", "variables.for.summaries", "sim.info"),
				# "sgpSummary", "pullData", "median_na", "mean_na", "sd_na", "num_non_missing", "sgp_standard_error", "percent_in_category", "boot.sgp"),
				.options.multicore=list(preschedule = FALSE, set.seed = FALSE), .packages="SGP", .inorder=FALSE, .errorhandling = "pass") %dopar% {
					return(sgpSummary(j, k, tmp.simulation.dt, state, sgp.summaries, confidence.interval.groups, my.sgp, sgp_key, variables.for.summaries, sim.info, db.path))
			}
		} else { # END FOREACH flavor


		### SNOW and MULTICORE
		# Don't use SNOW - run as doParallel instead.  Too much memory & time overhead, errors, etc....
		# if (identical(par.start[['par.type']], "SNOW")) {
		# 	tmp.summary <- parLapply(par.start$internal.cl, summary.iter,
		# 		function(iter) sgpSummary(iter[1], eval(parse(text=iter[2])), tmp.simulation.dt, state, sgp.summaries, confidence.interval.groups, my.sgp, sgp_key, variables.for.summaries, sim.info, db.path))
		# } # END 'SNOW' Flavor
		if (identical(par.start[['par.type']], "MULTICORE")) {
			tmp.summary <- mclapply(summary.iter,
				function(iter) sgpSummary(iter[1], eval(parse(text=iter[2])), tmp.simulation.dt, state, sgp.summaries, confidence.interval.groups, my.sgp, sgp_key, variables.for.summaries, sim.info, db.path), mc.cores=par.start$workers, mc.preschedule=FALSE)
		} # END 'MULTICORE' Flavor
		} # END else not FOREACH

		setattr(tmp.summary, 'names', gsub(', ', '__', sgp.groups))
		return(tmp.summary)
	} ### END summarizeSGP_INTERNAL


	###################################################################################
	###
	### Setup arguments for call to summarizeSGP_INTERNAL
	###
	###################################################################################

	if (is.null(content_areas)) {
		content_areas <- unique(sgp_object@Data["VALID_CASE"], by='CONTENT_AREA')[['CONTENT_AREA']]
	}

	if (is.null(SGP::SGPstateData[[state]][["SGP_Configuration"]][["state.multiple.year.summary"]])) {
		state.multiple.year.summary <- 5
	} else {
		state.multiple.year.summary <- SGP::SGPstateData[[state]][["SGP_Configuration"]][["state.multiple.year.summary"]]
	}
	tmp.years <- list()
	if (is.null(years)) {
		for (i in content_areas) {
			tmp.years[[i]] <- tail(sort(unique(sgp_object@Data[SJ("VALID_CASE", i)], by='YEAR')[['YEAR']]), state.multiple.year.summary)
		}
	} else {
		if (!is.list(years)) {
			for (i in content_areas) {
				tmp.years[[i]] <- tail(sort(unique(sgp_object@Data[SJ("VALID_CASE", i)], by='YEAR')[['YEAR']])[
					seq(which(sort(unique(sgp_object@Data[SJ("VALID_CASE", i)], by='YEAR')[['YEAR']])==tail(sort(years), 1)))], state.multiple.year.summary)
			}
		} else {
			if (!all(content_areas %in% names(years))) {
				stop("Supplied list of years does not contain all content areas specified for summarizeSGP.")
			} else {
				tmp.years <- years[content_areas]
			}
		}
	}

	for (i in names(tmp.years)) {
		tmp.years[[i]] <- data.table(CONTENT_AREA=i, YEAR=tmp.years[[i]])
	}
	content_areas.by.years <- rbindlist(tmp.years)

	if (is.null(sgp.summaries)) sgp.summaries <- summarizeSGP.config(sgp_object, "sgp.summaries")
	if (is.null(summary.groups)) summary.groups <- summarizeSGP.config(sgp_object, "summary.groups")
	if (is.null(confidence.interval.groups)) confidence.interval.groups <- summarizeSGP.config(sgp_object, "confidence.interval.groups")

	variables.for.summaries <- intersect(
		c(sgp_key, my.sgp, my.sgp.target, my.sgp.target.baseline, my.sgp.target.musu, my.sgp.target.musu.baseline,
		"ACHIEVEMENT_LEVEL", "ACHIEVEMENT_LEVEL_PRIOR",
		my.cuku.target, my.musu.target, my.cuku.target.baseline, my.musu.target.baseline,
		"SCALE_SCORE_PRIOR_STANDARDIZED", "SGP_SIMEX", "SGP_SIMEX_RANKED", "SGP_SIMEX_BASELINE", "SGP_SIMEX_BASELINE_RANKED",
		unique(as.character(unlist(summary.groups))),
		"YEAR_WITHIN"), sgp.data.names)


	### Define demographic subgroups and tables that will be calculated from all possible created by expand.grid

	selected.demographic.subgroups <- intersect(
		c(getFromNames("demographic"), "CATCH_UP_KEEP_UP_STATUS", "MOVE_UP_STAY_UP_STATUS", "CATCH_UP_KEEP_UP_STATUS_BASELINE", "MOVE_UP_STAY_UP_STATUS_BASELINE",
			"ACHIEVEMENT_LEVEL_PRIOR", "HIGH_NEED_STATUS"), sgp.data.names)
	if (is.null(SGP::SGPstateData[[state]][["Variable_Name_Lookup"]])) {
		selected.institution.types <- c("STATE", "DISTRICT_NUMBER", "SCHOOL_NUMBER")
	} else {
		selected.institution.types <- c(highest.level.summary.grouping, getFromNames("institution"))
	}
	selected.institution.types <- c(selected.institution.types, paste(selected.institution.types[grep("CURRENT", selected.institution.types, invert=TRUE)], "INSTRUCTOR_NUMBER", sep=", "))
	if ("SCHOOL_NUMBER_INSTRUCTOR" %in% names(sgp_object@Data_Supplementary[['INSTRUCTOR_NUMBER']])) {
		selected.institution.types <- c(selected.institution.types, "SCHOOL_NUMBER_INSTRUCTOR, INSTRUCTOR_NUMBER")
		summary.groups[["institution"]] <- c(summary.groups[["institution"]], "SCHOOL_NUMBER_INSTRUCTOR")
	}

	selected.summary.tables <- list()
	for (k in selected.institution.types) {
		if (length(grep("INSTRUCTOR_NUMBER", k)) > 0 | length(grep("CURRENT", k)) > 0) {
			if (length(grep("CURRENT", k)) > 0 | !"INSTRUCTOR_ENROLLMENT_STATUS" %in% names(sgp_object@Data_Supplementary[['INSTRUCTOR_NUMBER']])) {
				ENROLLMENT_STATUS_ARGUMENT <- NULL; ADD_MISSING_ARGUMENT <- TRUE
			}
			if ("INSTRUCTOR_ENROLLMENT_STATUS" %in% names(sgp_object@Data_Supplementary[['INSTRUCTOR_NUMBER']])) {
				ENROLLMENT_STATUS_ARGUMENT <- "INSTRUCTOR_ENROLLMENT_STATUS"; ADD_MISSING_ARGUMENT <- FALSE
			}

			if (length(grep("SCHOOL", k)) > 0) {
				selected.summary.tables[[k]] <- do.call(paste0,
					expand.grid(k,
						group.format("EMH_LEVEL"),
						group.format("CONTENT_AREA"),
						group.format("YEAR"),
						group.format("GRADE"),
						group.format(ENROLLMENT_STATUS_ARGUMENT, ADD_MISSING_ARGUMENT)))
			} else {
				selected.summary.tables[[k]] <- do.call(paste0,
					expand.grid(k,
						group.format("CONTENT_AREA"),
						group.format("YEAR"),
						group.format("GRADE"),
						group.format(ENROLLMENT_STATUS_ARGUMENT, ADD_MISSING_ARGUMENT)))
			}
		} else {
			if (is.null(summary.groups[["institution_inclusion"]][[k]])) {
				ENROLLMENT_STATUS_ARGUMENT <- NULL
				ADD_MISSING_ARGUMENT <- TRUE
			} else {
				ENROLLMENT_STATUS_ARGUMENT <- summary.groups[["institution_inclusion"]][[k]]
				ADD_MISSING_ARGUMENT <- FALSE
			}

			if (length(grep("SCHOOL", k)) > 0 & !is.null(summary.groups[["institution_inclusion"]][[k]])) {
				selected.summary.tables[[k]] <- do.call(paste0,
					expand.grid(k,
						group.format("EMH_LEVEL"),
						group.format("CONTENT_AREA"),
						group.format("YEAR"),
						group.format("GRADE"),
						group.format(selected.demographic.subgroups),
						group.format(ENROLLMENT_STATUS_ARGUMENT, ADD_MISSING_ARGUMENT)))
			} else {
				selected.summary.tables[[k]] <- do.call(paste0,
					expand.grid(k,
						group.format("CONTENT_AREA"),
						group.format("YEAR"),
						group.format("GRADE"),
						group.format(selected.demographic.subgroups),
						group.format(ENROLLMENT_STATUS_ARGUMENT, ADD_MISSING_ARGUMENT)))
			}
		}
	} ### End for k
	selected.summary.tables <- unlist(selected.summary.tables, use.names=FALSE)

	##############################################################
	###
	### Data prep and calculation of summary tables
	###
	##############################################################

	### Loop and send to summarizeSGP_INTERNAL

	sgp_data_for_summary <- dbConnect(SQLite(), dbname = file.path(tempdir(), "TMP_Summary_Data.sqlite"))

	if ("VALID_CASE_STATUS_ONLY" %in% names(sgp_object@Data)) {
		sgp_object@Data$VALID_CASE[sgp_object@Data$VALID_CASE_STATUS_ONLY=="VALID_CASE"] <- "VALID_CASE"
	}

	setkeyv(sgp_object@Data, sgp_key %w/o% "GRADE") #  Set to old SGP key so that it still subsets data correctly with content_areas.by.years lookup table

	tmp.dt <- sgp_object@Data[data.table("VALID_CASE", content_areas.by.years), nomatch=0][,
		variables.for.summaries, with=FALSE][, (highest.level.summary.grouping):=state]

	if (any(duplicated(tmp.dt, by=sgp_key))) {
		invisible(tmp.dt[, DUP_COUNT := .N, by=sgp_key])
		if (!"WEIGHT" %in% names(tmp.dt)) invisible(tmp.dt[, WEIGHT := 1L])
		invisible(tmp.dt[, WEIGHT := round((WEIGHT / DUP_COUNT), 3)])
	}

	if (any(!sapply(summary.groups[["growth_only_summary"]], is.null))) {
		invisible(tmp.dt[, BY_GROWTH_ONLY := factor(is.na(tmp.dt[[my.sgp[1]]]), levels=c(FALSE, TRUE), labels=c("Students without SGP", "Students with SGP"))])
	}
	dbWriteTable(sgp_data_for_summary, name = "summary_data", overwrite = TRUE, row.names=FALSE, value = tmp.dt)
	rm(tmp.dt)

	if (!is.null(confidence.interval.groups) & "CSEM" %in% confidence.interval.groups[['TYPE']]) {
		sim.info <- list(
			n.simulated.sgps = length(grep("SGP_SIM", names(sgp_object@SGP[["Simulated_SGPs"]][[1]]))),
			n.unique.cases = sum(sapply(sgp_object@SGP[["Simulated_SGPs"]], nrow)))
		if ((!identical(.Platform$OS.type, "unix") & !is.null(parallel.config)) | !is.null(parallel.config[["SNOW_TEST"]])) {
			# Write tmp.simulation.dt to disk for SNOW backends in Windows (& SNOT_TEST) Only
			tmp.simulation.dt <- "sqlite"
			dbWriteTable(sgp_data_for_summary, name = "sim_data", overwrite = TRUE, row.names=FALSE,
				value = combineSims(sgp_object)[,
				  SIM_NUM := rep(1:sim.info[["n.simulated.sgps"]], sim.info[["n.unique.cases"]])
				]
			)
		} else {
			tmp.simulation.dt <- combineSims(sgp_object)
		}
	} else tmp.simulation.dt <- sim.info <- NULL

	# Use FOREACH + doParallel exclusively for Windows/SNOW
	if ((!identical(.Platform$OS.type, "unix") & !is.null(parallel.config)) | !is.null(parallel.config[["SNOW_TEST"]])) {
		parallel.config[["BACKEND"]] <- "FOREACH"
		parallel.config[["TYPE"]] <- "doParallel"
	}
	if (!is.null(parallel.config)) {
		par.start <- startParallel(parallel.config, 'SUMMARY')
	} else {
		par.start <- list(par.type="NONE")
	}

	for (j in seq(length(summary.groups[["institution_multiple_membership"]])+1)) {
		for (i in summary.groups[["institution"]]) {
			if (j == 1) {
				sgp_object@Summary[[i]] <- summarizeSGP_INTERNAL(i)
			}
			if (j > 1) {

				### Create variable name and set up tmp.inst

				multiple.membership.variable.name <- summary.groups[["institution_multiple_membership"]][[j-1]][["VARIABLE.NAME"]]
				tmp.inst <- paste(i, multiple.membership.variable.name, sep=", ")

				if (!is.null(summary.groups[["institution_multiple_membership"]][[j-1]][["ENROLLMENT_STATUS"]])) {
					summary.groups[["institution_inclusion"]][[tmp.inst]] <- summary.groups[["institution_multiple_membership"]][[j-1]][["ENROLLMENT_STATUS"]]
					if (tmp.inst %in% confidence.interval.groups[['GROUPS']][['institution']]) {
						confidence.interval.groups[['GROUPS']][['institution_inclusion']][[tmp.inst]] <-
							summary.groups[["institution_multiple_membership"]][[j-1]][["ENROLLMENT_STATUS"]]
					}
				} else {
					summary.groups[["institution_inclusion"]][[tmp.inst]] <- paste(i, "ENROLLMENT_STATUS", sep="_")
					if (tmp.inst %in% confidence.interval.groups[['GROUPS']][['institution']]) {
						confidence.interval.groups[['GROUPS']][['institution_inclusion']][[tmp.inst]] <-
							paste(i, "ENROLLMENT_STATUS", sep="_")
					}
				}

				summary.groups[["growth_only_summary"]][[tmp.inst]] <- "BY_GROWTH_ONLY" # Do we have an option to NOT include "BY_GROWTH_ONLY"? (would we want this?)

				### Create LONGer data and run summarizeSGP_INTERNAL
				tmp.dt.long <- sgp_object@Data[data.table("VALID_CASE", content_areas.by.years), nomatch=0][,
					variables.for.summaries, with=FALSE][, (highest.level.summary.grouping):=state]
				invisible(tmp.dt.long[, BY_GROWTH_ONLY := factor(is.na(tmp.dt.long[[my.sgp[1]]]), levels=c(FALSE, TRUE), labels=c("Students without SGP", "Students with SGP"))])
				if (adj.weights.tf <- any(duplicated(tmp.dt.long, by=sgp_key))) invisible(tmp.dt.long[, DUP_COUNT := .N, by=sgp_key])
				if ("GRADE" %in% names(sgp_object@Data_Supplementary[[j-1]])) tmp_key <- sgp_key else tmp_key <- sgp_key %w/o% "GRADE"
				tmp.dt.long <- tmp.dt.long[data.table(sgp_object@Data_Supplementary[[j-1]][,VALID_CASE:="VALID_CASE"], key=tmp_key), nomatch=0, on=tmp_key]

				if (!is.null(summary.groups[["institution_multiple_membership"]][[j-1]][["WEIGHTS"]])) {
					setnames(tmp.dt.long, summary.groups[["institution_multiple_membership"]][[j-1]][["WEIGHTS"]], "WEIGHT")
				}

				if (adj.weights.tf & "WEIGHT" %in% names(tmp.dt.long)) invisible(tmp.dt.long[, WEIGHT := round((WEIGHT / DUP_COUNT), 3)])

				dbWriteTable(sgp_data_for_summary, name = "summary_data", overwrite = TRUE, row.names=FALSE, value = tmp.dt.long)

				rm(tmp.dt.long)

				sgp_object@Summary[[i]] <- c(sgp_object@Summary[[i]], summarizeSGP_INTERNAL(tmp.inst))
			}
		} ### End i loop over summary.groups[["institution"]]
	} ### END j loop over multiple membership groups (if they exist)

    dbDisconnect(sgp_data_for_summary)

	if (!is.null(parallel.config))	stopParallel(parallel.config, par.start)

	if ("VALID_CASE_STATUS_ONLY" %in% names(sgp_object@Data)) {
		sgp_object@Data$VALID_CASE[sgp_object@Data$VALID_CASE_STATUS_ONLY=="VALID_CASE"] <- "INVALID_CASE"
		setkeyv(sgp_object@Data, getKey(sgp_object))
	}

	messageSGP(paste("Finished summarizeSGP", prettyDate(), "in", convertTime(timetakenSGP(started.at)), "\n"))

	setkeyv(sgp_object@Data, getKey(sgp_object))
	return(sgp_object)
} ## END summarizeSGP Function
