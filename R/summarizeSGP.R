`summarizeSGP` <- 
function(sgp_object,
	state,
	years,
	content_areas,
	sgp.summaries=list(MEDIAN_SGP="median_na(SGP)",
		MEDIAN_SGP_COUNT="num_non_missing(SGP)",
		PERCENT_AT_ABOVE_PROFICIENT="percent_in_category(ACHIEVEMENT_LEVEL, list(c('Proficient', 'Advanced')), list(c('Unsatisfactory', 'Partially Proficient', 'Proficient', 'Advanced')))",
		PERCENT_AT_ABOVE_PROFICIENT_COUNT="num_non_missing(ACHIEVEMENT_LEVEL)"),
	summary.groups=list(institution=c("STATE", "DISTRICT_NUMBER", "SCHOOL_NUMBER"),
		content="CONTENT_AREA",
		time="YEAR",
		institution_level="GRADE",
		demographic=c("GENDER", "ETHNICITY", "FREE_REDUCED_LUNCH_STATUS", "ELL_STATUS", "IEP_STATUS", "GIFTED_AND_TALENTED_PROGRAM_STATUS", "CATCH_UP_KEEP_UP_STATUS"),
		institution_inclusion=list(STATE="STATE_ENROLLMENT_STATUS", DISTRICT_NUMBER="DISTRICT_ENROLLMENT_STATUS", SCHOOL_NUMBER="SCHOOL_ENROLLMENT_STATUS")),
	confidence.interval.groups=list(institution="SCHOOL_NUMBER",
		content="CONTENT_AREA",
		time="YEAR",
		institution_level= NULL,
		demographic=NULL,
		institution_inclusion=list(STATE=NULL, DISTRICT_NUMBER=NULL, SCHOOL_NUMBER="SCHOOL_ENROLLMENT_STATUS"))) {

        started.at <- proc.time()
        message(paste("Started summarizeSGP", date()))

	if (missing(sgp_object)) {
		stop("User must supply a list containing a Student slot with long data. See documentation for details.")
	}

	## If missing years and content_areas then determine year(s), and content_area(s) for summaries

	if (missing(content_areas)) {
		content_areas <- unique(sgp_object[["Student"]]["VALID_CASE"]$CONTENT_AREA)
		}
	if (missing(years)) {
		for (i in content_areas) {
			years <- sort(tail(unique(sgp_object[["Student"]][J("VALID_CASE", content_areas)]$YEAR), -2), decreasing=TRUE)
		}
	}

	## Functions

	rbind.all <- function(.list, ...){
		if(length(.list)==1) return(.list[[1]])
		Recall(c(list(rbind(.list[[1]], .list[[2]], ...)), .list[-(1:2)]), ...)
	}

	group.format <- function(my.group) {
		if (is.null(my.group)) {
			c("")
		} else {
			c("", unlist(lapply(my.group, function(x) paste(", ", x, sep=""))))
		}
	}

	median_na <- function(x) median(as.numeric(x), na.rm=TRUE)
	mean_na <- function(x, result.digits=1) round(mean(as.numeric(x), na.rm=TRUE), digits=result.digits)
	num_non_missing <- function(x) sum(!is.na(as.numeric(x)))

	percent_in_category <- function(x, in.categories, of.categories, result.digits=1) { ## NOTE: x must be a factor and categories levels
		if (!is.list(in.categories)) in.categories <- list(in.categories)
		if (!is.list(of.categories)) of.categories <- list(of.categories)
		tmp.result <- list()
		tmp <- summary(x[!is.na(x)])
		for (i in seq(length(in.categories))) {
			tmp.result[[i]] <-  round(100*sum(tmp[in.categories[[i]]])/sum(tmp[of.categories[[i]]]), digits=result.digits)
		}
	return(unlist(tmp.result))
	}

	percent_at_above_target <- function(sgp, target, result.digits=1) {
		tmp.logical <- sgp >= target
		tmp.pct <- round(sum(tmp.logical, na.rm=TRUE)/sum(!is.na(tmp.logical))*100, digits=result.digits)
		return(tmp.pct)
	}

	sgpSummary <- function(sgp.groups.to.summarize, confidence.interval.groups.to.summarize) {
  		SGP_SIM <- V1 <- .SD <- NULL  ## To prevent R CMD check warning
		ListExpr <- parse(text=paste("quote(as.list(c(", paste(unlist(sgp.summaries), collapse=", "),")))",sep=""))
		ByExpr <- parse(text=paste("quote(list(", paste(sgp.groups.to.summarize, collapse=", "), "))", sep=""))
		tmp <- tmp.dt[, eval(eval(ListExpr)), by=eval(eval(ByExpr))]
		names(tmp)[-seq(length(unlist(strsplit(as.character(sgp.groups.to.summarize), ", "))))] <- unlist(strsplit(names(sgp.summaries), "[.]"))
		if (confidence.interval.groups.to.summarize) {
			SIM_ByExpr1 <- parse(text=paste("quote(list(", paste(unlist(strsplit(as.character(sgp.groups.to.summarize), ", "))
  				[!(unlist(strsplit(as.character(sgp.groups.to.summarize), ", "))) %in% key(tmp.dt)], collapse=", "), 
			  	", ", paste(names(tmp.simulation.dt)[grep("SGP_SIM_", names(tmp.simulation.dt))], collapse=", "), "))", sep=""))
			SIM_ByExpr2 <- parse(text=paste("quote(list(", paste(sgp.groups.to.summarize, collapse=", "), "))", sep=""))
			tmp.sim <- tmp.dt[tmp.simulation.dt, eval(eval(SIM_ByExpr1))][, -(1:2), with=FALSE][,
				lapply(.SD, median_na), by=eval(eval(SIM_ByExpr2))][, 
				as.list(round(apply(.SD, 1, quantile, probs=c(0.025, 0.975)))), by=eval(eval(SIM_ByExpr2))]
			names(tmp.sim)[(dim(tmp.sim)[2]-1):dim(tmp.sim)[2]] <- c("LOWER_MEDIAN_SGP_95_CONF_BOUND", "UPPER_MEDIAN_SGP_95_CONF_BOUND")
			tmp <- data.table(merge.data.frame(tmp, tmp.sim, by = unlist(strsplit(as.character(sgp.groups.to.summarize), ", ")),all=TRUE))
		}
		message(paste("Finished with", sgp.groups.to.summarize))
		return(tmp)
	}

	combineSims <- function(sgp_object) {
		tmp.list <- list()
		tmp.names <- names(sgp_object[["SGP"]][["Simulated_SGPs"]]) 
		for (i in tmp.names) {
			tmp.list[[i]] <- data.frame(sgp_object[["SGP"]][["Simulated_SGPs"]][[i]],
				CONTENT_AREA=unlist(strsplit(i, "[.]"))[1],
				YEAR=type.convert(unlist(strsplit(i, "[.]"))[2]))
		}
	
		data.table(rbind.all(tmp.list), VALID_CASE=factor(1, levels=1:2, labels=c("VALID_CASE", "INVALID_CASE")),
			key=paste(key(tmp.dt), collapse=","))
	}
	 
	## Take subset of data

	tmp.dt <- data.table(STATE=state, sgp_object[["Student"]][CJ("VALID_CASE", content_areas, years), mult="all"], key="VALID_CASE, ID, CONTENT_AREA, YEAR")

	if (!is.null(confidence.interval.groups)) {
		tmp.simulation.dt <- combineSims(sgp_object); gc()
	}

	## Create summary tables

	for (i in summary.groups$institution) {
		sgp.groups <- do.call(paste, c(expand.grid(i,
			group.format(summary.groups[["content"]]),
			group.format(summary.groups[["time"]]),
			group.format(summary.groups[["institution_level"]]),
			group.format(summary.groups[["institution_inclusion"]][[i]]),
			group.format(summary.groups[["demographic"]])), sep=""))

  	if (!is.null(confidence.interval.groups) & i %in% confidence.interval.groups$institution) {
			ci.groups <- do.call(paste, c(expand.grid(i,
				group.format(confidence.interval.groups[["content"]]),
				group.format(confidence.interval.groups[["time"]]),
				group.format(confidence.interval.groups[["institution_level"]]),
				group.format(confidence.interval.groups[["institution_inclusion"]][[i]]),
				group.format(confidence.interval.groups[["demographic"]])), sep=""))
		}

		if (!is.null(confidence.interval.groups) & i %in% confidence.interval.groups$institution) {
			j <- k <- NULL ## To prevent R CMD check warnings
  		sgp_object[["Summary"]][[i]] <- foreach(j=iter(sgp.groups), k=iter(sgp.groups %in% ci.groups), 
				.options.multicore=list(preschedule = FALSE, set.seed = FALSE), .packages="data.table", .inorder=FALSE) %dopar% {return(sgpSummary(j, k))}
			names(sgp_object[["Summary"]][[i]]) <- gsub(", ", "__", sgp.groups)
		} else {
			j <- k <- NULL ## To prevent R CMD check warnings
    	sgp_object[["Summary"]][[i]] <- foreach(j=iter(sgp.groups), k=iter(rep(FALSE, length(sgp.groups))), 
				.options.multicore=list(preschedule = FALSE, set.seed = FALSE), .packages="data.table", .inorder=FALSE) %dopar% {return(sgpSummary(j, k))}
			names(sgp_object[["Summary"]][[i]]) <- gsub(", ", "__", sgp.groups)
		}
	} ## END summary.groups$institution summary loop

        message(paste("Finished summarizeSGP", date(), "in", timetaken(started.at), "\n"))
	return(sgp_object)
} ## END summarizeSGP Function
