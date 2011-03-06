`summarizeSGP` <- 
function(sgp_object,
	years,
	content_areas,
	state = "DEMO",
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
		if (is.null(my.group)) c("")
		else {
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

	sgpSummary <- function(sgp.groups.to.summarize, confidence.interval.groups.to.summarize) {
		SGP_SIM <- V1 <- NULL  ## To prevent R CMD check warnings
		ListExpr <- parse(text=paste("quote(as.list(c(",paste(unlist(sgp.summaries), collapse=", "),")))",sep=""))
		ByExpr <- parse(text=paste("quote(list(", paste(sgp.groups.to.summarize, collapse=", "), "))", sep=""))
		tmp <- tmp.dt[, eval(eval(ListExpr)), by=eval(eval(ByExpr))]
		names(tmp)[-seq(length(unlist(strsplit(as.character(sgp.groups.to.summarize), ", "))))] <- unlist(strsplit(names(sgp.summaries), "[.]"))
		if (confidence.interval.groups.to.summarize) {
			SIM_ByExpr1 <- parse(text=paste("quote(list(", paste(unlist(strsplit(as.character(sgp.groups.to.summarize), ", "))
				[!(unlist(strsplit(as.character(sgp.groups.to.summarize), ", "))) %in% key(tmp.dt)], collapse=", "), 
				", SGP_SIM, SGP_SIM_ITERATION))", sep=""))
			SIM_ByExpr2 <- parse(text=paste("quote(list(", paste(sgp.groups.to.summarize, collapse=", "), ", SGP_SIM_ITERATION))", sep=""))
			tmp.sim <- long.sim.data[tmp.dt, eval(eval(SIM_ByExpr1))][!is.na(SGP_SIM)][,
				median(as.numeric(SGP_SIM)), by=eval(eval(SIM_ByExpr2))][,
				as.list(round(quantile(V1, probs=c(0.025, 0.975))),0), by=eval(eval(ByExpr))]
			names(tmp.sim)[(dim(tmp.sim)[2]-1):dim(tmp.sim)[2]] <- c("LOWER_MEDIAN_SGP_95_CONF_BOUND", "UPPER_MEDIAN_SGP_95_CONF_BOUND")
			tmp <- data.table(merge.data.frame(tmp, tmp.sim, by = unlist(strsplit(as.character(sgp.groups.to.summarize), ", ")),all=TRUE))
		}
		print(paste("Finished with", sgp.groups.to.summarize))
		return(tmp)
	}

	combineSims <- function(tmp_sgp_object) {
		tmp.list <- list()
		tmp.names <- names(tmp_sgp_object[["SGP"]][["Simulated_SGPs"]]) 
		for(i in tmp.names) {
			tmp.list[[i]] <- data.frame(data.frame(ID=rep(tmp_sgp_object[["SGP"]][["Simulated_SGPs"]][[i]][,1], 
				dim(tmp_sgp_object[["SGP"]][["Simulated_SGPs"]][[i]])[2]-1),
				stack(tmp_sgp_object[["SGP"]][["Simulated_SGPs"]][[i]][,-1])),
				YEAR=as.integer(unlist(strsplit(i, "[.]"))[2]),
				CONTENT_AREA=unlist(strsplit(i, "[.]"))[1])
		}
		tmp.simulation.dt <- data.table(rbind.all(tmp.list), VALID_CASE=factor(1, levels=1:2, labels=c("VALID_CASE", "INVALID_CASE")),
				key=paste(key(tmp.dt), collapse=","))
		names(tmp.simulation.dt)[2:3] <- c("SGP_SIM", "SGP_SIM_ITERATION")
		return(tmp.simulation.dt)
	}
	 
	## Take subset of data

	tmp.dt <- data.table(STATE=state, sgp_object[["Student"]][CJ("VALID_CASE", content_areas, years), mult="all"], key="VALID_CASE, ID, CONTENT_AREA, YEAR")

	if (!is.null(confidence.interval.groups)) {
		long.sim.data <- combineSims(sgp_object); gc()
	}

	## Set up options for use with doMC
	mc.options <- list(preschedule = FALSE, set.seed = FALSE)

	## Create summary tables

	for (i in summary.groups$institution) {
		sgp.groups <- do.call(paste, c(expand.grid(i,
			group.format(summary.groups[["content"]]),
			group.format(summary.groups[["time"]]),
			group.format(summary.groups[["institution_level"]]),
			group.format(summary.groups[["institution_inclusion"]][[i]]),
			group.format(summary.groups[["demographic"]])), sep=""))

	if (!is.null(confidence.interval.groups)) {
		ci.groups <- do.call(paste, c(expand.grid(i,
			group.format(confidence.interval.groups[["content"]]),
			group.format(confidence.interval.groups[["time"]]),
			group.format(confidence.interval.groups[["institution_level"]]),
			group.format(confidence.interval.groups[["institution_inclusion"]][[i]]),
			group.format(confidence.interval.groups[["demographic"]])), sep=""))
	}

	if (is.null(confidence.interval.groups)) {
		j  <- NULL ## To prevent R CMD check warnings
		sgp_object[["Summary"]][[i]] <- foreach(i=iter(sgp.groups), j=iter(rep(FALSE, length(sgp.groups))), 
			.options.multicore = mc.options, .packages="data.table", .inorder=FALSE) %dopar% {return(sgpSummary(i, j))}
		names(sgp_object[["Summary"]][[i]]) <- gsub(", ", ".", sgp.groups)
	} else {
		j  <- NULL ## To prevent R CMD check warnings
		sgp_object[["Summary"]][[i]] <- foreach(i=iter(sgp.groups), j=iter(sgp.groups %in% ci.groups), 
			.options.multicore = mc.options, .packages="data.table", .inorder=FALSE) %dopar% {return(sgpSummary(i, j))}
		names(sgp_object[["Summary"]][[i]]) <- gsub(", ", ".", sgp.groups)
		}
	} ## END summary.groups$institution summary loop
return(sgp_object)
} ## END summarizeSGP function
