`combineSGP` <- 
function(sgp_object,
	state,
	sgp.percentiles=TRUE,
	sgp.projections.lagged=TRUE) {

        started.at <- proc.time()
        message(paste("Started combineSGP", date()))

	## Utility functions

	rbind.all <- function(.list, ...){
		if(length(.list)==1) return(.list[[1]])
		Recall(c(list(rbind(.list[[1]], .list[[2]], ...)), .list[-(1:2)]), ...)
	}


	## Merge SGP with student data
  
	if (sgp.percentiles) { 
		tmp.list <- list() 
       		tmp.names <- names(sgp_object[["SGP"]][["SGPercentiles"]])
		for (i in tmp.names) {
			tmp.list[[i]] <- data.table(CONTENT_AREA=unlist(strsplit(i, "[.]"))[1],
				YEAR=type.convert(unlist(strsplit(i, "[.]"))[2]),
				sgp_object[["SGP"]][["SGPercentiles"]][[i]])
		}
		sgp_object[["Student"]] <- data.table(rbind.all(tmp.list), VALID_CASE=factor(1, levels=1:2, labels=c("VALID_CASE", "INVALID_CASE")), 
			key=paste(key(sgp_object[["Student"]]), collapse=","))[sgp_object[["Student"]]]
	}


	## Create SGP targets and merge with student data

	if (sgp.projections.lagged) { 

		level.to.get <- which.max(stateData[[state]][["Achievement"]][["Levels"]][["Proficient"]]=="Proficient")-1

		## Create variable (CATCH_UP_KEEP_UP_STATUS_INITIAL) indicating catch up keep up status.
		
		ID <- CONTENT_AREA <- YEAR <- YEAR_INTEGER_TMP <- ACHIEVEMENT_LEVEL <- CATCH_UP_KEEP_UP_STATUS_INITIAL <- NULL  ## DONE to AVOID warnings during R CMD check
		sgp_object[["Student"]]$YEAR_INTEGER_TMP <- as.integer(sgp_object[["Student"]]$YEAR) ## To convert YEAR, when factor, to integer
		key(sgp_object[["Student"]]) <- c("ID", "CONTENT_AREA", "YEAR_INTEGER_TMP", "VALID_CASE") ## CRITICAL that Valid_Case is last in group
		sgp_object[["Student"]]$ACHIEVEMENT_LEVEL_PRIOR <- sgp_object[["Student"]]$CATCH_UP_KEEP_UP_STATUS_INITIAL <- 
			sgp_object[["Student"]][SJ(ID, CONTENT_AREA, YEAR_INTEGER_TMP-1, "VALID_CASE"), mult="last"][, ACHIEVEMENT_LEVEL]
		sgp_object[["Student"]]$YEAR_INTEGER_TMP <- NULL
		levels(sgp_object[["Student"]]$CATCH_UP_KEEP_UP_STATUS_INITIAL) <- stateData[[state]][["Achievement"]][["Levels"]][["Proficient"]]
		levels(sgp_object[["Student"]]$CATCH_UP_KEEP_UP_STATUS_INITIAL) <- c("Catching Up", "Keeping Up")

		## Merge Relevant Targets with CATCH_UP_KEEP_UP_STATUS_INITIAL

		tmp.list <- list()
		key(sgp_object[["Student"]]) <- c("VALID_CASE", "CONTENT_AREA", "YEAR", "ID")
		tmp.names <- names(sgp_object[["SGP"]][["SGProjections"]])[grep("LAGGED", names(sgp_object[["SGP"]][["SGProjections"]]))]	
		for (i in tmp.names) {
			tmp.list[[i]] <- data.table(CONTENT_AREA=unlist(strsplit(i, "[.]"))[1],
				YEAR=type.convert(unlist(strsplit(i, "[.]"))[2]),
				sgp_object[["SGP"]][["SGProjections"]][[i]][,c(1,grep(paste("LEVEL_", level.to.get, sep=""), 
					names(sgp_object[["SGP"]][["SGProjections"]][[i]]))[1:4])])
		}

	 	tmp_object_1 <- sgp_object[["Student"]][, c(key(sgp_object[["Student"]]), "CATCH_UP_KEEP_UP_STATUS_INITIAL"), with=FALSE][
			data.table(VALID_CASE=factor(1, levels=1:2, labels=c("VALID_CASE", "INVALID_CASE")), rbind.all(tmp.list),
				key=paste(key(sgp_object[["Student"]]), collapse=","))][!is.na(CATCH_UP_KEEP_UP_STATUS_INITIAL)]

		## Find min/max of targets based upon CATCH_UP_KEEP_UP_STATUS_INITIAL status

		VALID_CASE <- NULL
		catch_keep_functions <- c(min,max)
	  	jExpression <- parse(text=paste("quote({CATCH_UP_KEEP_UP_STATUS_INITIAL; catch_keep_functions[[unclass(CATCH_UP_KEEP_UP_STATUS_INITIAL)]](",paste(names(tmp_object_1)[grep("LEVEL", names(tmp_object_1))], collapse=", "),", na.rm=TRUE)})", sep=""))
		tmp_object_2 <- tmp_object_1[, eval(eval(jExpression)), by=list(ID, CONTENT_AREA, YEAR, VALID_CASE)]
		names(tmp_object_2)[dim(tmp_object_2)[2]] <- "SGP_TARGET"
		key(tmp_object_2) <- key(sgp_object[["Student"]])

		sgp_object[["Student"]] <- tmp_object_2[sgp_object[["Student"]]]

		## Create CATCH_UP_KEEP_UP_STATUS variable

		sgp_object[["Student"]]$CATCH_UP_KEEP_UP_STATUS <- factor(NA, levels=c(1:4), labels=c("Catch Up: No", "Catch Up: Yes", "Keep Up: No", "Keep Up: Yes"))

		sgp_object[["Student"]]$CATCH_UP_KEEP_UP_STATUS[sgp_object[["Student"]]$CATCH_UP_KEEP_UP_STATUS_INITIAL=="Keeping Up" &
			sgp_object[["Student"]]$SGP >= sgp_object[["Student"]]$SGP_TARGET] <- "Keep Up: Yes"

		sgp_object[["Student"]]$CATCH_UP_KEEP_UP_STATUS[sgp_object[["Student"]]$CATCH_UP_KEEP_UP_STATUS_INITIAL=="Keeping Up" &
			sgp_object[["Student"]]$SGP < sgp_object[["Student"]]$SGP_TARGET] <- "Keep Up: No"

		sgp_object[["Student"]]$CATCH_UP_KEEP_UP_STATUS[sgp_object[["Student"]]$CATCH_UP_KEEP_UP_STATUS_INITIAL=="Catching Up" &
			sgp_object[["Student"]]$SGP >= sgp_object[["Student"]]$SGP_TARGET] <- "Catch Up: Yes"

		sgp_object[["Student"]]$CATCH_UP_KEEP_UP_STATUS[sgp_object[["Student"]]$CATCH_UP_KEEP_UP_STATUS_INITIAL=="Catching Up" &
			sgp_object[["Student"]]$SGP < sgp_object[["Student"]]$SGP_TARGET] <- "Catch Up: No"

		sgp_object[["Student"]]$CATCH_UP_KEEP_UP_STATUS[sgp_object[["Student"]]$CATCH_UP_KEEP_UP_STATUS_INITIAL=="Keeping Up" &
			sgp_object[["Student"]]$CATCH_UP_KEEP_UP_STATUS == "Keep Up: Yes" &
			as.numeric(sgp_object[["Student"]]$ACHIEVEMENT_LEVEL) <= level.to.get] <- "Keep Up: No"

		sgp_object[["Student"]]$CATCH_UP_KEEP_UP_STATUS[sgp_object[["Student"]]$CATCH_UP_KEEP_UP_STATUS_INITIAL=="Catching Up" &
			sgp_object[["Student"]]$CATCH_UP_KEEP_UP_STATUS == "Catch Up: Yes" &
			as.numeric(sgp_object[["Student"]]$ACHIEVEMENT_LEVEL) <= level.to.get &
			sgp_object[["Student"]]$GRADE == max(sgp_object[["Student"]]$GRADE[!is.na(sgp_object[["Student"]]$SGP_TARGET)])] <- "Catch Up: No"
    
	} ## END sgp.projections.lagged=TRUE

        message(paste("Finished combineSGP", date(), "in", timetaken(started.at), "\n"))
	return(sgp_object)

} ## END combineSGP Function
