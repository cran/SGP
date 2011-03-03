`combineSGP` <- 
function(sgp_object=sgp_object,
	years=years,
	content_areas=content_areas,
	sgp.percentiles=sgp.percentiles,
	sgp.projections.lagged=sgp.projections.lagged,
	state=state) {

	if (sgp.percentiles) { ## Combine student growth percentiles into Student data
		tmp.list <- list() 
        	tmp.names <- names(sgp_object[["SGP"]][["SGPercentiles"]])
		for (i in tmp.names) {
			tmp.list[[i]] <- data.table(YEAR=unlist(strsplit(i, "[.]"))[2],
				CONTENT_AREA=unlist(strsplit(i, "[.]"))[1],
				sgp_object[["SGP"]][["SGPercentiles"]][[i]])
		}
		sgp_object[["Student"]] <- data.table(do.call(rbind, tmp.list), VALID_CASE=factor(1, levels=1:2, labels=c("VALID_CASE", "INVALID_CASE")), 
			key=paste(key(sgp_object[["Student"]]), collapse=","))[sgp_object[["Student"]]]
		return(sgp_object)
	}

	if (sgp.projections.lagged) { ## Combine create SGP targets for combination with student data

		## Create CATCH_KEEP_UP variable

		level.to.get <- which.max(stateData[[state]][["Achievement"]][["Levels"]][["Proficient"]]=="Proficient")-1

		## Create CATCH_KEEP_UP variable
		
		ID <- NULL; CONTENT_AREA <- NULL; YEAR <- NULL; ACHIEVEMENT_LEVEL <- NULL  ## DONE to AVOID warnings during R CMD check	
		key(sgp_object[["Student"]]) <- c("ID", "CONTENT_AREA", "YEAR", "VALID_CASE") ## CRITICAL that Valid_Case is last in group
		sgp_object[["Student"]]$ACHIEVEMENT_LEVEL_PRIOR <- sgp_object[["Student"]][SJ(ID, CONTENT_AREA, YEAR-1), mult="last"][, ACHIEVEMENT_LEVEL]
		sgp_object[["Student"]]$CATCH_KEEP_UP <- sgp_object[["Student"]]$ACHIEVEMENT_LEVEL_PRIOR
		levels(sgp_object[["Student"]]$CATCH_KEEP_UP) <- stateData[[state]][["Achievement"]][["Levels"]][["Proficient"]]

		## Merge Relevant Targets with CATCH_KEEP_UP

		tmp.list <- list()
		key(sgp_object[["Student"]]) <- c("VALID_CASE", "CONTENT_AREA", "YEAR", "ID")
		tmp.names <- names(sgp_object[["SGP"]][["SGProjections"]])[grep("LAGGED", names(sgp_object[["SGP"]][["SGProjections"]]))]	
		for (i in tmp.names) {
			tmp.list[[i]] <- data.table(CONTENT_AREA=unlist(strsplit(i, "[.]"))[1],
				YEAR=as.integer(unlist(strsplit(i, "[.]"))[2]),
				sgp_object[["SGP"]][["SGProjections"]][[i]][,c(1,grep(paste("LEVEL_", level.to.get, sep=""), 
					names(sgp_object[["SGP"]][["SGProjections"]][[i]]))[1:4])])
		}
		tmp_object_1 <- data.table(VALID_CASE=factor(1, levels=1:2, labels=c("VALID_CASE", "INVALID_CASE")), do.call(rbind, tmp.list),
			key=paste(key(sgp_object[["Student"]]), collapse=","))[sgp_object[["Student"]][, c(key(sgp_object[["Student"]]), "CATCH_KEEP_UP"), with=FALSE]]

		## Find min/max of targets based upon CATCH_KEEP_UP status

		catch_keep_functions <- c(min,max)
		jExpression <- parse(text=paste("quote({CATCH_KEEP_UP; catch_keep_functions[[unclass(CATCH_KEEP_UP)]](",paste(names(tmp_object_1)[grep("LEVEL", names(tmp_object_1))], collapse=", "),")})", sep=""))
		tmp_object_2 <- tmp_object_1[,eval(eval(jExpression)),by=ID]


	} ## END sgp.projections.lagged=TRUE
} ## END combineSGP
