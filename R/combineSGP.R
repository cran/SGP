`combineSGP` <- 
  function(sgp_object,
           state,
           years,
           content_areas,
           sgp.percentiles=TRUE,
           sgp.projections.lagged=TRUE,
           max.lagged.sgp.target.years.forward=4
           ) {

    started.at <- proc.time()
    message(paste("Started combineSGP", date()))

    ### Create state (if missing) from sgp_object (if possible)

        if (missing(state)) {
                tmp.name <- gsub("_", " ", deparse(substitute(sgp_object)))
                if (any(sapply(c(state.name, "Demonstration"), function(x) regexpr(x, tmp.name)))==1) {
                        state <- c(state.abb, "DEMO")[which(sapply(c(state.name, "Demonstration"), function(x) regexpr(x, tmp.name))==1)]
                }
        }

    ## Utility functions

    "%w/o%" <- function(x,y) x[!x %in% y]

    rbind.all <- function(.list, ...){
      if(length(.list)==1) return(.list[[1]])
      Recall(c(list(rbind(.list[[1]], .list[[2]], ...)), .list[-(1:2)]), ...)
    }


    ## Determine years and content_areas

    if (missing(content_areas)) {
       content_areas <- unique(sapply(strsplit(names(sgp_object@SGP[["SGPercentiles"]]), "[.]"), function(x) x[1]))
    } 
    if (missing(years)) {
       years <- type.convert(unique(sapply(strsplit(names(sgp_object@SGP[["SGPercentiles"]]), "[.]"), function(x) x[2])))
    } 


    ## Merge SGPs with student data
    
    if (sgp.percentiles) { 
      tmp.list <- list() 
      tmp.names <- do.call(paste, c(expand.grid(content_areas, years), sep="."))
      for (i in tmp.names) {
        tmp.list[[i]] <- data.table(CONTENT_AREA=unlist(strsplit(i, "[.]"))[1],
                                    YEAR=type.convert(unlist(strsplit(i, "[.]"))[2]),
                                    sgp_object@SGP[["SGPercentiles"]][[i]])
      }

      if (length(grep("SGP", names(sgp_object@Data)))==0) {
          sgp_object@Data <- data.table(rbind.all(tmp.list), VALID_CASE=factor(1, levels=1:2, labels=c("VALID_CASE", "INVALID_CASE")),
              key=paste(key(sgp_object@Data), collapse=","))[sgp_object@Data]
      } else {
          sgp_object@Data[CJ("VALID_CASE", content_areas, years)] <- data.table(rbind.all(tmp.list), VALID_CASE=factor(1, levels=1:2, labels=c("VALID_CASE", "INVALID_CASE")), 
              key=paste(key(sgp_object@Data), collapse=","))[sgp_object@Data[CJ("VALID_CASE", content_areas, years),  
              names(sgp_object@Data) %w/o% ((names(tmp.list[[1]]) %w/o% c("CONTENT_AREA", "YEAR", "ID"))), with=FALSE]][, names(sgp_object@Data), with=FALSE]
      }
    }



    ## Create SGP targets and merge with student data

    if (sgp.projections.lagged) { 

      level.to.get <- which.max(stateData[[state]][["Achievement"]][["Levels"]][["Proficient"]]=="Proficient")-1

      ## Create variable (CATCH_UP_KEEP_UP_STATUS_INITIAL) indicating catch up keep up status.
      
      ID <- CONTENT_AREA <- YEAR <- YEAR_INTEGER_TMP <- ACHIEVEMENT_LEVEL <- CATCH_UP_KEEP_UP_STATUS_INITIAL <- NULL  ## DONE to AVOID warnings during R CMD check
      sgp_object@Data$YEAR_INTEGER_TMP <- as.integer(sgp_object@Data$YEAR) ## To convert YEAR, when factor, to integer
      key(sgp_object@Data) <- c("ID", "CONTENT_AREA", "YEAR_INTEGER_TMP", "VALID_CASE") ## CRITICAL that Valid_Case is last in group
      sgp_object@Data$ACHIEVEMENT_LEVEL_PRIOR <- sgp_object@Data$CATCH_UP_KEEP_UP_STATUS_INITIAL <- 
        sgp_object@Data[SJ(ID, CONTENT_AREA, YEAR_INTEGER_TMP-1, "VALID_CASE"), mult="last"][, ACHIEVEMENT_LEVEL]
      sgp_object@Data$YEAR_INTEGER_TMP <- NULL
      levels(sgp_object@Data$CATCH_UP_KEEP_UP_STATUS_INITIAL) <- stateData[[state]][["Achievement"]][["Levels"]][["Proficient"]]
      levels(sgp_object@Data$CATCH_UP_KEEP_UP_STATUS_INITIAL) <- c("Catching Up", "Keeping Up")

      ## Merge Relevant Targets with CATCH_UP_KEEP_UP_STATUS_INITIAL

      tmp.list <- list()
      key(sgp_object@Data) <- c("VALID_CASE", "CONTENT_AREA", "YEAR", "ID")
      tmp.names <- do.call(paste, c(expand.grid(content_areas, years, "LAGGED"), sep="."))
      for (i in tmp.names) {
        cols.to.get <- grep(paste("LEVEL_", level.to.get, sep=""), names(sgp_object@SGP[["SGProjections"]][[i]]))
        num.cols.to.get <- min(max.lagged.sgp.target.years.forward, length(cols.to.get))
        tmp.list[[i]] <- data.table(CONTENT_AREA=unlist(strsplit(i, "[.]"))[1],
                                    YEAR=type.convert(unlist(strsplit(i, "[.]"))[2]),
                                    sgp_object@SGP[["SGProjections"]][[i]][,c(1,cols.to.get[1:num.cols.to.get])])
      }

      tmp_object_1 <- sgp_object@Data[, c(key(sgp_object@Data), "CATCH_UP_KEEP_UP_STATUS_INITIAL"), with=FALSE][
        data.table(VALID_CASE=factor(1, levels=1:2, labels=c("VALID_CASE", "INVALID_CASE")), rbind.all(tmp.list),key=paste(key(sgp_object@Data), collapse=","))][
        !is.na(CATCH_UP_KEEP_UP_STATUS_INITIAL)]

      ## Find min/max of targets based upon CATCH_UP_KEEP_UP_STATUS_INITIAL status

      VALID_CASE <- NULL
      catch_keep_functions <- c(min, max)
      jExpression <- parse(text=paste("quote({catch_keep_functions[[unclass(CATCH_UP_KEEP_UP_STATUS_INITIAL)]](",paste(names(tmp_object_1)[grep("LEVEL", names(tmp_object_1))], collapse=", "),", na.rm=TRUE)})", sep=""))
      tmp_object_2 <- tmp_object_1[, eval(eval(jExpression)), by=list(ID, CONTENT_AREA, YEAR, VALID_CASE)]
      names(tmp_object_2)[dim(tmp_object_2)[2]] <- "SGP_TARGET"
      key(tmp_object_2) <- key(sgp_object@Data)

      if (length(grep("SGP_TARGET", names(sgp_object@Data)))==0) {
           sgp_object@Data <- tmp_object_2[sgp_object@Data]
      } else {
           sgp_object@Data[CJ("VALID_CASE", content_areas, years)] <- tmp_object_2[sgp_object@Data[CJ("VALID_CASE", content_areas, years),  
              names(sgp_object@Data) %w/o% ((names(tmp_object_2) %w/o% c("VALID_CASE", "CONTENT_AREA", "YEAR", "ID"))), with=FALSE]][, names(sgp_object@Data), with=FALSE]
      }

      ## Create CATCH_UP_KEEP_UP_STATUS variable

      sgp_object@Data$CATCH_UP_KEEP_UP_STATUS <- factor(NA, levels=c(1:4), labels=c("Catch Up: No", "Catch Up: Yes", "Keep Up: No", "Keep Up: Yes"))

      sgp_object@Data$CATCH_UP_KEEP_UP_STATUS[sgp_object@Data$CATCH_UP_KEEP_UP_STATUS_INITIAL=="Keeping Up" &
                                                      sgp_object@Data$SGP >= sgp_object@Data$SGP_TARGET] <- "Keep Up: Yes"

      sgp_object@Data$CATCH_UP_KEEP_UP_STATUS[sgp_object@Data$CATCH_UP_KEEP_UP_STATUS_INITIAL=="Keeping Up" &
                                                      sgp_object@Data$SGP < sgp_object@Data$SGP_TARGET] <- "Keep Up: No"

      sgp_object@Data$CATCH_UP_KEEP_UP_STATUS[sgp_object@Data$CATCH_UP_KEEP_UP_STATUS_INITIAL=="Catching Up" &
                                                      sgp_object@Data$SGP >= sgp_object@Data$SGP_TARGET] <- "Catch Up: Yes"

      sgp_object@Data$CATCH_UP_KEEP_UP_STATUS[sgp_object@Data$CATCH_UP_KEEP_UP_STATUS_INITIAL=="Catching Up" &
                                                      sgp_object@Data$SGP < sgp_object@Data$SGP_TARGET] <- "Catch Up: No"

      sgp_object@Data$CATCH_UP_KEEP_UP_STATUS[sgp_object@Data$CATCH_UP_KEEP_UP_STATUS_INITIAL=="Keeping Up" &
                                                      sgp_object@Data$CATCH_UP_KEEP_UP_STATUS == "Keep Up: Yes" &
                                                      as.numeric(sgp_object@Data$ACHIEVEMENT_LEVEL) <= level.to.get] <- "Keep Up: No"

      sgp_object@Data$CATCH_UP_KEEP_UP_STATUS[sgp_object@Data$CATCH_UP_KEEP_UP_STATUS_INITIAL=="Catching Up" &
                                                      sgp_object@Data$CATCH_UP_KEEP_UP_STATUS == "Catch Up: Yes" &
                                                      as.numeric(sgp_object@Data$ACHIEVEMENT_LEVEL) <= level.to.get &
                                                      sgp_object@Data$GRADE == max(sgp_object@Data$GRADE[!is.na(sgp_object@Data$SGP_TARGET)])] <- "Catch Up: No"
      
    } ## END sgp.projections.lagged=TRUE

    key(sgp_object@Data) <- c("VALID_CASE", "CONTENT_AREA", "YEAR", "ID")

    message(paste("Finished combineSGP", date(), "in", timetaken(started.at), "\n"))
    return(sgp_object)

  } ## END combineSGP Function
