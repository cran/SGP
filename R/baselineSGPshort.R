`baselineSGPshort` <-
	function(sgp_object,
			 state,
			 years,
			 content_areas,
			 grade.sequences,
			 baseline.max.order,
			 calculate.sgps=FALSE,
			 ...) {

    started.at <- proc.time()
    message(paste("Started baselineSGP", date()))

	### Create state (if missing) from sgp_object (if possible)

	if (missing(state)) {
		tmp.name <- gsub("_", " ", deparse(substitute(sgp_object)))
		if (any(sapply(c(state.name, "Demonstration"), function(x) regexpr(x, tmp.name)))==1) {
			state <- c(state.abb, "DEMO")[which(sapply(c(state.name, "Demonstration"), function(x) regexpr(x, tmp.name))==1)]
		}
	}

	.mergeSGP <- function(list_1, list_2) {
		for (j in c("Coefficient_Matrices", "Cutscores", "Goodness_of_Fit", "Knots_Boundaries", "SGPercentiles", "SGProjections", "Simulated_SGPs")) {
			list_1[[j]] <- c(list_1[[j]], list_2[[j]])[!duplicated(names(c(list_1[[j]], list_2[[j]])))]
		}
		for (j in c("SGPercentiles", "SGProjections", "Simulated_SGPs")) {
			if (all(names(list_2[[j]]) %in% names(list_1[[j]])) & !identical(list_1[[j]], list_2[[j]])) { #all(), not identical
				for (k in names(list_1[[j]])) {
					list_1[[j]][[k]] <- rbind.fill(list_1[[j]][[k]], list_2[[j]][[k]][!list_2[[j]][[k]][["ID"]] %in% list_1[[j]][[k]][["ID"]],]);gc()
				}
			}
		}
		for (j in c("Coefficient_Matrices", "Goodness_of_Fit", "Knots_Boundaries")) {
			for (k in names(list_1[[j]])) {
				list_1[[j]][[k]] <- c(list_1[[j]][[k]], list_2[[j]][[k]])[!duplicated(names(c(list_1[[j]][[k]], list_2[[j]][[k]])))]
			}
		}
	list_1
	}

	### Create Relevant argument-variables if missing
	key(sgp_object@Data) <- c("VALID_CASE", "CONTENT_AREA", "YEAR", "ID")
	
	if (missing(content_areas)) content_areas <- unique(sgp_object@Data["VALID_CASE"][["CONTENT_AREA"]]) 
	if (missing(years)) years <- sort(unique(sgp_object@Data[J("VALID_CASE", content_areas)][["YEAR"]]))
	if (missing(baseline.max.order)) baseline.max.order <- length(years)-2 # AVI -- want to combine at least two cohorts of data
	if (missing(grade.sequences)) {
		valid.grades <- sort(unique(sgp_object@Data[J("VALID_CASE", content_areas)][["GRADE"]]))
		grade.sequences <- lapply(valid.grades[-1], function(x) tail(valid.grades[valid.grades <= x], (baseline.max.order+1)))    #deals with 'holes'
		for (g in seq_along(grade.sequences)) {
			grade.sequences[[g]] <- grade.sequences[[g]][tail(grade.sequences[[g]],1)-grade.sequences[[g]] <= baseline.max.order] #deals with 'holes'
		}
	}
		
	### Restructure data based on acceptable.grade.sequences and calculate coefficient matrices
	key(sgp_object@Data) <- c("VALID_CASE", "YEAR", "GRADE", "CONTENT_AREA")
	
	tmp_sgp_object <- list()
	
	if (!is.null(sgp_object@SGP)) {
		for (i in names(sgp_object@SGP)) {
			tmp_sgp_object[[i]] <- sgp_object@SGP[[i]]
		}
	}

	for (h in content_areas) {
		for (j in grade.sequences) {
			tmp.lookup <- data.table(CJ("VALID_CASE", years, j, h))
			key(tmp.lookup) <- names(tmp.lookup) <- c("VALID_CASE", "YEAR", "GRADE","CONTENT_AREA") # Simpler lookup for cases to include
			sgp.uber.data <- reshape(sgp_object@Data[tmp.lookup, nomatch=0],
									idvar="ID", 
									timevar="GRADE", 
									direction="wide",
									drop=c("VALID_CASE", "CONTENT_AREA", "YEAR"))
			sgp.uber.data <- sgp.uber.data[,c("ID", paste("SCALE_SCORE.", j, sep="")), with=FALSE]
			sgp.uber.data <- sgp.uber.data[!apply(is.na(sgp.uber.data[,(dim(sgp.uber.data)[2]-1):dim(sgp.uber.data)[2], with=FALSE]), 1, any)] #needs to be separate line...
			sgp.uber.data <- data.table(sgp.uber.data$ID, 
				as.data.frame(matrix(rep(j, each=dim(sgp.uber.data)[1]), ncol=length(j))), sgp.uber.data[,-1, with=FALSE])

			tmp_sgp_object[["Panel_Data"]] <- data.frame(sgp.uber.data) #function doesn't take data.tables ???  thought we fixed that...

			tmp_sgp_object <- studentGrowthPercentiles(panel.data = tmp_sgp_object,
													sgp.labels = list(my.year="BASELINE", my.subject=h), 
													use.my.knots.boundaries = state,
													calculate.sgps = FALSE,
													drop.nonsequential.grade.progression.variables = FALSE, #always taken care of in data reshape above.
													grade.progression = j,
													...)	
		} ## END loop over grade.sequences
	} ## END loop over content
	
	sgp_object@SGP <- .mergeSGP(sgp_object@SGP, tmp_sgp_object)
	key(sgp_object@Data) <- c("VALID_CASE", "CONTENT_AREA", "YEAR", "ID")
    message(paste("Finished baselineSGP", date(), "in", timetaken(started.at), "\n"))
    return(sgp_object)
  } ## END baselineSGP Function
