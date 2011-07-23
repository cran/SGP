`analyzeSGP` <- 
function(sgp_object,
         state,
         years,
         content_areas,
         grades,
         sgp.config,
         sgp.percentiles=TRUE, 
         sgp.projections=TRUE,
         sgp.projections.lagged=TRUE,
         simulate.sgps=TRUE,
         goodness.of.fit.print=TRUE,
         parallel.config,
         ...) {

	started.at <- proc.time()
	message(paste("Started analyzeSGP", date()))

        ### Create state (if missing) from sgp_object (if possible)

        if (missing(state)) {
                tmp.name <- gsub("_", " ", deparse(substitute(sgp_object)))
                if (any(sapply(c(state.name, "Demonstration"), function(x) regexpr(x, tmp.name)))==1) {
                        state <- c(state.abb, "DEMO")[which(sapply(c(state.name, "Demonstration"), function(x) regexpr(x, tmp.name))==1)]
                }
        }

	## Function to return sgp.config based upon a supplied year and content_area

	.get.config <- function(content_area, year, grades) {
		tmp.data <- sgp_object@Data[J("VALID_CASE", content_area), c("YEAR", "GRADE"), with=FALSE]
		.sgp.panel.years <- sort(unique(tmp.data$YEAR))[1:which(sort(unique(tmp.data$YEAR)) == year)]
		.sgp.content.areas <- rep(content_area, length(.sgp.panel.years))
		.tmp.sgp.grade.sequences <- lapply(grades, function(x) tail(seq(min(tmp.data$GRADE, na.rm=TRUE), x), length(.sgp.panel.years)))
		.tmp.table <- table(tmp.data$GRADE, tmp.data$YEAR)
		.sgp.grade.sequences <- sapply(.tmp.sgp.grade.sequences, 
			function(x) tail(x, sum(diag(.tmp.table[as.character(x), as.character(tail(.sgp.panel.years, length(x)))])!=0))) 
		list(sgp.content.areas=.sgp.content.areas, sgp.panel.years=.sgp.panel.years, sgp.grade.sequences=.sgp.grade.sequences) 
	}

	.analyzeSGP_Internal <- function(sgp.iter,
		sgp.percentiles_Internal=sgp.percentiles,
		sgp.projections_Internal=sgp.projections,
		sgp.projections.lagged_Internal=sgp.projections.lagged) {

		tmp_sgp_object <- list()

		if (!is.null(sgp_object@SGP)) {
			for (i in names(sgp_object@SGP)) {
				tmp_sgp_object[[i]] <- sgp_object@SGP[[i]]
			}
		}

		tmp_sgp_object[["Panel_Data"]] <- 
		as.data.frame(reshape(sgp_object@Data[J("VALID_CASE", sgp.iter[["sgp.content.areas"]], sgp.iter[["sgp.panel.years"]]), mult="all"], #VA-diff subjects, MA + AlgI no [1] in sgp.iter[["sgp.content.areas"]][1]
			idvar="ID",
			timevar="YEAR",
			drop=names(sgp_object@Data)[!names(sgp_object@Data) %in% c("ID", "GRADE", "SCALE_SCORE", "YEAR")],
			direction="wide"))
		suppressMessages(gc()) 

		## sgp.percentiles_Internal 

		if (sgp.percentiles_Internal) {
			sgp.vnames <- c("ID", paste("GRADE", sgp.iter[["sgp.panel.years"]], sep="."), 
				paste("SCALE_SCORE", sgp.iter[["sgp.panel.years"]], sep="."))
			if (simulate.sgps) {
				for (k in sgp.iter[["sgp.grade.sequences"]]) {
					tmp_sgp_object <- studentGrowthPercentiles(
						panel.data=tmp_sgp_object,
						sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)),
						use.my.knots.boundaries=state,
						growth.levels=state,
						panel.data.vnames=sgp.vnames,
						grade.progression=k,
						calculate.confidence.intervals=list(state=state,  
							confidence.quantiles=c(0.16,0.84),
							simulation.iterations=100, 
							distribution="Normal", round=1),
						...)
				} ## END k loop
			} else {
				for (k in sgp.iter[["sgp.grade.sequences"]]) {
					tmp_sgp_object <- studentGrowthPercentiles(
						panel.data=tmp_sgp_object,
						sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)),
						use.my.knots.boundaries=state,
						growth.levels=state,
						panel.data.vnames=sgp.vnames,
						grade.progression=k,
						...)
				} ## END k loop
			} 
		} ## END if sgp.percentiles_Internal

		## sgp.projections_Internal

		if (sgp.projections_Internal) {
			sgp.vnames <- c("ID", paste("GRADE", sgp.iter[["sgp.panel.years"]], sep="."), 
				paste("SCALE_SCORE", sgp.iter[["sgp.panel.years"]], sep="."))

			for (k in lapply(sgp.iter[["sgp.grade.sequences"]], function(x) head(x, -1))) {
				tmp_sgp_object <- studentGrowthProjections(
					panel.data=tmp_sgp_object,
					sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)),
					use.my.coefficient.matrices=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
					use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
					performance.level.cutscores=state,
					max.forward.progression=3,
					percentile.trajectory.values=c(1, stateData[[state]][["Growth"]][["Cutscores"]][["Cuts"]], 99),
					panel.data.vnames=sgp.vnames,
					grade.progression=k,
					...)
			}
		} ## END if sgp.projections_Internal

		## sgp.projections.lagged_Internval

		if (sgp.projections.lagged_Internal) {
			sgp.vnames <- c("ID", paste("GRADE", head(sgp.iter[["sgp.panel.years"]], -1), sep="."), 
				paste("SCALE_SCORE", head(sgp.iter[["sgp.panel.years"]], -1), sep="."))

			for (k in lapply(sgp.iter[["sgp.grade.sequences"]], function(x) head(x, -1))) {
				tmp_sgp_object <- studentGrowthProjections(
					panel.data=tmp_sgp_object,
					sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1), 
						my.extra.label="LAGGED"),
					use.my.coefficient.matrices=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
					use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
					performance.level.cutscores=state,
					panel.data.vnames=sgp.vnames,
					grade.progression=k,
					...)
			}
		} ## END sgp.projections.lagged_Internal
    	return(tmp_sgp_object)
	} ## END .analyzeSGP_Internal

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

	gof.print <- function(sgp_object) {
		if (length(sgp_object@SGP[["Goodness_of_Fit"]]) > 0) {
			for (i in names(sgp_object@SGP[["Goodness_of_Fit"]])) {
				dir.create(paste("Goodness_of_Fit/", i, sep=""), recursive=TRUE, showWarnings=FALSE)
					for (j in names(sgp_object@SGP[["Goodness_of_Fit"]][[i]])) {
						pdf(file=paste("Goodness_of_Fit/", i, "/", j, ".pdf", sep=""), width=8.5, height=4.5)
						grid.draw(sgp_object@SGP[["Goodness_of_Fit"]][[i]][[j]])
						dev.off()
					}
				}
		} else {
			message("No Goodness of Fit tables available to print. No tables will be produced.")
		}
	}

	## If missing sgp.config then determine year(s), content_area(s), and grade(s) if not explicitely provided

	if (missing(sgp.config)) {
		sgp.config <- tmp.years <- tmp.grades <- list()
		if (missing(content_areas)) {
			content_areas <- unique(sgp_object@Data["VALID_CASE"]$CONTENT_AREA)
		}
		if (missing(years)) {
			for (i in content_areas) {
				tmp.years[[i]] <- sort(tail(unique(sgp_object@Data[J("VALID_CASE", content_areas)]$YEAR), -2), decreasing=TRUE)
			}
		} else {
			for (i in content_areas) { 
				tmp.years[[i]] <- years
			}
		}
		if (missing(grades)) {
			for (i in content_areas) {
				for (j in tmp.years[[i]]) {
					tmp.grades[[paste(i,j,sep=".")]] <- tail(sort(unique(sgp_object@Data[J("VALID_CASE", i, j)]$GRADE)), -1)
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

	## studentGrowthPercentiles & studentGrowthProjections

	if (sgp.percentiles | sgp.projections | sgp.projections.lagged) {
		sgp.iter <- NULL ## To prevent R CMD check warning

		if (missing(parallel.config)) {
			tmp <- list()
			for (i in 1:length(sgp.config)) {
				tmp[[i]] <- .analyzeSGP_Internal(sgp.config[[i]])
			}
			for (s in 1:length(tmp)) {
				sgp_object@SGP <- .mergeSGP(sgp_object@SGP,tmp[[s]])
			}
		} else {
		
		if (toupper(parallel.config[["TYPE"]]) == "FOREACH") {
			require(foreach)
			foreach.options <-parallel.config[["OPTIONS"]] # works fine if NULL
			sgp_object@SGP <- foreach(sgp.iter=iter(sgp.config), .packages="SGP", .combine=".mergeSGP", .inorder=FALSE,
				.options.multicore=foreach.options, .options.mpi= foreach.options, .options.redis= foreach.options, .options.smp= foreach.options) %dopar% {
					return(.analyzeSGP_Internal(sgp.iter))
			}
		}

		if (toupper(parallel.config[["TYPE"]]) == "MULTICORE") {
			require(doMC)
			tmp <- mclapply(sgp.config, .analyzeSGP_Internal, mc.preschedule = FALSE)
			for (s in 1:length(tmp))	sgp_object@SGP <- .mergeSGP(sgp_object@SGP, tmp[[s]])
		}

		if (toupper(parallel.config[["TYPE"]]) == "SNOW") {
			require(snow)
			tmp <- parLapply(parallel.config[["CLUSTER"]], sgp.config, .analyzeSGP_Internal)
			for (s in 1:length(tmp))	sgp_object@SGP <- .mergeSGP(sgp_object@SGP, tmp[[s]])
		}

		if (!toupper(parallel.config[["TYPE"]]) %in% c("FOREACH", "MULTICORE", "SNOW")) {
			stop("'TYPE' provided in parallel.config not recognized.  Currently 'FOREACH', 'MULTICORE' and 'SNOW' are supported.  See help pages for additional information.")
		}}

	} ## END if (sgp.percentiles | sgp.projections | sgp.projections.lagged)

	if (goodness.of.fit.print) gof.print(sgp_object)

	message(paste("Finished analyzeSGP", date(), "in", timetaken(started.at), "\n"))
	return(sgp_object)
} ## END analyzeSGP Function
