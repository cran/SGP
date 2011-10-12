`analyzeSGP` <- 
function(sgp_object,
         state,
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
         goodness.of.fit.print=TRUE,
         sgp.config,
         sgp.baseline.config, 
         parallel.config=NULL,
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
		tmp.unique.years <- sort(unique(tmp.data$YEAR))
		.sgp.panel.years <- tmp.unique.years[1:which(tmp.unique.years == year)]
		.sgp.content.areas <- rep(content_area, length(.sgp.panel.years))
		tmp.sgp.grade.sequences <- lapply(grades[-1], function(x) tail(grades[grades <= x], length(tmp.unique.years)))
		.sgp.grade.sequences <- sapply(tmp.sgp.grade.sequences, function(x) x[(tail(x,1)-x) <= length(.sgp.panel.years)-1])
		list(sgp.content.areas=.sgp.content.areas, sgp.panel.years=.sgp.panel.years, sgp.grade.sequences=.sgp.grade.sequences) 
	}

	.mergeSGP <- function(list_1, list_2) {
		for (j in c("Coefficient_Matrices", "Cutscores", "Goodness_of_Fit", "Knots_Boundaries", "SGPercentiles", "SGProjections", "Simulated_SGPs")) {
			list_1[[j]] <- c(list_1[[j]], list_2[[j]])[!duplicated(names(c(list_1[[j]], list_2[[j]])))]
		}
		for (j in c("SGPercentiles", "SGProjections", "Simulated_SGPs")) {
			if (all(names(list_2[[j]]) %in% names(list_1[[j]])) & !identical(list_1[[j]], list_2[[j]])) {
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
			content_areas <- unique(sgp_object@Data["VALID_CASE"][["CONTENT_AREA"]])
		}
		if (missing(years)) {
			for (i in content_areas) {
				tmp.years[[i]] <- sort(tail(unique(sgp_object@Data[J("VALID_CASE", i)][["YEAR"]]), -2), decreasing=TRUE)
			}
		} else {
			for (i in content_areas) { 
				tmp.years[[i]] <- years
			}
		}
		if (missing(grades)) {
			for (i in content_areas) {
				for (j in tmp.years[[i]]) {
					tmp.grades[[paste(i,j,sep=".")]] <- sort(unique(sgp_object@Data[J("VALID_CASE", i, j)][["GRADE"]]))
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

	###  Parallel Backend Specific Setup - ADD TESTS?:
		
	if (!is.null(parallel.config)) {
		if (toupper(parallel.config[["BACKEND"]][["TYPE"]]) == "FOREACH") {
			require(foreach)
			eval(parse(text=paste("require(", parallel.config[["BACKEND"]][["FOREACH_TYPE"]], ")"))) # require(do*)   #  ONLY NEED ONCE
			if (parallel.config[["BACKEND"]][["FOREACH_TYPE"]]=="doMC" & 
				is.null(parallel.config[["BACKEND"]][["FOREACH_OPTIONS"]][["preschedule"]])) {
					parallel.config[["BACKEND"]][["FOREACH_OPTIONS"]][["preschedule"]]=FALSE
			}  #  ONLY NEED ONCE
			foreach.options <-parallel.config[["BACKEND"]][["FOREACH_OPTIONS"]] # works fine if NULL
		}

		if (toupper(parallel.config[["BACKEND"]][["TYPE"]]) == "MULTICORE") require(multicore)

		if (toupper(parallel.config[["BACKEND"]][["TYPE"]]) == "SNOW") require(snow)
	}

	
	#######################################################################################################################
	##   Baseline SGP - compute matrices first if they are not in stateData or merge them into sgp_object if in stateData
	#######################################################################################################################

	if (sgp.percentiles.baseline) {
		if (missing(sgp.baseline.config)) {
			sgp.baseline.config <- .content_areas <- .years <- .grades <- .sgp.grade.sequences <- list()
			.content_areas <- unique(sgp_object@Data["VALID_CASE"][["CONTENT_AREA"]]) #tail(sgp.iter[["sgp.content.areas"]], 1)
			.years <- sort(unique(sgp_object@Data[J("VALID_CASE", .content_areas)][["YEAR"]]))
			.grades <- sort(unique(sgp_object@Data[J("VALID_CASE", .content_areas)][["GRADE"]]))
			.baseline.max.order <- length(.years)-2
			tmp.sgp.grade.sequences <- lapply(.grades[-1], function(x) tail(.grades[.grades <= x], (.baseline.max.order+1)))
			.sgp.grade.sequences <- sapply(tmp.sgp.grade.sequences, function(x) x[(tail(x,1)-x) <= .baseline.max.order])

			for (i in .content_areas) {
				sgp.baseline.config[[as.character(i)]] <- list(baseline.content.areas=i, baseline.panel.years=.years, 
					baseline.max.order=.baseline.max.order, baseline.grade.sequences=.sgp.grade.sequences)
			}
		}

		for (i in names(sgp.baseline.config)) {
			if (!identical(names(sgp.baseline.config[[i]]), c("baseline.content.areas", "baseline.panel.years", "baseline.max.order", "baseline.grade.sequences"))) {
			 stop("Please specify an appropriate list of SGP function labels (sgp.baseline.config).	See help page for details.")
			}
		}
			
		if (is.null(stateData[[state]][["Baseline_splineMatrix"]])) {
			message("\n\tCreating baseline matrices...\n")
			
			if (!is.null(parallel.config)) {
				workers <- NULL
				if (!is.null(parallel.config[["ANALYSES"]][["BASELINE_MATRICES_WORKERS"]])) workers <- parallel.config[["ANALYSES"]][["BASELINE_MATRICES_WORKERS"]]
				if (!is.null(parallel.config[["ANALYSES"]][["WORKERS"]])) workers <- parallel.config[["ANALYSES"]][["WORKERS"]]
				
				par.baseline.config <- list(); cnt <- 1
				for (a in names(sgp.baseline.config)) {
					for (b in 1:length(sgp.baseline.config[[a]][["baseline.grade.sequences"]])) {
						par.baseline.config[[cnt]] <- sgp.baseline.config[[a]]
						par.baseline.config[[cnt]][["baseline.grade.sequences"]] <- sgp.baseline.config[[a]][["baseline.grade.sequences"]][b]
						cnt <- cnt + 1
					}
				}

				if (toupper(parallel.config[["BACKEND"]][["TYPE"]]) == "FOREACH") {
					if (!is.null(workers)) eval(parse(text=paste("registerDo", substr(parallel.config[["BACKEND"]][["FOREACH_TYPE"]],3,9), "(workers)", sep="")))#registerDo*(workers)
	
#.export="sgp_object" # Deal with this - NULL if not doSMP -- !is.null(sessionInfo()$otherPkgs$doSMP)  --  even needed?  still need to work on doSMP
					tmp <- foreach(sgp.iter=iter(par.baseline.config), .packages="SGP", .combine=".mergeSGP", .inorder=FALSE, .export="sgp_object",
						.options.multicore=foreach.options, .options.mpi= foreach.options, .options.redis= foreach.options, .options.smp= foreach.options) %dopar% {
						return(baselineSGP(
							sgp_object,
							state = state,
							years = sgp.iter[["baseline.panel.years"]],
							content_areas = sgp.iter[["baseline.content.areas"]],
							grade.sequences = sgp.iter[["baseline.grade.sequences"]],
							baseline.max.order = sgp.iter[["baseline.max.order"]],
							return.matrices.only=TRUE))
					}
					sgp_object@SGP <- .mergeSGP(sgp_object@SGP, tmp)
					rm(tmp)
				} # END if (FOREACH)
			
				if (toupper(parallel.config[["BACKEND"]][["TYPE"]]) == "SNOW") {
					if (!is.null(workers)) internal.cl <- makeCluster(workers, type=parallel.config[["BACKEND"]][["SNOW_TYPE"]])
					if (is.null(parallel.config[["BACKEND"]][["CLUSTER.OBJECT"]])) {
						cluster.object <- "internal.cl"
					}	else cluster.object <- parallel.config[["BACKEND"]][["CLUSTER.OBJECT"]]

#					clusterExport(eval(parse(text=cluster.object)),list("sgp_object", "state")) #, "baselineSGP"
					clusterEvalQ(eval(parse(text=cluster.object)), library(SGP))

					#  Add check for having both WORKERS and [["CLUSTER.OBJECT"]] - shouldn't have both - warn and use [["CLUSTER.OBJECT"]]
					
					tmp <- parLapply(eval(parse(text=cluster.object)), par.baseline.config, function(sgp.iter) baselineSGP(
						sgp_object,
						state = state,
						years = sgp.iter[["baseline.panel.years"]],
						content_areas = sgp.iter[["baseline.content.areas"]],
						grade.sequences = sgp.iter[["baseline.grade.sequences"]],
						baseline.max.order = sgp.iter[["baseline.max.order"]],
						return.matrices.only=TRUE))
					
					for (s in 1:length(tmp))	sgp_object@SGP <- .mergeSGP(sgp_object@SGP, tmp[[s]])
					if (exists("internal.cl")) stopCluster(internal.cl)
					rm(tmp)
				} # END if (SNOW)
				
				if (toupper(parallel.config[["BACKEND"]][["TYPE"]]) == "MULTICORE") {
					if (is.null(workers)) workers = getOption("cores")
					tmp <- mclapply(par.baseline.config, function(sgp.iter) baselineSGP(
						sgp_object,
						state = state,
						years = sgp.iter[["baseline.panel.years"]],
						content_areas = sgp.iter[["baseline.content.areas"]],
						grade.sequences = sgp.iter[["baseline.grade.sequences"]],
						baseline.max.order = sgp.iter[["baseline.max.order"]],
						return.matrices.only=TRUE), mc.cores = workers, mc.preschedule = FALSE)
					for (s in 1:length(tmp))	sgp_object@SGP <- .mergeSGP(sgp_object@SGP, tmp[[s]])
					rm(tmp)
				} # END if (MULTICORE)
			}  else { # process sequentially
				for (sgp.iter in sgp.baseline.config) {
					sgp_object <- baselineSGP(sgp_object,
						state = state,
						years = sgp.iter[["baseline.panel.years"]],
						content_areas = sgp.iter[["baseline.content.areas"]],
						grade.sequences = sgp.iter[["baseline.grade.sequences"]],
						baseline.max.order = sgp.iter[["baseline.max.order"]])
				}
			}
			suppressMessages(gc()) # clean up
			Baseline_Matrices <- list()
			for (ca in names(sgp.baseline.config)){
				eval(parse(text=paste("Baseline_Matrices[['", as.character(ca), ".BASELINE']] = sgp_object@SGP[['Coefficient_Matrices']][['", as.character(ca), ".BASELINE']]", sep="")))
			}
			save(Baseline_Matrices, file=paste(state, "_",  "Baseline_Matrices.Rdata", sep=""), compress=TRUE) 
			message("\n\tDone computing baseline matrices...\n")
		} else	sgp_object@SGP <- .mergeSGP(sgp_object@SGP, stateData[[state]][["Baseline_splineMatrix"]])
	} # END Get/Compute baseline coef matrices


	#######################################################################################################################
	##   Percentiles, Baseline Percentiles, Projections, Lagged Projections -  PARALLEL FLAVORS FIRST
	#######################################################################################################################

	if (!is.null(parallel.config)) {
		###  INIITIAL SET UP FOR ALL ANALYSES AND BACKEND TYPES
		# RE-Key data to select only grades in grade progression
		key(sgp_object@Data) <- c("VALID_CASE", "CONTENT_AREA", "YEAR", "GRADE")

		#  Stretch out sgp.config and add info for baseline sgps
		par.sgp.config <- list(); cnt <- 1
		for (a in names(sgp.config)) {
			for (b in 1:length(sgp.config[[a]][["sgp.grade.sequences"]])) {
				par.sgp.config[[cnt]] <- sgp.config[[a]]
				par.sgp.config[[cnt]][["sgp.grade.sequences"]] <- sgp.config[[a]][["sgp.grade.sequences"]][b]
				if (any(diff(par.sgp.config[[cnt]][["sgp.grade.sequences"]][[1]])>1)) {
					grade.span <- seq(min(par.sgp.config[[cnt]][["sgp.grade.sequences"]][[1]]),max(par.sgp.config[[cnt]][["sgp.grade.sequences"]][[1]]))
					index <- match(par.sgp.config[[cnt]][["sgp.grade.sequences"]][[1]], grade.span)
					par.sgp.config[[cnt]][["sgp.panel.years"]] <- par.sgp.config[[cnt]][["sgp.panel.years"]][index]
					par.sgp.config[[cnt]][["sgp.content.areas"]] <- par.sgp.config[[cnt]][["sgp.content.areas"]][index]
				}
				
				if (sgp.percentiles.baseline) {
					mtx.names <- names(sgp_object@SGP[["Coefficient_Matrices"]][[paste(strsplit(a, "\\.")[[1]][1], ".BASELINE", sep="")]])
					max.order.mtx <- mtx.names[[max(grep(paste("qrmatrix_", tail(par.sgp.config[[cnt]][["sgp.grade.sequences"]][[1]], 1), sep=""), mtx.names))]]
					max.order <- as.numeric(strsplit(max.order.mtx, "_")[[1]][3])
					if (length(par.sgp.config[[cnt]][["sgp.panel.years"]])-1 < max.order) max.order <- length(par.sgp.config[[cnt]][["sgp.panel.years"]])-1			
					if (sum(diff(tail(par.sgp.config[[cnt]][["sgp.grade.sequences"]][[1]], 1+max.order))) > length(par.sgp.config[[cnt]][["sgp.panel.years"]])) {
						base.gp <- par.sgp.config[[cnt]][["sgp.grade.sequences"]][[1]][tail(par.sgp.config[[cnt]][["sgp.grade.sequences"]][[1]], 1) - 
							par.sgp.config[[cnt]][["sgp.grade.sequences"]][[1]] <= max.order]
						max.order <- length(base.gp) - 1
					}	else base.gp <- tail(par.sgp.config[[cnt]][["sgp.grade.sequences"]][[1]], 1+max.order)  # not sure we need this if statement since years are corrected 9/27/11
					par.sgp.config[[cnt]][["base.gp"]] <- base.gp
					par.sgp.config[[cnt]][["max.order"]] <- max.order
				}
				cnt <- cnt + 1
			}
		}

		# create text objects to eval(parse()) in functions
		pctls.data <- "as.data.frame(reshape(sgp_object@Data[J('VALID_CASE', tail(sgp.iter[['sgp.content.areas']], length(sgp.iter[['sgp.grade.sequences']][[1]])), 
			tail(sgp.iter[['sgp.panel.years']], length(sgp.iter[['sgp.grade.sequences']][[1]])), sgp.iter[['sgp.grade.sequences']][[1]]), mult='all'],
			idvar='ID',
			timevar='YEAR',
			drop=names(sgp_object@Data)[!names(sgp_object@Data) %in% c('ID', 'GRADE', 'SCALE_SCORE', 'YEAR')],
			direction='wide'))"
					
		pctls.vnames <- "c('ID', paste('GRADE', tail(sgp.iter[['sgp.panel.years']], length(sgp.iter[['sgp.grade.sequences']][[1]])), sep='.'), 
			paste('SCALE_SCORE', tail(sgp.iter[['sgp.panel.years']], length(sgp.iter[['sgp.grade.sequences']][[1]])), sep='.'))"

		projs.data <- "as.data.frame(reshape(sgp_object@Data[J('VALID_CASE', tail(sgp.iter[['sgp.content.areas']], length(sgp.iter[['sgp.grade.sequences']][[1]])-1), 
			tail(sgp.iter[['sgp.panel.years']], length(sgp.iter[['sgp.grade.sequences']][[1]])-1), head(sgp.iter[['sgp.grade.sequences']][[1]], -1)), mult='all'],
			idvar='ID',
			timevar='YEAR',
			drop=names(sgp_object@Data)[!names(sgp_object@Data) %in% c('ID', 'GRADE', 'SCALE_SCORE', 'YEAR')],
			direction='wide'))"

		projs.vnames <- "c('ID', paste('GRADE', tail(sgp.iter[['sgp.panel.years']], length(sgp.iter[['sgp.grade.sequences']][[1]])-1), sep='.'), 
			paste('SCALE_SCORE', tail(sgp.iter[['sgp.panel.years']], length(sgp.iter[['sgp.grade.sequences']][[1]])-1), sep='.'))"
		
		lagged.projs.data <- "as.data.frame(reshape(sgp_object@Data[J('VALID_CASE', tail(sgp.iter[['sgp.content.areas']], length(sgp.iter[['sgp.grade.sequences']][[1]])-1), 
			head(tail(sgp.iter[['sgp.panel.years']], length(sgp.iter[['sgp.grade.sequences']][[1]])), -1), head(sgp.iter[['sgp.grade.sequences']][[1]], -1)), mult='all'],
			idvar='ID',
			timevar='YEAR',
			drop=names(sgp_object@Data)[!names(sgp_object@Data) %in% c('ID', 'GRADE', 'SCALE_SCORE', 'YEAR')],
			direction='wide'))"

		lagged.projs.vnames <- "c('ID', paste('GRADE', head(tail(sgp.iter[['sgp.panel.years']], length(sgp.iter[['sgp.grade.sequences']][[1]])), -1), sep='.'), 
			paste('SCALE_SCORE', head(tail(sgp.iter[['sgp.panel.years']], length(sgp.iter[['sgp.grade.sequences']][[1]])), -1), sep='.'))"


	##################################		
	###  PERCENTILES
	##################################

		if (sgp.percentiles) {
			workers <- NULL
			if (!is.null(parallel.config[["ANALYSES"]][["PERCENTILE_WORKERS"]])) workers <- parallel.config[["ANALYSES"]][["PERCENTILE_WORKERS"]]
			if (!is.null(parallel.config[["ANALYSES"]][["WORKERS"]])) workers <- parallel.config[["ANALYSES"]][["WORKERS"]]
		
			###  FOREACH flavor
			if (toupper(parallel.config[["BACKEND"]][["TYPE"]]) == "FOREACH") {
				if (!is.null(workers)) eval(parse(text=paste("registerDo", substr(parallel.config[["BACKEND"]][["FOREACH_TYPE"]], 3, 9), "(workers)", sep=""))) # registerDo*(workers)

				if (simulate.sgps) {
					if (!exists("calculate.confidence.intervals")) {
						calculate.confidence.intervals <- state
					}
					tmp <- foreach(sgp.iter=iter(par.sgp.config), .packages="SGP", .combine=".mergeSGP", .inorder=FALSE, .export="sgp_object",
						.options.multicore=foreach.options, .options.mpi= foreach.options, .options.redis= foreach.options, .options.smp= foreach.options) %dopar% {
						return(studentGrowthPercentiles(
							panel.data=list(Panel_Data=eval(parse(text= pctls.data)), Coefficient_Matrices=sgp_object@SGP[["Coefficient_Matrices"]],
								Knots_Boundaries=sgp_object@SGP[["Knots_Boundaries"]]),,
							sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)),
							use.my.knots.boundaries=state,
							growth.levels=state,
							calculate.confidence.intervals=calculate.confidence.intervals,
							panel.data.vnames=eval(parse(text=pctls.vnames)),
							grade.progression=sgp.iter[['sgp.grade.sequences']][[1]],
							drop.nonsequential.grade.progression.variables=FALSE, # taken care of with config
							...))
					}
				} else {
					tmp <- foreach(sgp.iter=iter(par.sgp.config), .packages="SGP", .combine=".mergeSGP", .inorder=FALSE, .export="sgp_object",
						.options.multicore=foreach.options, .options.mpi= foreach.options, .options.redis= foreach.options, .options.smp= foreach.options) %dopar% {
						return(studentGrowthPercentiles(
							panel.data=list(Panel_Data=eval(parse(text= pctls.data)), Coefficient_Matrices=sgp_object@SGP[["Coefficient_Matrices"]],
								Knots_Boundaries=sgp_object@SGP[["Knots_Boundaries"]]),,
							sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)),
							use.my.knots.boundaries=state,
							growth.levels=state,
							panel.data.vnames=eval(parse(text=pctls.vnames)),
							grade.progression=sgp.iter[['sgp.grade.sequences']][[1]],
							drop.nonsequential.grade.progression.variables=FALSE, # taken care of with config
							...))
					}
				}
				sgp_object@SGP <- .mergeSGP(sgp_object@SGP, tmp)
				rm(tmp)
			} # END FOREACH
			
			###  SNOW flavor
			if (toupper(parallel.config[["BACKEND"]][["TYPE"]]) == "SNOW") {
				if (!is.null(workers)) internal.cl <- makeCluster(workers, type=parallel.config[["BACKEND"]][["SNOW_TYPE"]])
				if (is.null(parallel.config[["BACKEND"]][["CLUSTER.OBJECT"]])) {
					cluster.object <- "internal.cl"
				}	else cluster.object <- parallel.config[["BACKEND"]][["CLUSTER.OBJECT"]]

#				clusterExport(eval(parse(text=cluster.object)),list("sgp_object", "state", "pctls.data", "pctls.vnames"))
				clusterEvalQ(eval(parse(text=cluster.object)), library(SGP))

				if (simulate.sgps) {
					if (!exists("calculate.confidence.intervals")) {
						calculate.confidence.intervals <- state
					}
					tmp <- parLapply(eval(parse(text=cluster.object)), par.sgp.config, 	function(sgp.iter)	studentGrowthPercentiles( 
						panel.data=list(Panel_Data=eval(parse(text= pctls.data)), Coefficient_Matrices=sgp_object@SGP[["Coefficient_Matrices"]],
							Knots_Boundaries=sgp_object@SGP[["Knots_Boundaries"]]),
						sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)),
						use.my.knots.boundaries=state,
						growth.levels=state,
						calculate.confidence.intervals=calculate.confidence.intervals,
						panel.data.vnames=eval(parse(text=pctls.vnames)),
						grade.progression=sgp.iter[['sgp.grade.sequences']][[1]],
						drop.nonsequential.grade.progression.variables=FALSE, # taken care of with config
						...))
				} else {
					tmp <- parLapply(eval(parse(text=cluster.object)), par.sgp.config, 	function(sgp.iter)	studentGrowthPercentiles( 
						panel.data=list(Panel_Data=eval(parse(text= pctls.data)), Coefficient_Matrices=sgp_object@SGP[["Coefficient_Matrices"]],
							Knots_Boundaries=sgp_object@SGP[["Knots_Boundaries"]]),,
						sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)),
						use.my.knots.boundaries=state,
						growth.levels=state,
						panel.data.vnames=eval(parse(text=pctls.vnames)),
						grade.progression=sgp.iter[['sgp.grade.sequences']][[1]],
						drop.nonsequential.grade.progression.variables=FALSE, # taken care of with config
						...))
				}
				for (s in 1:length(tmp))	sgp_object@SGP <- .mergeSGP(sgp_object@SGP, tmp[[s]])
				rm(tmp)
				if (exists("internal.cl")) stopCluster(internal.cl)
			} # END SNOW
			
			###  MULTICORE flavor
			if (toupper(parallel.config[["BACKEND"]][["TYPE"]]) == "MULTICORE") {
				if (is.null(workers)) workers = getOption("cores")

				if (simulate.sgps) {
					if (!exists("calculate.confidence.intervals")) {
						calculate.confidence.intervals <- state
					}
					tmp <- mclapply(par.sgp.config, function(sgp.iter)	studentGrowthPercentiles( 
						panel.data=list(Panel_Data=eval(parse(text= pctls.data)), Coefficient_Matrices=sgp_object@SGP[["Coefficient_Matrices"]],
							Knots_Boundaries=sgp_object@SGP[["Knots_Boundaries"]]),
						sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)),
						use.my.knots.boundaries=state,
						growth.levels=state,
						calculate.confidence.intervals=calculate.confidence.intervals,
						panel.data.vnames=eval(parse(text=pctls.vnames)),
						grade.progression=sgp.iter[['sgp.grade.sequences']][[1]],
						drop.nonsequential.grade.progression.variables=FALSE, # taken care of with config
						...), mc.cores=workers, mc.preschedule = FALSE)
				} else {
					tmp <- mclapply(par.sgp.config, function(sgp.iter)	studentGrowthPercentiles( 
						panel.data=list(Panel_Data=eval(parse(text= pctls.data)), Coefficient_Matrices=sgp_object@SGP[["Coefficient_Matrices"]],
							Knots_Boundaries=sgp_object@SGP[["Knots_Boundaries"]]),
						sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)),
						use.my.knots.boundaries=state,
						growth.levels=state,
						panel.data.vnames=eval(parse(text=pctls.vnames)),
						grade.progression=sgp.iter[['sgp.grade.sequences']][[1]],
						drop.nonsequential.grade.progression.variables=FALSE, # taken care of with config
						...), mc.cores=workers, mc.preschedule = FALSE)
				}
				for (s in 1:length(tmp))	sgp_object@SGP <- .mergeSGP(sgp_object@SGP, tmp[[s]])
				rm(tmp)
			} # End MULTICORE
		suppressMessages(gc()) # clean up
		} #END if (sgp.percentiles)


	####################################
	###  BASELINE PERCENTILES
	####################################

		if (sgp.percentiles.baseline) {
			workers <- NULL
			if (!is.null(parallel.config[["ANALYSES"]][["BASELINE_SGP_WORKERS"]])) workers <- parallel.config[["ANALYSES"]][["BASELINE_SGP_WORKERS"]]
			if (!is.null(parallel.config[["ANALYSES"]][["WORKERS"]])) workers <- parallel.config[["ANALYSES"]][["WORKERS"]]
		
			###  FOREACH flavor
			if (toupper(parallel.config[["BACKEND"]][["TYPE"]]) == "FOREACH") {
				if (!is.null(workers)) eval(parse(text=paste("registerDo", substr(parallel.config[["BACKEND"]][["FOREACH_TYPE"]], 3, 9), "(workers)", sep=""))) # registerDo*(workers)

				tmp <- foreach(sgp.iter=iter(par.sgp.config), .packages="SGP", .combine=".mergeSGP", .inorder=FALSE, .export="sgp_object",
					.options.multicore=foreach.options, .options.mpi= foreach.options, .options.redis= foreach.options, .options.smp= foreach.options) %dopar% {
					return(studentGrowthPercentiles(
						panel.data = list(Panel_Data=eval(parse(text= pctls.data)), Coefficient_Matrices=sgp_object@SGP[["Coefficient_Matrices"]],
							Knots_Boundaries=sgp_object@SGP[["Knots_Boundaries"]]),
						sgp.labels = list(my.year = tail(sgp.iter[["sgp.panel.years"]], 1), 
							my.subject = tail(sgp.iter[["sgp.content.areas"]], 1), my.extra.label="BASELINE"),
						use.my.knots.boundaries = state,
						use.my.coefficient.matrices = list(my.year = "BASELINE", my.subject = tail(sgp.iter[["sgp.content.areas"]], 1)),
						growth.levels = state,
						panel.data.vnames = eval(parse(text=pctls.vnames)),
						grade.progression = sgp.iter[["base.gp"]],
						num.prior =sgp.iter[["max.order"]],
						goodness.of.fit=TRUE,
						drop.nonsequential.grade.progression.variables=FALSE, # taken care of with config
						...))
					}
				sgp_object@SGP <- .mergeSGP(sgp_object@SGP, tmp)
				rm(tmp)
				} # END FOREACH
			
			###  SNOW flavor
			if (toupper(parallel.config[["BACKEND"]][["TYPE"]]) == "SNOW") {
				if (!is.null(workers)) internal.cl <- makeCluster(workers, type=parallel.config[["BACKEND"]][["SNOW_TYPE"]])
				if (is.null(parallel.config[["BACKEND"]][["CLUSTER.OBJECT"]])) {
					cluster.object <- "internal.cl"
				}	else cluster.object <- parallel.config[["BACKEND"]][["CLUSTER.OBJECT"]]

#				clusterExport(eval(parse(text=cluster.object)),list("sgp_object", "state", "pctls.data", "pctls.vnames"))
				clusterEvalQ(eval(parse(text=cluster.object)), library(SGP))

				tmp <- parLapply(eval(parse(text=cluster.object)), par.sgp.config, 	function(sgp.iter)	studentGrowthPercentiles(
					panel.data = list(Panel_Data=eval(parse(text= pctls.data)), Coefficient_Matrices=sgp_object@SGP[["Coefficient_Matrices"]],
						Knots_Boundaries=sgp_object@SGP[["Knots_Boundaries"]]),
					sgp.labels = list(my.year = tail(sgp.iter[["sgp.panel.years"]], 1), 
						my.subject = tail(sgp.iter[["sgp.content.areas"]], 1), my.extra.label="BASELINE"),
					use.my.knots.boundaries = state,
					use.my.coefficient.matrices = list(my.year = "BASELINE", my.subject = tail(sgp.iter[["sgp.content.areas"]], 1)),
					growth.levels = state,
					panel.data.vnames = eval(parse(text=pctls.vnames)),
					grade.progression = sgp.iter[["base.gp"]],
					num.prior =sgp.iter[["max.order"]],
					goodness.of.fit=TRUE,
					drop.nonsequential.grade.progression.variables=FALSE, # taken care of with config
					...))

				for (s in 1:length(tmp))	sgp_object@SGP <- .mergeSGP(sgp_object@SGP, tmp[[s]])
				rm(tmp)
				if (exists("internal.cl")) stopCluster(internal.cl)
			} # END SNOW
			
			###  MULTICORE flavor
			if (toupper(parallel.config[["BACKEND"]][["TYPE"]]) == "MULTICORE") {
				if (is.null(workers)) workers = getOption("cores")

				tmp <- mclapply(par.sgp.config, function(sgp.iter)	studentGrowthPercentiles(
					panel.data = list(Panel_Data=eval(parse(text= pctls.data)), Coefficient_Matrices=sgp_object@SGP[["Coefficient_Matrices"]],
						Knots_Boundaries=sgp_object@SGP[["Knots_Boundaries"]]),
					sgp.labels = list(my.year = tail(sgp.iter[["sgp.panel.years"]], 1), 
						my.subject = tail(sgp.iter[["sgp.content.areas"]], 1), my.extra.label="BASELINE"),
					use.my.knots.boundaries = state,
					use.my.coefficient.matrices = list(my.year = "BASELINE", my.subject = tail(sgp.iter[["sgp.content.areas"]], 1)),
					growth.levels = state,
					panel.data.vnames = eval(parse(text=pctls.vnames)),
					grade.progression = sgp.iter[["base.gp"]],
					num.prior = sgp.iter[["max.order"]],
					goodness.of.fit=TRUE,
					drop.nonsequential.grade.progression.variables=FALSE, # taken care of with config
					...), mc.cores=workers, mc.preschedule = FALSE)

#					sgp_object@SGP <- mclapply(tmp, .mergeSGP, sgp_object, mc.cores=workers, mc.preschedule = FALSE)
				for (s in 1:length(tmp))	sgp_object@SGP <- .mergeSGP(sgp_object@SGP, tmp[[s]])
				rm(tmp)
			} # End MULTICORE
		suppressMessages(gc()) # clean up
		} ## END if sgp.percentiles.baseline


	#######################################################
	###  PROJECTIONS (COHORT referenced)
	#######################################################

		if (sgp.projections) {
			workers <- NULL
			if (!is.null(parallel.config[["ANALYSES"]][["PROJECTIONS_WORKERS"]])) workers <- parallel.config[["ANALYSES"]][["PROJECTIONS_WORKERS"]]
			if (!is.null(parallel.config[["ANALYSES"]][["WORKERS"]])) workers <- parallel.config[["ANALYSES"]][["WORKERS"]]
		
			###  FOREACH flavor
			if (toupper(parallel.config[["BACKEND"]][["TYPE"]]) == "FOREACH") {
				if (!is.null(workers)) eval(parse(text=paste("registerDo", substr(parallel.config[["BACKEND"]][["FOREACH_TYPE"]], 3, 9), "(workers)", sep=""))) # registerDo*(workers)

				tmp <- foreach(sgp.iter=iter(par.sgp.config), .packages="SGP", .combine=".mergeSGP", .inorder=FALSE, .export="sgp_object",
					.options.multicore=foreach.options, .options.mpi= foreach.options, .options.redis= foreach.options, .options.smp= foreach.options) %dopar% {
					return(studentGrowthProjections(
						panel.data=list(Panel_Data=eval(parse(text= projs.data)), Coefficient_Matrices=sgp_object@SGP[["Coefficient_Matrices"]],
							Knots_Boundaries=sgp_object@SGP[["Knots_Boundaries"]]),
						sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)),
						use.my.coefficient.matrices=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
						use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
						performance.level.cutscores=state,
						max.forward.progression.years=3,
						percentile.trajectory.values=c(1, stateData[[state]][["Growth"]][["Cutscores"]][["Cuts"]], 99),
						panel.data.vnames=eval(parse(text=projs.vnames)),
						grade.progression=head(sgp.iter[["sgp.grade.sequences"]][[1]], -1),
						...))
				}
				sgp_object@SGP <- .mergeSGP(sgp_object@SGP, tmp)
				rm(tmp)
			} # END FOREACH

			###  SNOW flavor
			if (toupper(parallel.config[["BACKEND"]][["TYPE"]]) == "SNOW") {
				if (!is.null(workers)) internal.cl <- makeCluster(workers, type=parallel.config[["BACKEND"]][["SNOW_TYPE"]])
				if (is.null(parallel.config[["BACKEND"]][["CLUSTER.OBJECT"]])) {
					cluster.object <- "internal.cl"
				}	else cluster.object <- parallel.config[["BACKEND"]][["CLUSTER.OBJECT"]]

#				clusterExport(eval(parse(text=cluster.object)),list("sgp_object", "state", "projs.data", "projs.vnames"))
				clusterEvalQ(eval(parse(text=cluster.object)), library(SGP))

				tmp <- parLapply(eval(parse(text=cluster.object)), par.sgp.config, 	function(sgp.iter)	studentGrowthProjections(
					panel.data=list(Panel_Data=eval(parse(text= projs.data)), Coefficient_Matrices=sgp_object@SGP[["Coefficient_Matrices"]],
						Knots_Boundaries=sgp_object@SGP[["Knots_Boundaries"]]),
					sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)),
					use.my.coefficient.matrices=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
					use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
					performance.level.cutscores=state,
					max.forward.progression.years=3,
					percentile.trajectory.values=c(1, stateData[[state]][["Growth"]][["Cutscores"]][["Cuts"]], 99),
					panel.data.vnames=eval(parse(text=projs.vnames)),
					grade.progression=head(sgp.iter[["sgp.grade.sequences"]][[1]], -1),
					...))

				for (s in 1:length(tmp))	sgp_object@SGP <- .mergeSGP(sgp_object@SGP, tmp[[s]])
				rm(tmp)
				if (exists("internal.cl")) stopCluster(internal.cl)
			} # END SNOW
			
			###  MULTICORE flavor
			if (toupper(parallel.config[["BACKEND"]][["TYPE"]]) == "MULTICORE") {
				if (is.null(workers)) workers = getOption("cores")

				tmp <- mclapply(par.sgp.config, function(sgp.iter)	studentGrowthProjections(
					panel.data=list(Panel_Data=eval(parse(text= projs.data)), Coefficient_Matrices=sgp_object@SGP[["Coefficient_Matrices"]],
						Knots_Boundaries=sgp_object@SGP[["Knots_Boundaries"]]),
					sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)),
					use.my.coefficient.matrices=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
					use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
					performance.level.cutscores=state,
					max.forward.progression.years=3,
					percentile.trajectory.values=c(1, stateData[[state]][["Growth"]][["Cutscores"]][["Cuts"]], 99),
					panel.data.vnames=eval(parse(text=projs.vnames)),
					grade.progression=head(sgp.iter[["sgp.grade.sequences"]][[1]], -1),
					...), mc.cores=workers, mc.preschedule = FALSE)

				for (s in 1:length(tmp))	sgp_object@SGP <- .mergeSGP(sgp_object@SGP, tmp[[s]])
				rm(tmp)
			} # End MULTICORE
		suppressMessages(gc()) # clean up
		} ## END if sgp.projections


	#######################################################
	###  PROJECTIONS (BASELINE referenced)
	#######################################################

		if (sgp.projections.baseline) {
			workers <- NULL
			if (!is.null(parallel.config[["ANALYSES"]][["PROJECTIONS_WORKERS"]])) workers <- parallel.config[["ANALYSES"]][["PROJECTIONS_WORKERS"]]
			if (!is.null(parallel.config[["ANALYSES"]][["WORKERS"]])) workers <- parallel.config[["ANALYSES"]][["WORKERS"]]
		
			###  FOREACH flavor
			if (toupper(parallel.config[["BACKEND"]][["TYPE"]]) == "FOREACH") {
				if (!is.null(workers)) eval(parse(text=paste("registerDo", substr(parallel.config[["BACKEND"]][["FOREACH_TYPE"]], 3, 9), "(workers)", sep=""))) # registerDo*(workers)

				tmp <- foreach(sgp.iter=iter(par.sgp.config), .packages="SGP", .combine=".mergeSGP", .inorder=FALSE, .export="sgp_object",
					.options.multicore=foreach.options, .options.mpi= foreach.options, .options.redis= foreach.options, .options.smp= foreach.options) %dopar% {
					return(studentGrowthProjections(
						panel.data=list(Panel_Data=eval(parse(text= projs.data)), Coefficient_Matrices=sgp_object@SGP[["Coefficient_Matrices"]],
							Knots_Boundaries=sgp_object@SGP[["Knots_Boundaries"]]),
						sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1), 
							my.extra.label="BASELINE"),
						use.my.coefficient.matrices=list(my.year="BASELINE", my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
						use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
						performance.level.cutscores=state,
						max.forward.progression.years=3,
						percentile.trajectory.values=c(1, stateData[[state]][["Growth"]][["Cutscores"]][["Cuts"]], 99),
						panel.data.vnames=eval(parse(text=projs.vnames)),
						grade.progression=head(sgp.iter[["sgp.grade.sequences"]][[1]], -1),
						...))
				}
				sgp_object@SGP <- .mergeSGP(sgp_object@SGP, tmp)
				rm(tmp)
			} # END FOREACH

			###  SNOW flavor
			if (toupper(parallel.config[["BACKEND"]][["TYPE"]]) == "SNOW") {
				if (!is.null(workers)) internal.cl <- makeCluster(workers, type=parallel.config[["BACKEND"]][["SNOW_TYPE"]])
				if (is.null(parallel.config[["BACKEND"]][["CLUSTER.OBJECT"]])) {
					cluster.object <- "internal.cl"
				}	else cluster.object <- parallel.config[["BACKEND"]][["CLUSTER.OBJECT"]]

#				clusterExport(eval(parse(text=cluster.object)),list("sgp_object", "state", "projs.data", "projs.vnames"))
				clusterEvalQ(eval(parse(text=cluster.object)), library(SGP))

				tmp <- parLapply(eval(parse(text=cluster.object)), par.sgp.config, 	function(sgp.iter)	studentGrowthProjections(
					panel.data=list(Panel_Data=eval(parse(text= projs.data)), Coefficient_Matrices=sgp_object@SGP[["Coefficient_Matrices"]],
						Knots_Boundaries=sgp_object@SGP[["Knots_Boundaries"]]),
					sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1), 
						my.extra.label="BASELINE"),
					use.my.coefficient.matrices=list(my.year="BASELINE", my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
					use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
					performance.level.cutscores=state,
					max.forward.progression.years=3,
					percentile.trajectory.values=c(1, stateData[[state]][["Growth"]][["Cutscores"]][["Cuts"]], 99),
					panel.data.vnames=eval(parse(text=projs.vnames)),
					grade.progression=head(sgp.iter[["sgp.grade.sequences"]][[1]], -1),
					...))

				for (s in 1:length(tmp))	sgp_object@SGP <- .mergeSGP(sgp_object@SGP, tmp[[s]])
				rm(tmp)
				if (exists("internal.cl")) stopCluster(internal.cl)
			} # END SNOW
			
			###  MULTICORE flavor
			if (toupper(parallel.config[["BACKEND"]][["TYPE"]]) == "MULTICORE") {
				if (is.null(workers)) workers = getOption("cores")

				tmp <- mclapply(par.sgp.config, function(sgp.iter)	studentGrowthProjections(
					panel.data=list(Panel_Data=eval(parse(text= projs.data)), Coefficient_Matrices=sgp_object@SGP[["Coefficient_Matrices"]],
						Knots_Boundaries=sgp_object@SGP[["Knots_Boundaries"]]),
					sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1), 
						my.extra.label="BASELINE"),
					use.my.coefficient.matrices=list(my.year="BASELINE", my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
					use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
					performance.level.cutscores=state,
					max.forward.progression.years=3,
					percentile.trajectory.values=c(1, stateData[[state]][["Growth"]][["Cutscores"]][["Cuts"]], 99),
					panel.data.vnames=eval(parse(text=projs.vnames)),
					grade.progression=head(sgp.iter[["sgp.grade.sequences"]][[1]], -1),
					...), mc.cores=workers, mc.preschedule = FALSE)

				for (s in 1:length(tmp))	sgp_object@SGP <- .mergeSGP(sgp_object@SGP, tmp[[s]])
				rm(tmp)
			} # End MULTICORE
		suppressMessages(gc()) # clean up
		} ## END if sgp.projections.baseline



	#################################################
	###  LAGGED PROJECTIONS (COHORT Referenced)
	#################################################

		if (sgp.projections.lagged) {
			workers <- NULL
			if (!is.null(parallel.config[["ANALYSES"]][["LAGGED_PROJECTIONS_WORKERS"]])) workers <- parallel.config[["ANALYSES"]][["LAGGED_PROJECTIONS_WORKERS"]]
			if (!is.null(parallel.config[["ANALYSES"]][["WORKERS"]])) workers <- parallel.config[["ANALYSES"]][["WORKERS"]]
		
			###  FOREACH flavor
			if (toupper(parallel.config[["BACKEND"]][["TYPE"]]) == "FOREACH") {
				if (!is.null(workers)) eval(parse(text=paste("registerDo", substr(parallel.config[["BACKEND"]][["FOREACH_TYPE"]], 3, 9), "(workers)", sep=""))) # registerDo*(workers)

				tmp <- foreach(sgp.iter=iter(par.sgp.config), .packages="SGP", .combine=".mergeSGP", .inorder=FALSE, .export="sgp_object",
					.options.multicore=foreach.options, .options.mpi= foreach.options, .options.redis= foreach.options, .options.smp= foreach.options) %dopar% {
					return(studentGrowthProjections(
						panel.data=list(Panel_Data=eval(parse(text=lagged.projs.data)), Coefficient_Matrices=sgp_object@SGP[["Coefficient_Matrices"]],
							Knots_Boundaries=sgp_object@SGP[["Knots_Boundaries"]]),
						sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1), 
							my.extra.label="LAGGED"),
						use.my.coefficient.matrices=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
						use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
						performance.level.cutscores=state,
						panel.data.vnames=eval(parse(text=lagged.projs.vnames)),
						grade.progression=head(sgp.iter[["sgp.grade.sequences"]][[1]], -1),
						...))
				}
				sgp_object@SGP <- .mergeSGP(sgp_object@SGP, tmp)
				rm(tmp)
			} # END FOREACH

			###  SNOW flavor
			if (toupper(parallel.config[["BACKEND"]][["TYPE"]]) == "SNOW") {
				if (!is.null(workers)) internal.cl <- makeCluster(workers, type=parallel.config[["BACKEND"]][["SNOW_TYPE"]])
				if (is.null(parallel.config[["BACKEND"]][["CLUSTER.OBJECT"]])) {
					cluster.object <- "internal.cl"
				}	else cluster.object <- parallel.config[["BACKEND"]][["CLUSTER.OBJECT"]]

#				clusterExport(eval(parse(text=cluster.object)),list("sgp_object", "state", "lagged.projs.data", "lagged.projs.vnames"))
				clusterEvalQ(eval(parse(text=cluster.object)), library(SGP))

				tmp <- parLapply(eval(parse(text=cluster.object)), par.sgp.config, 	function(sgp.iter)	studentGrowthProjections(
					panel.data=list(Panel_Data=eval(parse(text=lagged.projs.data)), Coefficient_Matrices=sgp_object@SGP[["Coefficient_Matrices"]],
						Knots_Boundaries=sgp_object@SGP[["Knots_Boundaries"]]),
					sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1), 
						my.extra.label="LAGGED"),
					use.my.coefficient.matrices=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
					use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
					performance.level.cutscores=state,
					panel.data.vnames=eval(parse(text=lagged.projs.vnames)),
					grade.progression=head(sgp.iter[["sgp.grade.sequences"]][[1]], -1),
					...))

				for (s in 1:length(tmp))	sgp_object@SGP <- .mergeSGP(sgp_object@SGP, tmp[[s]])
				rm(tmp)
				if (exists("internal.cl")) stopCluster(internal.cl)
			} # END SNOW
			
			###  MULTICORE flavor
			if (toupper(parallel.config[["BACKEND"]][["TYPE"]]) == "MULTICORE") {
				if (is.null(workers)) workers = getOption("cores")

				tmp <- mclapply(par.sgp.config, function(sgp.iter)	studentGrowthProjections(
					panel.data=list(Panel_Data=eval(parse(text=lagged.projs.data)), Coefficient_Matrices=sgp_object@SGP[["Coefficient_Matrices"]],
						Knots_Boundaries=sgp_object@SGP[["Knots_Boundaries"]]),
					sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1), 
						my.extra.label="LAGGED"),
					use.my.coefficient.matrices=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
					use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
					performance.level.cutscores=state,
					panel.data.vnames=eval(parse(text=lagged.projs.vnames)),
					grade.progression=head(sgp.iter[["sgp.grade.sequences"]][[1]], -1),
					...), mc.cores=workers, mc.preschedule = FALSE)

				for (s in 1:length(tmp))	sgp_object@SGP <- .mergeSGP(sgp_object@SGP, tmp[[s]])
				rm(tmp)
			} # End MULTICORE
		suppressMessages(gc()) # clean up
		} ## END if sgp.projections.lagged


	#################################################
	###  LAGGED PROJECTIONS (BASELINE Referenced)
	#################################################

		if (sgp.projections.lagged.baseline) {
			workers <- NULL
			if (!is.null(parallel.config[["ANALYSES"]][["LAGGED_PROJECTIONS_WORKERS"]])) workers <- parallel.config[["ANALYSES"]][["LAGGED_PROJECTIONS_WORKERS"]]
			if (!is.null(parallel.config[["ANALYSES"]][["WORKERS"]])) workers <- parallel.config[["ANALYSES"]][["WORKERS"]]
		
			###  FOREACH flavor
			if (toupper(parallel.config[["BACKEND"]][["TYPE"]]) == "FOREACH") {
				if (!is.null(workers)) eval(parse(text=paste("registerDo", substr(parallel.config[["BACKEND"]][["FOREACH_TYPE"]], 3, 9), "(workers)", sep=""))) # registerDo*(workers)

				tmp <- foreach(sgp.iter=iter(par.sgp.config), .packages="SGP", .combine=".mergeSGP", .inorder=FALSE, .export="sgp_object",
					.options.multicore=foreach.options, .options.mpi= foreach.options, .options.redis= foreach.options, .options.smp= foreach.options) %dopar% {
					return(studentGrowthProjections(
						panel.data=list(Panel_Data=eval(parse(text=lagged.projs.data)), Coefficient_Matrices=sgp_object@SGP[["Coefficient_Matrices"]],
							Knots_Boundaries=sgp_object@SGP[["Knots_Boundaries"]]),
						sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1), 
							my.extra.label="LAGGED.BASELINE"),
						use.my.coefficient.matrices=list(my.year="BASELINE", my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
						use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
						performance.level.cutscores=state,
						panel.data.vnames=eval(parse(text=lagged.projs.vnames)),
						grade.progression=head(sgp.iter[["sgp.grade.sequences"]][[1]], -1),
						...))
				}
				sgp_object@SGP <- .mergeSGP(sgp_object@SGP, tmp)
				rm(tmp)
			} # END FOREACH

			###  SNOW flavor
			if (toupper(parallel.config[["BACKEND"]][["TYPE"]]) == "SNOW") {
				if (!is.null(workers)) internal.cl <- makeCluster(workers, type=parallel.config[["BACKEND"]][["SNOW_TYPE"]])
				if (is.null(parallel.config[["BACKEND"]][["CLUSTER.OBJECT"]])) {
					cluster.object <- "internal.cl"
				}	else cluster.object <- parallel.config[["BACKEND"]][["CLUSTER.OBJECT"]]

#				clusterExport(eval(parse(text=cluster.object)),list("sgp_object", "state", "lagged.projs.data", "lagged.projs.vnames"))
				clusterEvalQ(eval(parse(text=cluster.object)), library(SGP))

				tmp <- parLapply(eval(parse(text=cluster.object)), par.sgp.config, 	function(sgp.iter)	studentGrowthProjections(
					panel.data=list(Panel_Data=eval(parse(text=lagged.projs.data)), Coefficient_Matrices=sgp_object@SGP[["Coefficient_Matrices"]],
						Knots_Boundaries=sgp_object@SGP[["Knots_Boundaries"]]),
					sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1), 
						my.extra.label="LAGGED.BASELINE"),
					use.my.coefficient.matrices=list(my.year="BASELINE", my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
					use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
					performance.level.cutscores=state,
					panel.data.vnames=eval(parse(text=lagged.projs.vnames)),
					grade.progression=head(sgp.iter[["sgp.grade.sequences"]][[1]], -1),
					...))

				for (s in 1:length(tmp))	sgp_object@SGP <- .mergeSGP(sgp_object@SGP, tmp[[s]])
				rm(tmp)
				if (exists("internal.cl")) stopCluster(internal.cl)
			} # END SNOW
			
			###  MULTICORE flavor
			if (toupper(parallel.config[["BACKEND"]][["TYPE"]]) == "MULTICORE") {
				if (is.null(workers)) workers = getOption("cores")

				tmp <- mclapply(par.sgp.config, function(sgp.iter)	studentGrowthProjections(
					panel.data=list(Panel_Data=eval(parse(text=lagged.projs.data)), Coefficient_Matrices=sgp_object@SGP[["Coefficient_Matrices"]],
						Knots_Boundaries=sgp_object@SGP[["Knots_Boundaries"]]),
					sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1), 
						my.extra.label="LAGGED.BASELINE"),
					use.my.coefficient.matrices=list(my.year="BASELINE", my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
					use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
					performance.level.cutscores=state,
					panel.data.vnames=eval(parse(text=lagged.projs.vnames)),
					grade.progression=head(sgp.iter[["sgp.grade.sequences"]][[1]], -1),
					...), mc.cores=workers, mc.preschedule = FALSE)

				for (s in 1:length(tmp))	sgp_object@SGP <- .mergeSGP(sgp_object@SGP, tmp[[s]])
				rm(tmp)
			} # End MULTICORE
		suppressMessages(gc()) # clean up
		} ## END if sgp.projections.lagged.baseline
	key(sgp_object@Data) <- c("VALID_CASE", "CONTENT_AREA", "YEAR", "ID") # re-key data for combineSGP, etc.
	}  ## END if (!is.null(parallel.config))


#############################################################
###	SEQUENTIAL OPTION (NON-Parallel Option)
#############################################################

	if (is.null(parallel.config)) {

		tmp_sgp_object <- list(Coefficient_Matrices=sgp_object@SGP[["Coefficient_Matrices"]], Knots_Boundaries=sgp_object@SGP[["Knots_Boundaries"]])

		for (sgp.iter in sgp.config) {
			tmp_sgp_object[["Panel_Data"]] <- 
			as.data.frame(reshape(sgp_object@Data[J("VALID_CASE", sgp.iter[["sgp.content.areas"]], sgp.iter[["sgp.panel.years"]]), mult="all"],
				idvar="ID",
				timevar="YEAR",
				drop=names(sgp_object@Data)[!names(sgp_object@Data) %in% c("ID", "GRADE", "SCALE_SCORE", "YEAR")],
				direction="wide"))
			suppressMessages(gc()) 
			
			if (sgp.percentiles) {
				sgp.vnames <- c("ID", paste("GRADE", sgp.iter[["sgp.panel.years"]], sep="."), 
					paste("SCALE_SCORE", sgp.iter[["sgp.panel.years"]], sep="."))
				if (simulate.sgps) {
					if (!exists("calculate.confidence.intervals")) {
						calculate.confidence.intervals <- state
					}
					for (k in sgp.iter[["sgp.grade.sequences"]]) {
						tmp_sgp_object <- studentGrowthPercentiles(
							panel.data=tmp_sgp_object,
							sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)),
							use.my.knots.boundaries=state,
							growth.levels=state,
							panel.data.vnames=sgp.vnames,
							grade.progression=k,
							calculate.confidence.intervals=calculate.confidence.intervals,  
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
			} ## END if sgp.percentiles

			if (sgp.percentiles.baseline) {
				sgp.vnames <- c("ID", paste("GRADE", sgp.iter[["sgp.panel.years"]], sep="."), 
					paste("SCALE_SCORE", sgp.iter[["sgp.panel.years"]], sep="."))
					
				tmp_sgp_object <- .mergeSGP(tmp_sgp_object, stateData[[state]][["Baseline_splineMatrix"]])
				
				mtx.names <- names(tmp_sgp_object[["Coefficient_Matrices"]][[paste(toupper(tail(sgp.iter[["sgp.content.areas"]], 1)), ".BASELINE", sep="")]])
	
				for (k in sgp.iter[["sgp.grade.sequences"]]) {
					max.order.mtx <- mtx.names[[max(grep(paste("qrmatrix_", tail(k, 1), sep=""), mtx.names))]]
					max.order <- as.numeric(strsplit(max.order.mtx, "_")[[1]][3])
					
					if (length(sgp.iter[["sgp.panel.years"]])-1 < max.order) max.order <- length(sgp.iter[["sgp.panel.years"]])-1			
	
					if (sum(diff(tail(k, 1+max.order))) >= length(sgp.iter[["sgp.panel.years"]])) {	# deals with 'holes'
						base.gp <- k[tail(k, 1)-k <= max.order]
						max.order <- length(base.gp) - 1
					}	else base.gp <- tail(k, 1+max.order)
	
					tmp_sgp_object <- studentGrowthPercentiles(
						panel.data = tmp_sgp_object,
						sgp.labels = list(my.year = tail(sgp.iter[["sgp.panel.years"]], 1), 
							my.subject = tail(sgp.iter[["sgp.content.areas"]], 1), my.extra.label="BASELINE"),
						use.my.knots.boundaries = state,
						use.my.coefficient.matrices = list(my.year = "BASELINE", my.subject = tail(sgp.iter[["sgp.content.areas"]], 1)),
						growth.levels = state,
						panel.data.vnames = sgp.vnames,
						grade.progression = base.gp,
						num.prior = max.order,
						goodness.of.fit=TRUE,
						...)
				} ## END k loop
			} ## END if sgp.percentiles.baseline

	
			## sgp.projections
	
			if (sgp.projections) {
				sgp.vnames <- c("ID", paste("GRADE", sgp.iter[["sgp.panel.years"]], sep="."), 
					paste("SCALE_SCORE", sgp.iter[["sgp.panel.years"]], sep="."))
	
				for (k in lapply(sgp.iter[["sgp.grade.sequences"]], function(x) head(x, -1))) {
					tmp_sgp_object <- studentGrowthProjections(
						panel.data=tmp_sgp_object,
						sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)),
						use.my.coefficient.matrices=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
						use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
						performance.level.cutscores=state,
						max.forward.progression.years=3,
						percentile.trajectory.values=c(1, stateData[[state]][["Growth"]][["Cutscores"]][["Cuts"]], 99),
						panel.data.vnames=sgp.vnames,
						grade.progression=k,
						...)
				}
			} ## END if sgp.projections


			## sgp.projections.baseline
	
			if (sgp.projections.baseline) {
				sgp.vnames <- c("ID", paste("GRADE", sgp.iter[["sgp.panel.years"]], sep="."), 
					paste("SCALE_SCORE", sgp.iter[["sgp.panel.years"]], sep="."))
	
				for (k in lapply(sgp.iter[["sgp.grade.sequences"]], function(x) head(x, -1))) {
					tmp_sgp_object <- studentGrowthProjections(
						panel.data=tmp_sgp_object,
						sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1),
							my.extra.label="BASELINE"),
						use.my.coefficient.matrices=list(my.year="BASELINE", my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
						use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
						performance.level.cutscores=state,
						max.forward.progression.years=3,
						percentile.trajectory.values=c(1, stateData[[state]][["Growth"]][["Cutscores"]][["Cuts"]], 99),
						panel.data.vnames=sgp.vnames,
						grade.progression=k,
						...)
				}
			} ## END if sgp.projections.baseline
	
	
			## sgp.projections.lagged
	
			if (sgp.projections.lagged) {
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
			} ## END sgp.projections.lagged

			## sgp.projections.lagged.baseline
	
			if (sgp.projections.lagged.baseline) {
				sgp.vnames <- c("ID", paste("GRADE", head(sgp.iter[["sgp.panel.years"]], -1), sep="."), 
					paste("SCALE_SCORE", head(sgp.iter[["sgp.panel.years"]], -1), sep="."))
	
				for (k in lapply(sgp.iter[["sgp.grade.sequences"]], function(x) head(x, -1))) {
					tmp_sgp_object <- studentGrowthProjections(
						panel.data=tmp_sgp_object,
						sgp.labels=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1), 
							my.extra.label="LAGGED.BASELINE"),
						use.my.coefficient.matrices=list(my.year="BASELINE", my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
						use.my.knots.boundaries=list(my.year=tail(sgp.iter[["sgp.panel.years"]], 1), my.subject=tail(sgp.iter[["sgp.content.areas"]], 1)), 
						performance.level.cutscores=state,
						panel.data.vnames=sgp.vnames,
						grade.progression=k,
						...)
				}
			} ## END sgp.projections.lagged.baseline
		
			## sgp.percentiles.baseline

		} ## END for (sgp.iter in sgp.config)) -- SEQUENTIAL
		sgp_object@SGP <- .mergeSGP(sgp_object@SGP, tmp_sgp_object)
	} ## END sequential analyzeSGP

	if (goodness.of.fit.print) gof.print(sgp_object)

	message(paste("Finished analyzeSGP", date(), "in", timetaken(started.at), "\n"))
	return(sgp_object)
} ## END analyzeSGP Function
