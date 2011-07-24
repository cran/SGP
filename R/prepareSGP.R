`prepareSGP` <- 
	function(data, 
		var.names=NULL, 
		fix.duplicates="keep.all") {

	## Print start time

	started.at <- proc.time()
	message(paste("Started prepareSGP", date()))

	if (class(data)=="SGP") {

		message(paste("Finished prepareSGP", date(), "in", timetaken(started.at), "\n"))

		return(data)
	} else {
	
	## Required variables

	req.nms <- c("ID", "CONTENT_AREA", "YEAR", "GRADE", "SCALE_SCORE", "VALID_CASE")
	
	## Get the names of the original variables
	## These values will be reused in the output

	nms.original <- names(data)

	
	##  Create an object with default variable names

	default.var.names <- data.frame(nms.orig=req.nms, nms.sgp=req.nms)

	
	##  Check variable names

	if (!missing(var.names)) {
		var.names <- unlist(var.names)
		var.names <- data.frame(nms.orig=toupper(as.vector(var.names)), nms.sgp=toupper(as.vector(names(var.names))))
		
		## Include default variable names (as needed)

		tmp <- default.var.names[(default.var.names$nms.sgp %in% var.names$nms.sgp)==FALSE,]
		var.names <- rbind(var.names, tmp)
	} else {
		var.names <- default.var.names
	}

	
	## Compile the original variable names and the corresponding (capitalized) variable names used in SGP

	var.names.original <- data.frame(column=1:length(nms.original), nms.original=nms.original, nms.orig=toupper(nms.original))
	var.names$flag <- 1
	tmp.var.names <- merge(var.names.original, var.names, all.x=TRUE)
	tmp.var.names$nms.sgp[is.na(tmp.var.names$flag)] <- NA
	tmp.var.names$flag <- NULL
	tmp.var.names <- subset(tmp.var.names, select=c("column", "nms.original", "nms.sgp"))
	tmp.var.names <- tmp.var.names[order(tmp.var.names$column),]
	tmp.var.names$nms.original  <- as.character(tmp.var.names$nms.original)
	tmp.var.names$nms.sgp <- as.character(tmp.var.names$nms.sgp)

	
	## Check to see if any of the required variables are missing

	if (!all(req.nms %in% tmp.var.names$nms.sgp)) {
		stop(paste("The {data} object is missing the following column name: ",req.nms[(req.nms %in% tmp.var.names$nms.sgp)==FALSE],
		". You may need to identify the variable using the {var.names} argument.", sep=""))
	}

	
	##  Update variable names in the dataset

	names(data)[!is.na(tmp.var.names$nms.sgp)] <- tmp.var.names$nms.sgp[!is.na(tmp.var.names$nms.sgp)]

	
	##  Create keyed data.table and check for duplicate cases

	data <- data.table(data)
	key(data) <- c("VALID_CASE","CONTENT_AREA","YEAR","ID")

	################################################################	
	## INCLUDE CODE HERE TO HANDLE DUPLICATE CASES
	################################################################	
	
	
	##  Create the SGP object

	sgp_object <- new("SGP", Data=data, Variable_Name_Lookup=tmp.var.names)

	
	##  Print finish time
	message(paste("Finished prepareSGP", date(), "in", timetaken(started.at), "\n"))

	return(sgp_object)
	} ## END else
} ## END prepareSGP function
