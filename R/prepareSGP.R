`prepareSGP` <- 
function(long_data) {
        started.at <- proc.time()
        message(paste("Started prepareSGP", date()))
	sgp_object <- list(Student=data.table(long_data, key="VALID_CASE, CONTENT_AREA, YEAR, ID"))
        message(paste("Finished prepareSGP", date(), "in", timetaken(started.at), "\n"))
	return(sgp_object)
} ## END prepareSGP Function
