`prepareSGP` <- 
function(long_data) {
	return(list(Student=data.table(long_data, key="VALID_CASE, CONTENT_AREA, YEAR, ID")))
}
