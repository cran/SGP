.setUp <- function() {
	load(file.path(getwd(),'pkg/SGP/data/sgpData.rda'), envir=topenv())
}

.tearDown <- function() {
	rm(sgpData, envir=topenv())
}

test.test <- function() {
	checkEquals(sum(sgpData, na.rm=T), 439533703192)
}

