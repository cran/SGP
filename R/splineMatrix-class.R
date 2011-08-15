setClassUnion("date", c("POSIXct", "POSIXt"))
setClass("splineMatrix", contains='matrix', representation(Knots="list", Boundaries="list", Date="date"))
.Valid.splineMatrix <- function(object) {
       out <- NULL
       if (is.null(out)) out <- TRUE
       return(out)
}
setValidity("splineMatrix", .Valid.splineMatrix)
