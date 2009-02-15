`studentGrowthPercentiles` <-
function(student.data,                                ## REQUIRED
         num.panels,                                  ## REQUIRED
         num.prior=num.panels-1,                      ## OPTIONAL
         subset.grade,                                ## OPTIONAL
         percentile.cuts=c(1, 35, 65, 99),            ## OPTIONAL
         use.my.knots.and.boundaries=FALSE,           ## OPTIONAL
         print.other.gp=FALSE,                        ## OPTIONAL
         rq.method="br",                              ## OPTIONAL
         convert.0and100=TRUE,                        ## OPTIONAL
         percuts.digits=2,                            ## OPTIONAL
         save.matrices=TRUE,                          ## OPTIONAL
         isotonize=TRUE,                              ## OPTIONAL
         convert.using.loss.hoss=TRUE,                ## OPTIONAL
         sgp.function.labels){                        ## OPTIONAL, BUT ALMOST ALWAYS USED


###
### Without function
###

"%w/o%" <- function(x,y) x[!x %in% y] #-- x without y 

##
## Code for function that linearly interpolates missing values
##

smooth.and.isotonize.row <- function(x){
                                  x[which(is.na(x))] <- approx(x, xout=which(is.na(x)))$y
                                  if (isotonize) return(sort(x))
                                  else return(x)
}


##
## Function that picks the best growth percentile
##


return.best.sgp <- function(x){ 
                            if (sum(is.na(x)) == 0) return(x[length(x)]) 
                            if (sum(is.na(x)) != 0) return(x[max(which(!is.na(x)))])
}


###
### Function that selects the best growth projection percentiles
### (i.e., those based upon the maximum number of prior predictors)
###

return.best.sgp.percuts <- function(x, numpercentilecuts){
                                    num.blocks <- (length(x)-1)/numpercentilecuts
                                    block.start <- 2 + numpercentilecuts*0:(num.blocks-1)
                                    nonempty.blocks <- !is.na(x[block.start])
                                    best.block.start <- max(block.start[nonempty.blocks])
                                    best.block <- x[best.block.start + 0:(numpercentilecuts-1)]                 
                                    return(as.numeric(best.block))
}


###
### Functions that assigns proper knot and boundary values for GROWTH PERCENTILE calculations based upon grade and number of priors
###

get_gp_knots <- function(grade, subject, priors){
                         for (i in 8:(8-priors)) {
                           assign(paste("knots", i, sep=""), get(paste("knots_", subject, "_g", grade-(8-i), sep="")), inherits=TRUE)
}
}

get_gp_boundaries <- function(grade, subject, priors){
                              for (i in 8:(8-priors)) {
                                assign(paste("boundaries", i, sep=""), get(paste("boundaries_", subject, "_g", grade-(8-i), sep="")), inherits=TRUE)
}
}

create_gp_knots_and_boundaries <- function(scores, grade, subject) {
                                         assign(paste("knots_", subject, "_g", grade, sep=""), round(as.vector(quantile(scores, probs=c(.2, .4, .6, .8), na.rm=T)), digits=3), inherits=TRUE)
                                         assign(paste("boundaries_", subject, "_g", grade, sep=""), round(extendrange(scores, f=0.01), digits=3), inherits=TRUE)
                                         save(list=paste("knots_", subject, "_g", grade, sep=""), file=paste("Knots_Boundaries/knots_", subject, "_g", grade, ".Rdata", sep=""))
                                         save(list=paste("boundaries_", subject, "_g", grade, sep=""), file=paste("Knots_Boundaries/boundaries_", subject, "_g", grade, ".Rdata", sep=""))
}


###
### Commands for testing student.data and converting to a data.frame if necessary
###

if (class(student.data) != "data.frame") {student.data <- as.data.frame(student.data)}
if (2*num.panels+1 != dim(student.data)[2]) {print("WARNING: Number of columns for student.data does not appear to conform to data requirements!")}

###
### Commands for creating Results_Data subdirectory
###

if (is.na(file.info("Results_Data")$isdir)){
    dir.create("Results_Data")
}

###
### Commands for creating Coefficient_Matrix subdirectory and testing for correct arguments being passed to the function
###

if (is.na(file.info("Coefficient_Matrices")$isdir) & save.matrices){
    dir.create("Coefficient_Matrices")
}

if (save.matrices & missing(sgp.function.labels)) {
     stop("Please specify an appropriate list of matrix labels as described in the function's help page")
}

###
### Commands for creating Knots_Boundaries subdirectory, testing for correct arguments being passed to the function,
### and creating knots and boundaries if asked for.
###

if (is.na(file.info("Knots_Boundaries")$isdir) & !use.my.knots.and.boundaries){
    dir.create("Knots_Boundaries")
}

if (!use.my.knots.and.boundaries & missing(sgp.function.labels)) {
     stop("Please specify an appropriate list of knot and boundary labels as described in the function's help page")
}

if (use.my.knots.and.boundaries & length(list.files("Knots_Boundaries")) == 0) {
    stop("Knots_Boundaries directory is empty. Specify your knots and boundaries and place them in the Knots_Boundaries directory")
}


if (use.my.knots.and.boundaries) {
lapply(list.files("Knots_Boundaries", full.names=T, pattern=sgp.function.labels$my.subject), load, envir=.GlobalEnv)
}


if (!use.my.knots.and.boundaries) {
      all_scores <- data.frame(cbind(stack(student.data[,2:(2+num.panels-1)])[,1], stack(student.data[,(2+num.panels):(2+2*num.panels-1)])[,1]))
      names(all_scores) <- c("GRADE", "SCORE")
   for (i in sort(unique(stack(student.data[,2:(2+num.panels-1)])[,1]) %w/o% NA)) 
      create_gp_knots_and_boundaries(subset(all_scores, all_scores$GRADE==i, select="SCORE"), i, sgp.function.labels$my.subject)
   
}


###
### Rename variables so that they end with year 8
###

GD <- paste("GD",(9-num.panels):8,sep="")
SS <- paste("SS",(9-num.panels):8,sep="")
names(student.data) <- c("ID", GD, SS)


###
### Subset data by selected grade if requested
###

if (!missing(subset.grade)){
student.data <- subset(student.data, student.data$GD8 == subset.grade)
}


###
### Construction of Grade Specific Data Files
###


str1 <- " & !is.na(SS8)"
str2 <- character()
str3 <- "SS8"

for (i in 1:num.prior) {
	str1 <- paste(" & !is.na(",SS[num.panels-i],")", str1, sep="")
	str2 <- paste(str2," & ",GD[num.panels-i],"==",GD[num.panels],"-",i,sep="")
	str3 <- paste(SS[num.panels-i], ", ", str3, sep="")
	assign(paste("data.grade",i+1,"y",sep=""), eval(parse(text=paste("subset(student.data,", substring(str1,4), str2, ", select=c(ID, ", str3 ,"))",sep=""))))
}

	
###
### Quantile Regression Function Estimation
### Coefficient matrices are saved for later percentile growth trajectory calculation
###


prefix <- c("1st","2nd","3rd","4th","5th","6th","7th")
mod <- character()

get_gp_knots(sgp.function.labels$my.grade, sgp.function.labels$my.subject, num.prior)
get_gp_boundaries(sgp.function.labels$my.grade, sgp.function.labels$my.subject, num.prior)

for (i in 1:num.prior) {
	mod <- paste(mod, " + bs(", SS[num.panels-i], ", knots=knots", 8-i, ", Boundary.knots=boundaries", 8-i, ")",sep="")
	assign(paste("qr.", prefix[i],"order",sep=""), eval(parse(text=paste("rq(SS8 ~ ", substring(mod,4), 
		", tau=(1:100-0.5)/100, data=data.grade", i+1, "y, method=rq.method)", sep=""))))

	if (save.matrices == TRUE){
		assign(paste("qr_", prefix[i], "order_", sgp.function.labels$my.year, "_g", sgp.function.labels$my.grade, "_", 
			sgp.function.labels$my.subject, "_coefmatrix", sep=""), eval(parse(text=paste("qr.", prefix[i], "order$coefficients",sep=""))))

		save(list=paste("qr_", prefix[i], "order_", sgp.function.labels$my.year, "_g", sgp.function.labels$my.grade, "_", sgp.function.labels$my.subject, 
			"_coefmatrix", sep=""), file=paste("Coefficient_Matrices/qr_", prefix[i], "order_", sgp.function.labels$my.year, "_g", sgp.function.labels$my.grade, "_", 
			sgp.function.labels$my.subject, "_coefmatrix.Rdata", sep=""))
	}	
}


###
### Create percentile score predictions for each student
###


for (i in 1:num.prior) {
	tmp <- eval(parse(text=paste("predict(qr.", prefix[i], "order)", sep="")))
	tmp <- round(t(apply(tmp, 1, function(x) smooth.and.isotonize.row(x))), digits=5)
	assign(paste("predict.", prefix[i], "order", sep=""), tmp)
}



###
### Code to get percentile for each student's most recent scale score (i.e., the dependent variable)
###


for (i in 1:num.prior) {
	tmp <- eval(parse(text=paste("predict.", prefix[i], "order < data.grade", i+1, "y$SS8", sep="")))
	tmp <- cbind(tmp, FALSE)
	tmp <- apply(tmp, 1, function(x) which.min(x == TRUE)-1)
	if (convert.0and100 == TRUE) {
		tmp[tmp==0] <- 1
		tmp[tmp==100] <- 99
	}
	assign(paste("gp_", prefix[i], "order",sep=""), tmp)
}



###
### Code to get percentile cutpoints (if asked for) for each student based 
### upon prior student data AND attach percentile cut data to each student's record (if asked for)
###


if (!is.null(percentile.cuts)){
	
	for (i in 1:num.prior) {
		tmp <- eval(parse(text=paste("predict.", prefix[i], "order[ , percentile.cuts+1]", sep="")))
                if (convert.using.loss.hoss==TRUE) {
                tmp[tmp < get(paste("boundaries", 8, sep=""))[1]] <- get(paste("boundaries", 8, sep=""))[1]
                tmp[tmp > get(paste("boundaries", 8, sep=""))[2]] <- get(paste("boundaries", 8, sep=""))[2]
                }
		colnames(tmp) <- paste("p", prefix[i], percentile.cuts, sep="")
		assign(paste("percuts_", prefix[i], "order", sep=""), tmp)
		assign(paste("percuts.frame", i+1, "y", sep=""), eval(parse(text=paste("data.frame(ID=data.grade", i+1, "y$ID, percuts_", prefix[i], "order)", sep=""))))
	}
}



###
### Attach growth percentile to each student's record
###


for (i in 1:num.prior) {
	assign(paste("growth.frame", i+1, "y", sep=""), eval(parse(text=paste("data.frame(ID=data.grade", i+1, "y$ID, gp_", prefix[i], "order)", sep=""))))
}



###
### Combine data frames containing different order growth percentiles
###


for (i in 1:num.prior) {
   if (i == 1) growth.frame <- get(paste("growth.frame", i+1, "y", sep=""))
   else growth.frame <- merge(growth.frame, get(paste("growth.frame", i+1, "y", sep="")), all=T)
}

if (num.prior < 7) {
     str1 <- "gp_7thorder=NA"
     if (num.prior < 6) str1 <- c(paste("gp_", prefix[(num.prior+1):6], "order=NA,", sep=""), str1)
     growth.frame <- eval(parse(text=c("data.frame(growth.frame,", str1, ")")))
}

   

###
### Combine data frames containing different order percentile cuts (if asked for)
###

if (!is.null(percentile.cuts)){

for (i in 1:num.prior) {
    if (i == 1) percuts.frame <- get(paste("percuts.frame", i+1, "y", sep=""))
    else percuts.frame <- merge(percuts.frame, get(paste("percuts.frame", i+1, "y", sep="")), all=T)
   }
}

###
### Create best growth percentile from those in the file
###

gp_best <- apply(growth.frame, 1, return.best.sgp)

if (print.other.gp == TRUE) growth.frame <- data.frame(growth.frame, SGP=gp_best)
if (print.other.gp == FALSE) growth.frame <- data.frame(ID=growth.frame$ID, SGP=gp_best)


###
### Create the best percentile cuts from those in the file and merge
### with the growth frame (if asked for)
###

if (!is.null(percentile.cuts)){
percuts_best <- t(apply(percuts.frame, 1, return.best.sgp.percuts, numpercentilecuts=length(percentile.cuts)))
percuts_best <- round(percuts_best, digits=percuts.digits)
colnames(percuts_best) <- paste("CUT", as.character(percentile.cuts), sep="")
growth.frame <- data.frame(growth.frame, percuts_best)
}


###
### Return Growth Frame
###

return(growth.frame)

###
### End studentGrowthPercentile function
###

}

