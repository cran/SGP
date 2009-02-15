`studentGrowthProjections` <-
function(                            student.data,                                      ## REQUIRED
                                     num.panels,                                        ## REQUIRED
                                     max.num.scores,                                    ## REQUIRED
                                     proj.function.labels,                              ## REQUIRED
                                     num.prior.scores,                                  ## OPTIONAL
                                     subset.grade,                                      ## OPTIONAL
                                     chunk.size=10000,                                  ## OPTIONAL
                                     convert.0and100=TRUE,                              ## OPTIONAL
                                     percentile.trajectories=c(1,35,65,99),             ## OPTIONAL
                                     isotonize=TRUE,                                    ## OPTIONAL
                                     projcuts.digits=2){                                ## OPTIONAL


############################################################
###
### Code for INTERNAL student growth projection function
###
############################################################

studentGrowthProjections_Internal <- function(grade.data, num.prior.scores){

##
## Code for function that returns subject x grade specific knots and boundaries 
##


get_myknots <- function(subject, grade){
                        return(get(paste("knots_", subject, "_g", grade, sep="")))
}

get_myboundaries <- function(subject, grade){
                             return(get(paste("boundaries_", subject, "_g", grade, sep="")))
}


##
## Code for function that returns order x year x grade x subject specific qr matrix
##

get_myqrmatrix <- function(order, year, grade, subject){

   if (order==1) return(get(paste("qr_1storder_", year, "_g", grade, "_", subject, "_coefmatrix", sep="")))
   if (order==2) return(get(paste("qr_2ndorder_", year, "_g", grade, "_", subject, "_coefmatrix", sep="")))
   if (order==3) return(get(paste("qr_3rdorder_", year, "_g", grade, "_", subject, "_coefmatrix", sep="")))
   if (order==4) return(get(paste("qr_4thorder_", year, "_g", grade, "_", subject, "_coefmatrix", sep="")))
   if (order==5) return(get(paste("qr_5thorder_", year, "_g", grade, "_", subject, "_coefmatrix", sep="")))
   if (order==6) return(get(paste("qr_6thorder_", year, "_g", grade, "_", subject, "_coefmatrix", sep="")))
   if (order==7) return(get(paste("qr_7thorder_", year, "_g", grade, "_", subject, "_coefmatrix", sep="")))
   if (order==8) return(get(paste("qr_8thorder_", year, "_g", grade, "_", subject, "_coefmatrix", sep="")))
}


##
## Code for function that linearly interpolates missing values and prevents quantile crossing
##

smooth.and.isotonize.row <- function(x){
                       x[which(is.na(x))] <- approx(x, xout=which(is.na(x)))$y
                       if (isotonize) return(sort(x))
                       else return(x)
}


#######################################################################
## Create 1, 2, 3, and 4 year projections for each student
#######################################################################

##
## Create relevant variables
##

num.students <- length(grade.data$SS8)
my_intercept <- rep(1, num.students)
my_intercept_long <- rep(1, num.students*100)


##
## 1 year projections
##

if (identical(num.prior.scores[1], 1)){

          predictions_1year <- cbind(my_intercept, 
                               bs(grade.data$SS8,  
                                       knots=as.vector(get_myknots(proj.function.labels$my.subject, proj.function.labels$my.grade)), 
                                       Boundary.knots=as.vector(get_myboundaries(proj.function.labels$my.subject, proj.function.labels$my.grade)))) %*%
                               get_myqrmatrix(1, proj.function.labels$my.year, proj.function.labels$my.grade+1, proj.function.labels$my.subject)
          predictions_1year <- t(apply(predictions_1year, 1, function(x) smooth.and.isotonize.row(x)))
          predictions_1year[predictions_1year < get_myboundaries(proj.function.labels$my.subject, proj.function.labels$my.grade+1)[1]] <- get_myboundaries(proj.function.labels$my.subject, proj.function.labels$my.grade+1)[1]
          predictions_1year[predictions_1year > get_myboundaries(proj.function.labels$my.subject, proj.function.labels$my.grade+1)[2]] <- get_myboundaries(proj.function.labels$my.subject, proj.function.labels$my.grade+1)[2]
} 

if (identical(num.prior.scores[1], 2)){

          predictions_1year <-  cbind(my_intercept, 
                                  bs(grade.data$SS8,  
                                          knots=get_myknots(proj.function.labels$my.subject, proj.function.labels$my.grade), 
                                          Boundary.knots=get_myboundaries(proj.function.labels$my.subject, proj.function.labels$my.grade)), 
                                  bs(grade.data$SS7,  
                                          knots=get_myknots(proj.function.labels$my.subject, proj.function.labels$my.grade-1),
                                          Boundary.knots=get_myboundaries(proj.function.labels$my.subject, proj.function.labels$my.grade-1))) %*%
                                get_myqrmatrix(2, proj.function.labels$my.year, proj.function.labels$my.grade+1, proj.function.labels$my.subject)
          predictions_1year <- t(apply(predictions_1year, 1, function(x) smooth.and.isotonize.row(x)))
          predictions_1year[predictions_1year < get_myboundaries(proj.function.labels$my.subject, proj.function.labels$my.grade+1)[1]] <- get_myboundaries(proj.function.labels$my.subject, proj.function.labels$my.grade+1)[1]
          predictions_1year[predictions_1year > get_myboundaries(proj.function.labels$my.subject, proj.function.labels$my.grade+1)[2]] <- get_myboundaries(proj.function.labels$my.subject, proj.function.labels$my.grade+1)[2]
}

if (identical(num.prior.scores[1], 3)){

          predictions_1year <-  cbind(my_intercept, 
                                 bs(grade.data$SS8,  
                                        knots=get_myknots(proj.function.labels$my.subject, proj.function.labels$my.grade), 
                                        Boundary.knots=get_myboundaries(proj.function.labels$my.subject, proj.function.labels$my.grade)), 
                                bs(grade.data$SS7,  
                                        knots=get_myknots(proj.function.labels$my.subject, proj.function.labels$my.grade-1),
                                        Boundary.knots=get_myboundaries(proj.function.labels$my.subject, proj.function.labels$my.grade-1)),
                                bs(grade.data$SS6,  
                                        knots=get_myknots(proj.function.labels$my.subject, proj.function.labels$my.grade-2),
                                        Boundary.knots=get_myboundaries(proj.function.labels$my.subject, proj.function.labels$my.grade-2))) %*%
                                get_myqrmatrix(3, proj.function.labels$my.year, proj.function.labels$my.grade+1, proj.function.labels$my.subject)
          predictions_1year <- t(apply(predictions_1year, 1, function(x) smooth.and.isotonize.row(x)))
          predictions_1year[predictions_1year < get_myboundaries(proj.function.labels$my.subject, proj.function.labels$my.grade+1)[1]] <- get_myboundaries(proj.function.labels$my.subject, proj.function.labels$my.grade+1)[1]
          predictions_1year[predictions_1year > get_myboundaries(proj.function.labels$my.subject, proj.function.labels$my.grade+1)[2]] <- get_myboundaries(proj.function.labels$my.subject, proj.function.labels$my.grade+1)[2]
}

if (identical(num.prior.scores[1], 4)){

         predictions_1year <-  cbind(my_intercept,
                                bs(grade.data$SS8,  
                                        knots=get_myknots(proj.function.labels$my.subject, proj.function.labels$my.grade),
                                        Boundary.knots=get_myboundaries(proj.function.labels$my.subject, proj.function.labels$my.grade)),
                                bs(grade.data$SS7,  
                                        knots=get_myknots(proj.function.labels$my.subject, proj.function.labels$my.grade-1),
                                        Boundary.knots=get_myboundaries(proj.function.labels$my.subject, proj.function.labels$my.grade-1)),
                                bs(grade.data$SS6,  
                                        knots=get_myknots(proj.function.labels$my.subject, proj.function.labels$my.grade-2),
                                        Boundary.knots=get_myboundaries(proj.function.labels$my.subject, proj.function.labels$my.grade-2)),
                                bs(grade.data$SS5,  
                                        knots=get_myknots(proj.function.labels$my.subject, proj.function.labels$my.grade-3),
                                        Boundary.knots=get_myboundaries(proj.function.labels$my.subject, proj.function.labels$my.grade-3))) %*%
                          get_myqrmatrix(4, proj.function.labels$my.year, proj.function.labels$my.grade+1, proj.function.labels$my.subject)
          predictions_1year <- t(apply(predictions_1year, 1, function(x) smooth.and.isotonize.row(x)))
          predictions_1year[predictions_1year < get_myboundaries(proj.function.labels$my.subject, proj.function.labels$my.grade+1)[1]] <- get_myboundaries(proj.function.labels$my.subject, proj.function.labels$my.grade+1)[1]
          predictions_1year[predictions_1year > get_myboundaries(proj.function.labels$my.subject, proj.function.labels$my.grade+1)[2]] <- get_myboundaries(proj.function.labels$my.subject, proj.function.labels$my.grade+1)[2]
}


##
## 2 year projections
##

if (identical(num.prior.scores[2], 1)){
         temp.matrix <- cbind(my_intercept_long, 
                               bs(as.vector(t(predictions_1year)),  
                                     knots=get_myknots(proj.function.labels$my.subject, proj.function.labels$my.grade+1), 
                                     Boundary.knots=get_myboundaries(proj.function.labels$my.subject, proj.function.labels$my.grade+1))) 

          for (i in 1:100){
                if (i == 1) predictions_2year <- temp.matrix[i+0:(num.students-1)*100,] %*% 
                                             get_myqrmatrix(1, proj.function.labels$my.year, proj.function.labels$my.grade+2, proj.function.labels$my.subject)[,i]
                if (i > 1) predictions_2year <- c(predictions_2year, temp.matrix[i+0:(num.students-1)*100,] %*% 
                                             get_myqrmatrix(1, proj.function.labels$my.year, proj.function.labels$my.grade+2, proj.function.labels$my.subject)[,i])
            }

          predictions_2year <- matrix(predictions_2year, ncol=100)
          predictions_2year <- t(apply(predictions_2year, 1, function(x) smooth.and.isotonize.row(x)))
          dimnames(predictions_2year) <- dimnames(predictions_1year)
          predictions_2year[predictions_2year < get_myboundaries(proj.function.labels$my.subject, proj.function.labels$my.grade+2)[1]] <- get_myboundaries(proj.function.labels$my.subject, proj.function.labels$my.grade+2)[1]
          predictions_2year[predictions_2year > get_myboundaries(proj.function.labels$my.subject, proj.function.labels$my.grade+2)[2]] <- get_myboundaries(proj.function.labels$my.subject, proj.function.labels$my.grade+2)[2]
} 

if (identical(num.prior.scores[2], 2)){
         temp.matrix <- cbind(my_intercept_long, 
                               bs(as.vector(t(predictions_1year)),  
                                     knots=get_myknots(proj.function.labels$my.subject, proj.function.labels$my.grade+1), 
                                     Boundary.knots=get_myboundaries(proj.function.labels$my.subject, proj.function.labels$my.grade+1)), 
                               bs(rep(grade.data$SS8, each=100),  
                                     knots=get_myknots(proj.function.labels$my.subject, proj.function.labels$my.grade),
                                     Boundary.knots=get_myboundaries(proj.function.labels$my.subject, proj.function.labels$my.grade)))

          for (i in 1:100){
                if (i == 1) predictions_2year <- temp.matrix[i+0:(num.students-1)*100,] %*% 
                                             get_myqrmatrix(2, proj.function.labels$my.year, proj.function.labels$my.grade+2, proj.function.labels$my.subject)[,i]
                if (i > 1) predictions_2year <- c(predictions_2year, temp.matrix[i+0:(num.students-1)*100,] %*% 
                                             get_myqrmatrix(2, proj.function.labels$my.year, proj.function.labels$my.grade+2, proj.function.labels$my.subject)[,i])
            }

          predictions_2year <- matrix(predictions_2year, ncol=100)
          predictions_2year <- t(apply(predictions_2year, 1, function(x) smooth.and.isotonize.row(x)))
          dimnames(predictions_2year) <- dimnames(predictions_1year)
          predictions_2year[predictions_2year < get_myboundaries(proj.function.labels$my.subject, proj.function.labels$my.grade+2)[1]] <- get_myboundaries(proj.function.labels$my.subject, proj.function.labels$my.grade+2)[1]
          predictions_2year[predictions_2year > get_myboundaries(proj.function.labels$my.subject, proj.function.labels$my.grade+2)[2]] <- get_myboundaries(proj.function.labels$my.subject, proj.function.labels$my.grade+2)[2]
} 

if (identical(num.prior.scores[2], 3)){
         temp.matrix <- cbind(my_intercept_long, 
                               bs(as.vector(t(predictions_1year)),  
                                     knots=get_myknots(proj.function.labels$my.subject, proj.function.labels$my.grade+1), 
                                     Boundary.knots=get_myboundaries(proj.function.labels$my.subject, proj.function.labels$my.grade+1)), 
                               bs(rep(grade.data$SS8, each=100),  
                                     knots=get_myknots(proj.function.labels$my.subject, proj.function.labels$my.grade),
                                     Boundary.knots=get_myboundaries(proj.function.labels$my.subject, proj.function.labels$my.grade)),
                               bs(rep(grade.data$SS7, each=100),  
                                     knots=get_myknots(proj.function.labels$my.subject, proj.function.labels$my.grade-1),
                                     Boundary.knots=get_myboundaries(proj.function.labels$my.subject, proj.function.labels$my.grade-1)))

          for (i in 1:100){
                if (i == 1) predictions_2year <- temp.matrix[i+0:(num.students-1)*100,] %*% 
                                             get_myqrmatrix(3, proj.function.labels$my.year, proj.function.labels$my.grade+2, proj.function.labels$my.subject)[,i]
                if (i > 1) predictions_2year <- c(predictions_2year, temp.matrix[i+0:(num.students-1)*100,] %*% 
                                             get_myqrmatrix(3, proj.function.labels$my.year, proj.function.labels$my.grade+2, proj.function.labels$my.subject)[,i])
            }

          predictions_2year <- matrix(predictions_2year, ncol=100)
          predictions_2year <- t(apply(predictions_2year, 1, function(x) smooth.and.isotonize.row(x)))
          dimnames(predictions_2year) <- dimnames(predictions_1year)
          predictions_2year[predictions_2year < get_myboundaries(proj.function.labels$my.subject, proj.function.labels$my.grade+2)[1]] <- get_myboundaries(proj.function.labels$my.subject, proj.function.labels$my.grade+2)[1]
          predictions_2year[predictions_2year > get_myboundaries(proj.function.labels$my.subject, proj.function.labels$my.grade+2)[2]] <- get_myboundaries(proj.function.labels$my.subject, proj.function.labels$my.grade+2)[2]
} 

if (identical(num.prior.scores[2], 4)){

          temp.matrix <- cbind(my_intercept_long,
                               bs(as.vector(t(predictions_1year)),  
                                     knots=get_myknots(proj.function.labels$my.subject, proj.function.labels$my.grade+1),
                                     Boundary.knots=get_myboundaries(proj.function.labels$my.subject, proj.function.labels$my.grade+1)),
                               bs(rep(grade.data$SS8, each=100),  
                                     knots=get_myknots(proj.function.labels$my.subject, proj.function.labels$my.grade),
                                     Boundary.knots=get_myboundaries(proj.function.labels$my.subject, proj.function.labels$my.grade)),
                               bs(rep(grade.data$SS7, each=100),  
                                     knots=get_myknots(proj.function.labels$my.subject, proj.function.labels$my.grade-1),
                                     Boundary.knots=get_myboundaries(proj.function.labels$my.subject, proj.function.labels$my.grade-1)),
                               bs(rep(grade.data$SS6, each=100),  
                                     knots=get_myknots(proj.function.labels$my.subject, proj.function.labels$my.grade-2),
                                     Boundary.knots=get_myboundaries(proj.function.labels$my.subject, proj.function.labels$my.grade-2)))

          for (i in 1:100){
                if (i == 1) predictions_2year <- temp.matrix[i+0:(num.students-1)*100,] %*%
                                             get_myqrmatrix(4, proj.function.labels$my.year, proj.function.labels$my.grade+2, proj.function.labels$my.subject)[,i]
                if (i > 1) predictions_2year <- c(predictions_2year, temp.matrix[i+0:(num.students-1)*100,] %*%
                                             get_myqrmatrix(4, proj.function.labels$my.year, proj.function.labels$my.grade+2, proj.function.labels$my.subject)[,i])
            }

          predictions_2year <- matrix(predictions_2year, ncol=100)
          predictions_2year <- t(apply(predictions_2year, 1, function(x) smooth.and.isotonize.row(x)))
          dimnames(predictions_2year) <- dimnames(predictions_1year)
          predictions_2year[predictions_2year < get_myboundaries(proj.function.labels$my.subject, proj.function.labels$my.grade+2)[1]] <- get_myboundaries(proj.function.labels$my.subject, proj.function.labels$my.grade+2)[1]
          predictions_2year[predictions_2year > get_myboundaries(proj.function.labels$my.subject, proj.function.labels$my.grade+2)[2]] <- get_myboundaries(proj.function.labels$my.subject, proj.function.labels$my.grade+2)[2]
}


##
## 3 year projections
##

if (identical(num.prior.scores[3], 1)){

         temp.matrix <- cbind(my_intercept_long, 
                               bs(as.vector(t(predictions_2year)),  
                                     knots=get_myknots(proj.function.labels$my.subject, proj.function.labels$my.grade+2), 
                                     Boundary.knots=get_myboundaries(proj.function.labels$my.subject, proj.function.labels$my.grade+2))) 

          for (i in 1:100){
                if (i == 1) predictions_3year <- temp.matrix[i+0:(num.students-1)*100,] %*% 
                                             get_myqrmatrix(1, proj.function.labels$my.year, proj.function.labels$my.grade+3, proj.function.labels$my.subject)[,i]
                if (i > 1) predictions_3year <- c(predictions_3year, temp.matrix[i+0:(num.students-1)*100,] %*% 
                                             get_myqrmatrix(1, proj.function.labels$my.year, proj.function.labels$my.grade+3, proj.function.labels$my.subject)[,i])
            }

          predictions_3year <- matrix(predictions_3year, ncol=100)
          predictions_3year <- t(apply(predictions_3year, 1, function(x) smooth.and.isotonize.row(x)))
          dimnames(predictions_3year) <- dimnames(predictions_1year)
          predictions_3year[predictions_3year < get_myboundaries(proj.function.labels$my.subject, proj.function.labels$my.grade+3)[1]] <- get_myboundaries(proj.function.labels$my.subject, proj.function.labels$my.grade+3)[1]
          predictions_3year[predictions_3year > get_myboundaries(proj.function.labels$my.subject, proj.function.labels$my.grade+3)[2]] <- get_myboundaries(proj.function.labels$my.subject, proj.function.labels$my.grade+3)[2]
}

if (identical(num.prior.scores[3], 2)){

          temp.matrix <- cbind(my_intercept_long, 
                               bs(as.vector(t(predictions_2year)),  
                                     knots=get_myknots(proj.function.labels$my.subject, proj.function.labels$my.grade+2), 
                                     Boundary.knots=get_myboundaries(proj.function.labels$my.subject, proj.function.labels$my.grade+2)), 
                               bs(as.vector(t(predictions_1year)),  
                                     knots=get_myknots(proj.function.labels$my.subject, proj.function.labels$my.grade+1), 
                                     Boundary.knots=get_myboundaries(proj.function.labels$my.subject, proj.function.labels$my.grade+1))) 

          for (i in 1:100){
                if (i == 1) predictions_3year <- temp.matrix[i+0:(num.students-1)*100,] %*% 
                                             get_myqrmatrix(2, proj.function.labels$my.year, proj.function.labels$my.grade+3, proj.function.labels$my.subject)[,i]
                if (i > 1) predictions_3year <- c(predictions_3year, temp.matrix[i+0:(num.students-1)*100,] %*% 
                                             get_myqrmatrix(2, proj.function.labels$my.year, proj.function.labels$my.grade+3, proj.function.labels$my.subject)[,i])
            }

         predictions_3year <- matrix(predictions_3year, ncol=100)
         predictions_3year <- t(apply(predictions_3year, 1, function(x) smooth.and.isotonize.row(x)))
         dimnames(predictions_3year) <- dimnames(predictions_1year)
         predictions_3year[predictions_3year < get_myboundaries(proj.function.labels$my.subject, proj.function.labels$my.grade+3)[1]] <- get_myboundaries(proj.function.labels$my.subject, proj.function.labels$my.grade+3)[1]
         predictions_3year[predictions_3year > get_myboundaries(proj.function.labels$my.subject, proj.function.labels$my.grade+3)[2]] <- get_myboundaries(proj.function.labels$my.subject, proj.function.labels$my.grade+3)[2]
}

if (identical(num.prior.scores[3], 3)){

        temp.matrix <- cbind(my_intercept_long, 
                               bs(as.vector(t(predictions_2year)),  
                                     knots=get_myknots(proj.function.labels$my.subject, proj.function.labels$my.grade+2), 
                                     Boundary.knots=get_myboundaries(proj.function.labels$my.subject, proj.function.labels$my.grade+2)), 
                               bs(as.vector(t(predictions_1year)),  
                                     knots=get_myknots(proj.function.labels$my.subject, proj.function.labels$my.grade+1),
                                     Boundary.knots=get_myboundaries(proj.function.labels$my.subject, proj.function.labels$my.grade+1)), 
                               bs(rep(grade.data$SS8, each=100),  
                                     knots=get_myknots(proj.function.labels$my.subject, proj.function.labels$my.grade),
                                     Boundary.knots=get_myboundaries(proj.function.labels$my.subject, proj.function.labels$my.grade)))

          for (i in 1:100){
                if (i == 1) predictions_3year <- temp.matrix[i+0:(num.students-1)*100,] %*% 
                                             get_myqrmatrix(3, proj.function.labels$my.year, proj.function.labels$my.grade+3, proj.function.labels$my.subject)[,i]
                if (i > 1) predictions_3year <- c(predictions_3year, temp.matrix[i+0:(num.students-1)*100,] %*% 
                                             get_myqrmatrix(3, proj.function.labels$my.year, proj.function.labels$my.grade+3, proj.function.labels$my.subject)[,i])
            }

         predictions_3year <- matrix(predictions_3year, ncol=100)
         predictions_3year <- t(apply(predictions_3year, 1, function(x) smooth.and.isotonize.row(x)))
         dimnames(predictions_3year) <- dimnames(predictions_1year)
         predictions_3year[predictions_3year < get_myboundaries(proj.function.labels$my.subject, proj.function.labels$my.grade+3)[1]] <- get_myboundaries(proj.function.labels$my.subject, proj.function.labels$my.grade+3)[1]
         predictions_3year[predictions_3year > get_myboundaries(proj.function.labels$my.subject, proj.function.labels$my.grade+3)[2]] <- get_myboundaries(proj.function.labels$my.subject, proj.function.labels$my.grade+3)[2]
}

if (identical(num.prior.scores[3], 4)){

        temp.matrix <- cbind(my_intercept_long,
                               bs(as.vector(t(predictions_2year)),  
                                     knots=get_myknots(proj.function.labels$my.subject, proj.function.labels$my.grade+2),
                                     Boundary.knots=get_myboundaries(proj.function.labels$my.subject, proj.function.labels$my.grade+2)),
                               bs(as.vector(t(predictions_1year)),  
                                     knots=get_myknots(proj.function.labels$my.subject, proj.function.labels$my.grade+1),
                                     Boundary.knots=get_myboundaries(proj.function.labels$my.subject, proj.function.labels$my.grade+1)),
                               bs(rep(grade.data$SS8, each=100),  
                                     knots=get_myknots(proj.function.labels$my.subject, proj.function.labels$my.grade),
                                     Boundary.knots=get_myboundaries(proj.function.labels$my.subject, proj.function.labels$my.grade)),
                               bs(rep(grade.data$SS7, each=100),  
                                     knots=get_myknots(proj.function.labels$my.subject, proj.function.labels$my.grade-1),
                                     Boundary.knots=get_myboundaries(proj.function.labels$my.subject, proj.function.labels$my.grade-1)))

          for (i in 1:100){
                if (i == 1) predictions_3year <- temp.matrix[i+0:(num.students-1)*100,] %*%
                                             get_myqrmatrix(4, proj.function.labels$my.year, proj.function.labels$my.grade+3, proj.function.labels$my.subject)[,i]
                if (i > 1) predictions_3year <- c(predictions_3year, temp.matrix[i+0:(num.students-1)*100,] %*%
                                             get_myqrmatrix(4, proj.function.labels$my.year, proj.function.labels$my.grade+3, proj.function.labels$my.subject)[,i])
            }

         predictions_3year <- matrix(predictions_3year, ncol=100)
         predictions_3year <- t(apply(predictions_3year, 1, function(x) smooth.and.isotonize.row(x)))
         dimnames(predictions_3year) <- dimnames(predictions_1year)
         predictions_3year[predictions_3year < get_myboundaries(proj.function.labels$my.subject, proj.function.labels$my.grade+3)[1]] <- get_myboundaries(proj.function.labels$my.subject, proj.function.labels$my.grade+3)[1]
         predictions_3year[predictions_3year > get_myboundaries(proj.function.labels$my.subject, proj.function.labels$my.grade+3)[2]] <- get_myboundaries(proj.function.labels$my.subject, proj.function.labels$my.grade+3)[2]
}


##
## 4 year projections
##

if (identical(num.prior.scores[4], 1)){

         temp.matrix <- cbind(my_intercept_long, 
                               bs(as.vector(t(predictions_3year)),  
                                     knots=get_myknots(proj.function.labels$my.subject, proj.function.labels$my.grade+3), 
                                     Boundary.knots=get_myboundaries(proj.function.labels$my.subject, proj.function.labels$my.grade+3))) 

          for (i in 1:100){
                if (i == 1) predictions_4year <- temp.matrix[i+0:(num.students-1)*100,] %*% 
                                             get_myqrmatrix(1, proj.function.labels$my.year, proj.function.labels$my.grade+4, proj.function.labels$my.subject)[,i]
                if (i > 1) predictions_4year <- c(predictions_4year, temp.matrix[i+0:(num.students-1)*100,] %*% 
                                             get_myqrmatrix(1, proj.function.labels$my.year, proj.function.labels$my.grade+4, proj.function.labels$my.subject)[,i])
            }

          predictions_4year <- matrix(predictions_4year, ncol=100)
          predictions_4year <- t(apply(predictions_4year, 1, function(x) smooth.and.isotonize.row(x)))
          dimnames(predictions_4year) <- dimnames(predictions_1year)
          predictions_4year[predictions_4year < get_myboundaries(proj.function.labels$my.subject, proj.function.labels$my.grade+4)[1]] <- get_myboundaries(proj.function.labels$my.subject, proj.function.labels$my.grade+4)[1]
          predictions_4year[predictions_4year > get_myboundaries(proj.function.labels$my.subject, proj.function.labels$my.grade+4)[2]] <- get_myboundaries(proj.function.labels$my.subject, proj.function.labels$my.grade+4)[2]
}

if (identical(num.prior.scores[4], 2)){

          temp.matrix <- cbind(my_intercept_long, 
                               bs(as.vector(t(predictions_3year)),  
                                     knots=get_myknots(proj.function.labels$my.subject, proj.function.labels$my.grade+3), 
                                     Boundary.knots=get_myboundaries(proj.function.labels$my.subject, proj.function.labels$my.grade+3)), 
                               bs(as.vector(t(predictions_2year)),  
                                     knots=get_myknots(proj.function.labels$my.subject, proj.function.labels$my.grade+2), 
                                     Boundary.knots=get_myboundaries(proj.function.labels$my.subject, proj.function.labels$my.grade+2))) 

          for (i in 1:100){
                if (i == 1) predictions_4year <- temp.matrix[i+0:(num.students-1)*100,] %*% 
                                             get_myqrmatrix(2, proj.function.labels$my.year, proj.function.labels$my.grade+4, proj.function.labels$my.subject)[,i]
                if (i > 1) predictions_4year <- c(predictions_4year, temp.matrix[i+0:(num.students-1)*100,] %*% 
                                             get_myqrmatrix(2, proj.function.labels$my.year, proj.function.labels$my.grade+4, proj.function.labels$my.subject)[,i])
            }

         predictions_4year <- matrix(predictions_4year, ncol=100)
         predictions_4year <- t(apply(predictions_4year, 1, function(x) smooth.and.isotonize.row(x)))
         dimnames(predictions_4year) <- dimnames(predictions_1year)
         predictions_4year[predictions_4year < get_myboundaries(proj.function.labels$my.subject, proj.function.labels$my.grade+4)[1]] <- get_myboundaries(proj.function.labels$my.subject, proj.function.labels$my.grade+4)[1]
         predictions_4year[predictions_4year > get_myboundaries(proj.function.labels$my.subject, proj.function.labels$my.grade+4)[2]] <- get_myboundaries(proj.function.labels$my.subject, proj.function.labels$my.grade+4)[2]
}

if (identical(num.prior.scores[4], 3)){

        temp.matrix <- cbind(my_intercept_long, 
                               bs(as.vector(t(predictions_3year)),  
                                     knots=get_myknots(proj.function.labels$my.subject, proj.function.labels$my.grade+3), 
                                     Boundary.knots=get_myboundaries(proj.function.labels$my.subject, proj.function.labels$my.grade+3)), 
                               bs(as.vector(t(predictions_2year)),  
                                     knots=get_myknots(proj.function.labels$my.subject, proj.function.labels$my.grade+2),
                                     Boundary.knots=get_myboundaries(proj.function.labels$my.subject, proj.function.labels$my.grade+2)),
                               bs(as.vector(t(predictions_1year)),  
                                     knots=get_myknots(proj.function.labels$my.subject, proj.function.labels$my.grade+1),
                                     Boundary.knots=get_myboundaries(proj.function.labels$my.subject, proj.function.labels$my.grade+1))) 

          for (i in 1:100){
                if (i == 1) predictions_4year <- temp.matrix[i+0:(num.students-1)*100,] %*% 
                                             get_myqrmatrix(3, proj.function.labels$my.year, proj.function.labels$my.grade+4, proj.function.labels$my.subject)[,i]
                if (i > 1) predictions_4year <- c(predictions_4year, temp.matrix[i+0:(num.students-1)*100,] %*% 
                                             get_myqrmatrix(3, proj.function.labels$my.year, proj.function.labels$my.grade+4, proj.function.labels$my.subject)[,i])
            }

         predictions_4year <- matrix(predictions_4year, ncol=100)
         predictions_4year <- t(apply(predictions_4year, 1, function(x) smooth.and.isotonize.row(x)))
         dimnames(predictions_4year) <- dimnames(predictions_1year)
         predictions_4year[predictions_4year < get_myboundaries(proj.function.labels$my.subject, proj.function.labels$my.grade+4)[1]] <- get_myboundaries(proj.function.labels$my.subject, proj.function.labels$my.grade+4)[1]
         predictions_4year[predictions_4year > get_myboundaries(proj.function.labels$my.subject, proj.function.labels$my.grade+4)[2]] <- get_myboundaries(proj.function.labels$my.subject, proj.function.labels$my.grade+4)[2]
}

if (identical(num.prior.scores[4], 4)){

        temp.matrix <- cbind(my_intercept_long,
                               bs(as.vector(t(predictions_3year)),  
                                     knots=get_myknots(proj.function.labels$my.subject, proj.function.labels$my.grade+3), 
                                     Boundary.knots=get_myboundaries(proj.function.labels$my.subject, proj.function.labels$my.grade+3)), 
                               bs(as.vector(t(predictions_2year)),  
                                     knots=get_myknots(proj.function.labels$my.subject, proj.function.labels$my.grade+2),
                                     Boundary.knots=get_myboundaries(proj.function.labels$my.subject, proj.function.labels$my.grade+2)),
                               bs(as.vector(t(predictions_1year)),  
                                     knots=get_myknots(proj.function.labels$my.subject, proj.function.labels$my.grade+1),
                                     Boundary.knots=get_myboundaries(proj.function.labels$my.subject, proj.function.labels$my.grade+1)),
                               bs(rep(grade.data$SS8, each=100),  
                                     knots=get_myknots(proj.function.labels$my.subject, proj.function.labels$my.grade),
                                     Boundary.knots=get_myboundaries(proj.function.labels$my.subject, proj.function.labels$my.grade)))
                                     
          for (i in 1:100){
                if (i == 1) predictions_4year <- temp.matrix[i+0:(num.students-1)*100,] %*%
                                             get_myqrmatrix(4, proj.function.labels$my.year, proj.function.labels$my.grade+4, proj.function.labels$my.subject)[,i]
                if (i > 1) predictions_4year <- c(predictions_4year, temp.matrix[i+0:(num.students-1)*100,] %*%
                                             get_myqrmatrix(4, proj.function.labels$my.year, proj.function.labels$my.grade+4, proj.function.labels$my.subject)[,i])
            }

         predictions_4year <- matrix(predictions_4year, ncol=100)
         predictions_4year <- t(apply(predictions_4year, 1, function(x) smooth.and.isotonize.row(x)))
         dimnames(predictions_4year) <- dimnames(predictions_1year)
         predictions_4year[predictions_4year < get_myboundaries(proj.function.labels$my.subject, proj.function.labels$my.grade+4)[1]] <- get_myboundaries(proj.function.labels$my.subject, proj.function.labels$my.grade+4)[1]
         predictions_4year[predictions_4year > get_myboundaries(proj.function.labels$my.subject, proj.function.labels$my.grade+4)[2]] <- get_myboundaries(proj.function.labels$my.subject, proj.function.labels$my.grade+4)[2]
}


######################################################################################################
## Code to get percentile to achieve given perrformance level in 1, 2, 3, and 4 years
######################################################################################################

##
## 1 year projections and percentile trajectories (if requested)
##

for (i in 1:length(get_mycutscores(proj.function.labels$my.subject, proj.function.labels$my.grade))) {

if (!is.na(num.prior.scores[1])) {

tf.matrix <- predictions_1year < get_mycutscores(proj.function.labels$my.subject, proj.function.labels$my.grade+1)[i]
tf.matrix <- cbind(tf.matrix, FALSE)
temp <- apply(tf.matrix, 1, function(x) which.min(x)); temp[temp==101] <- 100
if (convert.0and100) {temp[temp==0] <- 1; temp[temp==100] <- 99}
assign(paste("gp_proj_1year_level", i, sep=""), temp)
if (!is.null(percentile.trajectories)) { gp_proj_1year_cuts <- round(predictions_1year[,percentile.trajectories+1], digits=projcuts.digits) }
}

else {
assign(paste("gp_proj_1year_level", i, sep=""), rep(NA, length(grade.data$ID)))
if (!is.null(percentile.trajectories)) { gp_proj_1year_cuts <- matrix(NA, nrow=length(grade.data$ID), ncol=length(percentile.trajectories)) }
}
}

##
## 2 year projections and percentile trajectories (if requested)
##

for (i in 1:length(get_mycutscores(proj.function.labels$my.subject, proj.function.labels$my.grade))) {

if (!is.na(num.prior.scores[2])) {

tf.matrix <- predictions_2year < get_mycutscores(proj.function.labels$my.subject, proj.function.labels$my.grade+2)[i]
tf.matrix <- cbind(tf.matrix, FALSE)
temp <- apply(tf.matrix, 1, function(x) which.min(x)); temp[temp==101] <- 100
if (convert.0and100) {temp[temp==0] <- 1; temp[temp==100] <- 99}
assign(paste("gp_proj_2year_level", i, sep=""), temp)
if (!is.null(percentile.trajectories)) { gp_proj_2year_cuts <- round(predictions_2year[,percentile.trajectories+1], digits=projcuts.digits) }
}

else {
assign(paste("gp_proj_2year_level", i, sep=""), rep(NA, length(grade.data$ID)))
if (!is.null(percentile.trajectories)) { gp_proj_2year_cuts <- matrix(NA, nrow=length(grade.data$ID), ncol=length(percentile.trajectories)) }
}
}

##
## 3 year projections and percentile trajectories (if requested)
##

for (i in 1:length(get_mycutscores(proj.function.labels$my.subject, proj.function.labels$my.grade))) {

if (!is.na(num.prior.scores[3])) {

tf.matrix <- predictions_3year < get_mycutscores(proj.function.labels$my.subject, proj.function.labels$my.grade+3)[i]
tf.matrix <- cbind(tf.matrix, FALSE)
temp <- apply(tf.matrix, 1, function(x) which.min(x)); temp[temp==101] <- 100
if (convert.0and100) {temp[temp==0] <- 1; temp[temp==100] <- 99}
assign(paste("gp_proj_3year_level", i, sep=""), temp)
if (!is.null(percentile.trajectories)) { gp_proj_3year_cuts <- round(predictions_3year[,percentile.trajectories+1], digits=projcuts.digits) }
}

else {
assign(paste("gp_proj_3year_level", i, sep=""), rep(NA, length(grade.data$ID)))
if (!is.null(percentile.trajectories)) { gp_proj_3year_cuts <- matrix(NA, nrow=length(grade.data$ID), ncol=length(percentile.trajectories)) }

}
}

##
## 4 year projections and percentile trajectories (if requested)
##

for (i in 1:length(get_mycutscores(proj.function.labels$my.subject, proj.function.labels$my.grade))) {

if (!is.na(num.prior.scores[4])) {

tf.matrix <- predictions_4year < get_mycutscores(proj.function.labels$my.subject, proj.function.labels$my.grade+4)[i]
tf.matrix <- cbind(tf.matrix, FALSE)
temp <- apply(tf.matrix, 1, function(x) which.min(x)); temp[temp==101] <- 100
if (convert.0and100) {temp[temp==0] <- 1; temp[temp==100] <- 99}
assign(paste("gp_proj_4year_level", i, sep=""), temp)
if (!is.null(percentile.trajectories)) { gp_proj_4year_cuts <- round(predictions_4year[,percentile.trajectories+1], digits=projcuts.digits) }
}

else {
assign(paste("gp_proj_4year_level", i, sep=""), rep(NA, length(grade.data$ID)))
if (!is.null(percentile.trajectories)) { gp_proj_4year_cuts <- matrix(NA, nrow=length(grade.data$ID), ncol=length(percentile.trajectories)) }

}
}


##
## Create data frame of Growth percentiles required to meet achievement levels in 1, 2, 3, and 4 year time frames
##

for (i in 1:4){
for (j in 1:length(get_mycutscores(proj.function.labels$my.subject, proj.function.labels$my.grade))) {
if (i==1 & j==1) {gp_proj_dataframe <- cbind(grade.data$ID, get(paste("gp_proj_", i, "year_level", j, sep="")))}
else {gp_proj_dataframe <- cbind(gp_proj_dataframe, get(paste("gp_proj_", i, "year_level", j, sep="")))}
}
}


colnames(gp_proj_dataframe) <- c("id", paste("level", 1:length(get_mycutscores(proj.function.labels$my.subject, proj.function.labels$my.grade)),
                                             "_in_1year_", proj.function.labels$my.year, sep=""),
                                       paste("level", 1:length(get_mycutscores(proj.function.labels$my.subject, proj.function.labels$my.grade)),
                                             "_in_2year_", proj.function.labels$my.year, sep=""),
                                       paste("level", 1:length(get_mycutscores(proj.function.labels$my.subject, proj.function.labels$my.grade)),
                                             "_in_3year_", proj.function.labels$my.year, sep=""),
                                       paste("level", 1:length(get_mycutscores(proj.function.labels$my.subject, proj.function.labels$my.grade)),
                                             "_in_4year_", proj.function.labels$my.year, sep="")) 


gp_proj_dataframe <- data.frame(gp_proj_dataframe, stringsAsFactors=FALSE)
gp_proj_list <- list(gp_proj_dataframe=gp_proj_dataframe)


##
## Create data frame of Growth percentiles cuts in 1, 2, 3, and 4 year time frames (if requested)
##

if (!is.null(percentile.trajectories)){

gp_cuts_dataframe <- data.frame(grade.data$ID, gp_proj_1year_cuts, gp_proj_2year_cuts, gp_proj_3year_cuts, gp_proj_4year_cuts, stringsAsFactors=FALSE)

names(gp_cuts_dataframe) <- c("id", paste("cut_", percentile.trajectories , "_in_1year_", proj.function.labels$my.year, sep=""),
                                    paste("cut_", percentile.trajectories , "_in_2year_", proj.function.labels$my.year, sep=""),
                                    paste("cut_", percentile.trajectories , "_in_3year_", proj.function.labels$my.year, sep=""),
                                    paste("cut_", percentile.trajectories , "_in_4year_", proj.function.labels$my.year, sep="")) 

gp_proj_list$gp_cuts_dataframe <- gp_cuts_dataframe
}


##
## Return Growth Projection Frame
##

return(gp_proj_list)

}


###########################################################
## End studentGrowthProjections_Internal  Function 
###########################################################

##
## Function that selects the best growth projection percentiles
## (i.e., those based upon the maximum number of prior predictors)
##

return.best.projection <- function(x, block_size){
                                   num.blocks <- (length(x)-1)/block_size
                                   block.start <- 2 + block_size*0:(num.blocks-1)
                                   nonempty.blocks <- !is.na(x[block.start])
                                   best.block.start <- max(block.start[nonempty.blocks])
                                   best.block <- x[c(1, best.block.start + 0:(block_size-1))]                 
                                   return(best.block)
}


##
## Code for function that returns subject x grade specific cutscores
##

get_mycutscores <- function(subject, grade){
                            return(get(paste("cutscores_", subject, "_g", grade, sep="")))
}


##
## Commands for testing student.data and converting to a data frame if necessary
##

if (class(student.data) != "data.frame") student.data <- as.data.frame(student.data, stringsAsFactors=FALSE)
if (2*num.panels+1 != dim(student.data)[2]) print("WARNING: Number of columns for student.data does not appear to conform to data requirements!")


##
## Test for existence of both knots/boundaries and coefficient matrices
##

if (length(list.files("Knots_Boundaries")) == 0) stop("Knots and Boundaries must be supplied to calculate percentile growth trajectories.")
if (length(list.files("Coefficient_Matrices")) == 0) stop("Coefficient Matrices must be supplied by first calculating growth percentiles to calculate percentile growth trajectories.")


##
## Get knots, boundaries and coefficient matrices
##

lapply(list.files("Knots_Boundaries", full.names=T, pattern=proj.function.labels$my.subject), load, envir=.GlobalEnv)
lapply(list.files("Coefficient_Matrices", full.names=T, pattern=proj.function.labels$my.subject), load, envir=.GlobalEnv)
lapply(list.files("Cutscores", full.names=T, pattern=proj.function.labels$my.subject), load, envir=.GlobalEnv)


##
## Create num.prior.scores vector if not specified
##

if (missing(num.prior.scores)){
if (max.num.scores == 1) {num.prior.scores <- list(c(1,2,3,4))}
if (max.num.scores == 2) {num.prior.scores <- list(c(1,2,3,4), c(2,3,4,4))}
if (max.num.scores == 3) {num.prior.scores <- list(c(1,2,3,4), c(2,3,4,4), c(3,4,4,4))}
if (max.num.scores >= 4) {num.prior.scores <- list(c(1,2,3,4), c(2,3,4,4), c(3,4,4,4), c(4,4,4,4))}
if (max.num.scores > 4) {warning("Maximum number of scores used for prediction is 4. Results reflect results using at most 4 scores")}
}


##
## Rename variables so that they end with year 8
##

GD <- paste("GD",(9-num.panels):8,sep="")
SS <- paste("SS",(9-num.panels):8,sep="")
names(student.data) <- c("ID", GD, SS)


##
## Subset data by selected grade if requested
##

if (!missing(subset.grade)){
student.data <- subset(student.data, student.data$GD8 == subset.grade)
}


####
#### Loop over integers up to max number of scores to calculate projections
####

for (j in 1:max.num.scores){


##
## Construction of Grade Specific Data Files based upon loop index j
##

str1 <- " !is.na(SS8)"
if (j > 1) str1 <- paste("!is.na(",SS[(num.panels-j+1):num.panels],") & ", sep="")

str2 <- character()
if (j == 2) str2 <- " GD7==GD8-1"
if (j > 2) str2 <- c("GD7==GD8-1", paste(" & ", GD[(num.panels-2):(num.panels-j+1)],"==",GD[num.panels],"-",2:(j-1),sep=""))

str3 <- "SS8"
if (j > 1) str3 <- c(paste(SS[(num.panels-j+1):(num.panels-1)], ", ", sep=""), str3)

grade_data <- eval(parse(text=c("subset(student.data,", c(str1, str2), ", select=c(ID, ", str3 ,"))")))


##
## Loop over chunks to calculate projections using studentGrowthProjections_Internal
##

num_rows <- dim(grade_data)[1]
num_chunks <- floor(num_rows/chunk.size)

for (i in 0:num_chunks){
   lower_index <- i*chunk.size + 1
   upper_index <- min((i+1)*chunk.size, num_rows)
   if (i == 0) {

         tmp <- studentGrowthProjections_Internal(grade_data[lower_index:upper_index,], 
                                                  num.prior.scores[[j]])
         assign(paste("growth_projections_", j, sep=""), tmp$gp_proj_dataframe)

         if(!is.null(percentile.trajectories)){ assign(paste("growth_projections_cuts_", j, sep=""), tmp$gp_cuts_dataframe) }

   } ## End if

   else {
         tmp <- studentGrowthProjections_Internal(grade_data[lower_index:upper_index,], 
                                                  num.prior.scores[[j]])
         assign(paste("growth_projections_", j, sep=""), rbind(get(paste("growth_projections_", j, sep="")), tmp$gp_proj_dataframe))

         if(!is.null(percentile.trajectories)){ assign(paste("growth_projections_cuts_", j, sep=""), rbind(get(paste("growth_projections_cuts_", j, sep="")), tmp$gp_cuts_dataframe)) } 
   } ## End else

} ###### End i loop
} ###### End j loop


##
## Merge together different ordered projections
##

for (i in 1:max.num.scores) {
   if (i == 1) growth_projections <- get(paste("growth_projections_", i, sep=""))
   else growth_projections <- merge(growth_projections, get(paste("growth_projections_", i, sep="")), by="id", all=TRUE)
}


##
## Merge together different ordered cuts
##

if (!is.null(percentile.trajectories)){
for (i in 1:max.num.scores) {
   if (i == 1) growth_projections_cuts <- get(paste("growth_projections_cuts_", i, sep=""))
   else growth_projections_cuts <- merge(growth_projections_cuts, get(paste("growth_projections_cuts_", i, sep="")), by="id", all=TRUE)
}
}


##
## Get best growth projections
##

growth_projections <- t(apply(growth_projections, 1, return.best.projection, block_size=4*length(get_mycutscores(proj.function.labels$my.subject, proj.function.labels$my.grade+1))))
colnames(growth_projections) <- toupper(colnames(get(paste("growth_projections_1"))))
growth_projections <- as.data.frame(growth_projections, stringsAsFactors=FALSE)


##
## Get best growth projection cuts (if asked for) and merge with growth projections
##

if (!is.null(percentile.trajectories)) {
growth_projections_cuts <- t(apply(growth_projections_cuts, 1, return.best.projection, block_size=4*length(percentile.trajectories)))
colnames(growth_projections_cuts) <- toupper(colnames(get(paste("growth_projections_cuts_1"))))
growth_projections_cuts <- as.data.frame(growth_projections_cuts, stringsAsFactors=FALSE)
growth_projections <- merge(growth_projections, growth_projections_cuts, by="ID", all=TRUE)
}


###
### Return projections
###

return(growth_projections)

}

