\name{studentGrowthProjections}
\alias{studentGrowthProjections}
\title{Student Growth Projections}
\description{
  Function to calculate percentile growth projections/trajectories using up to 8 panels of large scale assessment data.
  Function produces growth percentiles necessary for each student to reach a set of predefined
  scale score cut points in 1, 2, and 3 years. 
}

\usage{
studentGrowthProjections(student.data, 
                         num.panels, 
                         max.num.scores, 
                         proj.function.labels, 
                         num.prior.scores, 
                         subset.grade, 
                         chunk.size = 10000, 
                         convert.0and100 = "TRUE")
}

\arguments{
  \item{student.data}{Data frame containing longitudinal student data in wide format. 
  Data must be formatted so that student ID is the first variable/column, student grade variables for each year, from earliest to most recent, are the next variables/columns, 
  and student scale score variables for each year, from earliest to latest, are the remaining variables/columns. See \code{\link{sgpData}} for an exemplar data set. NOTE: The 
  column position of the variables IS IMPORTANT, NOT the names of the variables. The function renames variables internally.
 }
  \item{num.panels}{Number of panels in the data frame}
  \item{max.num.scores}{The MAXIMUM number of scores one wishes to use in the FIRST YEAR projection analysis. Function will perform analyses using \code{1, 2, ... max.num.scores}
  for the first year projections/trajectories. Note \code{max.num.scores < num.panels} and, in most cases, \code{max.num.scores = num.panels-1}. 
  Current function implementation limits \code{max.num.scores} to at most 4.
 }
  \item{proj.function.labels}{List of labels used to retrieve the appropriate coefficient matrices, knots, and boundaries for the projections/trajectories.
   The list corresponds to the list used to save the coefficient matrices and knots and boundaries in the growth percentile analyses. The list is of the form  
   \code{list(my.year= , my.subject= , my.grade= )}.
 }
  \item{num.prior.scores}{Number of prior scores used to calculate each of the 1, 2, and 3 year projections.
   A list of length \code{max.number.scores} consisting of vectors of length 3. 

   The first component of the list (a vector of length 3, a 3-tuple) indicates how many prior scores are used in the calculation of the 1, 2, and 3 
   year projections, respectively, in the case when \emph{1 prior score} is utilized. The first component of the 3-tuple is used to calculate the 1
   year projection. The second component of the 3-tuple indicates how many scores are used to calculate the 2 year projections
   and the third component of the 3-tuple indicates how many scores are used to calculate the 3 year projections. 

   If calculation of a given prediction is not appropriate, then the component of the vector should be \code{NA}. For example, for students currently
   in the penultimate grade that is assessed, only a 1 year projection is calculated. Thus, the latter two components of the vector are \code{NA, c(??, NA, NA)}
   
   The second component of the list (another 3-tuple) indicates the number of prior scores to be used in the calculation of the 1, 2, and 3
   year projections in the case when \emph{2 prior scores} are utilized. Similarly, the third and fourth component of the list indicate how many
   prior scores are to be used in the calculation of the 1, 2, and 3 year projections in the case when \emph{3 and 4 priors scores}, respectively, are utilized. 

   The \code{num.prior.scores} used for projection calculation are based upon the quantile regression based coefficient matrices
   calculated when growth percentiles are estimated. The \code{num.prior.scores} is USUALLY the maximum number of priors used for 
   growth percentile calculations (USUALLY \code{num.panels - 1}). 

   If \code{max.num.scores=1}, then \code{num.prior.scores} is USUALLY \code{list(c(1, 2, 3))}. That is, because there is only one score, only 1 score is
   used to calculate the 1st year projection. However, for the 2nd year, because there are now two scores available (the
   actual score used for the 1 year projection AND the 1 year projection itself), 2 scores are used to get the 2 year projection. However,
   in order to use two scores to get the 2 year projection, a prior 2nd order analysis (i.e., using 3 panels of data) must exist that relates
   the two predictor scores with the dependent variable. Hence, the values are dependent BOTH upon the number of predictors available AND also upon
   the prior analyses performed. Similarly, for the 3rd year, 3 scores are used to get the 3 year projection. 

   When not supplied, the function tries to set the appropriate values. These are:

\preformatted{
     max.num.scores=1:  num.prior.scores=list(c(1,2,3))
     max.num.scores=2:  num.prior.scores=list(c(1,2,3), 
                                             c(2,3,4))
     max.num.scores=3:  num.prior.scores=list(c(1,2,3), 
                                             c(2,3,4), 
                                             c(3,4,4))
     max.num.scores=4:  num.prior.scores=list(c(1,2,3), 
                                             c(2,3,4), 
                                             c(3,4,4), 
                                             c(4,4,4))
 }
}
  \item{subset.grade}{Student grade level for sub-setting. If the data fed into the function contains multiple
  grades, setting \code{subset.grade=5} selects out those students in grade five in the most recent year of the data. Defaults to NA, meaning all data 
  supplied are analyzed without any sub-setting occurring.
 }
  \item{chunk.size}{An integer (default of 10,000) indicating what the number of rows in the block of data (so that the program doesn't overwhelm memory) 
   fed into the internal growth projection function.}
  \item{convert.0and100}{Boolean argument (defaults to TRUE) indicating whether conversion of growth percentiles of 0 and 100 to growth percentiles
    of 1 and 99, respectively, occurs. The default produces growth percentiles ranging from 1 to 99.}
 }

\details{
  }

\value{Function returns a data frame consisting of student IDs and the associated percentile growth projections/trajectories.
 Depending upon the number of cut-scores specified, the data frame contains 3 columns (1, 2, and 3 year projections/trajectories)
 for each cut-point supplied (enumerated \file{LEVEL1}, \file{LEVEL2}, \file{LEVEL3}, ...).
}

\references{
Betebenner, D. W. (2008). Toward a normative understanding of student growth. In K. E. Ryan & 
L. A. Shepard (Eds.), \emph{The Future of Test Based Accountability} (pp. 155-170). New York: Routledge.
}

\author{Damian W. Betebenner \email{dbetebenner@nciea.org} }

\note{Use of this function assumes prior calculation of student growth percentiles with \code{save.matrices=TRUE} 
  (saving the coefficient matrices associated with student growth percentile calculations within the \file{Coefficient\_Matrices} 
  directory). Additionally percentile growth projections/trajectories require future achievement levels (e.g., performance level cut-scores) 
  against which percentile growth projections/trajectories are calculated. These values must be available in the \file{Cutscores} 
  directory using specific naming conventions. See the example for syntax on how to create and save cut-score vectors. 
}

\seealso{\code{\link{studentGrowthPercentiles}}, \code{\link{sgpData}}}

\examples{
###########################################################################
####
#### Code for master file which computes percentile growth 
#### projections/trajectories associated with 2004 to 2006
#### data using provided sgpData
####
########################################################################### 


###
### Establish and Save Reading Cut Scores
###

if (is.na(file.info("Cutscores")$isdir)) {
    dir.create("Cutscores")
}


cutscores_read_g3 <- c(465.5, 525.5, 655.5)
cutscores_read_g4 <- c(516.5, 571.5, 670.5)
cutscores_read_g5 <- c(537.5, 587.5, 690.5)
cutscores_read_g6 <- c(542.5, 599.5, 695.5)
cutscores_read_g7 <- c(566.5, 619.5, 715.5)

save(cutscores_read_g3, file="Cutscores/cutscores_read_g3.Rdata")
save(cutscores_read_g4, file="Cutscores/cutscores_read_g4.Rdata")
save(cutscores_read_g5, file="Cutscores/cutscores_read_g5.Rdata")
save(cutscores_read_g6, file="Cutscores/cutscores_read_g6.Rdata")
save(cutscores_read_g7, file="Cutscores/cutscores_read_g7.Rdata")


###
### Load Data
###

data(sgpData)

###
### Create subset of data for 2004 to 2006
###

attach(sgpData)
sgpData <- data.frame(ID, GRADE_04, GRADE_05, GRADE_06, SS_04, SS_05, SS_06)
detach(sgpData)


###
### Submit to Growth Projection Function
###

##
## 333333333333333333333333333333333333333333333333333333333
##

##
## 3rd graders receiving projections
##


sgp_g2s_read_2007_g3 <- studentGrowthProjections(
                           student.data = sgpData,
                           num.panels = 3,
                           max.num.scores = 1,
                           num.prior.scores=list(c(1,2,3)),
                           proj.function.labels = list(my.year=2007, my.subject="read", my.grade=3),
                           subset.grade = 3)

## save(sgp_g2s_read_2007_g3, file="Results_Data/sgp_g2s_read_2007_g3.Rdata")


##
## 444444444444444444444444444444444444444444444444444444444
##


sgp_g2s_read_2007_g4 <- studentGrowthProjections(
                           student.data = sgpData,
                           num.panels = 3,
                           max.num.scores = 2,
                           num.prior.scores=list(c(1,2,3), c(2,3,3)),
                           proj.function.labels = list(my.year=2007, my.subject="read", my.grade=4),
                           subset.grade = 4)

## save(sgp_g2s_read_2007_g4, file="Results_Data/sgp_g2s_read_2007_g4.Rdata")



##
## 555555555555555555555555555555555555555555555555555555555555
## 


sgp_g2s_read_2007_g5 <- studentGrowthProjections(
                           student.data = sgpData,
                           num.panels = 3,
                           max.num.scores = 3,
                           num.prior.scores=list(c(1,2,NA), c(2,3,NA), c(3,3,NA)),
                           proj.function.labels = list(my.year=2007, my.subject="read", my.grade=5),
                           subset.grade = 5)

## save(sgp_g2s_read_2007_g5, file="Results_Data/sgp_g2s_read_2007_g5.Rdata")


##
## 66666666666666666666666666666666666666666666666666666666666
##

sgp_g2s_read_2007_g6 <- studentGrowthProjections(
                           student.data = sgpData,
                           num.panels = 3,
                           max.num.scores = 3,
                           num.prior.scores=list(c(1,NA,NA), c(2,NA,NA), c(3,NA,NA)),
                           proj.function.labels = list(my.year=2007, my.subject="read", my.grade=6),
                           subset.grade = 6)

## save(sgp_g2s_read_2007_g6, file="Results_Data/sgp_g2s_read_2007_g6.Rdata")






###
### Combine all the different grade growth projection files into a single file 
### and write the results to a comma separated data set
###

sgp_g2s_read_2007_gall <- rbind(sgp_g2s_read_2007_g3,
                                      sgp_g2s_read_2007_g4,
                                      sgp_g2s_read_2007_g5,
                                      sgp_g2s_read_2007_g6)

save(sgp_g2s_read_2007_gall, file="Results_Data/sgp_g2s_read_2007_gall.Rdata")
write.table(sgp_g2s_read_2007_gall, file="Results_Data/sgp_g2s_read_2007_gall.dat", 
            sep=" , ", row.names=FALSE, quote=FALSE)



###
### End of Master Program for Percentile Growth Projections/Trajectories
###
}


\keyword{ misc }
\keyword{ models }
