\name{studentGrowthPercentiles}
\alias{studentGrowthPercentiles}
\title{Student Growth Percentiles}
\description{
Function to calculate student growth percentiles using up to 8 panels of large scale assessment data. 
Outputs growth percentiles for each student and supplies various options as function arguments.
Output from this function is used to calculate percentile growth projections/trajectories.   
}
\usage{
studentGrowthPercentiles(student.data, 
                         num.panels, 
                         num.prior = num.panels - 1, 
                         subset.grade, 
                         percentile.cuts = c(1, 35, 65, 99), 
                         use.my.knots.and.boundaries = FALSE, 
                         print.other.gp = FALSE, 
                         rq.method = "br", 
                         convert.0and100 = TRUE, 
                         percuts.digits=2,
                         save.matrices = TRUE,
                         isotonize = TRUE,
                         convert.using.loss.hoss=TRUE, 
                         sgp.function.labels)
}

\arguments{
  \item{student.data}{Data frame containing longitudinal student data in wide format. 
  Data must be formatted so that student ID is the first variable/column, student grade variables for each year, from earliest to most recent, are the next variables/columns, 
  and student scale score variables for each year, from earliest to latest, are the remaining variables/columns. See \code{\link{sgpData}} for an exemplar data set. NOTE: The 
  column position of the variables IS IMPORTANT, NOT the names of the variables. The function renames variables internally.
}
  \item{num.panels}{Number of panels in the data frame.
}
  \item{num.prior}{Number of prior scores one wishes to use in the analysis. Defaults to \code{num.panels-1}. 
  If \code{num.prior=1}, then only 1st order growth percentiles are computed, if \code{num.prior=2}, then 1st and 2nd order are computed, 
  if \code{num.prior=3}, 1st, 2nd, and 3rd ... NOTE: specifying \code{num.prior} is necessary in some situations (in early grades for example)
  where the number of prior data points is small compared to the number of panels of data. 
}
  \item{subset.grade}{Student grade level for sub-setting. If the data fed into the function contains multiple
  grades, setting \code{subset.grade=5} selects out those students in grade five in the most recent year of the data. If no sub-setting is desired,
  argument do not include the \code{subset.grade} argument.
}
  \item{percentile.cuts}{Additional percentile cuts (supplied as a vector) associated with each student's conditional distribution. 
   Default is to supply the 1st, 35th, 65th, and 99th growth percentile cuts (scale scores associated with 
   those growth percentiles) for each student. If percentile cuts are not wanted, set \code{percentile.cuts=NULL}.
 }
  \item{use.my.knots.and.boundaries}{Boolean argument (default is FALSE) specifying whether the growth percentile function calculates B-splines using pre-specified
   knots and boundaries from the user or whether the program creates its own and saves those results to the directory \file{Knots\_Boundaries}. 
   \emph{KNOTS} are vectors of cut-points used in B-spline interpolations and \emph{BOUNDARIES} are vectors of length 2 specifying minimum and maximum values. 

   If, \code{use.my.knots.and.boundaries=FALSE}, then knots and boundaries
   are created and saved to the subdirectory \file{Knots\_Boundaries} using the naming convention \file{boundaries\_SUBJECT\_gNUMBER}, and 
   \file{knots\_SUBJECT\_gNUMBER}, where \file{SUBJECT} is the subject tested and \file{NUMBER} is the grade number. \file{SUBJECT} and \file{NUMBER} must be specified in 
   \code{knot.boundary.labels} if \code{use.my.knots.and.boundaries=FALSE}. 

  If \code{knots.and.boundaries=TRUE} then directory \file{Knots\_Boundaries} exists and contains knots and boundaries given in the form 
  \file{knots\_SUBJECT\_gNUMBER} and \file{boundaries\_SUBJECT\_gNUMBER}.  See example for how this can be accomplished within a larger master file.
 }
  \item{print.other.gp}{Boolean argument (defaults to FALSE) indicating whether growth percentiles of all orders should be returned. The default returns only the highest order growth 
   percentile for each student.
 }
  \item{rq.method}{An argument that defines the estimation method used in the quantile regression calculations. The default is the the \code{"br"} method
   referring to the Barrondale and Roberts l1 estimation detailed in Koenker (2005) and in the help for the quantile regression package.
 }
  \item{convert.0and100}{Boolean argument (defaults to TRUE) indicating whether conversion of growth percentiles of 0 and 100 to growth percentiles
    of 1 and 99, respectively, occurs. The default produces growth percentiles ranging from 1 to 99. 
 }
  \item{percuts.digits}{Argument specifying how many digits (defaults to 2) to print percentile cuts (if asked for) with.
 }
  \item{save.matrices}{Boolean argument (defaults to FALSE) indicating whether the coefficient matrices derived by the quantile regression analyses are saved to the
    \file{Coefficient\_Matrices} directory for later use (e.g., for use in the percentile growth projection/trajectory analyses).
 }
  \item{isotonize}{Boolean argument (defaults to TRUE) indicating whether quantile regression results are isotonized to prevent quantile crossing following the methods derived by
    Dette & Volgushev (2008). 
 }
  \item{convert.using.loss.hoss}{Boolean argument (defaults to TRUE) indicating whether requested percentile cuts are adjusted using the lowest obtainable scale score (LOSS) and
    highest obtainable scale score (HOSS). Those percentile cuts above the HOSS are replaced with the HOSS and those percentile cuts below the LOSS are replaced with the LOSS. The LOSS 
    and HOSS are obtained from the boundaries used for spline calculations. 
 }
  \item{sgp.function.labels}{If \code{save.matrices=TRUE} or \code{save.knots.boundaries=TRUE}, the function requires the user to specify a list, \code{sgp.function.labels}, of the form  
    \code{list(my.year= , my.subject= , my.grade= )}. The user specified values are used to save the coefficient matrices and/or knots/boundaries in an orderly fashion using an appropriate 
    combination of year, subject and grade.
 }
}

\details{Typical use of the function is to submit a data frame to the function containing records of all students across all grades, allowing the function to subset out the specified grade 
 using \code{subset.grade}. Function will subset those students with non-canonical grade progressions. If percentile growth projection/trajectory analyses are also desired, coefficient 
 matrices from the quantile regression analyses must be saved for use in those analyses. The example provides a generic file for use in analyzing assessment data across multiple grades. 
}

\value{Function returns a data frame containing student IDs and the associated student growth percentiles. If percentile.cuts or other order growth percentiles are requested then
additional variables are supplied in the returned data frame.
}

\references{
Koenker, R. (2005). \emph{Quantile regression}. Cambridge: Cambridge University Press.

Betebenner, D. W. (2008). Toward a normative understanding of student growth. In K. E. Ryan & 
L. A. Shepard (Eds.), \emph{The Future of Test Based Accountability} (pp. 155-170). New York: Routledge.

Dette, H. & Volgushev, S. Non-crossing non-parametric estimates of quantile curves. Journal of the Royal Statistical Society B,
70(3), 609-627.
}

\author{Damian W. Betebenner \email{dbetebenner@nciea.org} }

\note{If using user specified knots and boundaries (\code{use.my.knots.and.boundaries=TRUE}), knot and boundary vectors must be available within the 
Knots\_Boundaries directory using a specific naming conventions. See the example for syntax.}

\seealso{\code{\link{studentGrowthProjections}}, \code{\link{sgpData}}}


\examples{
############################################################################
####
#### Code for master file which computes student growth percentiles for 2007
#### using provided sgpData dataset
####
############################################################################


###
### Create knots and save to Knots\_Boundaries directory
### Uncomment to specify these knots/boundaries
### and add argument use.my.knots.and.boundaries=TRUE
###


##if (is.na(file.info("Knots_Boundaries")$isdir)) {
##    dir.create("Knots_Boundaries")
##}

##boundaries_read_g3 <- c(150, 795); knots_read_g3 <- c(510, 550, 580, 615)
##boundaries_read_g4 <- c(180, 940); knots_read_g4 <- c(542, 580, 606, 635)
##boundaries_read_g5 <- c(220, 955); knots_read_g5 <- c(562, 602, 632, 665)
##boundaries_read_g6 <- c(260, 970); knots_read_g6 <- c(575, 615, 645, 675)
##boundaries_read_g7 <- c(300, 980); knots_read_g7 <- c(586, 625, 655, 690)

##save(boundaries_read_g3, file="Knots_Boundaries/boundaries_read_g3.Rdata")
##save(knots_read_g3, file="Knots_Boundaries/knots_read_g3.Rdata")

##save(boundaries_read_g4, file="Knots_Boundaries/boundaries_read_g4.Rdata")
##save(knots_read_g4, file="Knots_Boundaries/knots_read_g4.Rdata")

##save(boundaries_read_g5, file="Knots_Boundaries/boundaries_read_g5.Rdata")
##save(knots_read_g5, file="Knots_Boundaries/knots_read_g5.Rdata")

##save(boundaries_read_g6, file="Knots_Boundaries/boundaries_read_g6.Rdata")
##save(knots_read_g6, file="Knots_Boundaries/knots_read_g6.Rdata")

##save(boundaries_read_g7, file="Knots_Boundaries/boundaries_read_g7.Rdata")
##save(knots_read_g7, file="Knots_Boundaries/knots_read_g7.Rdata")

###
### Load relevant datasets
###

data(sgpData)

###
### Calculate student growth percentiles
###


## 444444444444444444444444444444444444444444444444444444444444


g4_sgp <- studentGrowthPercentiles(
             student.data = sgpData, 
             num.panels = 4, 
             num.prior = 1, 
             subset.grade = 4,
             percuts.digits = 0, 
             sgp.function.labels = list(my.year=2007, my.subject="read", my.grade=4))

## save(g4_sgp, file="Results_Data/sgp_read_2007_g4.Rdata")


## 555555555555555555555555555555555555555555555555555555555555


g5_sgp <- studentGrowthPercentiles(
             student.data = sgpData, 
             num.panels = 4, 
             num.prior = 2,  
             subset.grade = 5,
             percuts.digits = 0,
             use.my.knots.and.boundaries=TRUE, 
             sgp.function.labels = list(my.year=2007, my.subject="read", my.grade=5))

## save(g5_sgp, file="Results_Data/sgp_read_2007_g5.Rdata")


## 6666666666666666666666666666666666666666666666666666666666666


g6_sgp <- studentGrowthPercentiles(
             student.data = sgpData, 
             num.panels = 4, 
             num.prior = 3, 
             subset.grade = 6,
             percuts.digits = 0,
             use.my.knots.and.boundaries=TRUE,
             sgp.function.labels = list(my.year=2007, my.subject="read", my.grade=6))

## save(g6_sgp, file="Results_Data/sgp_read_2007_g6.Rdata")


## 7777777777777777777777777777777777777777777777777777777777777


g7_sgp <- studentGrowthPercentiles(
             student.data = sgpData, 
             num.panels = 4, 
             num.prior = 3, 
             subset.grade = 7,
             percuts.digits = 0,
             use.my.knots.and.boundaries=TRUE,
             sgp.function.labels = list(my.year=2007, my.subject="read", my.grade=7))

## save(g7_sgp, file="Results_Data/sgp_read_2007_g7.Rdata")



###
### Bind grade results and save output
###

sgp_read_2007_gall <- rbind(g4_sgp, g5_sgp, g6_sgp, g7_sgp)
save(sgp_read_2007_gall, file="Results_Data/sgp_read_2007_gall.Rdata")
write.table(sgp_read_2007_gall, file="Results_Data/sgp_read_2007_gall.dat", 
            sep=",", row.names=FALSE, quote=FALSE)


###
### End of Master Program for Student Growth Percentiles
###
}

\keyword{ misc }
\keyword{ models }
