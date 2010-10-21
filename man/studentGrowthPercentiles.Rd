\name{studentGrowthPercentiles}
\alias{studentGrowthPercentiles}
\title{Student Growth Percentiles}
\description{
Function to calculate student growth percentiles using large scale assessment data. 
Outputs growth percentiles for each student and supplies various options as function arguments.
Results from this function are utilized to calculate percentile growth projections/trajectories.   
}
\usage{
studentGrowthPercentiles(panel.data,                                                    
                         sgp.labels,                                          
                         panel.data.vnames,                                 
                         grade.progression,                               
                         num.prior,                                         
                         subset.grade,                                     
                         percentile.cuts,                                
                         use.my.knots.boundaries,                       
                         use.my.coefficient.matrices,                 
                         print.other.gp,                             
                         calculate.sgps=TRUE,                                       
                         rq.method="br",                                           
                         knot.cut.percentiles=c(0.2,0.4,0.6,0.8),                 
                         convert.0and100=TRUE,                                   
                         sgp.quantiles="Percentiles",                          
                         percuts.digits=0,                                  
                         isotonize=TRUE,                                 
                         convert.using.loss.hoss=TRUE,                
                         goodness.of.fit=TRUE)                 
}

\arguments{
  \item{panel.data}{REQUIRED. Object of class list, data.frame, or matrix containing longitudinal student data in wide format. If supplied as part of a list, data should be 
  contained in \code{panel.data$Panel_Data}. Data must be formatted so that student ID is the first variable/column, student grade/time variables for each time period, 
  from earliest to most recent, are the next variables/columns, and student scale score variables for each year, from earliest to latest, are the remaining 
  variables/columns. See \code{\link{sgpData}} for an exemplar data set. NOTE: The column position of the variables IS IMPORTANT, NOT the names of the variables. 
  }
  \item{sgp.labels}{REQUIRED. A list, \code{sgp.labels}, of the form  \code{list(my.year= ,} \cr
  \code{my.subject= )}. The user-specified values are used to save the student growth percentiles, coefficient matrices, knots/boundaries, and goodness of fit results in an orderly fashion using an appropriate combination of year & 
  subject.
  }
  \item{panel.data.vnames}{Vector of variables to use in student growth percentile calculations. If not specified, function attempts to use all available variables.
  }
  \item{grade.progression}{Preferred argument to specify a student grade/time progression in the data. For example, \code{3:4} would indicate to subset the
  data where the two most recent years of data available are 3 and 4, respectively. Also allows for non-sequential grade progressions to be analyzed. For example,
  \code{c(7,8,10)} combined with an appropriate \code{panel.data.vnames} argument to remove a year of data would analyze students progressing from 7 to 8 to 10. 
  }
  \item{num.prior}{Number of prior scores one wishes to use in the analysis. Defaults to \code{num.panels-1}. 
  If \code{num.prior=1}, then only 1st order growth percentiles are computed, if \code{num.prior=2}, then 1st and 2nd order are computed, 
  if \code{num.prior=3}, 1st, 2nd, and 3rd ... NOTE: specifying \code{num.prior} is necessary in some situations (in early grades for example)
  where the number of prior data points is small compared to the number of panels of data. 
  }
  \item{subset.grade}{Student grade level for sub-setting. If the data fed into the function contains multiple
  grades, setting \code{subset.grade=5} selects out those students in grade five in the most recent year of the data. If no sub-setting is desired,
  argument do not include the \code{subset.grade} argument. If \code{grade.progression} is supplied, then a subset grade is implicitly specified.
  }
  \item{percentile.cuts}{Additional percentile cuts (supplied as a vector) associated with each student's conditional distribution. 
   Default is to provide NO growth percentile cuts (scale scores associated with those growth percentiles) for each student.
  }
  \item{use.my.knots.boundaries}{A list of the form  \code{list(my.year= , my.subject= )} specifying a set of pre-calculated
   knots and boundaries for B-spline calculations. Most often used to utilize knots and boundaries calculated from a previous analysis. Knots and boundaries are stored
   (and must be made available) with  \code{panel.data} supplied as a list in \code{panel.data$Knots_Boundaries$my.year.my.subject}. If missing, function automatically calculates knots, 
   boundaries, and loss.hoss values and stores them in \code{panel.data$Knots_Boundaries} \cr
   \code{$my.year.my.subject} where \code{my.year} and \code{my.subject} are provided by \code{sgp.labels}.  
  }
  \item{use.my.coefficient.matrices}{A list of the form  \code{list(my.year= , my.subject= )} specifying a set of pre-calculated
   coefficient matrices to use for student grwoth percentile calculations. Can be used to calculate baseline referenced student growth percentiles or to calculate student growth percentiles for small groups of excluded students without recalculating an entire set of data. If missing, coefficient matrices are calculated based upon the provided data and stores them in \cr
 \code{panel.data$Coefficient_Matrices$my.year.my.subject} where \code{my.year} and \code{my.subject} are provided by \code{sgp.labels}.
  }
  \item{print.other.gp}{Boolean argument (defaults to FALSE) indicating whether growth percentiles of all orders should be returned. The default returns only the highest order growth percentile for each student.
  }
  \item{calculate.sgps}{Boolean argument (defaults to TRUE) indicating whether student growth percentiles should be calculated following coefficient matrix calculation. 
  }
  \item{rq.method}{Argument to define the estimation method used in the quantile regression calculations. The default is the the \code{"br"} method referring to the Barrodale and Roberts l1 estimation detailed in Koenker (2005) and in the help for the quantile regression package.
  }
  \item{knot.cut.percentiles}{Argument that specifies the quantiles to be used for calculation of B-spline knots. Default is to place knots at the 0.2, 0.4, 0.6, and 0.8 quantiles. 
  }
  \item{convert.0and100}{Boolean argument (defaults to TRUE) indicating whether conversion of growth percentiles of 0 and 100 to growth percentiles of 1 and 99, respectively, occurs. The default produces growth percentiles ranging from 1 to 99. 
  }
  \item{sgp.quantiles}{Argument to specify quantiles for quantile regression estimation. Default is Percentiles. User can additionally submit a vector of quantiles (between 0 and 1). Goodness of fit output only available currently for PERCENTILES. 
  }
  \item{percuts.digits}{Argument specifying how many digits (defaults to 2) to print percentile cuts (if asked for) with.
  }
  \item{isotonize}{Boolean argument (defaults to TRUE) indicating whether quantile regression results are isotonized to prevent quantile crossing following the 
  methods derived by Dette & Volgushev (2008). 
  }
  \item{convert.using.loss.hoss}{Boolean argument (defaults to TRUE) indicating whether requested percentile cuts are adjusted using the lowest obtainable scale 
   score (LOSS) and highest obtainable scale score (HOSS). Those percentile cuts above the HOSS are replaced with the HOSS and those percentile cuts below the 
   LOSS are replaced with the LOSS. The LOSS and HOSS are obtained from the loss and hoss calculated with the knots and boundaries used for spline calculations. 
  }
  \item{goodness.of.fit}{Boolean argument (defaults to TRUE) indicating whether to produce goodness of fit results associated with produced student growth percentiles.
   Goodness of fit results are grid.grobs stored in \code{panel.data$Goodness_of_Fit} \cr   
   \code{$my.year.my.subject} where \code{my.year} and \code{my.subject} are provided by \code{sgp.labels}.
  }
} 


\details{Typical use of the function is to submit a data frame to the function containing records of all students across all grades, allowing the function to subset 
out specific grade progressions using \code{grade.progression}. Additional uses include using precalculated results to recalculate SGPs for baseline referencing.
The examples provide code for use in analyzing assessment data across multiple grades. 
}

\value{Function returns an object of class list containing the following lists: Coefficient_Matrices, Goodness_of_Fit, Knots_Boundaries, Panel_Data, SGPercentiles.
}

\references{
Betebenner, D. W. (2009). Norm- and criterion-referenced student growth. 
Educational Measurement: Issues and Practice, 28(4):42-51.

Betebenner, D. W. (2008). Toward a normative understanding of student growth. In K. E. Ryan & 
L. A. Shepard (Eds.), \emph{The Future of Test Based Accountability} (pp. 155-170). New York: Routledge.

Dette, H. & Volgushev, S. (2008). Non-crossing non-parametric estimates of quantile curves. Journal of the Royal Statistical Society B,
70(3), 609-627.

Koenker, R. (2005). \emph{Quantile regression}. Cambridge: Cambridge University Press.
}

\author{Damian W. Betebenner \email{dbetebenner@nciea.org} }

\seealso{\code{\link{studentGrowthProjections}}, \code{\link{sgpData}}}


\examples{
### Calculate 4th grade student growth percentiles

my.sgpData <- studentGrowthPercentiles(panel.data=sgpData,
                sgp.labels=list(my.year=2010, my.subject="Reading"),
                percentile.cuts=c(1,35,65,99),
                grade.progression=3:4)


## Loop to calculate all SGPs for all grades without percentile cuts
\dontrun{

my.grade.sequences <- list(3:4, 3:5, 3:6, 3:7, 4:8)

for (i in seq_along(my.grade.sequences)) {
     my.sgpData <- studentGrowthPercentiles(panel.data=sgpData,
                     sgp.labels=list(my.year=2010, my.subject="Reading"),
                     grade.progression=my.grade.sequences[[i]])
}
#  Save Student Growth Percentiles results to a .csv file:

write.csv(my.sgpData$SGPercentiles$READING.2010, 
file= "2010_Reading_SGPercentiles.csv", row.names=FALSE, quote=FALSE)}

## Loop to calculate all SGPs for all grades using 2006 to 2009 data 
## (embedding results in my.sgpData from previous example)
\dontrun{

my.grade.sequences <- list(3:4, 3:5, 3:6, 4:7, 5:8)

for (i in seq_along(my.grade.sequences)) {
     my.sgpData <- studentGrowthPercentiles(panel.data=my.sgpData,
                     panel.data.vnames=c("ID", "GRADE_2006", 
                          "GRADE_2007", "GRADE_2008", "GRADE_2009",
                          "SS_2006", "SS_2007", "SS_2008", "SS_2009"),
                     sgp.labels=list(my.year=2009, my.subject="Reading"),
                     grade.progression=my.grade.sequences[[i]])
}
#  Save Student Growth Percentiles results to a .csv file:

write.csv(my.sgpData$SGPercentiles$READING.2009, 
file= "2010_Reading_SGPercentiles.csv", row.names=FALSE, quote=FALSE)
}}

\keyword{ misc }
\keyword{ models }
