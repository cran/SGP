\name{studentGrowthProjections}
\alias{studentGrowthProjections}
\title{Student Growth Projections}
\description{
  Function to calculate percentile growth projections/trajectories using large scale assessment data using results derived from student growth percentile calculation.
  Function can produce percentile growth trajectories as well as growth percentiles sufficient for each student to reach a set of predefined scale score cut.
}

\usage{
studentGrowthProjections(panel.data, 
         sgp.labels,               
         grade.progression,       
         max.forward.progression, 
         max.order.for.progression,  
         use.my.knots.boundaries,   
         use.my.coefficient.matrices,  
         panel.data.vnames,         
         performance.level.cutscores, 
         chunk.size=5000,          
         convert.0and100=TRUE,    
         projection.unit="YEAR",       
         percentile.trajectory.values=1:99, 
         isotonize=TRUE,     
         projcuts.digits=2)
}

\arguments{
  \item{panel.data}{Object of class \code{list} containing longitudinal student data in wide format in \code{panel.data$Panel_Data}. See \code{\link{studentGrowthPercentiles}}
  for data requirements. List object must also contain \code{panel.data$Knots_Boundaries} and \code{panel.data$Coefficient_Matrices}. See \code{\link{sgpData}} for an 
  exemplar data set. NOTE: The column position of the variables IS IMPORTANT, NOT the names of the variables. 
  }
  \item{sgp.labels}{REQUIRED. A list, \code{sgp.labels}, of the form  \code{list(my.year= , my.subject=)}. The user specified values are used to save the 
  percentile growth projections/trajectories and identify coefficient matrices and knots & boundaries for calculation if \code{use.my.coefficient.matrices} or
  \code{use.my.knots.boundaries} is missing. Partly replaces previous argument \code{proj.function.labels}. 
  }
  \item{grade.progression}{REQUIRED. Argument to specify a student grade/time progression in the data to be used for percentile growth projection/trajectory calculation. 
  This argument help in replacing previous arguments \code{num.panels}, \code{max.num.scores}, and \code{num.prior.scores}.
  }
  \item{max.forward.progression}{The MAXIMUM number of years/grades/time periods to project forward conditional upon available coefficient matrices. If missing, function
  will project forward as far as allowed by available coefficient matrices. 
  }
  \item{max.order.for.progression}{Argument to specify the maximum coefficient matrix order to be used for percentile growth projection/trajectory calculation. 
  If missing, the function utilizes the highest matrix order available. 
  }
  \item{use.my.knots.boundaries}{A list of the form  \code{list(my.year= , my.subject=)} specifying the set of pre-calculated
   knots and boundaries for B-spline calculations. Knot and boundaries are stored (and must be made available) with panel.data supplied as a 
   list in \code{panel.data$Knots_Boundaries$my.year.my.subject}. If missing, function tries to retrieve knots and boundaries from 
   \code{panel.data$Knots_Boundaries$my.year.my.subject} where my.year and my.subject are provided by \code{sgp.labels}.  
  }
  \item{use.my.coefficient.matrices}{A list of the form  \code{list(my.year= , my.subject=)} specifying the set of pre-calculated
   coefficient matrices to use for percentile growth projection/trajectory calculations. Coefficient matrices are stores (and must be available) with \code{panel.data}
   supplied as a list in \code{panel.data$Coefficient_Matrices$my.year.my.subject}. If missing, function tries to retrieve coefficient matrices from
   \code{panel.data$Coefficient_Matrices$my.year.my.subject} where my.year and my.subject are provided by \code{sgp.labels}.
  }
  \item{panel.data.vnames}{Vector of variables to use in percentile growth projection/trajectory calculations. If not specified, function attempts to use all available 
   variables.
  }
  \item{performance.level.cutscores}{Argument for supplying performance level cutscores to be used for determining growth-to-standard percentile growth trajectory
  calculations. Argument accepts a two letter state acronymn (e.g., "CO") that retrieves cutscores that are automatically embedded in a data set contained in
  the SGP package. Argument also accepts a subject specific list of the form:
  \preformatted{
    performance.level.cutscores <- list(MATH=list(GRADE_3=c(cut1, cut2, cut3),
                                                  GRADE_4=c(cut1, cut2, cut3),
                                                  GRADE_5=c(cut1, cut2, cut3),
                                                  GRADE_6=c(cut1, cut2, cut3),
                                                  GRADE_7=c(cut1, cut2, cut3),
                                                  GRADE_8=c(cut1, cut2, cut3)))
 
  }
  If not cuts are desired leave the cutscore unspecified (the default), which is the default. Note that the subject name must match that provided by 
  \code{sgp.labels}. If your states cutscores are not included in the \code{\link{stateData}} data set or are incorrect, please contact 
  \email{dbetebenner@nciea.org} to have the cutscores added or corrected!
  }
  \item{chunk.size}{An integer (default of 5,000) indicating what the number of rows in the block of data (so that the program doesn't overwhelm memory). 
  }
  \item{convert.0and100}{Boolean argument (defaults to TRUE) indicating whether conversion of growth percentiles of 0 and 100 to growth percentiles
    of 1 and 99, respectively, occurs. The default produces growth percentiles ranging from 1 to 99.
  }
  \item{projection.unit}{Argument specifying the units in which the projections/trajectories and cuts are reported. Either \code{"YEAR"}, the default, or 
  \code{"GRADE"}.
  }       
  \item{percentile.trajectory.values}{Returns a vector of percentile trajectories (defaults to 1 to 99) associated with percentile growth projections/trajectories
  produced.  in addition to the percentiles sufficient to reach the performance thresholds. If no percentile trajectories are desired, set 
  \code{percentile.trajectory.values=NULL}. 
  }
  \item{isotonize}{Boolean argument (defaults to TRUE) indicating whether quantile regression results are isotonized to prevent quantile crossing following 
  the methods derived by Dette & Volgushev (2008).
  }
  \item{projcuts.digits}{The number of digits (defaults to 2) percentile trajectories (if requested) are formatted.
  }
}

\value{Function returns the input panel.data list object with the additional percentile growth trajectories/percentiles stored in 
\code{panel.data$SGProjections$my.year.my.subject} consisting of student IDs and the associated percentile growth projections/trajectories and cuts.
Depending upon the number of performance level cut-scores and percentile trajectories specified, the data frame contains projections/trajectories
for each performance level cut-point supplied and each percentile cut specified.
}

\references{
Betebenner, D. W. (2009). Norm- and criterion-referenced student growth. 
Educational Measurement: Issues and Practice, 28(4):42-51.

Betebenner, D. W. (2008). Toward a normative understanding of student growth. In K. E. Ryan & 
L. A. Shepard (Eds.), \emph{The Future of Test Based Accountability} (pp. 155-170). New York: Routledge.

Dette, H. & Volgushev, S. (2008). Non-crossing non-parametric estimates of quantile curves. Journal of the Royal Statistical Society B, 
70(3), 609-627.
}

\author{Damian W. Betebenner \email{dbetebenner@nciea.org} }

\note{Use of this function assumes prior calculation of student growth percentiles with \code{save.matrices=TRUE} 
  (saving the coefficient matrices associated with student growth percentile calculations within the \file{Coefficient\_Matrices} 
  directory). Additionally percentile growth projections/trajectories require future achievement levels (e.g., performance level cut-scores) 
  against which percentile growth projections/trajectories are calculated. These values must be available in the \file{Cutscores} 
  directory using specific naming conventions. See the example for syntax on how to create and save cut-score vectors. 
}

\seealso{\code{\link{studentGrowthPercentiles}}, \code{\link{sgpData}}, \code{\link{stateData}}}

\examples{
\dontrun{
### First calculate SGPs for 2010

my.grade.sequences <- list(3:4, 3:5, 3:6, 3:7, 4:8)

for (i in seq_along(my.grade.sequences)) {
     my.sgpData <- studentGrowthPercentiles(panel.data=sgpData,
                                            sgp.labels=list(my.year=2010, my.subject="Reading"),
                                            grade.progression=my.grade.sequences[[i]])
}

### Calculate Growth Projections

my.grade.progressions <- list(3, 3:4, 3:5, 3:6, 3:7)

for (i in seq_along(my.grade.progressions)) {
     my.sgpData <- studentGrowthProjections(panel.data=my.sgpData,
                                            sgp.labels=list(my.year=2010, my.subject="Reading"),
                                            projcuts.digits=0,
                                            projection.unit="GRADE",
                                            percentile.trajectory.values=c(1,35,50,65,99),
                                            grade.progression=my.grade.progressions[[i]])
}
}
}

\keyword{ misc }
\keyword{ models }
