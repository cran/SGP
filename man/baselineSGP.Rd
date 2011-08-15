\name{ baselineSGP}
\alias{ baselineSGP}
\title{Analyze student data to produce student growth percentiles and coefficient matrices from a baseline (i.e. multiple cohort) norm group}
\description{
  Utility function/exemplar used to produce student growth percentiles using long formatted data like that provided by \code{\link{prepareSGP}}.
}

\usage{
 baselineSGP(sgp_object,
         state,
         years,
         content_areas,
        grade.sequences,
        baseline.max.order,
        calculate.sgps=FALSE,
         ...)
}

\arguments{
  \item{sgp_object}{An object of class \code{SGP} containing long formatted data in the \code{code} (from \code{\link{prepareSGP}}) slot. 
  }
  \item{state}{Acronym indicating state associated with the data for access to embedded knot and boundaries, cutscores and other state related assessment data.
  }
  \item{years}{A vector indicating year(s) in which to produce student growth percentiles and/or student growth projections/trajectories. If missing the 
function will use the data to infer the year(s) based upon the assumption of having at least three years of panel data for analyses.
  }
  \item{content_areas}{A vector indicating content area(s) in which to produce student growth percentiles and/or student growth projections/trajectories. 
If left missing the function will use the data to infer the content area(s) available for analyses.
  }
  \item{grade.sequences}{A vector indicating acceptable grade sequences for which to calculate student growth percentiles and/or student growth projections/trajectories. If left missing the function will use the data to infer all the grade progressions for student growth percentile and student growth projections/trajectories analyses.
  }
  \item{baseline.max.order}{Maximum number of prior scores one wishes to use in the analysis. Defaults to \code{num.panels-2} so that at least two cohorts comprise each baseline norm group. If \code{baseline.max.order=1}, then only 1st order growth percentiles are computed, if \code{baseline.max.order=2}, then 1st and 2nd order are computed, if \code{baseline.max.order=3}, 1st, 2nd, and 3rd ...  }
  \item{calculate.sgps}{Boolean variable indicating whether to calculate student growth percentiles. Defaults to FALSE to only produce coefficient matrices.
  }
  \item{...}{Arguments to be passed to \code{\link{studentGrowthPercentiles}} for finer control over SGP calculations. 
  }
}

\value{Function returns a list containing the long data set in the \code{SGP} slot as a \code{data.table} keyed using \code{VALID_CASE}, \code{CONTENT_AREA}, 
\code{YEAR}, \code{ID} and the student growth percentile results in the \code{SGP} slot. 
}

\author{Adam Van Iwaarden \email{Vaniwaarden@colorado.edu}, Ben Domingue \email{ben.domingue@gmail.com} and Damian W. Betebenner \email{dbetebenner@nciea.org}}

\seealso{\code{\link{prepareSGP}}, \code{\link{analyzeSGP}}, \code{\link{combineSGP}}}

\examples{
\dontrun{
##  Create baseline coefficient matrices using all available cohorts in the example data
Demonstration_Data <- prepareSGP(sgpData_LONG)
Demonstration_Data <-  baselineSGP(Demonstration_Data)
}  
}

\keyword{documentation}
