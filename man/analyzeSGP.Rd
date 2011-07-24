\name{analyzeSGP}
\alias{analyzeSGP}
\title{Analyze student data to produce student growth percentiles and student growth projections}
\description{
  Utility function/exemplar used to produce student growth percentiles and student growth projections using long formatted data like that provided by \code{\link{prepareSGP}}.
}

\usage{
analyzeSGP(sgp_object,
         state,
         years,
         content_areas,
         grades,
         sgp.config,
         sgp.percentiles=TRUE,
         sgp.projections=TRUE,
         sgp.projections.lagged=TRUE,
         simulate.sgps=TRUE,
         goodness.of.fit.print=TRUE,
         parallel.config,
         ...)
}

\arguments{
  \item{sgp_object}{An object of class \code{SGP} containing long formatted data in the \code{code} (from \code{\link{prepareSGP}}) slot. 
  }
  \item{state}{Acronym indicating state associated with the data for access to embedded knot and boundaries, cutscores, CSEMs, and other state related assessment data.
  }
  \item{years}{A vector indicating year(s) in which to produce student growth percentiles and/or student growth projections/trajectories. If missing the 
function will use the data to infer the year(s) based upon the assumption of having at least three years of panel data for analyses.
  }
  \item{content_areas}{A vector indicating content area(s) in which to produce student growth percentiles and/or student growth projections/trajectories. 
If left missing the function will use the data to infer the content area(s) available for analyses.
  }
  \item{grades}{A vector indicating grades for which to calculate student growth percentiles and/or student growth projections/trajectories. 
If left missing the function will use the data to infer all the grade progressions for student growth percentile and student growth projections/trajectories analyses.
  }
  \item{sgp.config}{If \code{years}, \code{content_areas}, and \code{grades} are missing, user can directly specify a list containing three vectors: \code{sgp.content.areas}, 
\code{sgp.panel.years}, and \code{sgp.grade.sequences}. This advanced option is helpful for analysis of non-traditional grade progressions and other special cases. 
See examples for use cases.
  }
  \item{sgp.percentiles}{Boolean variable indicating whether to calculate student growth percentiles. Defaults to TRUE.
  }
  \item{sgp.projections}{Boolean variable indicating whether to calculate student growth projections. Defaults to TRUE.
  }
  \item{sgp.projections.lagged}{Boolean variable indicating whether to calculate lagged student growth projections often used for growth to standard analyses. Defaults to TRUE.
  }
  \item{simulate.sgps}{Boolean variable indicating whether to simulate SGP values for students based on test-specific Conditional Standard Errors of Measurement (CSEM).  Test CSEM data must be available for simulation and included in \code{\link{stateData}}.  This argument must be set to TRUE for confidence interval construction. Defaults to TRUE.
  }
  \item{goodness.of.fit.print}{Boolean variable indicating whether to print out Goodness of Fit figures as pdf into a directory labeled Goodness of Fit. Defaults to TRUE.
  }
  \item{parallel.config}{One of \code{FOREACH}, \code{SNOW}, \code{MULTICORE} allowing the user to specify the back end for parallel computation. 
  }
  \item{...}{Arguments to be passed to \code{\link{studentGrowthPercentiles}} or \code{\link{studentGrowthProjections}} for finer control over SGP calculations. 
  }
}

\value{Function returns a list containing the long data set in the \code{Student} slot as a \code{data.table} keyed using \code{VALID_CASE}, \code{CONTENT_AREA}, 
\code{YEAR}, \code{ID} and the student growth percentile and/or student growth projection/trajectory results in the \code{SGP} slot. 
}

\author{Damian W. Betebenner \email{dbetebenner@nciea.org} and Adam Van Iwaarden \email{Vaniwaarden@colorado.edu}}

\seealso{\code{\link{prepareSGP}}, \code{\link{combineSGP}}}

\examples{
\dontrun{
## analyzeSGP is Step 2 of 5 of abcSGP
Demonstration_Data <- sgpData_LONG
Demonstration_Data <- prepareSGP(Demonstration_Data)
Demonstration_Data <- analyzeSGP(Demonstration_Data)

## Or (explictely pass state argument)

Demo_Data <- prepareSGP(sgpData_LONG)
Demo_Data <- analyzeSGP(Deo_Data, state="DEMO")
}  
}

\keyword{documentation}
