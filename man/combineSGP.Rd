\name{combineSGP}
\alias{combineSGP}
\title{Combine student data and SGP results}
\description{
  Utility function/exemplar used to merge student long data with results from studentGrowthPercentiles and/or studentGrowthProjections calculations. Default values of this
  function are designed to be used following use of other utility functions: \code{\link{prepareSGP}} and \code{\link{analyzeSGP}}.
}

\usage{
combineSGP(sgp_object=sgp_object,
	years=years,
	content_areas=content_areas,
	sgp.percentiles=sgp.percentiles,
	sgp.projections.lagged=sgp.projections.lagged,
	state=state)
}

\arguments{
  \item{sgp_object}{A list containing both \code{Student} (from \code{prepareSGP}) and \code{SGP} (from \code{analyzeSGP}) slots. 
  }
  \item{years}{A vector indicating what years are being analyzed.
  }
  \item{content_areas}{A vector indicating what content areas are being analyzed.
  }
  \item{sgp.percentiles}{A boolean variable indicating whether to combine student growth percentiles.
  }
  \item{sgp.projections.lagged}{A boolean variable indicating whether to combine lagged projections and calculate catch-up/keep-up values.
  }
  \item{state}{Acronym for which state is to be used for the lagged projections and growth to standard analyses.
  }
}

\value{Function returns a list containing the input long data set in the \code{Student} slot as a data.table keyed using \code{VALID_CASE}, \code{CONTENT_AREA}, 
\code{YEAR}, \code{ID} merged with student growth percentiles and/or lagged projection targets and catch up/keep up status.
}

\author{Damian W. Betebenner \email{dbetebenner@nciea.org} }

\seealso{\code{\link{prepareSGP}}, \code{\link{analyzeSGP}}}

\examples{
\dontrun{
## combineSGP is Step 3 of 5
DEMO_Data <- prepareSGP(sgpData_LONG)
DEMO_Data <- analyzeSGP(DEMO_Data)
DEMO_Data <- combineSGP(DEMO_Data)
}  
}

\keyword{documentation}
