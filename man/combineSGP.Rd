\name{combineSGP}
\alias{combineSGP}
\title{Combine student data and SGP results}
\description{
  Utility function/exemplar used to merge student long data with results from student growth percentiles and/or student growth projections calculations. Default values of this
  function are designed to be used following use of other utility functions: \code{\link{prepareSGP}} and \code{\link{analyzeSGP}}.
}

\usage{
combineSGP(sgp_object,
	state,
	sgp.percentiles=TRUE,
	sgp.projections.lagged=TRUE,
	max.lagged.sgp.target.years.forward=4)
}

\arguments{
  \item{sgp_object}{A list containing both \code{Student} (from \code{prepareSGP}) and \code{SGP} (from \code{analyzeSGP}) slots. 
  }
  \item{state}{Acronym for which state is to be used for the lagged projections and growth to standard analyses.
  }
  \item{sgp.percentiles}{A boolean variable indicating whether to combine student growth percentiles.
  }
  \item{sgp.projections.lagged}{A boolean variable indicating whether to combine lagged projections and calculate catch-up/keep-up values.
  }
  \item{max.lagged.sgp.target.years.forward}{A integer indicating the number of years forward from the lagged (last year's) score to project forward for growth to standard calculations. Default is 4 years from last year or 3 years from present, which is the standard in most growth to standard calculations used by state departments of education.
  }
}

\value{Function returns a list containing the input long data set in the \code{Student} slot as a \code{data.table} keyed using \code{VALID_CASE}, \code{CONTENT_AREA}, 
\code{YEAR}, \code{ID} merged with student growth percentiles and/or lagged projection targets and catch up/keep up status.
}

\author{Damian W. Betebenner \email{dbetebenner@nciea.org}}

\seealso{\code{\link{prepareSGP}}, \code{\link{analyzeSGP}}}

\examples{
\dontrun{
## combineSGP is Step 3 of 5 of abcSGP
DEMO_Data <- combineSGP(DEMO_Data, state="DEMO")
}  
}

\keyword{documentation}
