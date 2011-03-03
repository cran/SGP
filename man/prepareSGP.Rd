\name{prepareSGP}
\alias{prepareSGP}
\title{Prepare data for SGP analyses}
\description{
  Utility function/exemplar used to embed supplied long data into a list object as a keyed data.table.
}

\usage{
prepareSGP(long_data) 
}

\arguments{
  \item{long_data}{A panel data set in long form. See embedded \code{\link{sgpData_LONG}} data set for an exemplar. 
  }
}

\value{Function returns a list containing the input long data set in the \code{Student} slot as a data.table keyed using \code{VALID_CASE}, \code{CONTENT_AREA}, 
\code{YEAR}, \code{ID}. 
}

\author{Damian W. Betebenner \email{dbetebenner@nciea.org} }

\seealso{\code{\link{sgpData_LONG}}}

\examples{
\dontrun{
DEMO_Data <- prepareSGP(sgpData_LONG)

}  
}

\keyword{documentation}
