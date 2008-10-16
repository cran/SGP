\name{SGP-package}
\alias{SGP-package}
\alias{SGP}
\docType{package}
\title{Functions to calculate student growth percentiles and percentile growth projections/trajectories following methodology found in Betebenner (2008)
}
\description{SGP contains two functions, \code{studentGrowthPercentiles} and \code{studentGrowthProjections} that are used to calculate 
growth percentiles and projections for students using large scale, longitudinal assessment data. These norm referenced growth values 
are currently used in state testing and accountability systems. The functions employ quantile regression techniques (using the \code{quantreg}
package) to estimate the conditional density associated with each student's achievement history. Percentile growth projections/trajectories
are calculated using the coefficient matrices derived from the student growth percentile quantile regression analyses.}

\details{
\tabular{ll}{
Package: \tab SGP\cr
Type: \tab Package\cr
Version: \tab 0.0-1\cr
Date: \tab 2008-10-10\cr
License: \tab Creative Commons Attribution + ShareAlike (by-sa)\cr
LazyLoad: \tab yes\cr
}
Calculation of student growth percentiles is typically performed by grade and subject. Data for growth percentile calculation must be specifically formatted. 
See \code{sgpData} for an example data set. Batch R syntax for performing analyses across all grades is provided in the examples of the studentGrowthPercentiles function. 
Calculation of percentile growth projections/trajectories follows calculation of student growth percentiles and requires coefficient matrices derived during 
student growth percentile estimation.}

\author{Damian W. Betebenner \email{DBetebenner@nciea.org}} 

\references{ 
Koenker, R. (2005). \emph{Quantile regression}. Cambridge: Cambridge University Press.

Betebenner, D. W. (2008). Toward a normative understanding of student growth. In K. E. Ryan & 
L. A. Shepard (Eds.), \emph{The Future of Test Based Accountability} (pp. 155-170). New York: Routledge.
}

\keyword{package}
