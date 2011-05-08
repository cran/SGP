\name{SGP-package}
\alias{SGP-package}
\alias{SGP}
\docType{package}
\title{SGP: An R Package for the Calculation and Visualization of Student Growth Percentiles.}
\description{
SGP contains functions to calculate student growth percentiles and percentile growth projections/trajectories following methodology found in Betebenner (2008, 2009).
The package contains two primary functions, \code{\link{studentGrowthPercentiles}} and \code{\link{studentGrowthProjections}}, and numerous higher level functions
that make use of them including: \code{prepareSGP}, \code{\link{analyzeSGP}}, \code{\link{combineSGP}}, \code{\link{summarizeSGP}}, and \code{reportSGP} 
used to calculate and report student growth percentiles and percentile growth projections/trajectories for students using large scale, longitudinal assessment data. 
These norm referenced growth values are currently used in a number of state testing and accountability systems. The functions employ quantile 
regression (using the \code{quantreg} package) to estimate the conditional density for current achievement using each student's achievement history. 
Percentile growth projections/trajectories are calculated using the coefficient matrices derived from the student growth percentile analyses. These quantities
are summarized in a variety of ways to describe student growth. As of the 0.0-7.0 release, the package also includes functions (e.g., 
\code{\link{bubblePlot}} and \code{\link{studentGrowthPlot}}) to produce graphical representations associated with the student growth percentile analyses. 
}

\details{
\tabular{ll}{
Package: \tab SGP\cr
Type: \tab Package\cr
Version: \tab 0.0-8.0\cr
Date: \tab 2011-5-5\cr
License: \tab CC BY-SA 3.0 US | CC BY-NC-SA 3.0 + file LICENSE\cr
LazyLoad: \tab yes\cr
}
Calculation of student growth percentiles and percentile growth projections/trajectories is typically performed by grade and subject. Data for growth percentile 
calculation must be specifically formatted.  See \code{\link{sgpData}} for an example data set. Batch R syntax for performing analyses across all grades and years is 
provided in the examples of the \code{\link{studentGrowthPercentiles}} and \code{\link{studentGrowthProjections}} using the higher level functions 
\code{\link{prepareSGP}}, \code{\link{analyzeSGP}}, \code{\link{combineSGP}}, \code{\link{summarizeSGP}}, and \code{reportSGP}.} 

\author{Damian W. Betebenner \email{dbetebenner@nciea.org} and Adam Van Iwaarden \email{Vaniwaarden@colorado.edu}} 

\references{ 
Betebenner, D. W. (2009). Norm- and criterion-referenced student growth. 
Educational Measurement: Issues and Practice, 28(4):42-51.

Betebenner, D. W. (2008). Toward a normative understanding of student growth. In K. E. Ryan & 
L. A. Shepard (Eds.), \emph{The Future of Test Based Accountability} (pp. 155-170). New York: Routledge.

Koenker, R. (2005). \emph{Quantile regression}. Cambridge: Cambridge University Press.
}

\keyword{package}
