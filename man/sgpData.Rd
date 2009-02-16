\name{sgpData}
\alias{sgpData}
\docType{data}
\title{Longitudinal Assessment Data}
\description{
A dataset comprising a subset of four years of state assessment data suitable for student growth percentile
and percentile growth projection/trajectory analyses. The dataset is used in examples provided in the documentation
for the \code{studentGrowthPercentiles} and \code{studentGrowthProjections} functions and serves as an exemplar for user construction 
of their own datasets for growth percentile analyses. 
}
\usage{data(sgpData)}
\format{
  A data frame of student level observations for four years on the following  variables.
  \describe{
    \item{\code{ID}}{Student ID}
    \item{\code{GRADE_04}}{Student Grade Tested 2004, possibly missing}
    \item{\code{GRADE_05}}{Student Grade Tested 2005, possibly missing}
    \item{\code{GRADE_06}}{Student Grade Tested 2006, possibly missing}
    \item{\code{GRADE_07}}{Student Grade Tested 2007, possibly missing}
    \item{\code{SS_04}}{Student Scale Score 2004, possibly missing}
    \item{\code{SS_05}}{Student Scale Score 2005, possibly missing}
    \item{\code{SS_06}}{Student Scale Score 2006, possibly missing}
    \item{\code{SS_07}}{Student Scale Score 2007, possibly missing}
  }
}

\note{All datasets used with \code{studentGrowthPercentiles} and \code{studentGrowthProjections} must be specifically formatted as \emph{wide} format files. The first 
variable/column is the student ID variable. The next set of columns provide the grade of the student across all the years provided in the dataset (possibly missing). 
The last set of columns provide the scales scores of the student in the respective grades.} 


\seealso{\code{\link{studentGrowthPercentiles}}, \code{\link{studentGrowthProjections}}}

\source{Anonymized state assessment data}
\keyword{datasets}
