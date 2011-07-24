\name{SGP-class}
\docType{class}
\alias{SGP-class}
\alias{list.null-class}
\title{Class "SGP"}
\description{The formal S4 class for SGP. This class stores the data object object for use with the function "analyzeSGP" and the resulting coefficient matrices and updated data object.}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("SGP", ...)}, but this is not encouraged. Use the function \code{\link{prepareSGP}} instead.
}

\section{Slots}{
  \describe{
    \item{\code{Data}:}{A data.table including student-level data in a (long) format. Each VALID_CASE, CONTENT_AREA, YEAR, ID combination represents a unique case in the data. See sgpData_LONG for an examplar data set}
    \item{\code{Variable_Name_Lookup}:}{A data.frame with three columns. The data.frame is used as a lookup table to translate state specific variable names to SGP variable names}
    \item{\code{SGP}:}{A list including the output from \code{\link{analyzeSGP}}}
    \item{\code{Summary}:}{A list including the output from \code{\link{summarizeSGP}}}
  }
}

\details{
  \describe{
    \item{list.null:}{combines class \code{list} and class \code{NULL} 
  }
    \item{SGP:}{The slot \code{Variable_Names} should be a data.frame with three columns. The first column 
    (named "column") identifies the column in the original data for the associated variable. 
    The second column is a character vector (named "nms.original") of original variable names 
    for all of the variables in \code{data}. The third column is a character vector (named 
    "nms.sgp") that identifies the capitalized variable names used with various functions from 
    the \code{SGP} package. Original variables that are not needed for \code{SGP} functions should 
    be marked as \code{NA} in the "nms.sgp" column.
  }
 }
}


\author{Jonathan P. Weeks \email{weeksjp@gmail.com}}

\seealso{
\code{\link{prepareSGP}}
}
\keyword{classes}
