\name{ viewSummaryGroups}
\alias{ viewSummaryGroups}
\title{View summary and confidence interval groups that will be used in \code{\link{summarizeSGP}}}
\description{
 Utility function that produces a data.frame from the lists of user supplied summary and confidence interval group variables. Intended to complement \code{\link{summarizeSGP}} and promote proper group summary specification.
}

\usage{
 viewSummaryGroups(
     summary.groups=list(institution=c("STATE", "SCHOOL_NUMBER"),
        content="CONTENT_AREA",
        time="YEAR",
        institution_level="GRADE",
        demographic=c("GENDER", "ETHNICITY", "FREE_REDUCED_LUNCH_STATUS", 
             "ELL_STATUS", "CATCH_KEEP_UP"),
        institution_inclusion=list(STATE="OCTOBER_ENROLLMENT_STATUS",
              SCHOOL_NUMBER="OCTOBER_ENROLLMENT_STATUS")),
    confidence.interval.groups=list(institution="SCHOOL_NUMBER",
        content="CONTENT_AREA",
        time="YEAR",
        institution_level= NULL,
        demographic=NULL,
        institution_inclusion=list(STATE=NULL,
              SCHOOL_NUMBER="OCTOBER_ENROLLMENT_STATUS")),
    confidence.interval.groups.only=FALSE)
}

\arguments{
  \item{summary.groups}{A list containing the grouping variables upon which to aggregate in \code{\link{summarizeSGP}}. This includes the \code{institution}, \code{content area}, \code{time}, \code{institution_level}, \code{demographic} groups, and \code{institution_inclusion} indicator variables. All group slots MUST be included in the list, although \code{NULL} can be provided if a grouping subset is not desired. All possible combinations of the group variables are produced. There are 192 permutations in the default specification.
  }
  \item{confidence.interval.groups}{A list containing the grouping variables for which confidence intervals should be constructed in \code{\link{summarizeSGP}}. List slots set to \code{NULL} will not produce confidence intervals. NOTE: Groups to be included should be identified selectively because confidence interval construction is currently very memory intensive!
  }
  \item{confidence.interval.groups.only}{Boolean variable indicating whether to show only those group combinations that will include a confidence interval for the Median SGP. Defaults to FALSE.
  }
}

\details{The user provides the arguments required by \code{\link{summarizeSGP}} in order to see the group permutations for which summary tables will be produced in \code{\link{summarizeSGP}}.
}

\value{Function returns data.frame containing the summary group permutations and a Boolean vector indicating whether or not the permutation will also have a 95\% confidence interval provided when the summary table is produced using \code{\link{summarizeSGP}}.
}

\author{Damian W. Betebenner \email{dbetebenner@nciea.org} }

\seealso{\code{\link{prepareSGP}}, \code{\link{analyzeSGP}}, \code{\link{combineSGP}}}

\examples{
\dontrun{
## View only summary.groups combinations that will have confidence intervals
viewSummaryGroups(confidence.interval.groups.only=TRUE)
}  
}

\keyword{documentation}
