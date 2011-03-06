\name{abcSGP}
\alias{abcSGP}
\title{Perform 4 step sequence: prepareSGP, analyzeSGP, combineSGP, and summarizeSGP}
\description{
  Utility function to perform sequence of steps going from data preparation, \code{\link{prepareSGP}}, SGP data analysis, \code{\link{analyzeSGP}},
  data combining, \code{\link{combineSGP}}, and data summarization, \code{\link{summarizeSGP}}. Calculating and using
  student growth percentiles is as easy as ABC.
}

\usage{
abcSGP(sgp_object=sgpData_LONG,
    state="DEMO",
    years,
    content_areas,
    grades,
    sgp.config,
    sgp.percentiles=TRUE,
    sgp.projections=TRUE,
    sgp.projections.lagged=TRUE,
    simulate.sgps=TRUE,
    sgp.summaries=list(MEDIAN_SGP="median_na(SGP)",
       MEDIAN_SGP_COUNT="num_non_missing(SGP)",
       PERCENT_AT_ABOVE_PROFICIENT=
           "percent_in_category(ACHIEVEMENT_LEVEL, list(c(1,4)), list(1:5))",
       PERCENT_AT_ABOVE_PROFICIENT_COUNT=
           "num_non_missing(ACHIEVEMENT_LEVEL)"),
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
            SCHOOL_NUMBER="OCTOBER_ENROLLMENT_STATUS")))
}

\arguments{
  \item{sgp_object}{A list containing long formatted data in the \code{Student} (from \code{\link{prepareSGP}}) slot. 
  }
  \item{state}{Acronym indicating state associated with the data for access to embedded knot and boundaries and cutscores.
  }
  \item{years}{A vector indicating years in which to produce student growth percentiles and/or student growth projections/trajectories. If left missing the 
function will use the data to infer the years based upon the assumption of having at least three years of panel data for analyses.
  }
  \item{content_areas}{A vector indicating content areas in which to produce student growth percentiles and/or student growth projections/trajectories. 
If left missing the function will use the data to infer the content areas available for analyses.
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
  \item{simulate.sgps}{Boolean variable indicating whether to simulate SGP values for students based on test-specific Conditional Standard Errors of Measurement (CSEM).  Test CSEM data must be available for simulation.  Must be set to TRUE for confidence interval construction. Defaults to TRUE in \code{abcSGP} only.
  }
  \item{sgp.summaries}{A list giving the summaries requested for each group analyzed based upon the \code{summary.group} argument.
  }
  \item{summary.groups}{A list consisting of 5 types of groups across which all summaries are taken: \code{institution}, \code{content}, \code{time}, \code{institution_level},
\code{demographic}, and \code{institution_inclusion}. Summaries generated in summarizeSGP are for all possible combinations of the 5 types of group. See documentation for \code{\link{summarizeSGP}} and \code{\link{viewSummaryGroups}} for more detail. 
  }
  \item{confidence.interval.groups}{A subset of the groups provided in the \code{summary.groups} argument indicating which groups to provide confidence intervals for. See documentation for \code{\link{summarizeSGP}} and \code{\link{viewSummaryGroups}} for more detail.
  }
}

\value{Function returns a list containing the input long data set in the \code{Student} slot as a data.table keyed using \code{VALID_CASE}, \code{CONTENT_AREA}, 
\code{YEAR}, \code{ID}, SGP results including student growth percentile and student growth projections/trajectories in the SGP slot, and summary results in the 
Summary slot. 
}

\author{Damian W. Betebenner \email{dbetebenner@nciea.org}}

\seealso{\code{\link{sgpData_LONG}}, \code{\link{prepareSGP}}, \code{\link{analyzeSGP}}, \code{\link{combineSGP}}, \code{\link{summarizeSGP}},  \cr
\code{\link{studentGrowthPercentiles}}, and \code{\link{studentGrowthProjections}}}

\examples{
\dontrun{
DEMO_Data <- abcSGP(sgpData_LONG)
}  
}

\keyword{documentation}
