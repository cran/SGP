\name{summarizeSGP}
\alias{summarizeSGP}
\title{Summarize student scale scores, proficiency levels and student growth percentiles according to user specified summary group variables}
\description{
 Utility function used to produce summary tables using long formatted data that contain student growth percentiles.  An exemplar is provided from the successive execution of \code{\link{prepareSGP}}, \code{\link{analyzeSGP}} and \code{\link{combineSGP}}.
}

\usage{
summarizeSGP(sgp_object,
	state,
	years,
	content_areas,
	sgp.summaries=list(MEDIAN_SGP="median_na(SGP)",
		MEDIAN_SGP_COUNT="num_non_missing(SGP)",
		PERCENT_AT_ABOVE_PROFICIENT="percent_in_category(ACHIEVEMENT_LEVEL, list(c('Proficient', 'Advanced')), list(c('Unsatisfactory', 'Partially Proficient', 'Proficient', 'Advanced')))",
		PERCENT_AT_ABOVE_PROFICIENT_COUNT="num_non_missing(ACHIEVEMENT_LEVEL)"),
	summary.groups=list(institution=c("STATE", "DISTRICT_NUMBER", "SCHOOL_NUMBER"),
		content="CONTENT_AREA",
		time="YEAR",
		institution_level="GRADE",
		demographic=c("GENDER", "ETHNICITY", "FREE_REDUCED_LUNCH_STATUS", "ELL_STATUS", "IEP_STATUS", "GIFTED_AND_TALENTED_PROGRAM_STATUS", "CATCH_UP_KEEP_UP_STATUS"),
		institution_inclusion=list(STATE="STATE_ENROLLMENT_STATUS", DISTRICT_NUMBER="DISTRICT_ENROLLMENT_STATUS", SCHOOL_NUMBER="SCHOOL_ENROLLMENT_STATUS")),
	confidence.interval.groups=list(institution="SCHOOL_NUMBER", 
		content="CONTENT_AREA",
		time="YEAR",
		institution_level= NULL,
		demographic=NULL,
		institution_inclusion=list(STATE=NULL, DISTRICT_NUMBER=NULL, SCHOOL_NUMBER="SCHOOL_ENROLLMENT_STATUS")))
}

\arguments{
  \item{sgp_object}{A list containing long formatted data in the \code{Student} slot. If summaries of student growth percentiles are requested, those quantities must first be produced (possibly by first using \code{\link{analyzeSGP}}) and subsequently combined with the \code{Student} data (possibly with \code{\link{combineSGP}}).
  }
  \item{state}{Acronym indicating state associated with the summaries for access to assessment program information embedded in \code{\link{stateData}}.
  }
  \item{years}{A vector indicating years in which to produce student growth percentiles and/or student growth projections/trajectories. If left missing the 
function will use the data to infer the years based upon the assumption of having at least three years of panel data for analyses.
  }
  \item{content_areas}{A vector indicating content areas in which to produce student growth percentiles and/or student growth projections/trajectories. 
If left missing the function will use the data to infer the content areas available for analyses.
  }
  \item{sgp.summaries}{A list giving the summaries requested for each group analyzed based upon the \code{summary.group} argument. The default summaries include the group level \code{MEDIAN_SGP}, \code{MEDIAN_SGP_COUNT} (the number of students used to compute the median), \code{PERCENT_AT_ABOVE_PROFICIENT} for the appropriate achievement levels and \code{PERCENT_AT_ABOVE_PROFICIENT_COUNT}. NOTE: The \code{percent_in_category()} summary function requires a variable that MUST be a factor with proficiency categories as levels. The function looks for the percent of students from the variable that are IN one or more categories that identify students that are at or above proficiency (the first list argument) OUT of all possible categories (the second list argument). For example, in the \code{\link{sgpData_LONG}} dataset \code{ACHIEVEMENT_LEVEL} has 5 levels.  Levels 3 and 4 are "Proficient" and "Advanced" respectively. The default summary looks for the percent of students in categories 3 and 4 out of the five possible categories (1:5).
  }
  \item{summary.groups}{A list consisting of 5 types of groups across which all summaries are taken, including the \code{institution} (e.g. state, districts and schools), \code{content area}, \code{time}, \code{institution_level} (usually grade), \code{demographic} group variables, and \code{institution_inclusion} indicator variables. All group slots MUST be included in the list, although \code{NULL} can be provided if a grouping subset is not desired. All possible combinations of the group variables are produced. Specification of \code{summary.groups} can be tested prior to use of \code{summarizeSGP} with the \code{\link{viewSummaryGroups}} function.  See that function's documentation for details.
  }
  \item{confidence.interval.groups}{A list consisting of the group summaries for which confidence intervals should be constructed. This argument requires that simulated SGPs have been produced (see \code{\link{analyzeSGP}} for more information). List slots set to \code{NULL} will not produce confidence intervals. NOTE: This is currently an experimental functionality and is very memory intensive. Groups to be included should be identified selectively! The default 95\% confidence intervals are provided in the selected summary tables as two additional columns named \code{LOWER_MEDIAN_SGP_95_CONF_BOUND} and \code{UPPER_MEDIAN_SGP_95_CONF_BOUND}. Specification of \cr
   \code{confidence.interval.groups} can be tested prior to use of \code{summarizeSGP} with the \code{\link{viewSummaryGroups}} function.  See that documentation for details.
  }
}

\details{Function makes use of the \code{foreach} package to parallel process summary tables of student data. It is the user's responsibility to register a parallel backend of their choice before running the \code{summarizeSGP} function. The proper choice may be dependent upon the user's operating system, software and system memory capacity. Please see the \code{foreach} documentation for details. If no parallel backend is specified, the function will process the summary tables sequentially.
}

\value{Function returns lists containing the summary tables as data.table objects in the \code{Summary} slot of the SGP data object. Each \code{institution} has a slot in the \code{Summary} list. 
}

\author{Damian W. Betebenner \email{dbetebenner@nciea.org}}

\seealso{\code{\link{prepareSGP}}, \code{\link{analyzeSGP}}, \code{\link{combineSGP}}}

\examples{
\dontrun{
## summarizeSGP is Step 4 of 5 of abcSGP
DEMO_Data <- summarizeSGP(DEMO_Data, state="DEMO")
}  
}

\keyword{documentation}
