\name{visualizeSGP}
\alias{visualizeSGP}
\title{Visualize data from SGP analyses}
\description{
Utility function to produce a variety of graphical displays associated with student growth percentile/percentile growth trajectory results. Function currently includes facility to produce
individual student growth and achievement reports as well and interactive bubble charts showing growth and achievement data.
}

\usage{
visualizeSGP(sgp_object,
        state,
        years,
        content_areas,
        districts,
        schools,
        grades,
        plot.types=c("bubblePlot", "studentGrowthPlot", "growthAchievementPlot"),
        bubblePlot.config=list(summary.data=sgp_object@Summary[["SCHOOL_NUMBER"]][["SCHOOL_NUMBER__CONTENT_AREA__YEAR__SCHOOL_ENROLLMENT_STATUS"]][
		SCHOOL_ENROLLMENT_STATUS=="Enrolled Full Academic Year in School"],
                use.school.names=TRUE,
                x.variable="MEDIAN_SGP", 
                y.variable=list(CURRENT="PERCENT_AT_ABOVE_PROFICIENT", PRIOR="PERCENT_AT_ABOVE_PROFICIENT_PRIOR"), 
                size.variable="MEDIAN_SGP_COUNT",
                subset.factor=NULL,
                main.title=list(CURRENT="Growth and Current Achievement", PRIOR="Growth and Prior Achievement"),
                sub1.title="Demonstration State School Performance",
                plot.extras=c("grid.text(x=unit(50, 'native'), y=unit(50, 'native'), 
			'DRAFT - DO NOT DISTRIBUTE', rot=-30, gp=gpar(col='grey80', cex=2.9, alpha=0.8, fontface=2))", 
			"grid.lines(x=unit(50, 'native'), y=c(0,1), gp=gpar(col='grey40', lwd=1.5, lty=2, alpha=0.5))"),
                pdf.folder="Visualizations/Summary"),
        studentGrowthPlot.config=list(
		header.footer.color="goldenrod3",
                front.page=NULL,
                pdf.folder="Visualizations/Individual",
                folder.names="number",
                student.growth.projection.fan=TRUE,
                anonymize=FALSE,
                remove.auxillary.files=TRUE)) 
}

\arguments{
  \item{sgp_object}{A list containing long formatted data in the \code{Student} slot that will be used for the production of student growth and achievement plots, summary data from \code{\link{summarizeSGP}} in the \code{Summary} for bubble plots.  
  }
  \item{state}{Acronym indicating state associated with the summaries for access to assessment program information embedded in \code{\link{stateData}}.
  }
  \item{years}{A vector indicating year(s) in which to produce charts associated data available in sgp_object. If missing the function will use the last year available in the data to produce plots.
  }
  \item{content_areas}{NOTE: This argument does not apply to student growth plot production. A vector indicating content area(s) to produce summary plots (e.g., bubble plots).  If missing the function will produce plots for all available content areas provided in the data.
  }
  \item{districts}{A vector indicating districts for which to produce student growth plots or bubble plots. If missing the function will produce plots for all available districts in the data.
  }
  \item{schools}{NOTE: This argument does not apply to bubble plot production. A vector indicating schools for which to produce student growth plots. If missing the function will produce student growth plots for all available schools in the data.
  }
  \item{grades}{NOTE: This argument does not apply to bubble plot production. A vector indicating grades for which to produce student growth plots. If missing the function will produce student growth plots for all available grades in the data.
  }
  \item{plot.types}{A vector indicating which plot types to plot. Currently implemented plots include studentGrowthPlot and bubblePlot. Default is to produce all plot types.
  }
  \item{bubblePlot.config}{A list giving configuration arguments to be supplied to the bubblePlot function. Current arguments include:
\tabular{ll}{
\code{summary.data} \tab zzz.\cr
\code{use.school.names} \tab zzz.\cr
\code{y.variable} \tab zzz.\cr
\code{x.variable} \tab zzz.\cr
\code{size.variable} \tab zzz.\cr
\code{subset.factor} \tab zzz.\cr
\code{main.title} \tab zzz.\cr 
\code{sub1.title} \tab zzz.\cr 
\code{plot.extras} \tab zzz.\cr 
\code{pdf.folder} \tab zzz.\cr
}
  }
  \item{studentGrowthPlot.config}{A list giving configuration arguments to be supplied to the studentGrowthPlot function. Current arguments include:
\tabular{ll}{
\code{header.footer.color}: \tab A color indicating the color of the header and footer in the student growth and achievement report.\cr
\code{front.page}: \tab A character vector giving the file name associated with the front page.\cr
\code{pdf.folder}: \tab A character vector giving the path to the folder to contain the student growth charts.\cr 
\code{folder.names}: \tab Either "number" or "names" indicating how the directory path folders are named.\cr
\code{student.growth.projection.fan}: \tab A boolean argument defaulting to true indicating whether to produce the 1 year projection fans.\cr
\code{anonymize}: \tab A boolean argument defaulting to FALSE indicating whether to produce dummy names for students growth and achievement charts.\cr 
\code{remove.auxillary.files}: \tab A boolean argument defaulting to TRUE indicating whether to remove files created by LaTeX in the construction of student growth and achievement reports.\cr
}
  }
}

\value{Function produces numerous pdf files in a folder structure specified by the user and supplied through arguments to the function.
}

\author{Damian W. Betebenner \email{dbetebenner@nciea.org} and Adam Van Iwaarden \email{Vaniwaarden@colorado.edu}}

\seealso{\code{\link{sgpData_LONG}}}

\examples{
\dontrun{
## visualizeSGP is Step 5 of 5 of abcSGP
visualizeSGP(DEMO_Data)


## Production of sample student growth and achievement plots

visualizeSGP(sgp_object=Demo_Data, 
	state="DEMO",
	districts=470,
	schools=c(6418, 8008),
	plot.types="studentGrowthPlot",
        studentGrowthPlot.config=list(
		header.footer.color="#4CB9CC",   ### Other good colors goldenrod3
                front.page=NULL,
                pdf.folder="Visualizations/Individual_Visualizations",
                folder.names="number",
                student.growth.projection.fan=TRUE,
                anonymize=FALSE,
                remove.auxillary.files=TRUE))

}  
}

\keyword{documentation}
