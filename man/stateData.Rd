\name{stateData}
\alias{stateData}
\docType{data}
\title{State assessment data information currrently including only performance level cutscores for large scale state assessments}
\description{
A list containing state specific information on testing systems. Currently the list contains only performance level cutscores for the state assessments. 
These cutscores are currently used by the studentGrowthProjections function to calculate growth-to-standard values by supplying a state's two letter acronym 
to the argument \code{performance.level.cutscores}. In the future, this list will expand to include other relevant data about the state assessment system.
Currently, states included in the data set include Arizona (AZ), Colorado (CO), Connecticut (CT), Indiana (IN), Maine (ME), Nevada (NV), New Hampshire (NH),
Rhode Island (RI), Vermont (VT), Wisconsin (WI).  
}
\usage{data(stateData)}

\seealso{\code{\link{studentGrowthProjections}}}

\source{State assessment data and technical assessment documentation}
\keyword{datasets}
