% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CoreID_ID3_scheduleexchanges.R
\name{JAOPuTo_CoreID_ID3_scheduledexchanges}
\alias{JAOPuTo_CoreID_ID3_scheduledexchanges}
\title{CoreID - ID3 - Scheduled exchanges}
\usage{
JAOPuTo_CoreID_ID3_scheduledexchanges(StartDateTime, EndDateTime)
}
\arguments{
\item{StartDateTime}{POSIXct-class variable containing the start datetime for data download}

\item{EndDateTime}{POSIXct-class variable containing the end datetime for data download}
}
\value{
a dataframe containing the scheduled bilateral exchanges between each combination of Core bidding zones
}
\description{
CoreID - ID3 - Scheduled exchanges
}
\examples{
JAOPuTo_CoreID_ID3_scheduledexchanges(as.POSIXct("2024-07-01 00:00", "Europe/Brussels"),
as.POSIXct("2024-07-01 23:00", "Europe/Brussels"))
}
