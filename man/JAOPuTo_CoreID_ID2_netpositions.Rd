% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CoreID_ID2_netpositions.R
\name{JAOPuTo_CoreID_ID2_netpositions}
\alias{JAOPuTo_CoreID_ID2_netpositions}
\title{Core - ID2 - Net Positions}
\usage{
JAOPuTo_CoreID_ID2_netpositions(StartDateTime, EndDateTime)
}
\arguments{
\item{StartDateTime}{POSIXct-class variable containing the start datetime for data download}

\item{EndDateTime}{POSIXct-class variable containing the end datetime for data download}
}
\value{
a dataframe containing the Core net positions for all Core bidding zones
}
\description{
Core - ID2 - Net Positions
}
\examples{
JAOPuTo_CoreID_ID2_netpositions(as.POSIXct("2024-07-01 00:00", "Europe/Brussels"),
as.POSIXct("2024-07-01 23:00", "Europe/Brussels"))
}
