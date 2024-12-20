% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Nordic_referencenetpositions.R
\name{JAOPuTo_Nordic_refnetpositions}
\alias{JAOPuTo_Nordic_refnetpositions}
\title{Nordic - Reference Net Positions}
\usage{
JAOPuTo_Nordic_refnetpositions(StartDateTime, EndDateTime)
}
\arguments{
\item{StartDateTime}{POSIXct-class variable containing the start datetime for data download}

\item{EndDateTime}{POSIXct-class variable containing the end datetime for data download}
}
\value{
a dataframe containing the reference net positions for non-Nordic bidding zones
}
\description{
Nordic - Reference Net Positions
}
\examples{
JAOPuTo_Nordic_refnetpositions(as.POSIXct("2024-10-30 00:00", "Europe/Brussels"),
as.POSIXct("2024-10-30 23:00", "Europe/Brussels"))
}
