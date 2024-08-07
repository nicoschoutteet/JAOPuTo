% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Core_ltn.R
\name{JAOPuTo_Core_longtermnominations}
\alias{JAOPuTo_Core_longtermnominations}
\title{Core - Long-Term Nominations}
\usage{
JAOPuTo_Core_longtermnominations(StartDateTime, EndDateTime)
}
\arguments{
\item{StartDateTime}{POSIXct-class variable containing the start datetime for data download}

\item{EndDateTime}{POSIXct-class variable containing the end datetime for data download}
}
\value{
a dataframe containing the long-term nominations on Core bidding zone borders
}
\description{
Core - Long-Term Nominations
}
\examples{
JAOPuTo_Core_longtermnominations(as.POSIXct("2024-01-01 00:00", "Europe/Brussels"),
as.POSIXct("2024-01-01 23:00", "Europe/Brussels"))
}
