% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pricespread.R
\name{JAOPuTo_pricespreads}
\alias{JAOPuTo_pricespreads}
\title{Price spreads}
\usage{
JAOPuTo_pricespreads(StartDateTime, EndDateTime)
}
\arguments{
\item{StartDateTime}{POSIXct-class variable containing the start datetime for data download}

\item{EndDateTime}{POSIXct-class variable containing the end datetime for data download}
}
\value{
a dataframe containing the price spreads between Core bidding zones
}
\description{
Price spreads
}
\examples{
df <- JAOPuTo_pricespreads(as.POSIXct("2022-06-09 00:00", "CET"), as.POSIXct("2022-12-31 23:00", "CET"))
}
