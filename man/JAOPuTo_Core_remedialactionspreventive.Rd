% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Core_remedialactionspreventive.R
\name{JAOPuTo_Core_remedialactionspreventive}
\alias{JAOPuTo_Core_remedialactionspreventive}
\title{Core - Remedial Actions (Preventive)}
\usage{
JAOPuTo_Core_remedialactionspreventive(StartDateTime, EndDateTime)
}
\arguments{
\item{StartDateTime}{POSIXct-class variable containing the start datetime for data download}

\item{EndDateTime}{POSIXct-class variable containing the end datetime for data download}
}
\value{
a dataframe containing the preventive remedial actions applied by Core TSOs
}
\description{
Core - Remedial Actions (Preventive)
}
\examples{
JAOPuTo_Core_remedialactionspreventive(as.POSIXct("2024-01-01 00:00", "Europe/Brussels"),
as.POSIXct("2024-01-01 23:00", "Europe/Brussels"))
}
