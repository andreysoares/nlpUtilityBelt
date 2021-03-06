% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/biogrid.R
\name{getLatestBiogridData}
\alias{getLatestBiogridData}
\title{Get latest version of BioGRID}
\usage{
getLatestBiogridData()
}
\value{
A data frame with the latest BioGRID content
}
\description{
\code{getLatestBiogridData} downloads the latest BioGRID file
(BIOGRID-ALL-LATEST.tab2.zip) available at \url{https://thebiogrid.org/download.php}
}
\details{
This function requires the package "data.table".
}
\examples{
## Downloading the latest BioGRID data
biogrid <- getLatestBiogridData()
}
