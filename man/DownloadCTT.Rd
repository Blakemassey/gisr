% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gps.R
\name{DownloadCTT}
\alias{DownloadCTT}
\title{Download CTT data}
\usage{
DownloadCTT(units = "", download = "recent")
}
\arguments{
\item{units}{String, "deployed", "reserve", or "none".}

\item{download}{String, "all" or "recent". Default is "recent".}
}
\value{
Dataframe of downloaded files
}
\description{
Downloads data files from CTT's website
}
\examples{
This is ENTIRELY DEPENDENT on my Python scripts/locations.
}