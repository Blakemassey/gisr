% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/kml.R
\name{UpdateIndByYearKMLs}
\alias{UpdateIndByYearKMLs}
\title{Creates or updates .kml files of each individual by year}
\usage{
ExportKMLPolygon(df, update_year, update_gdrive)
}
\arguments{
\item{df}{Dataframe of location data.}

\item{update_year}{integer, year to update. Default is NULL, which means all
years are updated.}

\item{update_gdrive}{Logical, whether to update the files on the GDrive.
Default is TRUE.}
}
\value{
Creates .kml files in "Data/GPS" and copies them to
    "Google Drive/BAEA Project/Telemetry Data/Individuals" folders.
}
\description{
Creates and updates year KML files on local and GDrive folders.
}
