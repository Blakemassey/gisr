% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gis.R
\name{PlotRasterAsMatrix}
\alias{PlotRasterAsMatrix}
\title{Plot a raster as a matrix}
\usage{
PlotRasterAsMatrix(
  raster_in,
  title = NULL,
  legend = "Value",
  all_coords = TRUE
)
}
\arguments{
\item{raster_in, }{RasterLayer}

\item{title}{character, title of plt}

\item{legend}{character, legend title}

\item{all_coords}{logical, whether to show all coordinates on x/y axes}
}
\value{
ggplot
}
\description{
Plot a raster as a matrix
}
