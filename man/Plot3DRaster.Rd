% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gis.R
\name{Plot3DRaster}
\alias{Plot3DRaster}
\title{Wrapper function for hist3D() and plotrgl() that plots an rgl plot from a
  Raster layer}
\usage{
Plot3DRaster(raster, azimuth, colaltitude, col, border, x_lab, y_lab,
  z_lab, z_lim, main, legend_lab, rgl, rgl_window, spin, movie, movie_name,
  ...)
}
\arguments{
\item{raster}{Raster layer to plot}

\item{azimuth}{azimuth angle, default is 45}

\item{coaltitude}{coaltitude angle, default is 30}

\item{col}{color palette. Default is plot3D::gg.col()}

\item{border}{color of the lines drawn around surface facets, default is
"black".}

\item{x_lab}{label for x-axis, default is "Longitude"}

\item{y_lab}{label for y-axis, default is "Latitude"}

\item{z_lab}{label for z-axis, default is ""}

\item{z_lim}{range of values for z axis, default is range(z)}

\item{main}{plot title, default is raster object name}

\item{legend_lab}{label for legend, default is z_lab}

\item{rgl}{logical, create an interactive rgl object, default is TRUE}

\item{rgl_window}{sets rgl window size, either "screen" or "image",
optimized for viewing on screen or as an 1024x768 pixel image,
respectively. Default is "screen".}

\item{spin}{logical, spin the plot 360 degrees once, default is TRUE}

\item{movie}{logical, create a .gif movie from the rgl plot. Default is
FALSE.}

\item{movie_name}{name of output gif movie. Default is saved in in working
directroy as "RasterSpin.gif"}

\item{...}{additional arguments for the hist3D() function}
}
\value{
Plot of Raster layer in RStudio, 3d plot in interactive rgl device
  (optional), and a .gif movie of the plot rotating 360 degrees (optional)
}
\description{
Wrapper function for hist3D() and plotrgl() that plots an rgl plot from a
  Raster layer
}
\details{
For additional arguments see ?persp3D. If a movie is made, a new
  rgl window will open set with the proper dimensions, record the movie,
  then automatically close. All NA values are converted to 0 because the
  hist3d() plots were not working with NA values included in the matrix.
}
