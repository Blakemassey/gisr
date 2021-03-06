% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gis.R
\name{PlotBWOptimRasters2}
\alias{PlotBWOptimRasters2}
\title{Plot 4 ggplots for optimization procedure}
\usage{
PlotBWOptimRasters2(
  covar_ras,
  covar_ras_smooth,
  prob_ras,
  pred_ras,
  in_intercept = in_intercept,
  in_beta1 = in_beta1,
  in_sigma = in_sigma,
  col_option = "D"
)
}
\arguments{
\item{covar_ras}{RasterLayer}

\item{covar_ras_smooth}{RasterLayer}

\item{prob_ras}{RasterLayer}

\item{pred_ras}{RasterLayer}

\item{in_intercept}{numeric, input value for intercept of Logistic}

\item{in_beta1}{numeric, input value for beta of Logisitic}

\item{in_sigma}{numeric, input value for sigma of Gaussian Kernel Smoothing}

\item{col_option}{color option for color viridis}
}
\value{
ggplot
}
\description{
Plot 4 ggplots for optimization procedure
}
