% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gps.R
\name{SummarizeOvernightDistances}
\alias{SummarizeOvernightDistances}
\title{Summarizes overnight distances}
\usage{
SummarizeOvernightDistances(df = df, individual = NULL, cuts = (0.05))
}
\arguments{
\item{df}{Dataframe}

\item{individual}{String, individual (from "id" column) to keep, optional.}

\item{cuts}{Numeric, proportional quantile cuts. Default is 0.05.}
}
\value{
Dataframe of quantiles
}
\description{
Summarizes overnight distances
}
\details{
Require RemoveNonsequentialDays() function
}
