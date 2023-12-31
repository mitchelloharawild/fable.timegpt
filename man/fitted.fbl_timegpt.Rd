% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model.R
\name{fitted.fbl_timegpt}
\alias{fitted.fbl_timegpt}
\title{Obtain historical 1-step forecasts from TimeGPT Historic}
\usage{
\method{fitted}{fbl_timegpt}(object, token = Sys.getenv("TIMEGPT_TOKEN"), ...)
}
\arguments{
\item{object}{A model for which forecasts are required.}

\item{token}{The API token obtained from \url{https://dashboard.nixtla.io/}}

\item{...}{Other arguments passed to methods}
}
\value{
A vector of fitted values.
}
\description{
Based on the provided data, this endpoint predicts time series data for the
in-sample period (historical period). It takes a JSON as an input, including
information like the series’ frequency and the historical data.
}
