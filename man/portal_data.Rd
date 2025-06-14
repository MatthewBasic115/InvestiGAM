% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_doc.R
\docType{data}
\name{portal_data}
\alias{portal_data}
\title{Rodent Captures from the Portal Project provided by mvgam package (Ernest et. al., 2018; Clark, 2024)}
\format{
A data frame with columns:
\describe{
\item{time}{time of sampling, in lunar monthly cycles}
\item{series}{factor indicator of the time series, i.e. the species}
\item{captures}{total captures across all control plots}
\item{mintemp}{monthly mean minimum temperature}
\item{ndvi_ma12}{12-month moving average of the mean Normalised Difference Vegetation Index}
}
}
\source{
{mvgam} https://github.com/weecology/PortalData/blob/main/SiteandMethods/Methods.md
}
\description{
A dataset containing timeseries of total captures (across all control plots) for select rodent species from the Portal Project
}
