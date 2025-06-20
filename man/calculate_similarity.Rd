% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculate_similarity.R
\name{calculate_similarity}
\alias{calculate_similarity}
\title{Calculate similarity between spectra}
\usage{
calculate_similarity(
  method,
  query_spectrum,
  target_spectrum,
  query_precursor,
  target_precursor,
  dalton,
  ppm,
  return_matched_peaks = FALSE,
  ...
)
}
\arguments{
\item{method}{Method ("entropy" or "gnps")}

\item{query_spectrum}{Query spectrum matrix}

\item{target_spectrum}{Target spectrum matrix}

\item{query_precursor}{Query precursor}

\item{target_precursor}{Target precursor}

\item{dalton}{Dalton tolerance}

\item{ppm}{PPM tolerance}

\item{return_matched_peaks}{Return matched peaks. Not compatible with 'entropy'. Default: FALSE}

\item{...}{Not documented for now}
}
\value{
Similarity score or NA_real_ if calculation fails
}
\description{
Efficiently calculates similarity scores between query and target spectra
using either entropy or GNPS methods
}
\examples{
NULL
}
