% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/annotate_spectra.R
\name{annotate_spectra}
\alias{annotate_spectra}
\title{Annotate spectra}
\usage{
annotate_spectra(
  input = get_params(step = "annotate_spectra")$files$spectral$raw,
  libraries = get_params(step = "annotate_spectra")$files$libraries$spectral,
  polarity = get_params(step = "annotate_spectra")$ms$polarity,
  output = get_params(step = "annotate_spectra")$files$annotations$raw$spectral$spectral,
  method = get_params(step = "annotate_spectra")$similarities$methods$annotations,
  threshold = get_params(step = "annotate_spectra")$similarities$thresholds$annotations,
  ppm = get_params(step = "annotate_spectra")$ms$tolerances$mass$ppm$ms2,
  dalton = get_params(step = "annotate_spectra")$ms$tolerances$mass$dalton$ms2,
  qutoff = get_params(step = "annotate_spectra")$ms$thresholds$ms2$intensity,
  approx = get_params(step = "annotate_spectra")$annotations$ms2approx
)
}
\arguments{
\item{input}{Query file containing spectra. Currently an '.mgf' file}

\item{libraries}{Libraries containing spectra to match against.
Can be '.mgf' or '.sqlite' (Spectra formatted)}

\item{polarity}{MS polarity. Must be 'pos' or 'neg'.}

\item{output}{Output file.}

\item{method}{Similarity method}

\item{threshold}{Minimal similarity to report}

\item{ppm}{Relative ppm tolerance to be used}

\item{dalton}{Absolute Dalton tolerance to be used}

\item{qutoff}{Intensity under which ms2 fragments will be removed.}

\item{approx}{Perform matching without precursor match}
}
\description{
This function annotates spectra
}
\details{
It takes two files as input.
A query file that will be matched against a library file.
}
\examples{
\dontrun{
copy_backbone()
go_to_cache()
get_file(
  url = get_default_paths()$urls$examples$spectra_mini,
  export = get_params(step = "annotate_spectra")$files$spectral$raw
)
get_file(
  url = get_default_paths()$urls$examples$spectral_lib_mini$with_rt,
  export = get_default_paths()$data$source$libraries$spectra$exp$with_rt
)
annotate_spectra(
  libraries = get_default_paths()$data$source$libraries$spectra$exp$with_rt
)
unlink("data", recursive = TRUE)
}
}
