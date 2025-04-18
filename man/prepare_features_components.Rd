% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prepare_features_components.R
\name{prepare_features_components}
\alias{prepare_features_components}
\title{Prepare features components}
\usage{
prepare_features_components(
  input = get_params(step =
    "prepare_features_components")$files$networks$spectral$components$raw,
  output = get_params(step =
    "prepare_features_components")$files$networks$spectral$components$prepared
)
}
\arguments{
\item{input}{Input file}

\item{output}{Output file}
}
\value{
The path to the prepared features' components
}
\description{
This function prepares the components
(clusters in molecular network) for further use
}
\examples{
\dontrun{
copy_backbone()
go_to_cache()
github <- "https://raw.githubusercontent.com/"
repo <- "taxonomicallyinformedannotation/tima-example-files/main/"
dir <- paste0(github, repo)
input <- get_params(step = "prepare_features_components")$files$networks$spectral$components$raw
get_file(url = paste0(dir, input), export = input)
prepare_features_components(
  input = input
)
unlink("data", recursive = TRUE)
}
}
