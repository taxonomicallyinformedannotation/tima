% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prepare_features_edges.R
\name{prepare_features_edges}
\alias{prepare_features_edges}
\title{Prepare features edges}
\usage{
prepare_features_edges(
  input = get_params(step = "prepare_features_edges")$files$networks$spectral$edges$raw,
  output = get_params(step =
    "prepare_features_edges")$files$networks$spectral$edges$prepared,
  name_source = get_params(step = "prepare_features_edges")$names$source,
  name_target = get_params(step = "prepare_features_edges")$names$target
)
}
\arguments{
\item{input}{Input file if 'manual'}

\item{output}{Output file}

\item{name_source}{Name of the source features column}

\item{name_target}{Name of the target features column}
}
\value{
The path to the prepared edges
}
\description{
This function prepares edges for further use
}
\examples{
\dontrun{
copy_backbone()
go_to_cache()
github <- "https://raw.githubusercontent.com/"
repo <- "taxonomicallyinformedannotation/tima-example-files/main/"
dir <- paste0(github, repo)
input_1 <- get_params(step = "prepare_features_edges")$files$networks$spectral$edges$raw$ms1
input_2 <- get_params(step = "prepare_features_edges")$files$networks$spectral$edges$raw$spectral
get_file(url = paste0(dir, input_1), export = input_1)
get_file(url = paste0(dir, input_2), export = input_2)
prepare_features_edges(
  input = list("ms1" = input_1, "spectral" = input_2)
)
unlink("data", recursive = TRUE)
}
}
