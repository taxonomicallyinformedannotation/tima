% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_components.R
\name{create_components}
\alias{create_components}
\title{Create components}
\usage{
create_components(
  input = get_params(step = "create_components")$files$networks$spectral$edges$prepared,
  output = get_params(step = "create_components")$files$networks$spectral$components$raw
)
}
\arguments{
\item{input}{Input file(s) containing edges}

\item{output}{Output file.}
}
\value{
The path to the created components
}
\description{
This function create components from edges
}
\examples{
\dontrun{
copy_backbone()
go_to_cache()
github <- "https://raw.githubusercontent.com/"
repo <- "taxonomicallyinformedannotation/tima-example-files/main/"
data_interim <- "data/interim/"
dir <- paste0(github, repo)
dir <- paste0(dir, data_interim)
get_file(
  url = paste0(dir, "features/example_edges.tsv"),
  export = get_params(step = "create_components")$files$networks$spectral$edges$prepared
)
create_components()
unlink("data", recursive = TRUE)
}
}
