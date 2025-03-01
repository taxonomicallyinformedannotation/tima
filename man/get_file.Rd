% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_file.R
\name{get_file}
\alias{get_file}
\title{Get file}
\usage{
get_file(url, export, limit = 3600)
}
\arguments{
\item{url}{URL of the file to be downloaded}

\item{export}{File path where the file should be saved}

\item{limit}{Timeout limit (in seconds)}
}
\value{
The path to the file
}
\description{
This function get files
}
\examples{
\dontrun{
git <- "https://github.com/"
org <- "taxonomicallyinformedannotation"
repo <- "tima-example-files"
branch <- "main"
file <- "example_metadata.tsv"
get_file(
  url = paste(git, org, repo, "raw", branch, file, sep = "/"),
  export = "data/source/example_metadata.tsv"
)
}
}
