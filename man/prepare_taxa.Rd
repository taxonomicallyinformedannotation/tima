% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prepare_taxa.R
\name{prepare_taxa}
\alias{prepare_taxa}
\title{Prepare taxa}
\usage{
prepare_taxa(
  input = get_params(step = "prepare_taxa")$files$features$prepared,
  extension = get_params(step = "prepare_taxa")$names$extension,
  name_filename = get_params(step = "prepare_taxa")$names$filename,
  colname = get_params(step = "prepare_taxa")$names$taxon,
  metadata = get_params(step = "prepare_taxa")$files$metadata$raw,
  org_tax_ott = get_params(step =
    "prepare_taxa")$files$libraries$sop$merged$organisms$taxonomies$ott,
  output = get_params(step = "prepare_taxa")$files$metadata$prepared,
  taxon = get_params(step = "prepare_taxa")$organisms$taxon
)
}
\arguments{
\item{input}{File containing your features intensities}

\item{extension}{Does your column names contain the file extension?
(mzmine mainly)}

\item{name_filename}{Name of the file name column in the metadata file}

\item{colname}{Name of the column containing biological source information}

\item{metadata}{File containing your metadata including biological source}

\item{org_tax_ott}{File containing Open Tree of Life Taxonomy}

\item{output}{Output file}

\item{taxon}{If you want to enforce all features to a given taxon,
put its name here.}
}
\value{
The path to the prepared taxa
}
\description{
This function performs taxon name preparation
to match the Open Tree of Life taxonomy
}
\details{
Depending if the features are aligned between samples
originating from various organisms or not,
It can either attribute all features to a single organism,
or attribute them to multiple ones,
according to their relative intensities among the samples.
}
\examples{
\dontrun{
copy_backbone()
go_to_cache()
github <- "https://raw.githubusercontent.com/"
repo <- "taxonomicallyinformedannotation/tima-example-files/main/"
dir <- paste0(github, repo)
org_tax_ott <- paste0(
  "data/interim/libraries/",
  "sop/merged/organisms/taxonomies/ott.tsv"
)
get_file(url = paste0(dir, org_tax_ott), export = org_tax_ott)
get_file(
  url = paste0(dir, "data/interim/features/example_features.tsv"),
  export = get_params(step = "prepare_taxa")$files$features$prepared
)
prepare_taxa(
  taxon = "Homo sapiens",
  org_tax_ott = org_tax_ott
)
unlink("data", recursive = TRUE)
}
}
