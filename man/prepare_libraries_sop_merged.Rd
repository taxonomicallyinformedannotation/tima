% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prepare_libraries_sop_merged.R
\name{prepare_libraries_sop_merged}
\alias{prepare_libraries_sop_merged}
\title{Prepare merged structure organism pairs libraries}
\usage{
prepare_libraries_sop_merged(
  files = get_params(step = "prepare_libraries_sop_merged")$files$libraries$sop$prepared,
  filter = get_params(step = "prepare_libraries_sop_merged")$organisms$filter$mode,
  level = get_params(step = "prepare_libraries_sop_merged")$organisms$filter$level,
  value = get_params(step = "prepare_libraries_sop_merged")$organisms$filter$value,
  cache = get_params(step =
    "prepare_libraries_sop_merged")$files$libraries$sop$merged$structures$processed,
  output_key = get_params(step =
    "prepare_libraries_sop_merged")$files$libraries$sop$merged$keys,
  output_org_tax_ott = get_params(step =
    "prepare_libraries_sop_merged")$files$libraries$sop$merged$organisms$taxonomies$ott,
  output_str_stereo = get_params(step =
    "prepare_libraries_sop_merged")$files$libraries$sop$merged$structures$stereo,
  output_str_met = get_params(step =
    "prepare_libraries_sop_merged")$files$libraries$sop$merged$structures$metadata,
  output_str_nam = get_params(step =
    "prepare_libraries_sop_merged")$files$libraries$sop$merged$structures$names,
  output_str_tax_cla = get_params(step =
    "prepare_libraries_sop_merged")$files$libraries$sop$merged$structures$taxonomies$cla,
  output_str_tax_npc = get_params(step =
    "prepare_libraries_sop_merged")$files$libraries$sop$merged$structures$taxonomies$npc
)
}
\arguments{
\item{files}{List of libraries to be merged}

\item{filter}{Boolean. TRUE or FALSE if you want to filter the library}

\item{level}{Biological rank to be filtered.
Kingdom, phylum, family, genus, ...}

\item{value}{Name of the taxon or taxa to be kept,
e.g. 'Gentianaceae|Apocynaceae'}

\item{cache}{Cache where already processed SMILES are located}

\item{output_key}{Output file for keys}

\item{output_org_tax_ott}{Output file for organisms taxonomy (OTT)}

\item{output_str_stereo}{Output file for structures stereo}

\item{output_str_met}{Output file for structures metadata}

\item{output_str_nam}{Output file for structures names}

\item{output_str_tax_cla}{Output file for structures taxonomy (Classyfire)}

\item{output_str_tax_npc}{Output file for structures taxonomy (NPC)}
}
\value{
The path to the prepared structure-organism pairs library MERGED
}
\description{
This function prepares the libraries made of
all sub-libraries containing structure-organism pairs
}
\details{
It can be restricted to specific taxa to have
more biologically meaningful annotation.
}
\examples{
\dontrun{
copy_backbone()
go_to_cache()
github <- "https://raw.githubusercontent.com/"
repo <- "taxonomicallyinformedannotation/tima-example-files/main/"
dir <- paste0(github, repo)
files <- get_params(step = "prepare_libraries_sop_merged")$files$libraries$sop$prepared$lotus |>
  gsub(
    pattern = ".gz",
    replacement = "",
    fixed = TRUE
  )
get_file(url = paste0(dir, files), export = files)
prepare_libraries_sop_merged(files = files)
unlink("data", recursive = TRUE)
}
}
