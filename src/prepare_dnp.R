start <- Sys.time()

source(file = "R/helpers.R")

log_debug(
  "This script prepares DNP referenced structure-organism pairs \n",
  "for further processing. \n"
)
log_debug("Authors: AR")
log_debug("Contributors: ...")

log_debug("Loading packages")
library(dplyr)
library(docopt)
library(purrr)
library(readr)
library(yaml)

step <- "prepare_dnp"
paths <- parse_yaml_paths()
params <- get_params(step = step)

if (file.exists(params$input)) {
  log_debug(x = "Loading DNP")
  dnp <-
    readr::read_delim(file = params$input)

  log_debug(x = "Formatting DNP")
  dnp_prepared <- dnp |>
    dplyr::mutate(structure_inchikey_2D = substring(
      text = structure_inchikey,
      first = 1,
      last = 14
    )) |>
    dplyr::select(
      # structure_name,
      structure_inchikey_2D,
      structure_smiles_2D,
      structure_molecular_formula,
      structure_exact_mass,
      # structure_xlogp,
      structure_taxonomy_npclassifier_01pathway,
      structure_taxonomy_npclassifier_02superclass,
      structure_taxonomy_npclassifier_03class,
      organism_name,
      organism_taxonomy_01domain,
      organism_taxonomy_02kingdom,
      organism_taxonomy_03phylum,
      organism_taxonomy_04class,
      organism_taxonomy_05order,
      organism_taxonomy_06family,
      organism_taxonomy_07tribe,
      organism_taxonomy_08genus,
      organism_taxonomy_09species,
      organism_taxonomy_10varietas,
      # reference_title
    ) |>
    dplyr::distinct() |>
    dplyr::mutate(reference_doi = NA)

  log_debug(x = "Exporting ...")
  ifelse(
    test = !dir.exists(paths$data$interim$path),
    yes = dir.create(paths$data$interim$path),
    no = paste(paths$data$interim$path, "exists")
  )

  log_debug(
    x = "... path to export is",
    params$output
  )
  readr::write_delim(
    x = dnp_prepared,
    file = params$output
  )

  export_params(
    parameters = params,
    directory = paths$data$interim$config$path,
    step = step
  )
} else {
  log_debug("Sorry, you do not have access to the DNP")
}

end <- Sys.time()

log_debug("Script finished in", format(end - start))
