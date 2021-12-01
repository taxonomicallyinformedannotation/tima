start <- Sys.time()

require(package = "timaR",
        quietly = TRUE)

log_debug(
  "This script prepares closed referenced structure-organism pairs \n",
  "for further processing. \n"
)
log_debug("Authors: AR")
log_debug("Contributors: ...")

log_debug("Loading packages")
if (!require(docopt)) {
  install.packages("docopt")
  library(
    package = "docopt",
    quietly = TRUE
  )
}
if (!require(dplyr)) {
  install.packages("dplyr")
  library(
    package = "dplyr",
    quietly = TRUE,
    warn.conflicts = FALSE
  )
}
if (!require(purrr)) {
  install.packages("purrr")
  library(
    package = "purrr",
    quietly = TRUE
  )
}
if (!require(readr)) {
  install.packages("readr")
  library(
    package = "readr",
    quietly = TRUE
  )
}
if (!require(yaml)) {
  install.packages("yaml")
  library(
    package = "yaml",
    quietly = TRUE
  )
}

step <- "prepare_closed"
paths <- parse_yaml_paths()
params <- get_params(step = step)

if (file.exists(params$input)) {
  log_debug(x = "Loading closed resources")
  closed <-
    readr::read_delim(file = params$input)

  log_debug(x = "Formatting closed resource")
  closed_prepared <- closed |>
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

  ifelse(
    test = !dir.exists(dirname(params$output)),
    yes = dir.create(dirname(params$output)),
    no = paste(dirname(params$output), "exists")
  )

  log_debug(
    x = "... path to export is",
    params$output
  )
  readr::write_delim(
    x = closed_prepared,
    file = params$output,
    delim = "\t"
  )

  export_params(
    parameters = params,
    directory = paths$data$interim$config$path,
    step = step
  )
} else {
  log_debug("Sorry, you do not have access to the closed resource")
}

end <- Sys.time()

log_debug("Script finished in", format(end - start))
