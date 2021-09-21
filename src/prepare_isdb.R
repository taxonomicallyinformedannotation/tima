start <- Sys.time()

source(file = "src/R/helpers.R")

log_debug(
  "This script treats ISDB results to obtain following file : \n
    metadata_table_spectral_annotation"
)
log_debug("Authors: AR")
log_debug("Contributors: ...")

log_debug("Loading packages")
library(package = dplyr, quietly = TRUE)
library(package = docopt, quietly = TRUE)
library(package = purrr, quietly = TRUE)
library(package = readr, quietly = TRUE)
library(package = yaml, quietly = TRUE)

step <- "prepare_isdb"
paths <- parse_yaml_paths()
params <- get_params(step = step)

log_debug(x = "Loading and formatting ISDB results")
table <- readr::read_delim(file = params$input) |>
  dplyr::distinct(
    feature_id,
    inchikey_2D = short_inchikey,
    smiles,
    molecular_formula,
    structure_exact_mass = exact_mass,
    score_input = msms_score
  ) |>
  dplyr::mutate(
    library = "ISDB",
    inchikey = NA,
    smiles_2D = smiles,
    structure_taxonomy_npclassifier_01pathway = NA,
    structure_taxonomy_npclassifier_02superclass = NA,
    structure_taxonomy_npclassifier_03class = NA,
  )

log_debug(x = "Exporting ...")
ifelse(
  test = !dir.exists(paths$data$path),
  yes = dir.create(paths$data$path),
  no = paste(paths$data$path, "exists")
)
ifelse(
  test = !dir.exists(paths$data$interim$path),
  yes = dir.create(paths$data$interim$path),
  no = paste(paths$data$interim$path, "exists")
)
ifelse(
  test = !dir.exists(paths$data$interim$config$path),
  yes = dir.create(paths$data$interim$config$path),
  no = paste(paths$data$interim$config$path, "exists")
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
  x = table,
  file = params$output,
  delim = "\t"
)

export_params(
  parameters = params,
  directory = paths$data$interim$config$path,
  step = step
)

end <- Sys.time()

log_debug("Script finished in", format(end - start))
