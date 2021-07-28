start <- Sys.time()

source(file = "R/helpers.R")
source(file = "R/get_gnps.R")

log_debug("This script fills features metadata (mz, rt and component id)")
log_debug("Authors: AR")
log_debug("Contributors: ...")

log_debug("Loading packages")
library(dplyr)
library(docopt)
library(purrr)
library(readr)
library(yaml)

step <- "prepare_features_components"
paths <- parse_yaml_paths()
params <- get_params(step = step)

log_debug(x = "Loading files ...")
log_debug(x = "... features table")
table <- readr::read_delim(file = params$input)

log_debug(x = "... cluster table")
log_debug(x = "THIS STEP CAN BE IMPROVED BY CALCULATING THE CLUSTERS WITHIN SPEC2VEC")
## TODO
components <-
  read_clusters(id = params$gnps) |>
  dplyr::select(
    feature_id = `cluster index`,
    component_id = componentindex,
    rt = RTMean,
    mz = `precursor mass`
  ) |>
  dplyr::distinct()

log_debug(x = "Adding components to features")
table_filled <-
  dplyr::left_join(components, table) |>
  dplyr::distinct() |>
  dplyr::arrange(desc(score_input)) |>
  dplyr::arrange(as.numeric(feature_id)) |>
  dplyr::select(
    feature_id,
    component_id,
    rt,
    mz,
    inchikey_2D,
    smiles_2D,
    molecular_formula,
    structure_exact_mass,
    score_input,
    library,
    structure_taxonomy_npclassifier_01pathway,
    structure_taxonomy_npclassifier_02superclass,
    structure_taxonomy_npclassifier_03class
  )

log_debug(x = "Calculating mz error")
## TODO can be improved
if (params$mode == "pos") {
  table_filled <- table_filled |>
    dplyr::mutate(mz_error = mz - 1.007276 - structure_exact_mass)
} else {
  table_filled <- table_filled |>
    dplyr::mutate(mz_error = mz + 1.007276 - structure_exact_mass)
}

log_debug(x = "Exporting ...")
ifelse(
  test = !dir.exists(paths$data$interim$path),
  yes = dir.create(paths$data$interim$path),
  no = paste(paths$data$interim$path, "exists")
)
ifelse(
  test = !dir.exists(paths$data$interim$annotations$path),
  yes = dir.create(paths$data$interim$annotations$path),
  no = paste(paths$data$interim$annotations$path, "exists")
)
ifelse(
  test = !dir.exists(paths$data$interim$config$path),
  yes = dir.create(paths$data$interim$config$path),
  no = paste(paths$data$interim$config$path, "exists")
)

log_debug(
  x = "... path to export is",
  params$output
)
readr::write_delim(
  x = table_filled,
  file = params$output
)

export_params(
  parameters = params,
  directory = paths$data$interim$config$path,
  step = step
)

end <- Sys.time()

log_debug("Script finished in", format(end - start))
