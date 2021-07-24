start <- Sys.time()

source(file = "R/helpers.R")
source(file = "R/get_gnps.R")

log_debug("This script fills features metadata (mz, rt and component id)")
log_debug("Authors: AR")
log_debug("Contributors: ...")

library(dplyr)
library(docopt)
library(purrr)
library(readr)
library(yaml)

paths <- parse_yaml_paths()

params <- get_params(step = "prepare_features_components")

log_debug(x = "loading files")

log_debug(x = "loading original annotation table")

table <- readr::read_delim(file = params$input)

log_debug(x = "... cluster table")
log_debug(x = "THIS STEP CAN BE IMPROVED BY CALCULATING THE CLUSTERS WITHIN SPEC2VEC")
## THIS STEP CAN BE IMPROVED BY CALCULATING THE CLUSTERS WITHIN SPEC2VEC
components <-
  read_clusterinfo(id = params$gnps) |>
  dplyr::select(
    feature_id = `cluster index`,
    component_id = componentindex,
    rt = RTMean,
    mz = `precursor mass`
  ) |>
  dplyr::distinct()

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

if (params$mode == "pos") {
  table_filled <- table_filled |>
    dplyr::mutate(mz_error = mz - 1.007276 - structure_exact_mass)
} else {
  table_filled <- table_filled |>
    dplyr::mutate(mz_error = mz + 1.007276 - structure_exact_mass)
}

log_debug(x = "exporting metadata_table_spectral_annotation and parameters used ...")
log_debug("ensuring directories exist ...")
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
  x = "... metadata_table_spectral_annotation is saved in",
  params$output
)

readr::write_delim(
  x = table_filled,
  file = params$output
)

log_debug(x = "... parameters used are saved in", paths$data$interim$config$path)
yaml::write_yaml(
  x = params,
  file = file.path(
    paths$data$interim$config$path,
    paste(
      format(Sys.time(), "%y%m%d_%H%M%OS"),
      "prepare_features_components.yaml",
      sep = "_"
    )
  )
)

end <- Sys.time()

log_debug("Script finished in", format(end - start))
