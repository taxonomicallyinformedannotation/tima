start <- Sys.time()

source(file = "R/helpers.R")

log_debug(
  "This script treats ISDB results to obtain following file : \n
    metadata_table_spectral_annotation"
)
log_debug("Authors: AR")
log_debug("Contributors: ...")

library(dplyr)
library(docopt)
library(purrr)
library(readr)
library(yaml)

paths <- parse_yaml_paths()

params <- get_params(step = "prepare_isdb")

log_debug(x = "loading original annotation table")

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

log_debug(x = "exporting metadata_table_spectral_annotation and parameters used ...")
log_debug("ensuring directories exist ...")
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
  test = !dir.exists(paths$data$interim$edges$path),
  yes = dir.create(paths$data$interim$edges$path),
  no = paste(paths$data$interim$edges$path, "exists")
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
  x = table,
  file = params$output,
)

log_debug(x = "... parameters used are saved in", paths$data$interim$config$path)
yaml::write_yaml(
  x = params,
  file = file.path(
    paths$data$interim$config$path,
    paste(
      format(Sys.time(), "%y%m%d_%H%M%OS"),
      "prepare_isdb.yaml",
      sep = "_"
    )
  )
)

end <- Sys.time()

log_debug("Script finished in", format(end - start))
