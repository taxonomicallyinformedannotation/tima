start <- Sys.time()

source(file = "R/helpers.R")

log_debug(
  "This script treats ISDB results to obtain following file : \n
    metadata_table_spectral_annotation"
)
log_debug("Authors: AR")
log_debug("Contributors: ...")

library(data.table)
library(dplyr)
library(docopt)
library(purrr)
library(yaml)

paths <- parse_yaml_paths()

params <- get_params(step = "prepare_isdb")






log_debug(x = "loading original annotation table")

table <- data.table::fread(file = params$file$input) |>
  dplyr::distinct(feature_id,
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
  test = !dir.exists(data_interim),
  yes = dir.create(data_interim),
  no = paste(data_interim, "exists")
)
ifelse(
  test = !dir.exists(data_processed),
  yes = dir.create(data_processed),
  no = paste(data_processed, "exists")
)
ifelse(
  test = !dir.exists(data_processed_params),
  yes = dir.create(data_processed_params),
  no = paste(data_processed_params, "exists")
)

log_debug(
  x = "... metadata_table_spectral_annotation is saved in",
  params$file$output
)

fwrite(
  x = table,
  file = params$file$output,
  sep = "\t"
)

log_debug(x = "... parameters used are saved in", data_processed_params_treat_isdb)
write_yaml(x = params, file = data_processed_params_treat_isdb)

end <- Sys.time()

log_debug("Script finished in", format(end - start))
