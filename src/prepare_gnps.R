start <- Sys.time()

source(file = "R/helpers.R")
source(file = "R/features.R")
source(file = "R/parse_cli_params.R")
source(file = "R/parse_yaml_paths.R")
source(file = "R/parse_yaml_params.R")

log_debug(
  "This script treats GNPS and NAP results to obtain following file : \n
    metadata_table_spectral_annotation"
)
log_debug("Authors: AR")
log_debug("Contributors: ...")

library(data.table)
library(dplyr)
library(docopt)
library(purrr)
library(splitstackshape)
library(yaml)

paths <- parse_yaml_paths()

step <- "prepare_gnps"
doc_path <- file.path(paths$src$docopt, paste0(step, ".txt"))
default_path <- file.path(paths$config$default$path, paste0(step, ".yaml"))
params_path <- file.path(paths$config$params$path, paste0(step, ".yaml"))

doc <- readChar(con = doc_path,
                nchars = file.info(doc_path)$size)

arguments <- docopt(doc)

params <- parse_yaml_params()

params <- parse_cli_params()

log_debug("loading files ...")
## starting it now
## will finish later on, when decided if some values will be directly available in GNPS output
## see https://github.com/CCMS-UCSD/GNPS_Workflows/issues/747
table <- read_library_hits(id = params$job$gnps) |>
  select(
    feature_id = X.Scan.,
    smiles = Smiles,
    # smiles_2D, ## not available for now
    inchikey = InChIKey,
    inchikey_2D = InChIKey.Planar,
    structure_taxonomy_npclassifier_01pathway = npclassifier_pathway,
    structure_taxonomy_npclassifier_02superclass = npclassifier_superclass,
    structure_taxonomy_npclassifier_03class = npclassifier_class,
    # molecular_formula, ## not available for now
    structure_exact_mass = ExactMass,
    score_input = MQScore
  ) |>
  mutate(library = "GNPS",
         smiles_2D = NA,
         molecular_formula = NA)

if (!is.null(params$job$nap)) {
  ## look at recent NAP outputs
  ## might be outdated
  log_debug(x = "... annotations table")
  table <- read_nap(id = params$job$nap) |>
    dplyr::select(feature_id = cluster.index,
                  score_input = FusionScore,
                  smiles = FusionSMILES) |>
    mutate(
      library = "GNPS",
      smiles_2D = NA,
      inchikey = NA,
      inchikey_2D = NA,
      molecular_formula = NA,
      structure_taxonomy_npclassifier_01pathway = NA,
      structure_taxonomy_npclassifier_02superclass = NA,
      structure_taxonomy_npclassifier_03class = NA,
      structure_exact_mass = NA
    )
}

table[] <-
  lapply(table,
         function(x) {
           y_as_na(x, y = "N/A")
         })

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
log_debug(x = "... metadata_table_spectral_annotation is saved in",
          params$file$output)
data.table::fwrite(x = table,
                   file = params$file$output,
                   sep = "\t")

log_debug(x = "... parameters used are saved in", data_processed_params_treat_gnps_nap)
yaml::write_yaml(x = params, file = data_processed_params_treat_gnps_nap)

end <- Sys.time()

log_debug("Script finished in", format(end - start))
