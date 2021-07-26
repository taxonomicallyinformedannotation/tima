start <- Sys.time()

source(file = "R/helpers.R")
source(file = "R/get_gnps.R")

log_debug(
  "This script treats GNPS and NAP results to obtain following file : \n
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

params <- get_params(step = "prepare_gnps")


log_debug("loading files ...")
## starting it now
## will finish later on, when decided if some values will be directly available in GNPS output
## see https://github.com/CCMS-UCSD/GNPS_Workflows/issues/747
table <- read_results(id = params$gnps) |>
  select(
    feature_id = `#Scan#`,
    smiles = Smiles,
    # smiles_2D, ## not available for now
    inchikey = InChIKey,
    inchikey_2D = `InChIKey-Planar`,
    structure_taxonomy_npclassifier_01pathway = npclassifier_pathway,
    structure_taxonomy_npclassifier_02superclass = npclassifier_superclass,
    structure_taxonomy_npclassifier_03class = npclassifier_class,
    # molecular_formula, ## not available for now
    structure_exact_mass = ExactMass,
    score_input = MQScore
  ) |>
  mutate(
    library = "GNPS",
    smiles_2D = NA,
    molecular_formula = NA
  )

if (!is.null(params$nap)) {
  ## look at recent NAP outputs
  ## might be outdated
  log_debug(x = "... annotations table")
  table <- read_nap(id = params$nap) |>
    dplyr::select(
      feature_id = cluster.index,
      score_input = FusionScore,
      smiles = FusionSMILES
    ) |>
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
  lapply(
    table,
    function(x) {
      y_as_na(x, y = "N/A")
    }
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
      "prepare_gnps.yaml",
      sep = "_"
    )
  )
)

end <- Sys.time()

log_debug("Script finished in", format(end - start))
