start <- Sys.time()

source(file = "R/helpers.R")
source(file = "R/get_gnps.R")

log_debug("This script treats GNPS (and NAP) results")
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

step <- "prepare_gnps"
paths <- parse_yaml_paths()
params <- get_params(step = step)

log_debug("Loading and formatting GNPS results")
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
  log_debug("Loading NAP results")
  ## TODO look at recent NAP outputs
  ## might be outdated
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
