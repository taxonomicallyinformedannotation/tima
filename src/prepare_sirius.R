start <- Sys.time()

source(file = "src/R/helpers.R")

log_debug("This script treats SIRIUS results")
log_debug("Authors: AR")
log_debug("Contributors: ...")

log_debug("Loading packages")
library(package = dplyr, quietly = TRUE)
library(package = docopt, quietly = TRUE)
library(package = purrr, quietly = TRUE)
library(package = readr, quietly = TRUE)
library(package = yaml, quietly = TRUE)

step <- "prepare_sirius"
paths <- parse_yaml_paths()
params <- get_params(step = step)

log_debug("Loading and formatting SIRIUS results")
canopus <-
  readr::read_delim(file = file.path(
    params$directory,
    "canopus_summary.tsv"
  ))

canopus_adducts <-
  readr::read_delim(file = file.path(
    params$directory,
    "canopus_summary_adducts.tsv"
  ))

formula <-
  readr::read_delim(file = file.path(
    params$directory,
    "formula_identifications.tsv"
  ))

formula_adducts <-
  readr::read_delim(file = file.path(
    params$directory,
    "formula_identifications_adducts.tsv"
  ))

compound <-
  readr::read_delim(file = file.path(
    params$directory,
    "compound_identifications.tsv"
  ))

compound_adducts <-
  readr::read_delim(file = file.path(
    params$directory,
    "compound_identifications_adducts.tsv"
  ))

## TODO compound classes if npclassifier one day
canopus_prepared <- canopus |>
  dplyr::mutate(feature_id = gsub(
    pattern = ".*_",
    replacement = "",
    x = name
  ))

## TODO compound classes if npclassifier one day
canopus_adducts_prepared <- canopus_adducts |>
  dplyr::mutate(feature_id = gsub(
    pattern = ".*_",
    replacement = "",
    x = name
  ))

## TODO score not optimal
compound_prepared <- compound |>
  dplyr::mutate(
    feature_id = gsub(
      pattern = ".*_",
      replacement = "",
      x = id
    ),
    score_input = ifelse(
      test = ConfidenceScore != "N/A",
      yes = ConfidenceScore,
      no = -10 / `CSI:FingerIDScore`
    )
  ) |>
  dplyr::select(
    feature_id,
    smiles,
    inchikey_2D = InChIkey2D,
    molecular_formula = molecularFormula,
    score_input
  ) |>
  dplyr::mutate(
    library = "SIRIUS",
    inchikey = NA,
    smiles_2D = smiles,
    structure_taxonomy_npclassifier_01pathway = NA,
    structure_taxonomy_npclassifier_02superclass = NA,
    structure_taxonomy_npclassifier_03class = NA
  )

compound_adducts_prepared <- compound_adducts |>
  dplyr::mutate(
    feature_id = gsub(
      pattern = ".*_",
      replacement = "",
      x = id
    ),
    score_input = ifelse(
      test = ConfidenceScore != "N/A",
      yes = ConfidenceScore,
      no = -10 / `CSI:FingerIDScore`
    )
  ) |>
  dplyr::select(
    feature_id,
    smiles,
    inchikey_2D = InChIkey2D,
    molecular_formula = molecularFormula,
    score_input
  ) |>
  dplyr::mutate(
    library = "SIRIUS",
    inchikey = NA,
    smiles_2D = smiles,
    structure_taxonomy_npclassifier_01pathway = NA,
    structure_taxonomy_npclassifier_02superclass = NA,
    structure_taxonomy_npclassifier_03class = NA
  )

formula_prepared <- formula |>
  dplyr::mutate(feature_id = gsub(
    pattern = ".*_",
    replacement = "",
    x = id
  )) |>
  mutate(structure_exact_mass = ionMass - `massErrorPrecursor(ppm)` * ionMass * 0.000001) |>
  distinct(feature_id, molecular_formula = molecularFormula, structure_exact_mass)

formula_adducts_prepared <- formula_adducts |>
  dplyr::mutate(feature_id = gsub(
    pattern = ".*_",
    replacement = "",
    x = id
  )) |>
  mutate(structure_exact_mass = ionMass - `massErrorPrecursor(ppm)` * ionMass * 0.000001) |>
  distinct(feature_id, molecular_formula = molecularFormula, structure_exact_mass)

compounds_prepared <-
  rbind(compound_prepared, compound_adducts_prepared) |>
  distinct()

formulas_prepared <-
  rbind(formula_prepared, formula_adducts_prepared) |>
  distinct()

table <- left_join(compounds_prepared, formulas_prepared)

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
  file = params$output
)

export_params(
  parameters = params,
  directory = paths$data$interim$config$path,
  step = step
)

end <- Sys.time()

log_debug("Script finished in", format(end - start))
