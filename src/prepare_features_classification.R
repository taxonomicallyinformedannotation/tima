start <- Sys.time()

source(file = "R/helpers.R")

log_debug("This script prepares features metadata (chemical classes)")
log_debug("Authors: AR")
log_debug("Contributors: ...")

library(dplyr)
library(docopt)
library(purrr)
library(readr)
library(yaml)

paths <- parse_yaml_paths()

params <- get_params(step = "prepare_features_classification")

log_debug(x = "loading files")

lotus <-
  readr::read_delim(
    file = params$library,
    col_select = c(
      inchikey_2D = structure_inchikey_2D,
      smiles_2D = structure_smiles_2D,
      structure_exact_mass,
      molecular_formula = structure_molecular_formula,
      structure_taxonomy_npclassifier_01pathway,
      structure_taxonomy_npclassifier_02superclass,
      structure_taxonomy_npclassifier_03class
    )
  ) |>
  dplyr::distinct()

table <- readr::read_delim(file = params$input)

table_missing_classification <- table |>
  dplyr::filter(
    is.na(structure_taxonomy_npclassifier_01pathway) &
      is.na(structure_taxonomy_npclassifier_02superclass) &
      is.na(structure_taxonomy_npclassifier_03class)
  ) |>
  dplyr::distinct(inchikey_2D, smiles_2D)

table_missing_mass <- table |>
  dplyr::filter(is.na(structure_exact_mass)) |>
  dplyr::distinct(inchikey_2D, smiles_2D)

table_missing_formula <- table |>
  dplyr::filter(is.na(molecular_formula)) |>
  dplyr::distinct(inchikey_2D, smiles_2D)

table_with_classification <-
  dplyr::anti_join(table, table_missing_classification)

table_with_mass <- dplyr::anti_join(table, table_missing_mass)

table_with_formula <- dplyr::anti_join(table, table_missing_formula)

table_classified_lotus <-
  dplyr::left_join(
    table_missing_classification,
    lotus |>
      dplyr::distinct(
        inchikey_2D,
        smiles_2D,
        structure_taxonomy_npclassifier_01pathway,
        structure_taxonomy_npclassifier_02superclass,
        structure_taxonomy_npclassifier_03class
      )
  )

table_massed_lotus <-
  dplyr::left_join(
    table_missing_mass,
    lotus |>
      dplyr::distinct(
        inchikey_2D, smiles_2D,
        structure_exact_mass
      )
  )

table_formuled_lotus <-
  dplyr::left_join(
    table_missing_formula,
    lotus |>
      dplyr::distinct(
        inchikey_2D, smiles_2D,
        molecular_formula
      )
  )

table_classified_lotus_missing <- table_classified_lotus |>
  dplyr::filter(
    is.na(structure_taxonomy_npclassifier_01pathway) &
      is.na(structure_taxonomy_npclassifier_02superclass) &
      is.na(structure_taxonomy_npclassifier_03class)
  ) |>
  dplyr::distinct(inchikey_2D, smiles_2D, .keep_all = TRUE) |>
  dplyr::filter(!is.na(smiles_2D))

table_massed_lotus_missing <- table_massed_lotus |>
  dplyr::filter(is.na(structure_exact_mass)) |>
  dplyr::distinct(inchikey_2D, smiles_2D, .keep_all = TRUE) |>
  dplyr::filter(!is.na(smiles_2D))

table_formuled_lotus_missing <- table_formuled_lotus |>
  dplyr::filter(is.na(molecular_formula)) |>
  dplyr::distinct(inchikey_2D, smiles_2D, .keep_all = TRUE) |>
  dplyr::filter(!is.na(smiles_2D))

table_with_classification_lotus <-
  dplyr::anti_join(table_classified_lotus, table_classified_lotus_missing)

table_with_mass_lotus <-
  dplyr::anti_join(table_massed_lotus, table_massed_lotus_missing)

table_with_formula_lotus <-
  dplyr::anti_join(table_formuled_lotus, table_formuled_lotus_missing)

table_classified <-
  rbind(
    table_with_classification |>
      dplyr::distinct(
        inchikey_2D,
        smiles_2D,
        structure_taxonomy_npclassifier_01pathway,
        structure_taxonomy_npclassifier_02superclass,
        structure_taxonomy_npclassifier_03class
      ),
    table_classified_lotus,
    table_classified_lotus_missing,
    fill = TRUE
  ) |>
  dplyr::distinct(
    inchikey_2D,
    smiles_2D,
    structure_taxonomy_npclassifier_01pathway,
    structure_taxonomy_npclassifier_02superclass,
    structure_taxonomy_npclassifier_03class
  )

table_massed <-
  rbind(
    table_with_mass |> dplyr::distinct(
      inchikey_2D,
      smiles_2D,
      structure_exact_mass
    ),
    table_massed_lotus,
    table_massed_lotus_missing,
    fill = TRUE
  ) |>
  dplyr::distinct(
    inchikey_2D,
    smiles_2D,
    structure_exact_mass
  )

table_formuled <-
  rbind(
    table_with_formula |>
      dplyr::distinct(
        inchikey_2D,
        smiles_2D,
        molecular_formula
      ),
    table_formuled_lotus,
    table_formuled_lotus_missing,
    fill = TRUE
  ) |>
  dplyr::distinct(
    inchikey_2D,
    smiles_2D,
    molecular_formula
  )

table_final <- dplyr::left_join(
  table |>
    dplyr::distinct(
      feature_id,
      component_id,
      mz,
      rt,
      inchikey_2D,
      smiles_2D,
      score_input,
      library,
      mz_error
    ),
  table_classified
) |>
  dplyr::left_join(table_formuled) |>
  dplyr::left_join(table_massed)

table_final[] <-
  lapply(
    table_final,
    function(x) {
      y_as_na(x, y = "")
    }
  )

if (params$quickmode == FALSE) {
  ## add GNPS query steps for formula, exact mass, and classification
  ## I'll do it later on
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
  x = table_final,
  file = params$output
)

log_debug(x = "... parameters used are saved in", paths$data$interim$config$path)
yaml::write_yaml(
  x = params,
  file = file.path(
    paths$data$interim$config$path,
    paste(
      format(Sys.time(), "%y%m%d_%H%M%OS"),
      "prepare_features_classification.yaml",
      sep = "_"
    )
  )
)

end <- Sys.time()

log_debug("Script finished in", format(end - start))
