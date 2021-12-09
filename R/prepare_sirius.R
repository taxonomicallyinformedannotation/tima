#' Title
#'
#' @param input_directory TODO
#' @param npc TODO
#' @param output TODO
#'
#' @return TODO
#' @export
#'
#' @examples
prepare_sirius <-
  function(input_directory = params$directory,
           npc = params$npc,
           output = params$output) {
    stopifnot("Your input directory does not exist" = dir.exists(input_directory))
    stopifnot("NPC must be 'true' or 'false" = npc %in% c(TRUE, FALSE))
    stopifnot("Your npc summary file must be named 'canopus_npc_summary.csv" = file.exists(file.path(
      input_directory,
      "canopus_npc_summary.csv"
    )))

    log_debug("Loading and formatting SIRIUS results")
    if (npc == TRUE) {
      canopus <-
        readr::read_delim(file = file.path(input_directory,
                                           "canopus_npc_summary.csv"))
    } else {
      cat(
        "Please compute NPClassifier Canopus summary file, we do not support Classyfire anymore"
      )
    }

    canopus_adducts <-
      readr::read_delim(file = file.path(input_directory,
                                         "canopus_summary_adducts.tsv"))

    formula <-
      readr::read_delim(file = file.path(input_directory,
                                         "formula_identifications.tsv"))

    formula_adducts <-
      readr::read_delim(file = file.path(input_directory,
                                         "formula_identifications_adducts.tsv"))

    compound <-
      readr::read_delim(file = file.path(input_directory,
                                         "compound_identifications.tsv"))

    compound_adducts <-
      readr::read_delim(file = file.path(input_directory,
                                         "compound_identifications_adducts.tsv"))

    ## TODO compound classes if npclassifier one day
    canopus_prepared <- canopus |>
      dplyr::mutate(feature_id = gsub(
        pattern = ".*_",
        replacement = "",
        x = name
      )) |>
      dplyr::select(
        feature_id,
        structure_taxonomy_npclassifier_01pathway = pathway,
        pathwayProbability,
        structure_taxonomy_npclassifier_02superclass = superclass,
        superclassProbability,
        structure_taxonomy_npclassifier_03class = class,
        classProbability
      )

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
      dplyr::mutate(library = "SIRIUS",
                    inchikey = NA,
                    smiles_2D = smiles)

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
      dplyr::mutate(library = "SIRIUS",
                    inchikey = NA,
                    smiles_2D = smiles)

    formula_prepared <- formula |>
      dplyr::mutate(feature_id = gsub(
        pattern = ".*_",
        replacement = "",
        x = id
      )) |>
      dplyr::mutate(structure_exact_mass = ionMass - `massErrorPrecursor(ppm)` * ionMass * 0.000001) |>
      dplyr::distinct(feature_id, molecular_formula = molecularFormula, structure_exact_mass)

    formula_adducts_prepared <- formula_adducts |>
      dplyr::mutate(feature_id = gsub(
        pattern = ".*_",
        replacement = "",
        x = id
      )) |>
      dplyr::mutate(structure_exact_mass = ionMass - `massErrorPrecursor(ppm)` * ionMass * 0.000001) |>
      dplyr::distinct(feature_id, molecular_formula = molecularFormula, structure_exact_mass)

    compounds_prepared <-
      dplyr::bind_rows(compound_prepared, compound_adducts_prepared) |>
      dplyr::distinct()

    formulas_prepared <-
      dplyr::bind_rows(formula_prepared, formula_adducts_prepared) |>
      dplyr::distinct()

    table <- compounds_prepared |>
      dplyr::left_join(formulas_prepared) |>
      dplyr::left_join(canopus_prepared) |>
      dplyr::distinct()

    table[] <-
      lapply(table,
             function(x) {
               y_as_na(x, y = "N/A")
             })

    if (nrow(table |> dplyr::filter(is.na(structure_exact_mass))) > 0) {
      cat(
        "Warning:",
        nrow(table |> dplyr::filter(is.na(
          structure_exact_mass
        ))),
        "features have no exact mass.",
        "This is somehow unexpected and under investigation."
      )
    }

    log_debug(x = "Sirius weighting is still not optimal given the input candidate list ...")
    log_debug(x = "See https://github.com/boecker-lab/sirius/issues/50 for more info...")

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
      test = !dir.exists(dirname(output)),
      yes = dir.create(dirname(output)),
      no = paste(dirname(output), "exists")
    )

    log_debug(x = "... path to export is",
              output)
    readr::write_delim(x = table,
                       file = output,
                       delim = "\t")

    export_params(
      parameters = params,
      directory = paths$data$interim$config$path,
      step = step
    )
  }
