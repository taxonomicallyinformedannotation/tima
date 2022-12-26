#' @title Prepare sirius
#'
#' @description TODO
#'
#' @param input_directory Directory containing the Sirius results
#' @param npc Boolean. NPClassifier classes computed. TRUE or FALSE
#' @param output Output where to save prepared results
#'
#' @return NULL
#'
#' @export
#'
#' @importFrom dplyr bind_rows distinct filter left_join mutate mutate_all
#' @importFrom dplyr na_if select
#' @importFrom readr read_delim write_delim
#'
#' @examples NULL
prepare_sirius <-
  function(input_directory = params$directory,
           npc = params$npc,
           output = params$output) {
    stopifnot("Your input directory does not exist" = dir.exists(input_directory))
    stopifnot("NPC must be 'true' or 'false" = npc %in% c(TRUE, FALSE))
    stopifnot("Your npc summary file must be named 'canopus_compound_summary.tsv" = file.exists(
      file.path(
        input_directory,
        "canopus_compound_summary.tsv"
      )
    ))
    stopifnot(
      "You must generate the summaries before submission. \n
              Please do `sirius -i $WORKSPACE write-summaries -o $WORKSPACE_SUMMARIES.zip -c --digits 3" = length(
        list.files(
          path = input_directory,
          pattern = "structure_candidates.tsv",
          recursive = TRUE
        )
      ) != 0
    )

    log_debug("Loading and formatting SIRIUS results")
    if (npc == TRUE) {
      canopus <-
        readr::read_delim(file = file.path(
          input_directory,
          "canopus_compound_summary.tsv"
        ))
    } else {
      cat(
        "Please compute NPClassifier Canopus summary file, we do not support Classyfire anymore"
      )
    }

    formula <-
      readr::read_delim(file = file.path(
        input_directory,
        "formula_identifications.tsv"
      ))

    formula_adducts <-
      readr::read_delim(file = file.path(
        input_directory,
        "formula_identifications_adducts.tsv"
      ))

    compound <-
      readr::read_delim(file = file.path(
        input_directory,
        "compound_identifications.tsv"
      ))

    compound_adducts <-
      readr::read_delim(file = file.path(
        input_directory,
        "compound_identifications_adducts.tsv"
      ))

    compound_summary <- lapply(
      X = list.files(
        path = input_directory,
        pattern = "structure_candidates.tsv",
        full.names = TRUE,
        recursive = TRUE
      ),
      FUN = readr::read_delim,
      col_types = "c",
      col_select = c(
        "ConfidenceScore",
        "CSI:FingerIDScore",
        "molecularFormula",
        "adduct",
        "InChIkey2D",
        "InChI",
        "name",
        "smiles",
        "xlogp"
      )
    )

    names(compound_summary) <- list.files(
      path = input_directory,
      pattern = "structure_candidates.tsv",
      recursive = TRUE
    ) |>
      pretreat_names_sirius() |>
      treat_names_sirius()


    compound_summary_ready <-
      lapply(compound_summary, FUN = dplyr::mutate_all, as.character) |>
      dplyr::bind_rows(.id = "feature_id")

    canopus_npc_prepared <- canopus |>
      dplyr::mutate(feature_id = treat_names_sirius(id)) |>
      dplyr::select(
        feature_id,
        structure_taxonomy_npclassifier_01pathway = `NPC#pathway`,
        `NPC#pathway Probability`,
        structure_taxonomy_npclassifier_02superclass = `NPC#superclass`,
        `NPC#superclass Probability`,
        structure_taxonomy_npclassifier_03class = `NPC#class`,
        `NPC#class Probability`
      )

    ## TODO score not optimal
    compound_prepared <- compound_summary_ready |>
      dplyr::mutate(
        score_input = ifelse(
          test = ConfidenceScore != "N/A",
          yes = ConfidenceScore,
          no = -10 / as.numeric(`CSI:FingerIDScore`)
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
        smiles_2D = smiles
      )

    compound_adducts_prepared <- compound_adducts |>
      dplyr::mutate(
        feature_id = treat_names_sirius(id),
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
        smiles_2D = smiles
      )

    formula_prepared <- formula |>
      dplyr::mutate(feature_id = treat_names_sirius(id)) |>
      dplyr::mutate(structure_exact_mass = ionMass - `massErrorPrecursor(ppm)` * ionMass * 0.000001) |>
      dplyr::distinct(feature_id, molecular_formula = molecularFormula, structure_exact_mass)

    formula_adducts_prepared <- formula_adducts |>
      dplyr::mutate(feature_id = treat_names_sirius(id)) |>
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
      dplyr::left_join(canopus_npc_prepared) |>
      dplyr::distinct() |>
      complement_metadata() |>
      dplyr::mutate_all(dplyr::na_if, "N/A")

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

    log_debug(x = "Exporting ...")
    export_params(step = "prepare_sirius")
    export_output(x = table, file = output)
  }
