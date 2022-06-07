source("R/pretreat_names_sirius.R")
source("R/treat_names_sirius.R")

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
      dplyr::distinct()

    table[] <- lapply(
      table,
      function(x) {
        y_as_na(x, y = "N/A")
      }
    )

    #' temp CASMI fix
    table <- table |>
      dplyr::mutate(
        feature_id = gsub(
          pattern = "adduct",
          replacement = "0000",
          x = feature_id
        )
      )

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

    log_debug(
      x = "... path to export is",
      output
    )
    readr::write_delim(
      x = table,
      file = output,
      delim = "\t"
    )

    export_params(
      parameters = params,
      directory = paths$data$interim$config$path,
      step = "prepare_sirius"
    )
  }
