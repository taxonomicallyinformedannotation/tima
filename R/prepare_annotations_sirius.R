#' @title Prepare annotations SIRIUS
#'
#' @description This function prepares Sirius results to make them compatible
#'
#' @param input_directory Directory containing the Sirius results
#' @param output Output where to save prepared results
#' @param str_2D_3D File containing 2D and 3D structures
#' @param str_met File containing structures metadata
#' @param str_nam File containing structures names
#' @param str_tax_cla File containing Classyfire taxonomy
#' @param str_tax_npc File containing NPClassifier taxonomy
#' @param parameters Params
#'
#' @return NULL
#'
#' @export
#'
#' @examples NULL
prepare_annotations_sirius <-
  function(input_directory = params$files$annotations$raw$sirius,
           output = params$files$annotations$prepared,
           str_2D_3D = params$files$libraries$sop$merged$structures$dd_ddd,
           str_met = params$files$libraries$sop$merged$structures$metadata,
           str_nam = params$files$libraries$sop$merged$structures$names,
           str_tax_cla = params$files$libraries$sop$merged$structures$taxonomies$cla,
           str_tax_npc = params$files$libraries$sop$merged$structures$taxonomies$npc,
           parameters = params) {
    params <<- parameters
    if (file.exists(input_directory)) {
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
      canopus <-
        readr::read_delim(file = file.path(
          input_directory,
          "canopus_compound_summary.tsv"
        ))

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

      # compound <-
      #   readr::read_delim(file = file.path(
      #     input_directory,
      #     "compound_identifications.tsv"
      #   ))

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
        col_types = readr::cols(.default = "c"),
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
        pre_harmonize_names_sirius() |>
        harmonize_names_sirius()

      compound_summary_ready <-
        lapply(compound_summary, FUN = dplyr::mutate_all, as.character) |>
        dplyr::bind_rows(.id = "feature_id")

      canopus_npc_prepared <- canopus |>
        dplyr::mutate(feature_id = harmonize_names_sirius(id)) |>
        dplyr::select(dplyr::any_of(
          c(
            "feature_id",
            "structure_taxonomy_npclassifier_01pathway" = "NPC#pathway",
            "NPC#pathway Probability",
            "structure_taxonomy_npclassifier_02superclass" = "NPC#superclass",
            "NPC#superclass Probability",
            "structure_taxonomy_npclassifier_03class" = "NPC#class",
            "NPC#class Probability",
            "structure_taxonomy_classyfire_01kingdom" = "ClassyFire#TODO",
            "ClassyFire#TODO Probability",
            "structure_taxonomy_classyfire_02superclass" = "ClassyFire#superclass",
            "ClassyFire#superclass probability",
            "structure_taxonomy_classyfire_03class" = "ClassyFire#class",
            "ClassyFire#class Probability",
            "structure_taxonomy_classyfire_04directparent" = "ClassyFire#most specific class",
            "ClassyFire#most specific class Probability"
          )
        ))

      ## TODO score not optimal
      compound_prepared <- compound_summary_ready |>
        dplyr::mutate(
          score_input = as.numeric(
            ifelse(
              test = ConfidenceScore != "N/A",
              yes = ConfidenceScore,
              no = -10 / as.numeric(`CSI:FingerIDScore`)
            )
          ),
          structure_xlogp = as.numeric(xlogp)
        ) |>
        dplyr::select(
          feature_id,
          structure_name = name,
          smiles_2D = smiles,
          inchikey_2D = InChIkey2D,
          molecular_formula = molecularFormula,
          score_input
        ) |>
        dplyr::mutate(
          library = "SIRIUS",
          inchikey = NA,
          smiles = NA
        )

      compound_adducts_prepared <- compound_adducts |>
        dplyr::mutate(
          feature_id = harmonize_names_sirius(id),
          score_input = as.numeric(
            ifelse(
              test = ConfidenceScore != "N/A",
              yes = ConfidenceScore,
              no = -10 / `CSI:FingerIDScore`
            )
          )
        ) |>
        dplyr::select(
          feature_id,
          structure_name = name,
          smiles_2D = smiles,
          inchikey_2D = InChIkey2D,
          molecular_formula = molecularFormula,
          structure_xlogp = xlogp,
          score_input
        ) |>
        dplyr::mutate(
          library = "SIRIUS",
          inchikey = NA,
          smiles = NA
        )

      formula_prepared <- formula |>
        dplyr::mutate(feature_id = harmonize_names_sirius(id)) |>
        dplyr::mutate(
          structure_exact_mass = ionMass - `massErrorPrecursor(ppm)` * ionMass * 1E-6,
          mz_error = ionMass * `massErrorPrecursor(ppm)` * 1E-6
        ) |>
        dplyr::distinct(feature_id,
          molecular_formula = molecularFormula,
          structure_exact_mass,
          mz_error
        )

      formula_adducts_prepared <- formula_adducts |>
        dplyr::mutate(feature_id = harmonize_names_sirius(id)) |>
        dplyr::mutate(
          structure_exact_mass = ionMass - `massErrorPrecursor(ppm)` * ionMass * 1E-6,
          mz_error = ionMass * `massErrorPrecursor(ppm)` * 1E-6
        ) |>
        dplyr::distinct(feature_id,
          molecular_formula = molecularFormula,
          structure_exact_mass,
          mz_error
        )

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
        dplyr::mutate(
          rt_error = NA,
          structure_taxonomy_classyfire_chemontid = NA,
          structure_taxonomy_classyfire_01kingdom = NA
        ) |>
        dplyr::select(
          feature_id,
          mz_error,
          rt_error,
          structure_name,
          # structure_inchikey = inchikey,
          structure_inchikey_2D = inchikey_2D,
          # structure_smiles = smiles,
          structure_smiles_2D = smiles_2D,
          structure_molecular_formula = molecular_formula,
          structure_exact_mass,
          structure_xlogp,
          library,
          score_input,
          structure_taxonomy_npclassifier_01pathway,
          structure_taxonomy_npclassifier_02superclass,
          structure_taxonomy_npclassifier_03class,
          ## TODO until better
          structure_taxonomy_classyfire_chemontid,
          structure_taxonomy_classyfire_01kingdom,
          structure_taxonomy_classyfire_02superclass,
          structure_taxonomy_classyfire_03class,
          structure_taxonomy_classyfire_04directparent
        ) |>
        dplyr::mutate_all(as.character) |>
        dplyr::mutate_all(dplyr::na_if, "N/A") |>
        dplyr::mutate_all(dplyr::na_if, "null") |>
        round_reals() |>
        complement_metadata_structures(
          str_2D_3D = str_2D_3D,
          str_met = str_met,
          str_nam = str_nam,
          str_tax_cla = str_tax_cla,
          str_tax_npc = str_tax_npc
        )

      if (nrow(table |>
        dplyr::filter(is.na(structure_exact_mass))) > 0) {
        log_debug(
          "Warning:",
          nrow(table |> dplyr::filter(is.na(
            structure_exact_mass
          ))),
          "features have no exact mass.",
          "This is somehow unexpected and under investigation."
        )
      }
    } else {
      log_debug("Sorry, your input directory does not exist, returning an empty file instead")
      table <- data.frame(
        feature_id = NA,
        mz_error = NA,
        rt_error = NA,
        structure_name = NA,
        # structure_inchikey = NA,
        structure_inchikey_2D = NA,
        # structure_smiles = NA,
        structure_smiles_2D = NA,
        structure_molecular_formula = NA,
        structure_exact_mass = NA,
        structure_xlogp = NA,
        library = NA,
        score_input = NA,
        structure_taxonomy_npclassifier_01pathway = NA,
        structure_taxonomy_npclassifier_02superclass = NA,
        structure_taxonomy_npclassifier_03class = NA,
        structure_taxonomy_classyfire_chemontid = NA,
        structure_taxonomy_classyfire_01kingdom = NA,
        structure_taxonomy_classyfire_02superclass = NA,
        structure_taxonomy_classyfire_03class = NA,
        structure_taxonomy_classyfire_04directparent = NA
      )
    }
    log_debug(x = "Exporting ...")
    export_params(step = "prepare_annotations_sirius")
    export_output(x = table, file = output[[1]])

    return(output[[1]])
  }
