utils::globalVariables(
  c(
    "ConfidenceScore",
    "count_peaks_explained",
    "count_peaks_matched",
    "CSI:FingerIDScore",
    "error_mz",
    "error_rt",
    "explainedIntensity",
    "feature_id",
    "id",
    "inchikey_2D",
    "InChIkey2D",
    "ionMass",
    "IsotopeScore",
    "massErrorPrecursor(ppm)",
    "molecular_formula",
    "molecularFormula",
    "name",
    "numExplainedPeaks",
    "params",
    "score_input",
    # "score_input_normalized",
    # "score_sirius_csi",
    # "score_sirius_sirius",
    # "score_sirius_zodiac",
    "SiriusScore",
    "smiles",
    "smiles_2D",
    "structure_exact_mass",
    "structure_name",
    "structure_taxonomy_classyfire_01kingdom",
    "structure_taxonomy_classyfire_02superclass",
    "structure_taxonomy_classyfire_03class",
    "structure_taxonomy_classyfire_04directparent",
    "structure_taxonomy_classyfire_chemontid",
    "structure_taxonomy_npclassifier_01pathway",
    "structure_taxonomy_npclassifier_02superclass",
    "structure_taxonomy_npclassifier_03class",
    "structure_xlogp",
    "TreeScore",
    "xlogp",
    "ZodiacScore"
  )
)

#' @title Prepare annotations SIRIUS
#'
#' @description This function prepares Sirius results to make them compatible
#'
#' @include export_output.R
#' @include export_params.R
#' @include harmonize_names_sirius.R
#' @include pre_harmonize_names_sirius.R
#' @include select_annotations_columns.R
#'
#' @param input_directory Directory containing the Sirius results
#' @param output Output where to save prepared results
#' @param str_2d_3d File containing 2D and 3D structures
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
           str_2d_3d = params$
             files$
             libraries$
             sop$
             merged$
             structures$
             dd_ddd,
           str_met = params$
             files$
             libraries$
             sop$
             merged$
             structures$
             metadata,
           str_nam = params$
             files$
             libraries$
             sop$
             merged$
             structures$
             names,
           str_tax_cla = params$
             files$
             libraries$
             sop$
             merged$
             structures$
             taxonomies$
             cla,
           str_tax_npc = params$
             files$
             libraries$
             sop$
             merged$
             structures$
             taxonomies$
             npc,
           parameters = params) {
    params <<- parameters
    if (file.exists(input_directory)) {
      stopifnot("Your npc summary file must be named
                'canopus_compound_summary.tsv" = file.exists(
        file.path(
          input_directory,
          "canopus_compound_summary.tsv"
        )
      ))
      stopifnot(
        "You must generate the summaries before submission. \n
              Please do
        `sirius -i $WORKSPACE write-summaries -o $WORKSPACE_SUMMARIES.zip
        -c --digits 3" = length(
          list.files(
            path = input_directory,
            pattern = "structure_candidates.tsv",
            recursive = TRUE
          )
        ) != 0
      )
      log_debug("Loading and formatting SIRIUS results")
      canopus <-
        tidytable::fread(
          file = file.path(
            input_directory,
            "canopus_compound_summary.tsv"
          ),
          na.strings = c("", "NA")
        )

      formula <-
        tidytable::fread(
          file = file.path(
            input_directory,
            "formula_identifications.tsv"
          ),
          na.strings = c("", "NA")
        )

      formula_adducts <-
        tidytable::fread(
          file = file.path(
            input_directory,
            "formula_identifications_adducts.tsv"
          ),
          na.strings = c("", "NA")
        )

      # compound <-
      #   tidytable::fread(file = file.path(
      #     input_directory,
      #     "compound_identifications.tsv"
      #   ),
      #         na.strings = c("","NA"))

      compound_adducts <-
        tidytable::fread(
          file = file.path(
            input_directory,
            "compound_identifications_adducts.tsv"
          ),
          na.strings = c("", "NA")
        )

      compound_summary <- lapply(
        X = list.files(
          path = input_directory,
          pattern = "structure_candidates.tsv",
          full.names = TRUE,
          recursive = TRUE
        ),
        FUN = tidytable::fread,
        na.strings = c("", "NA"),
        colClasses = "character"
      )

      names(compound_summary) <- list.files(
        path = input_directory,
        pattern = "structure_candidates.tsv",
        recursive = TRUE
      ) |>
        pre_harmonize_names_sirius() |>
        harmonize_names_sirius()

      compound_summary <-
        compound_summary[lapply(compound_summary, nrow) > 0]

      compound_summary_ready <- compound_summary |>
        tidytable::bind_rows(.id = "feature_id")

      canopus_npc_prepared <- canopus |>
        tidyft::mutate(feature_id = harmonize_names_sirius(id)) |>
        tidytable::select(tidytable::any_of(
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

      compound_prepared <- compound_summary_ready |>
        tidytable::select(
          feature_id,
          structure_name = name,
          structure_smiles_2D = smiles,
          structure_inchikey_2D = InChIkey2D,
          structure_molecular_formula = molecularFormula,
          structure_xlogp = xlogp,
          score_input = ConfidenceScore,
          score_sirius_csi = `CSI:FingerIDScore`
        ) |>
        tidytable::mutate(
          library = "SIRIUS",
          inchikey = NA_character_,
          smiles = NA_character_
        )

      compound_adducts_prepared <- compound_adducts |>
        tidyft::mutate(feature_id = harmonize_names_sirius(id)) |>
        tidytable::select(
          feature_id,
          structure_name = name,
          structure_smiles_2D = smiles,
          structure_inchikey_2D = InChIkey2D,
          structure_molecular_formula = molecularFormula,
          structure_xlogp = xlogp,
          score_input = ConfidenceScore,
          score_sirius_csi = `CSI:FingerIDScore`
        ) |>
        tidytable::mutate(
          library = "SIRIUS",
          inchikey = NA_character_,
          smiles = NA_character_
        )

      formula_prepared <- formula |>
        tidyft::mutate(feature_id = harmonize_names_sirius(id)) |>
        tidyft::mutate(
          structure_exact_mass = ionMass -
            `massErrorPrecursor(ppm)` *
              ionMass *
              1E-6,
          error_mz = ionMass * `massErrorPrecursor(ppm)` * 1E-6
        ) |>
        tidytable::distinct(
          feature_id,
          structure_molecular_formula = molecularFormula,
          structure_exact_mass,
          error_mz,
          score_sirius_zodiac = ZodiacScore,
          score_sirius_sirius = SiriusScore,
          score_sirius_tree = TreeScore,
          score_sirius_isotope = IsotopeScore,
          count_peaks_explained = numExplainedPeaks,
          score_sirius_intensity = explainedIntensity
        )

      formula_adducts_prepared <- formula_adducts |>
        tidyft::mutate(feature_id = harmonize_names_sirius(id)) |>
        tidyft::mutate(
          structure_exact_mass = ionMass -
            `massErrorPrecursor(ppm)`
            *
              ionMass *
              1E-6,
          error_mz = ionMass * `massErrorPrecursor(ppm)` * 1E-6
        ) |>
        tidytable::distinct(
          feature_id,
          structure_molecular_formula = molecularFormula,
          structure_exact_mass,
          error_mz,
          score_sirius_zodiac = ZodiacScore,
          score_sirius_sirius = SiriusScore,
          score_sirius_tree = TreeScore,
          score_sirius_isotope = IsotopeScore,
          count_peaks_explained = numExplainedPeaks,
          score_sirius_intensity = explainedIntensity
        )

      compounds_prepared <-
        tidytable::bind_rows(compound_prepared, compound_adducts_prepared) |>
        tidytable::distinct()

      formulas_prepared <-
        tidytable::bind_rows(formula_prepared, formula_adducts_prepared) |>
        tidytable::distinct()

      table <- compounds_prepared |>
        tidytable::left_join(formulas_prepared) |>
        tidytable::left_join(canopus_npc_prepared) |>
        tidytable::distinct() |>
        tidyft::mutate(
          error_rt = NA,
          structure_taxonomy_classyfire_chemontid = NA,
          structure_taxonomy_classyfire_01kingdom = NA,
          ## mirror spectral match
          count_peaks_matched = NA
        ) |>
        select_annotations_columns(
          str_2d_3d = str_2d_3d,
          str_met = str_met,
          str_nam = str_nam,
          str_tax_cla = str_tax_cla,
          str_tax_npc = str_tax_npc
        )
    } else {
      log_debug("Sorry, your input directory does not exist,
                returning an empty file instead")
      table <- fake_annotations_columns()
    }
    log_debug(x = "Exporting ...")
    export_params(step = "prepare_annotations_sirius")
    export_output(x = table, file = output[[1]])

    return(output[[1]])
  }
