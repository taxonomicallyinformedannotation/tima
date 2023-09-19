#' @title Prepare annotations SIRIUS
#'
#' @description This function prepares Sirius results to make them compatible
#'
#' @include columns_model.R
#' @include harmonize_names_sirius.R
#' @include pre_harmonize_names_sirius.R
#' @include select_annotations_columns.R
#' @include select_sirius_columns.R
#'
#' @param input_directory Directory containing the Sirius results
#' @param output_ann Output where to save prepared annotation results
#' @param output_can Output where to save prepared canopus results
#' @param output_for Output where to save prepared formula results
#' @param str_stereo File containing structures stereo
#' @param str_met File containing structures metadata
#' @param str_nam File containing structures names
#' @param str_tax_cla File containing Classyfire taxonomy
#' @param str_tax_npc File containing NPClassifier taxonomy
#'
#' @return NULL
#'
#' @export
#'
#' @examples NULL
prepare_annotations_sirius <-
  function(input_directory = get_params(step = "prepare_annotations_sirius")$files$annotations$raw$sirius,
           output_ann = get_params(step = "prepare_annotations_sirius")$files$annotations$prepared$structural,
           output_can = get_params(step = "prepare_annotations_sirius")$files$annotations$prepared$canopus,
           output_for = get_params(step = "prepare_annotations_sirius")$files$annotations$prepared$formula,
           str_stereo = get_params(step = "prepare_annotations_sirius")$files$libraries$sop$merged$structures$stereo,
           str_met = get_params(step = "prepare_annotations_sirius")$files$libraries$sop$merged$structures$metadata,
           str_nam = get_params(step = "prepare_annotations_sirius")$files$libraries$sop$merged$structures$names,
           str_tax_cla = get_params(step = "prepare_annotations_sirius")$files$libraries$sop$merged$structures$taxonomies$cla,
           str_tax_npc = get_params(step = "prepare_annotations_sirius")$files$libraries$sop$merged$structures$taxonomies$npc) {
    if (file.exists(input_directory)) {
      stopifnot("Your npc summary file must be named
                'canopus_compound_summary.tsv" = file.exists(file.path(input_directory, "canopus_compound_summary.tsv")))
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
          na.strings = c("", "NA"),
          colClasses = "character"
        )

      formula <-
        tidytable::fread(
          file = file.path(
            input_directory,
            "formula_identifications.tsv"
          ),
          na.strings = c("", "NA"),
          colClasses = "character"
        )

      formula_adducts <-
        tidytable::fread(
          file = file.path(
            input_directory,
            "formula_identifications_adducts.tsv"
          ),
          na.strings = c("", "NA"),
          colClasses = "character"
        )

      # compound <-
      #   tidytable::fread(file = file.path(
      #     input_directory,
      #     "compound_identifications.tsv"
      #   ),
      #         na.strings = c("","NA")),
      #     colClasses = "character"

      compound_adducts <-
        tidytable::fread(
          file = file.path(
            input_directory,
            "compound_identifications_adducts.tsv"
          ),
          na.strings = c("", "NA"),
          colClasses = "character"
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
      rm(compound_summary)

      canopus_prepared <- canopus |>
        tidytable::mutate(feature_id = harmonize_names_sirius(id)) |>
        tidytable::select(tidytable::any_of(
          c(
            "feature_id",
            "candidate_structure_molecular_formula" = "molecularFormula",
            "feature_pred_tax_npc_01pat_val" = "NPC#pathway",
            "feature_pred_tax_npc_01pat_score" = "NPC#pathway Probability",
            "feature_pred_tax_npc_02sup_val" = "NPC#superclass",
            "feature_pred_tax_npc_02sup_score" = "NPC#superclass Probability",
            "feature_pred_tax_npc_03cla_val" = "NPC#class",
            "feature_pred_tax_npc_03cla_score" = "NPC#class Probability",
            "feature_pred_tax_cla_01kin_val" = "ClassyFire#TODO",
            "feature_pred_tax_cla_01kin_score" = "ClassyFire#TODO Probability",
            "feature_pred_tax_cla_02sup_val" = "ClassyFire#superclass",
            "feature_pred_tax_cla_02sup_score" =
              "ClassyFire#superclass probability",
            "feature_pred_tax_cla_03cla_val" = "ClassyFire#class",
            "feature_pred_tax_cla_03cla_score" = "ClassyFire#class Probability",
            "feature_pred_tax_cla_04dirpar_val" =
              "ClassyFire#most specific class",
            "feature_pred_tax_cla_04dirpar_score" =
              "ClassyFire#most specific class Probability"
          )
        ))
      rm(canopus)

      compound_prepared <- compound_summary_ready |>
        select_sirius_columns()
      rm(compound_summary_ready)

      compound_adducts_prepared <- compound_adducts |>
        tidytable::mutate(feature_id = harmonize_names_sirius(id)) |>
        select_sirius_columns()
      rm(compound_adducts)

      formula_prepared <- formula |>
        select_sirius_columns_2()
      rm(formula)

      formula_adducts_prepared <- formula_adducts |>
        select_sirius_columns_2()
      rm(formula_adducts)

      compounds_prepared <-
        tidytable::bind_rows(compound_prepared, compound_adducts_prepared) |>
        tidytable::distinct()
      rm(compound_prepared, compound_adducts_prepared)

      formulas_prepared <-
        tidytable::bind_rows(formula_prepared, formula_adducts_prepared) |>
        tidytable::distinct()
      rm(formula_prepared, formula_adducts_prepared)

      table <- compounds_prepared |>
        tidytable::left_join(formulas_prepared) |>
        tidytable::left_join(canopus_prepared) |>
        tidytable::distinct() |>
        tidytable::mutate(
          candidate_structure_tax_cla_chemontid = NA,
          candidate_structure_tax_cla_01kin = NA
        ) |>
        select_annotations_columns()
      rm(compounds_prepared, formulas_prepared, canopus_prepared)
    } else {
      log_debug("Sorry, your input directory does not exist,
                returning an empty file instead")
      table <- fake_annotations_columns() |>
        tidytable::mutate(
          feature_pred_tax_cla_02sup_val = NA,
          feature_pred_tax_cla_02sup_score = NA,
          feature_pred_tax_cla_03cla_val = NA,
          feature_pred_tax_cla_03cla_score = NA,
          feature_pred_tax_cla_04dirpar_val = NA,
          feature_pred_tax_cla_04dirpar_score = NA,
          feature_pred_tax_npc_01pat_val = NA,
          feature_pred_tax_npc_01pat_score = NA,
          feature_pred_tax_npc_02sup_val = NA,
          feature_pred_tax_npc_02sup_score = NA,
          feature_pred_tax_npc_03cla_val = NA,
          feature_pred_tax_npc_03cla_score = NA,
          candidate_count_sirius_peaks_explained = NA,
          candidate_score_sirius_intensity = NA,
          candidate_score_sirius_isotope = NA,
          candidate_score_sirius_sirius = NA,
          candidate_score_sirius_tree = NA,
          candidate_score_sirius_zodiac = NA,
          candidate_score_sirius_confidence = NA,
          candidate_score_sirius_csi = NA
        ) |>
        tidytable::select(
          -candidate_structure_error_rt,
          -candidate_score_similarity,
          -candidate_count_similarity_peaks_matched
        )
    }
    log_debug("Splitting SIRIUS results")
    model <- columns_model()

    table_can <- table |>
      tidytable::select(tidytable::any_of(c(
        model$features_columns,
        model$features_calculated_columns
      ))) |>
      tidytable::distinct()

    table_for <- table |>
      tidytable::select(tidytable::any_of(c(
        model$features_columns,
        model$candidates_sirius_for_columns
      ))) |>
      tidytable::distinct()

    table_str <- table |>
      tidytable::select(tidytable::any_of(c(
        model$features_columns,
        model$candidates_structures_columns,
        model$candidates_spectra_columns,
        model$candidates_sirius_str_columns
      ))) |>
      tidytable::distinct()
    rm(table)

    log_debug(x = "Exporting ...")
    export_params(parameters = get_params(step = "prepare_annotations_sirius"), step = "prepare_annotations_sirius")
    export_output(x = table_can, file = output_can)
    export_output(x = table_for, file = output_for)
    export_output(x = table_str, file = output_ann[[1]])

    rm(table_can, table_for, table_str)
    return(
      c(
        "canopus" = output_can,
        "formula" = output_for,
        "structural" = output_ann[[1]]
      )
    )
  }
