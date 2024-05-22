#' @title Prepare annotations SIRIUS
#'
#' @description This function prepares Sirius results to make them compatible
#'
#' @include columns_model.R
#' @include harmonize_names_sirius.R
#' @include pre_harmonize_names_sirius.R
#' @include read_from_sirius_zip.R
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
           output_ann = get_params(step = "prepare_annotations_sirius")$files$annotations$prepared$structural$sirius,
           output_can = get_params(step = "prepare_annotations_sirius")$files$annotations$prepared$canopus,
           output_for = get_params(step = "prepare_annotations_sirius")$files$annotations$prepared$formula,
           str_stereo = get_params(step = "prepare_annotations_sirius")$files$libraries$sop$merged$structures$stereo,
           str_met = get_params(step = "prepare_annotations_sirius")$files$libraries$sop$merged$structures$metadata,
           str_nam = get_params(step = "prepare_annotations_sirius")$files$libraries$sop$merged$structures$names,
           str_tax_cla = get_params(step = "prepare_annotations_sirius")$files$libraries$sop$merged$structures$taxonomies$cla,
           str_tax_npc = get_params(step = "prepare_annotations_sirius")$files$libraries$sop$merged$structures$taxonomies$npc) {
    if (file.exists(input_directory)) {
      log_debug("Loading and formatting SIRIUS results")
      canopus <- input_directory |>
        read_from_sirius_zip(file = "canopus_compound_summary.tsv")

      formula <- input_directory |>
        read_from_sirius_zip(file = "formula_identifications.tsv")

      compound <- input_directory |>
        read_from_sirius_zip(file = "compound_identifications.tsv")

      list <- unzip(input_directory, list = TRUE)
      summary_files <- list$Name[list$Name |>
        grepl(
          pattern = "structure_candidates.tsv", fixed =
            TRUE
        )] |>
        gsub(
          pattern = basename(input_directory) |>
            gsub(
              pattern = ".zip",
              replacement = "",
              fixed = TRUE
            ),
          replacement = ""
        ) |>
        gsub(pattern = "^/", replacement = "")

      compound_summary <- lapply(
        X = summary_files,
        FUN = read_from_sirius_zip,
        sirius_zip = input_directory
      )

      names(compound_summary) <- summary_files |>
        pre_harmonize_names_sirius() |>
        harmonize_names_sirius()

      compound_summary <-
        compound_summary[lapply(compound_summary, nrow) > 0]

      # Allow for summaries only
      if (length(compound_summary) != 0) {
        compound_summary_ready <- compound_summary |>
          tidytable::bind_rows(.id = "feature_id")
      } else {
        compound_summary_ready <- tidytable::tidytable()
      }
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

      compound_prepared_2 <- compound |>
        tidytable::mutate(feature_id = harmonize_names_sirius(id)) |>
        select_sirius_columns()
      rm(compound)

      formulas_prepared <- formula |>
        select_sirius_columns_2()
      rm(formula)

      compounds_prepared <-
        tidytable::bind_rows(compound_prepared, compound_prepared_2) |>
        tidytable::distinct()
      rm(compound_prepared, compound_prepared_2)

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
      tidytable::filter(!is.na(!!as.name(model$features_columns[1]))) |>
      tidytable::distinct()

    table_for <- table |>
      tidytable::select(tidytable::any_of(c(
        model$features_columns,
        model$candidates_sirius_for_columns
      ))) |>
      tidytable::filter(!is.na(!!as.name(model$features_columns[1]))) |>
      tidytable::distinct()

    table_str <- table |>
      tidytable::select(tidytable::any_of(c(
        model$features_columns,
        model$candidates_structures_columns,
        model$candidates_spectra_columns,
        model$candidates_sirius_str_columns
      ))) |>
      tidytable::filter(!is.na(!!as.name(model$features_columns[1]))) |>
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
