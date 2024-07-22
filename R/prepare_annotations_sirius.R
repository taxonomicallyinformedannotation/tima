import::from(tidytable, any_of, .into = environment())
import::from(tidytable, bind_rows, .into = environment())
import::from(tidytable, distinct, .into = environment())
import::from(tidytable, filter, .into = environment())
import::from(tidytable, left_join, .into = environment())
import::from(tidytable, mutate, .into = environment())
import::from(tidytable, select, .into = environment())
import::from(tidytable, tidytable, .into = environment())

#' @title Prepare annotations SIRIUS
#'
#' @description This function prepares Sirius results to make them compatible
#'
#' @importFrom tidytable any_of
#' @importFrom tidytable bind_rows
#' @importFrom tidytable distinct
#' @importFrom tidytable filter
#' @importFrom tidytable left_join
#' @importFrom tidytable mutate
#' @importFrom tidytable select
#' @importFrom tidytable tidytable
#'
#' @include columns_model.R
#' @include get_params.R
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
#' @param sirius_version Sirius version
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
           sirius_version = get_params(step = "prepare_annotations_sirius")$tools$sirius$version,
           str_stereo = get_params(step = "prepare_annotations_sirius")$files$libraries$sop$merged$structures$stereo,
           str_met = get_params(step = "prepare_annotations_sirius")$files$libraries$sop$merged$structures$metadata,
           str_nam = get_params(step = "prepare_annotations_sirius")$files$libraries$sop$merged$structures$names,
           str_tax_cla = get_params(step = "prepare_annotations_sirius")$files$libraries$sop$merged$structures$taxonomies$cla,
           str_tax_npc = get_params(step = "prepare_annotations_sirius")$files$libraries$sop$merged$structures$taxonomies$npc) {
    if (is.null(input_directory)) {
      input_directory <- "Th1sd1rw0nt3x1st"
    }
    if (file.exists(input_directory)) {
      log_debug("Loading parameters for SIRIUS", sirius_version)
      sirius_version <- as.character(sirius_version)
      canopus_filename <- switch(sirius_version,
        "5" = "canopus_compound_summary.tsv",
        "6" = "canopus_structure_summary.tsv"
      )
      formulas_filename <- switch(sirius_version,
        "5" = "formula_identifications_all.tsv",
        "6" = "formula_identifications_all.tsv"
      )
      structures_filename <- switch(sirius_version,
        "5" = "compound_identifications_all.tsv",
        "6" = "structure_identifications_all.tsv"
      )
      denovo_filename <- switch(sirius_version,
        "5" = NULL,
        "6" = "denovo_structure_identifications_all.tsv"
      )
      spectral_filename <- switch(sirius_version,
        "5" = NULL,
        "6" = "spectral_matches_all.tsv"
      )

      log_debug("Loading and formatting SIRIUS results")
      canopus <- input_directory |>
        read_from_sirius_zip(file = canopus_filename)

      formulas <- input_directory |>
        read_from_sirius_zip(file = formulas_filename)

      structures <- input_directory |>
        read_from_sirius_zip(file = structures_filename)

      # TODO
      if (!is.null(denovo_filename)) {
        denovo <- input_directory |>
          read_from_sirius_zip(file = denovo_filename)
      } else {
        denovo <- tidytable() |>
          mutate(mappingFeatureId = NA)
      }

      # TODO
      if (!is.null(spectral_filename)) {
        spectral <- input_directory |>
          read_from_sirius_zip(file = spectral_filename)
      }

      # dirty to support old zip
      list <- tryCatch(
        expr = {
          unzip(input_directory, list = TRUE)
        },
        error = function(err) {
          list <- list()
          list$Name <- list.files(input_directory)
          return(list)
        }
      )
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

      structures_summary <- lapply(
        X = summary_files,
        FUN = read_from_sirius_zip,
        sirius_zip = input_directory
      )

      names(structures_summary) <- summary_files |>
        pre_harmonize_names_sirius() |>
        harmonize_names_sirius()

      structures_summary <-
        structures_summary[lapply(structures_summary, nrow) > 0]

      # Allow for summaries only
      if (length(structures_summary) != 0) {
        structures_summary_ready <- structures_summary |>
          bind_rows(.id = "feature_id")
      } else {
        structures_summary_ready <- tidytable()
      }
      rm(structures_summary)

      canopus_prepared <- canopus |>
        select_sirius_columns_canopus(sirius_version = sirius_version)

      rm(canopus)

      formulas_prepared <- formulas |>
        select_sirius_columns_formulas(sirius_version = sirius_version)
      rm(formulas)

      structures_prepared <- structures_summary_ready |>
        select_sirius_columns_structures(sirius_version = sirius_version)
      rm(structures_summary_ready)

      structures_prepared_2 <- structures |>
        mutate(feature_id = switch(sirius_version,
          "5" = harmonize_names_sirius(id),
          "6" = mappingFeatureId
        )) |>
        select_sirius_columns_structures(sirius_version = sirius_version)
      rm(structures)

      structures_prepared <-
        bind_rows(structures_prepared, structures_prepared_2) |>
        distinct()
      rm(structures_prepared_2)

      denovo_prepared <- denovo |>
        mutate(feature_id = mappingFeatureId) |>
        select_sirius_columns_structures(sirius_version = sirius_version)

      table <- structures_prepared |>
        left_join(formulas_prepared) |>
        left_join(canopus_prepared) |>
        left_join(denovo_prepared) |>
        # TODO add spectral
        distinct() |>
        mutate(
          candidate_structure_tax_cla_chemontid = NA,
          candidate_structure_tax_cla_01kin = NA
        ) |>
        select_annotations_columns()
      rm(
        structures_prepared,
        formulas_prepared,
        canopus_prepared,
        denovo_prepared
      )
    } else {
      log_debug("Sorry, your input directory does not exist,
                returning an empty file instead")
      table <- fake_annotations_columns() |>
        mutate(
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
          candidate_score_sirius_csi = NA,
          candidate_score_sirius_msnovelist = NA
        ) |>
        select(
          -candidate_structure_error_rt,
          -candidate_score_similarity,
          -candidate_count_similarity_peaks_matched
        )
    }
    log_debug("Splitting SIRIUS results")
    model <- columns_model()

    table_can <- table |>
      select(any_of(
        c(model$features_columns, model$features_calculated_columns)
      )) |>
      filter(!is.na(!!as.name(model$features_columns[1]))) |>
      distinct()

    table_for <- table |>
      select(any_of(
        c(
          model$features_columns,
          model$candidates_sirius_for_columns
        )
      )) |>
      filter(!is.na(!!as.name(model$features_columns[1]))) |>
      distinct()

    table_str <- table |>
      select(any_of(
        c(
          model$features_columns,
          model$candidates_structures_columns,
          model$candidates_spectra_columns,
          model$candidates_sirius_str_columns
        )
      )) |>
      filter(!is.na(!!as.name(model$features_columns[1]))) |>
      distinct()
    rm(table)

    log_debug(x = "Exporting ...")
    export_params(
      parameters = get_params(step = "prepare_annotations_sirius"),
      step = "prepare_annotations_sirius"
    )
    export_output(x = table_can, file = output_can)
    export_output(x = table_for, file = output_for)
    export_output(x = table_str, file = output_ann[[1]])

    rm(table_can, table_for, table_str)
    return(c(
      "canopus" = output_can,
      "formula" = output_for,
      "structural" = output_ann[[1]]
    ))
  }
