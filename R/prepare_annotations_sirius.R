#' @title Prepare annotations SIRIUS
#'
#' @description This function prepares SIRIUS annotation results (structure
#'     predictions, CANOPUS chemical classifications, and formula predictions)
#'     by harmonizing formats across SIRIUS versions (v5/v6), standardizing
#'     column names, and integrating with structure metadata.
#'
#' @include columns_model.R
#' @include get_params.R
#' @include harmonize_names_sirius.R
#' @include pre_harmonize_names_sirius.R
#' @include read_from_sirius_zip.R
#' @include select_annotations_columns.R
#' @include select_sirius_columns.R
#'
#' @param input_directory Character string path to directory or zip file
#'     containing SIRIUS results
#' @param output_ann Character string path for prepared annotation results output
#' @param output_can Character string path for prepared CANOPUS results output
#' @param output_for Character string path for prepared formula results output
#' @param sirius_version Character string SIRIUS version ("v5" or "v6")
#' @param str_stereo Character string path to file with structure stereochemistry
#' @param str_met Character string path to file with structure metadata
#' @param str_nam Character string path to file with structure names
#' @param str_tax_cla Character string path to file with ClassyFire taxonomy
#' @param str_tax_npc Character string path to file with NPClassifier taxonomy
#'
#' @return Character string path to the prepared SIRIUS annotations file
#'
#' @export
#'
#' @examples
#' \dontrun{
#' copy_backbone()
#' go_to_cache()
#' prepare_annotations_sirius()
#' unlink("data", recursive = TRUE)
#' }
prepare_annotations_sirius <-
  function(
    input_directory = get_params(
      step = "prepare_annotations_sirius"
    )$files$annotations$raw$sirius,
    output_ann = get_params(
      step = "prepare_annotations_sirius"
    )$files$annotations$prepared$structural$sirius,
    output_can = get_params(
      step = "prepare_annotations_sirius"
    )$files$annotations$prepared$canopus,
    output_for = get_params(
      step = "prepare_annotations_sirius"
    )$files$annotations$prepared$formula,
    sirius_version = get_params(
      step = "prepare_annotations_sirius"
    )$tools$sirius$version,
    str_stereo = get_params(
      step = "prepare_annotations_sirius"
    )$files$libraries$sop$merged$structures$stereo,
    str_met = get_params(
      step = "prepare_annotations_sirius"
    )$files$libraries$sop$merged$structures$metadata,
    str_nam = get_params(
      step = "prepare_annotations_sirius"
    )$files$libraries$sop$merged$structures$names,
    str_tax_cla = get_params(
      step = "prepare_annotations_sirius"
    )$files$libraries$sop$merged$structures$taxonomies$cla,
    str_tax_npc = get_params(
      step = "prepare_annotations_sirius"
    )$files$libraries$sop$merged$structures$taxonomies$npc
  ) {
    # Validate SIRIUS version
    if (!sirius_version %in% c("5", "6", 5, 6)) {
      stop("sirius_version must be '5' or '6', got: ", sirius_version)
    }

    # Validate output paths
    output_paths <- list(
      output_ann = output_ann,
      output_can = output_can,
      output_for = output_for
    )

    for (param_name in names(output_paths)) {
      param_value <- output_paths[[param_name]]
      if (!is.character(param_value) || length(param_value) != 1L) {
        stop(param_name, " must be a single character string")
      }
    }

    # Validate structure file paths
    str_files <- list(
      str_stereo = str_stereo,
      str_met = str_met,
      str_nam = str_nam,
      str_tax_cla = str_tax_cla,
      str_tax_npc = str_tax_npc
    )

    for (param_name in names(str_files)) {
      param_value <- str_files[[param_name]]
      if (!is.character(param_value) || length(param_value) != 1L) {
        stop(param_name, " must be a single character string")
      }
      if (!file.exists(param_value)) {
        stop(param_name, " file not found: ", param_value)
      }
    }

    logger::log_info("Preparing SIRIUS {sirius_version} annotations")

    # Handle NULL or missing input directory
    if (is.null(input_directory)) {
      input_directory <- "Th1sd1rw0nt3x1st"
    }

    logger::log_debug("SIRIUS directory: {input_directory}")

    if (file.exists(input_directory)) {
      logger::log_trace("Loading SIRIUS results...")
      sirius_version <- as.character(sirius_version)
      canopus_filename <- switch(
        sirius_version,
        "5" = "canopus_compound_summary.tsv",
        "6" = "canopus_formula_summary_all.tsv"
      )
      formulas_filename <- switch(
        sirius_version,
        "5" = "formula_identifications_all.tsv",
        "6" = "formula_identifications_all.tsv"
      )
      structures_filename <- switch(
        sirius_version,
        "5" = "compound_identifications_all.tsv",
        "6" = "structure_identifications_all.tsv"
      )
      denovo_filename <- switch(
        sirius_version,
        "5" = NULL,
        "6" = "denovo_structure_identifications_all.tsv"
      )
      spectral_filename <- switch(
        sirius_version,
        "5" = NULL,
        "6" = "spectral_matches_all.tsv"
      )

      logger::log_trace("Loading and formatting SIRIUS results...")
      canopus <- input_directory |>
        read_from_sirius_zip(file = canopus_filename)
      # not available in previous SIRIUS version
      if (sirius_version == "6") {
        canopus <- canopus |>
          tidytable::filter(formulaRank == 1L)
      }
      logger::log_trace("... CANOPUS loaded")

      formulas <- input_directory |>
        read_from_sirius_zip(file = formulas_filename)
      # not available in previous SIRIUS version
      if (sirius_version == "6") {
        formulas <- formulas |>
          tidytable::filter(formulaRank == 1L)
      }
      logger::log_trace("... formulas loaded")

      structures <- input_directory |>
        read_from_sirius_zip(file = structures_filename)
      logger::log_trace("... structures loaded")

      files <- input_directory |>
        utils::unzip(list = TRUE)

      denovo <- tidytable::tidytable() |>
        tidytable::mutate(mappingFeatureId = NA)
      if (!is.null(denovo_filename)) {
        if (
          grepl(pattern = denovo_filename, x = files$Name) |>
            any()
        ) {
          denovo <- input_directory |>
            read_from_sirius_zip(file = denovo_filename)
        }
      }
      logger::log_trace("... de novo loaded")

      # TODO
      # if (!is.null(spectral_filename)) {
      #   if (grepl(pattern = spectral_filename, x = files$Name) |>
      #     any()) {
      #     spectral <- input_directory |>
      #       read_from_sirius_zip(file = spectral_filename)
      #   }
      # }

      # dirty to support old zip
      list <- tryCatch(
        expr = {
          utils::unzip(input_directory, list = TRUE)
        },
        error = function(e) {
          list <- list()
          list$Name <- list.files(input_directory)
          return(list)
        }
      )
      summary_files <- list$Name[
        list$Name |>
          grepl(
            pattern = "structure_candidates.tsv",
            fixed = TRUE
          )
      ] |>
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

      structures_summary <- purrr::map(
        .x = summary_files,
        .f = read_from_sirius_zip,
        sirius_zip = input_directory
      )

      names(structures_summary) <- summary_files |>
        pre_harmonize_names_sirius() |>
        harmonize_names_sirius()

      structures_summary <-
        structures_summary[purrr::map(.x = structures_summary, .f = nrow) > 0]

      # Allow for summaries only
      if (length(structures_summary) != 0) {
        structures_summary_ready <- structures_summary |>
          tidytable::bind_rows(.id = "feature_id")
      } else {
        structures_summary_ready <- tidytable::tidytable()
      }
      rm(structures_summary)

      canopus_prepared <- canopus |>
        select_sirius_columns_canopus(sirius_version = sirius_version)
      rm(canopus)
      logger::log_trace("... CANOPUS prepared")

      formulas_prepared <- formulas |>
        select_sirius_columns_formulas(sirius_version = sirius_version)
      rm(formulas)
      logger::log_trace("... formulas prepared")

      structures_prepared <- structures_summary_ready |>
        select_sirius_columns_structures(sirius_version = sirius_version)
      rm(structures_summary_ready)

      structures_prepared_2 <- structures |>
        tidytable::mutate(
          feature_id = switch(
            sirius_version,
            "5" = harmonize_names_sirius(id),
            "6" = mappingFeatureId
          )
        ) |>
        select_sirius_columns_structures(sirius_version = sirius_version)
      rm(structures)

      structures_prepared <-
        tidytable::bind_rows(structures_prepared, structures_prepared_2) |>
        tidytable::distinct()
      rm(structures_prepared_2)
      logger::log_trace("... structures prepared")

      denovo_prepared <- denovo |>
        tidytable::mutate(feature_id = mappingFeatureId) |>
        select_sirius_columns_structures(sirius_version = sirius_version)
      logger::log_trace("... denovo prepared")

      # TODO add spectral
      supp_tables <- list(formulas_prepared, canopus_prepared, denovo_prepared)
      table <- purrr::reduce(
        .x = supp_tables,
        .init = structures_prepared,
        .f = function(x, y) {
          tidytable::left_join(x, y)
        }
      )
      logger::log_trace("Everything joined together")
      table <- table |>
        tidytable::distinct() |>
        tidytable::mutate(
          candidate_structure_tax_cla_chemontid = NA,
          candidate_structure_tax_cla_01kin = NA
        )
      table <- table |>
        select_annotations_columns()
      rm(
        structures_prepared,
        formulas_prepared,
        canopus_prepared,
        denovo_prepared
      )
    } else {
      logger::log_warn(
        "Sorry, your input directory does not exist,
                returning an empty file instead"
      )
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
          candidate_score_sirius_csi = NA,
          candidate_score_sirius_msnovelist = NA
        ) |>
        tidytable::select(
          -candidate_structure_error_rt,
          -candidate_score_similarity,
          -candidate_count_similarity_peaks_matched
        )
    }
    logger::log_trace("Splitting SIRIUS results")
    model <- columns_model()

    table_can <- table |>
      tidytable::select(tidyselect::any_of(
        c(model$features_columns, model$features_calculated_columns)
      )) |>
      tidytable::filter(
        !is.na(!!as.name(model$features_calculated_columns[5]))
      ) |>
      tidytable::filter(!is.na(!!as.name(model$features_columns[1]))) |>
      tidytable::distinct()

    table_for <- table |>
      tidytable::select(tidyselect::any_of(
        c(
          model$features_columns,
          model$candidates_sirius_for_columns
        )
      )) |>
      tidytable::filter(
        !is.na(!!as.name(model$candidates_sirius_for_columns[2]))
      ) |>
      tidytable::filter(!is.na(!!as.name(model$features_columns[1]))) |>
      tidytable::distinct()

    table_str <- table |>
      tidytable::select(tidyselect::any_of(
        c(
          model$features_columns,
          model$candidates_structures_columns,
          model$candidates_spectra_columns,
          model$candidates_sirius_str_columns
        )
      )) |>
      tidytable::filter(!is.na(!!as.name(model$features_columns[1]))) |>
      tidytable::distinct()
    rm(table)

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
