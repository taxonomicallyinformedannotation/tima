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
    str_tax_cla = get_params(
      step = "prepare_annotations_sirius"
    )$files$libraries$sop$merged$structures$taxonomies$cla,
    str_tax_npc = get_params(
      step = "prepare_annotations_sirius"
    )$files$libraries$sop$merged$structures$taxonomies$npc,
    max_analog_abs_mz_error = get_params(
      step = "prepare_annotations_sirius"
    )$tools$sirius$max_analog_abs_mz_error
  ) {
    ctx <- log_operation("prepare_annotations_sirius", version = sirius_version)

    # Validation ----
    sirius_version <- as.character(sirius_version)
    validate_sirius_inputs(
      sirius_version = sirius_version,
      output_ann = output_ann,
      output_can = output_can,
      output_for = output_for,
      str_stereo = str_stereo,
      str_met = str_met,
      str_tax_cla = str_tax_cla,
      str_tax_npc = str_tax_npc,
      max_analog_abs_mz_error = max_analog_abs_mz_error
    )

    log_debug("SIRIUS version: %s", sirius_version)
    # Handle missing input ----
    if (is.null(input_directory)) {
      input_directory <- "Th1sd1rw0nt3x1st"
    }
    log_debug("SIRIUS directory: %s", input_directory)
    if (!file.exists(input_directory)) {
      log_warn(
        "SIRIUS input directory does not exist; returning empty template"
      )
      table <- create_empty_sirius_annotations()
    } else {
      # Load SIRIUS results ----
      tables <- load_sirius_tables(input_directory, version = sirius_version)
      summaries <- load_sirius_summaries(input_directory)
      # Prepare tables ----
      log_debug(
        "Preparing CANOPUS, formulas, structures for version %s",
        sirius_version
      )
      canopus_prepared <- tables$canopus |>
        select_sirius_columns_canopus(sirius_version = sirius_version)
      formulas_prepared <- tables$formulas |>
        select_sirius_columns_formulas(sirius_version = sirius_version)
      structures_prepared_summary <- summaries |>
        select_sirius_columns_structures(sirius_version = sirius_version)
      structures_prepared_main <- tables$structures |>
        tidytable::mutate(
          feature_id = switch(
            sirius_version,
            "5" = harmonize_names_sirius(id),
            "6" = mappingFeatureId
          )
        ) |>
        select_sirius_columns_structures(sirius_version = sirius_version)
      structures_prepared <- tidytable::bind_rows(
        structures_prepared_summary,
        structures_prepared_main
      ) |>
        tidytable::distinct()
      denovo_prepared <- tables$denovo |>
        tidytable::mutate(feature_id = mappingFeatureId) |>
        select_sirius_columns_structures(sirius_version = sirius_version)
      spectral_prepared <- tables$spectral |>
        select_sirius_columns_spectral(sirius_version = sirius_version)
      log_debug("Joining SIRIUS results")

      structures_enriched <- join_sirius_annotation_tables(
        structures_prepared = structures_prepared,
        formulas_prepared = formulas_prepared,
        canopus_prepared = canopus_prepared,
        denovo_prepared = denovo_prepared
      )

      merged_structures <- merge_sirius_structures_with_spectral(
        structures_enriched,
        spectral_prepared,
        max_analog_abs_mz_error = max_analog_abs_mz_error
      )
      table <- merged_structures |>
        tidytable::mutate(
          candidate_structure_tax_cla_chemontid = NA_character_,
          candidate_structure_tax_cla_01kin = NA_character_
        )
      rm(
        canopus_prepared,
        formulas_prepared,
        structures_prepared,
        structures_enriched,
        merged_structures,
        denovo_prepared,
        spectral_prepared,
        tables,
        summaries
      )
      log_debug("Selecting annotation columns and integrating metadata")
      table <- table |>
        select_annotations_columns(
          str_stereo = str_stereo,
          str_met = str_met,
          str_tax_cla = str_tax_cla,
          str_tax_npc = str_tax_npc
        )
    }
    # Split and export ----
    log_debug(
      "Splitting results into CANOPUS, formula, and structure tables"
    )
    splits <- split_sirius_results(table)

    log_complete(
      ctx,
      n_canopus = nrow(splits$canopus),
      n_formulas = nrow(splits$formula),
      n_structures = nrow(splits$structures)
    )

    export_params(
      parameters = get_params(step = "prepare_annotations_sirius"),
      step = "prepare_annotations_sirius"
    )
    export_output(x = splits$canopus, file = output_can)
    export_output(x = splits$formula, file = output_for)
    export_output(x = splits$structures, file = output_ann[[1L]])

    invisible(c(
      "canopus" = output_can,
      "formula" = output_for,
      "structural" = output_ann[[1L]]
    ))
  }

read_sirius_internal_file <- function(input_directory, internal_file) {
  from_zip <- tryCatch(
    {
      archive::archive_read(archive = input_directory, file = internal_file) |>
        utils::read.delim(
          quote = "",
          na.strings = c("", "NA"),
          colClasses = "character",
          stringsAsFactors = FALSE
        ) |>
        tidytable::tidytable()
    },
    error = function(e) NULL
  )

  if (!is.null(from_zip)) {
    return(from_zip)
  }

  path <- file.path(input_directory, internal_file)
  if (!file.exists(path)) {
    return(tidytable::tidytable())
  }

  utils::read.delim(
    file = path,
    quote = "",
    na.strings = c("", "NA"),
    colClasses = "character",
    stringsAsFactors = FALSE
  ) |>
    tidytable::tidytable()
}
