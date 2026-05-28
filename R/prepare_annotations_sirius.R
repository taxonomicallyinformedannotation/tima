#' @title Prepare annotations SIRIUS
#'
#' @description Prepares SIRIUS annotation results (structure predictions,
#'     CANOPUS chemical classifications, and formula predictions) by harmonizing
#'     formats across SIRIUS versions (v5/v6), standardizing column names, and
#'     integrating with structure metadata.
#'
#' @details
#' This function:
#' \itemize{
#'   \item Validates inputs (version, paths, file existence).
#' \item Loads SIRIUS output files (CANOPUS, formulas, structures, denovo,
#'     spectral matches).
#'   \item Harmonizes column names across SIRIUS v5 and v6.
#'   \item Joins with structure metadata (stereochemistry, names, taxonomy).
#' \item Splits results into three output files: annotations, CANOPUS, formulas.
#'   \item Exports parameters and results.
#' }
#'
#' If the input directory does not exist, returns an empty template with
#'     expected
#' columns to ensure downstream compatibility.
#'
#' @include columns_utils.R
#' @include get_params.R
#' @include harmonize_names_sirius.R
#' @include pre_harmonize_names_sirius.R
#' @include predicates_utils.R
#' @include read_from_sirius_zip.R
#' @include select_annotations_columns.R
#' @include select_sirius_columns.R
#'
#' @param input_directory [character] Character path to directory or zip file
#'     containing
#'     SIRIUS results.
#' @param output_ann [character] Character path for prepared structure
#'     annotation output.
#' @param output_can [character] Character path for prepared CANOPUS output.
#' @param output_for [character] Character path for prepared formula output.
#' @param sirius_version [character] Character SIRIUS version ("5" or "6").
#' @param str_stereo [character] Character path to structure stereochemistry
#'     file.
#' @param str_met [character] Character path to structure metadata file.
#' @param str_tax_cla [character] Character path to ClassyFire taxonomy file.
#' @param str_tax_npc [character] Character path to NPClassifier taxonomy file.
#' @param max_analog_abs_mz_error [numeric] Maximum allowed absolute m/z
#'     deviation (Da) for keeping SIRIUS spectral analog hits.
#'
#' @return Character path to the prepared SIRIUS annotations file (invisible).
#'
#' @family preparation
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
    input_directory <- .normalize_sirius_input_directory(input_directory)
    log_debug("SIRIUS directory: %s", input_directory)
    if (!file.exists(input_directory)) {
      log_warn(
        "SIRIUS input directory does not exist; returning empty template"
      )
      table <- create_empty_sirius_annotations()
    } else {
      table <- .prepare_sirius_annotations_table(
        input_directory = input_directory,
        sirius_version = sirius_version,
        max_analog_abs_mz_error = max_analog_abs_mz_error,
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

    .export_sirius_outputs(
      splits = splits,
      output_can = output_can,
      output_for = output_for,
      output_ann = output_ann
    )

    invisible(c(
      "canopus" = output_can,
      "formula" = output_for,
      "structural" = output_ann[[1L]]
    ))
  }

.normalize_sirius_input_directory <- function(input_directory) {
  if (is.null(input_directory)) {
    return("Th1sd1rw0nt3x1st")
  }
  input_directory
}

.prepare_sirius_annotations_table <- function(
  input_directory,
  sirius_version,
  max_analog_abs_mz_error,
  str_stereo,
  str_met,
  str_tax_cla,
  str_tax_npc
) {
  tables <- load_sirius_tables(input_directory, version = sirius_version)
  summaries <- load_sirius_summaries(input_directory)

  log_debug(
    "Preparing CANOPUS, formulas, structures for version %s",
    sirius_version
  )

  prepared <- .prepare_sirius_table_components(
    tables = tables,
    summaries = summaries,
    sirius_version = sirius_version
  )

  log_debug("Joining SIRIUS results")
  structures_enriched <- join_sirius_annotation_tables(
    structures_prepared = prepared$structures,
    formulas_prepared = prepared$formulas,
    canopus_prepared = prepared$canopus,
    denovo_prepared = prepared$denovo
  )

  merged_structures <- merge_sirius_structures_with_spectral(
    structures_enriched,
    prepared$spectral,
    max_analog_abs_mz_error = max_analog_abs_mz_error
  )

  table <- merged_structures |>
    tidytable::mutate(
      candidate_structure_tax_cla_chemontid = NA_character_,
      candidate_structure_tax_cla_01kin = NA_character_
    )

  log_debug("Selecting annotation columns and integrating metadata")
  select_annotations_columns(
    table,
    str_stereo = str_stereo,
    str_met = str_met,
    str_tax_cla = str_tax_cla,
    str_tax_npc = str_tax_npc
  )
}

.prepare_sirius_table_components <- function(
  tables,
  summaries,
  sirius_version
) {
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

  list(
    canopus = canopus_prepared,
    formulas = formulas_prepared,
    structures = structures_prepared,
    denovo = denovo_prepared,
    spectral = spectral_prepared
  )
}

.export_sirius_outputs <- function(splits, output_can, output_for, output_ann) {
  export_params(
    parameters = get_params(step = "prepare_annotations_sirius"),
    step = "prepare_annotations_sirius"
  )
  export_output(x = splits$canopus, file = output_can)
  export_output(x = splits$formula, file = output_for)
  export_output(x = splits$structures, file = output_ann[[1L]])
}

.read_sirius_tabular <- function(con) {
  utils::read.delim(
    con,
    quote = "",
    na.strings = c("", "NA"),
    colClasses = "character",
    stringsAsFactors = FALSE
  ) |>
    tidytable::tidytable()
}

read_sirius_internal_file <- function(input_directory, internal_file) {
  from_zip <- tryCatch(
    {
      archive::archive_read(archive = input_directory, file = internal_file) |>
        .read_sirius_tabular()
    },
    error = function(e) {
      invisible(e)
      NULL
    }
  )

  if (!is.null(from_zip)) {
    return(from_zip)
  }

  path <- file.path(input_directory, internal_file)
  if (!file.exists(path)) {
    return(tidytable::tidytable())
  }

  .read_sirius_tabular(path)
}
