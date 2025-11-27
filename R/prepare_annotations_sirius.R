#' Validate Inputs for prepare_annotations_sirius
#'
#' @description Internal helper to validate inputs for prepare_annotations_sirius.
#'     Checks version, output paths, and structure file existence.
#'
#' @param sirius_version Character SIRIUS version ("5", "6", or coercible to those).
#' @param output_ann Character output path for annotations.
#' @param output_can Character output path for CANOPUS.
#' @param output_for Character output path for formulas.
#' @param str_stereo Character path to stereo file.
#' @param str_met Character path to metadata file.
#' @param str_nam Character path to names file.
#' @param str_tax_cla Character path to ClassyFire taxonomy.
#' @param str_tax_npc Character path to NPClassifier taxonomy.
#'
#' @return NULL (invisible). Stops on validation failure.
#' @keywords internal
validate_sirius_inputs <- function(
  sirius_version,
  output_ann,
  output_can,
  output_for,
  str_stereo,
  str_met,
  str_nam,
  str_tax_cla,
  str_tax_npc
) {
  if (!sirius_version %in% c("5", "6", 5, 6)) {
    stop(
      "sirius_version must be '5' or '6', got: ",
      sirius_version,
      call. = FALSE
    )
  }
  output_paths <- list(
    output_ann = output_ann,
    output_can = output_can,
    output_for = output_for
  )
  for (nm in names(output_paths)) {
    val <- output_paths[[nm]]
    if (!is.character(val) || length(val) != 1L) {
      stop(nm, " must be a single character string", call. = FALSE)
    }
  }
  str_files <- list(
    str_stereo = str_stereo,
    str_met = str_met,
    str_nam = str_nam,
    str_tax_cla = str_tax_cla,
    str_tax_npc = str_tax_npc
  )
  missing <- purrr::keep(str_files, ~ !file.exists(.x))
  if (length(missing) > 0L) {
    stop(
      "Structure file(s) not found: ",
      paste(names(missing), missing, sep = " at ", collapse = "; "),
      call. = FALSE
    )
  }
  invisible(NULL)
}

#' Get SIRIUS Version-Specific Filenames
#'
#' @description Internal helper returning a list of expected filenames for a
#'     given SIRIUS version (v5 vs v6).
#'
#' @param version Character "5" or "6".
#'
#' @return Named list with keys: canopus, formulas, structures, denovo, spectral.
#' @keywords internal
get_sirius_filenames <- function(version) {
  if (version == "5") {
    list(
      canopus = "canopus_compound_summary.tsv",
      formulas = "formula_identifications_all.tsv",
      structures = "compound_identifications_all.tsv",
      denovo = NULL,
      spectral = NULL
    )
  } else {
    list(
      canopus = "canopus_formula_summary_all.tsv",
      formulas = "formula_identifications_all.tsv",
      structures = "structure_identifications_all.tsv",
      denovo = "denovo_structure_identifications_all.tsv",
      spectral = "spectral_matches_all.tsv"
    )
  }
}

#' Load SIRIUS Result Tables
#'
#' @description Internal helper to load CANOPUS, formulas, structures, and
#'     denovo tables from a SIRIUS output directory/zip.
#'
#' @param input_directory Character path to SIRIUS output (directory or zip).
#' @param version Character "5" or "6".
#'
#' @return Named list with canopus, formulas, structures, denovo data frames.
#' @keywords internal
load_sirius_tables <- function(input_directory, version) {
  fnames <- get_sirius_filenames(version)
  logger::log_debug("Loading SIRIUS tables (version {version})")
  canopus <- read_from_sirius_zip(input_directory, file = fnames$canopus)
  if (version == "6" && nrow(canopus) > 0L) {
    canopus <- canopus |> tidytable::filter(formulaRank == 1L)
  }
  formulas <- read_from_sirius_zip(input_directory, file = fnames$formulas)
  if (version == "6" && nrow(formulas) > 0L) {
    formulas <- formulas |> tidytable::filter(formulaRank == 1L)
  }
  structures <- read_from_sirius_zip(input_directory, file = fnames$structures)
  denovo <- tidytable::tidytable(mappingFeatureId = NA_character_)
  if (!is.null(fnames$denovo)) {
    files <- tryCatch(
      utils::unzip(input_directory, list = TRUE),
      error = function(e) list(Name = list.files(input_directory))
    )
    if (any(grepl(fnames$denovo, files$Name, fixed = TRUE))) {
      denovo <- read_from_sirius_zip(input_directory, file = fnames$denovo)
    }
  }
  logger::log_debug(
    "Loaded SIRIUS tables: CANOPUS={nrow(canopus)}, formulas={nrow(formulas)}, structures={nrow(structures)}, denovo={nrow(denovo)} rows"
  )
  list(
    canopus = canopus,
    formulas = formulas,
    structures = structures,
    denovo = denovo
  )
}

#' Load SIRIUS Structure Summaries
#'
#' @description Internal helper to load per-feature structure candidate summaries.
#'
#' @param input_directory Character path to SIRIUS output.
#'
#' @return Data frame with structure summaries (bound rows), or empty tidytable.
#' @keywords internal
load_sirius_summaries <- function(input_directory) {
  zip_list <- tryCatch(
    utils::unzip(zipfile = input_directory, list = TRUE),
    error = function(e) {
      out <- list()
      out$Name <- list.files(input_directory)
      out
    }
  )
  summary_files <- zip_list$Name[grepl(
    "structure_candidates.tsv",
    zip_list$Name,
    fixed = TRUE
  )]
  if (length(summary_files) == 0L) {
    logger::log_debug("No structure candidate summaries found")
    return(tidytable::tidytable())
  }
  base_name <- basename(input_directory) |>
    gsub(pattern = ".zip", replacement = "", fixed = TRUE)
  summary_files <- summary_files |>
    gsub(pattern = base_name, replacement = "") |>
    gsub(pattern = "^/", replacement = "")
  logger::log_debug("Loading {length(summary_files)} structure summary files")
  summaries <- purrr::map(
    summary_files,
    ~ read_from_sirius_zip(input_directory, file = .x)
  )
  names(summaries) <- summary_files |>
    pre_harmonize_names_sirius() |>
    harmonize_names_sirius()
  summaries <- summaries[purrr::map_int(summaries, nrow) > 0L]
  if (length(summaries) == 0L) {
    return(tidytable::tidytable())
  }
  tidytable::bind_rows(summaries, .id = "feature_id")
}

#' Create Empty SIRIUS Annotations Template
#'
#' @description Internal helper returning an empty template with all expected
#'     SIRIUS-specific columns when no data is available.
#'
#' @return Data frame (tidytable) with SIRIUS-specific columns, 1 row of NAs.
#' @keywords internal
create_empty_sirius_annotations <- function() {
  fake_annotations_columns() |>
    tidytable::mutate(
      feature_pred_tax_cla_02sup_val = NA_character_,
      feature_pred_tax_cla_02sup_score = NA_real_,
      feature_pred_tax_cla_03cla_val = NA_character_,
      feature_pred_tax_cla_03cla_score = NA_real_,
      feature_pred_tax_cla_04dirpar_val = NA_character_,
      feature_pred_tax_cla_04dirpar_score = NA_real_,
      feature_pred_tax_npc_01pat_val = NA_character_,
      feature_pred_tax_npc_01pat_score = NA_real_,
      feature_pred_tax_npc_02sup_val = NA_character_,
      feature_pred_tax_npc_02sup_score = NA_real_,
      feature_pred_tax_npc_03cla_val = NA_character_,
      feature_pred_tax_npc_03cla_score = NA_real_,
      candidate_count_sirius_peaks_explained = NA_integer_,
      candidate_score_sirius_intensity = NA_real_,
      candidate_score_sirius_isotope = NA_real_,
      candidate_score_sirius_sirius = NA_real_,
      candidate_score_sirius_tree = NA_real_,
      candidate_score_sirius_zodiac = NA_real_,
      candidate_score_sirius_confidence = NA_real_,
      candidate_score_sirius_csi = NA_real_,
      candidate_score_sirius_msnovelist = NA_real_
    ) |>
    tidytable::select(
      -candidate_structure_error_rt,
      -candidate_score_similarity,
      -candidate_count_similarity_peaks_matched
    )
}

#' Split SIRIUS Results into Output Tables
#'
#' @description Internal helper to split combined SIRIUS table into CANOPUS,
#'     formula, and structure-specific tables based on column model.
#'
#' @include columns_utils.R
#'
#' @param table Data frame with combined SIRIUS results.
#'
#' @return Named list with keys: canopus, formula, structures.
#' @keywords internal
split_sirius_results <- function(table) {
  model <- columns_model()
  canopus <- table |>
    tidytable::select(
      tidyselect::any_of(c(
        model$features_columns,
        model$features_calculated_columns
      ))
    ) |>
    tidytable::filter(
      !is.na(!!as.name(model$features_calculated_columns[5]))
    ) |>
    tidytable::filter(!is.na(!!as.name(model$features_columns[1]))) |>
    tidytable::distinct()
  formula <- table |>
    tidytable::select(
      tidyselect::any_of(c(
        model$features_columns,
        model$candidates_sirius_for_columns
      ))
    ) |>
    tidytable::filter(
      !is.na(!!as.name(model$candidates_sirius_for_columns[2]))
    ) |>
    tidytable::filter(!is.na(!!as.name(model$features_columns[1]))) |>
    tidytable::distinct()
  structures <- table |>
    tidytable::select(
      tidyselect::any_of(c(
        model$features_columns,
        model$candidates_structures_columns,
        model$candidates_spectra_columns,
        model$candidates_sirius_str_columns
      ))
    ) |>
    tidytable::filter(!is.na(!!as.name(model$features_columns[1]))) |>
    tidytable::distinct()
  logger::log_debug(
    "Split results: CANOPUS={nrow(canopus)}, formulas={nrow(formula)}, structures={nrow(structures)} rows"
  )
  list(canopus = canopus, formula = formula, structures = structures)
}

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
#'   \item Loads SIRIUS output files (CANOPUS, formulas, structures, denovo).
#'   \item Harmonizes column names across SIRIUS v5 and v6.
#'   \item Joins with structure metadata (stereochemistry, names, taxonomy).
#'   \item Splits results into three output files: annotations, CANOPUS, formulas.
#'   \item Exports parameters and results.
#' }
#'
#' If the input directory does not exist, returns an empty template with expected
#' columns to ensure downstream compatibility.
#'
#' @include columns_utils.R
#' @include get_params.R
#' @include harmonize_names_sirius.R
#' @include pre_harmonize_names_sirius.R
#' @include read_from_sirius_zip.R
#' @include select_annotations_columns.R
#' @include select_sirius_columns.R
#'
#' @param input_directory Character path to directory or zip file containing
#'     SIRIUS results.
#' @param output_ann Character path for prepared structure annotation output.
#' @param output_can Character path for prepared CANOPUS output.
#' @param output_for Character path for prepared formula output.
#' @param sirius_version Character SIRIUS version ("5" or "6").
#' @param str_stereo Character path to structure stereochemistry file.
#' @param str_met Character path to structure metadata file.
#' @param str_nam Character path to structure names file.
#' @param str_tax_cla Character path to ClassyFire taxonomy file.
#' @param str_tax_npc Character path to NPClassifier taxonomy file.
#'
#' @return Character path to the prepared SIRIUS annotations file (invisible).
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
    # Validation ----
    sirius_version <- as.character(sirius_version)
    validate_sirius_inputs(
      sirius_version = sirius_version,
      output_ann = output_ann,
      output_can = output_can,
      output_for = output_for,
      str_stereo = str_stereo,
      str_met = str_met,
      str_nam = str_nam,
      str_tax_cla = str_tax_cla,
      str_tax_npc = str_tax_npc
    )
    logger::log_info("Preparing SIRIUS v{sirius_version} annotations")
    # Handle missing input ----
    if (is.null(input_directory)) {
      input_directory <- "Th1sd1rw0nt3x1st"
    }
    logger::log_debug("SIRIUS directory: {input_directory}")
    if (!file.exists(input_directory)) {
      logger::log_warn(
        "SIRIUS input directory does not exist; returning empty template"
      )
      table <- create_empty_sirius_annotations()
    } else {
      # Load SIRIUS results ----
      tables <- load_sirius_tables(input_directory, version = sirius_version)
      summaries <- load_sirius_summaries(input_directory)
      # Prepare tables ----
      logger::log_debug(
        "Preparing CANOPUS, formulas, structures for version {sirius_version}"
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
      logger::log_debug("Joining SIRIUS results")
      table <- purrr::reduce(
        .x = list(formulas_prepared, canopus_prepared, denovo_prepared),
        .init = structures_prepared,
        .f = tidytable::left_join
      ) |>
        tidytable::distinct() |>
        tidytable::mutate(
          candidate_structure_tax_cla_chemontid = NA_character_,
          candidate_structure_tax_cla_01kin = NA_character_
        )
      logger::log_debug("Selecting annotation columns and integrating metadata")
      table <- table |>
        select_annotations_columns(
          str_stereo = str_stereo,
          str_met = str_met,
          str_nam = str_nam,
          str_tax_cla = str_tax_cla,
          str_tax_npc = str_tax_npc
        )
    }
    # Split and export ----
    logger::log_debug(
      "Splitting results into CANOPUS, formula, and structure tables"
    )
    splits <- split_sirius_results(table)
    logger::log_info("Exporting SIRIUS results to {length(splits)} files")
    export_params(
      parameters = get_params(step = "prepare_annotations_sirius"),
      step = "prepare_annotations_sirius"
    )
    export_output(x = splits$canopus, file = output_can)
    export_output(x = splits$formula, file = output_for)
    export_output(x = splits$structures, file = output_ann[[1]])
    logger::log_success("SIRIUS annotations prepared successfully")
    invisible(c(
      "canopus" = output_can,
      "formula" = output_for,
      "structural" = output_ann[[1]]
    ))
  }
