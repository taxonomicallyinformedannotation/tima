#' Validate Inputs for prepare_annotations_sirius
#'
#' @description Internal helper to validate inputs for
#'     prepare_annotations_sirius.
#'     Checks version, output paths, and structure file existence.
#'
#' @param sirius_version [character] Character SIRIUS version ("5", "6", or
#'     coercible to those).
#' @param output_ann [character] Character output path for annotations.
#' @param output_can [character] Character output path for CANOPUS.
#' @param output_for [character] Character output path for formulas.
#' @param str_stereo [character] Character path to stereo file.
#' @param str_met [character] Character path to metadata file.
#' @param str_tax_cla [character] Character path to ClassyFire taxonomy.
#' @param str_tax_npc [character] Character path to NPClassifier taxonomy.
#' @param max_analog_abs_mz_error [numeric] Maximum allowed absolute m/z
#'     deviation (Da) for keeping SIRIUS spectral analog hits.
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
  str_tax_cla,
  str_tax_npc,
  max_analog_abs_mz_error
) {
  if (!sirius_version %in% c("5", "6", 5, 6)) {
    cli::cli_abort(
      "sirius_version must be '5' or '6', got {.val {sirius_version}}",
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }

  # Validate output paths
  output_paths <- list(
    output_ann = output_ann,
    output_can = output_can,
    output_for = output_for
  )

  validate_all_single_strings(output_paths, "Output path(s)")

  # Validate structure files exist
  validate_file_existence(
    file_list = list(
      str_stereo = str_stereo,
      str_met = str_met,
      str_tax_cla = str_tax_cla,
      str_tax_npc = str_tax_npc
    ),
    allow_null = FALSE
  )

  validate_numeric_range(
    value = max_analog_abs_mz_error,
    min_value = 0,
    max_value = Inf,
    param_name = "max_analog_abs_mz_error",
    context = paste(
      "Maximum absolute precursor m/z deviation for keeping",
      "SIRIUS spectral analog candidates"
    )
  )

  invisible(NULL)
}

#' Get SIRIUS Version-Specific Filenames
#'
#' @description Internal helper returning a list of expected filenames (v5) or
#'     regex patterns (v6) for matching SIRIUS output files. The v6 patterns
#'     accept any suffix after the base name (e.g. \code{_all.tsv},
#'     \code{-15.tsv}, \code{_analog_top-15.tsv}).
#'
#' @param version [character] Character "5" or "6".
#'
#' @return Named list with keys: canopus, formulas, structures, denovo,
#'     spectral.
#'     Values are fixed filenames (v5) or regex patterns (v6).
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
    # Patterns use regex to match any suffix after the base name
    # (e.g. _all.tsv, -15.tsv, _analog_top-15.tsv)
    list(
      canopus = "canopus_formula_summary[^/]*\\.tsv$",
      formulas = "(^|/)formula_identifications[^/]*\\.tsv$",
      structures = "(^|/)structure_identifications[^/]*\\.tsv$",
      denovo = "denovo_structure_identifications[^/]*\\.tsv$",
      spectral = "spectral_matches[^/]*\\.tsv$"
    )
  }
}

#' Load SIRIUS Result Tables
#'
#' @description Internal helper to load CANOPUS, formulas, structures, and
#'     denovo tables from a SIRIUS output directory/zip.
#'
#' @param input_directory [character] Character path to SIRIUS output (directory
#'     or zip).
#' @param version [character] Character "5" or "6".
#'
#' @return Named list with canopus, formulas, structures, denovo, spectral data
#'     frames.
#' @keywords internal
load_sirius_tables <- function(input_directory, version) {
  fnames <- get_sirius_filenames(version)
  log_debug("Loading SIRIUS tables (version %s)", version)

  canopus <- read_from_sirius_zip(input_directory, file = fnames$canopus)
  if (
    version == "6" && nrow(canopus) > 0L && "formulaRank" %in% names(canopus)
  ) {
    canopus <- canopus |>
      tidytable::mutate(
        .formula_rank = as.integer(formulaRank)
      ) |>
      tidytable::filter(!is.na(.formula_rank), .formula_rank == 1L) |>
      tidytable::select(-.formula_rank)
  }

  formulas <- read_from_sirius_zip(input_directory, file = fnames$formulas)
  if (
    version == "6" && nrow(formulas) > 0L && "formulaRank" %in% names(formulas)
  ) {
    formulas <- formulas |>
      tidytable::mutate(
        .formula_rank = as.integer(formulaRank)
      ) |>
      tidytable::filter(!is.na(.formula_rank), .formula_rank == 1L) |>
      tidytable::select(-.formula_rank)
  }

  structures <- read_from_sirius_zip(input_directory, file = fnames$structures)

  files <- tryCatch(
    utils::unzip(zipfile = input_directory, list = TRUE),
    error = function(e) {
      invisible(e)
      out <- list()
      out$Name <- list.files(input_directory, recursive = TRUE)
      out
    }
  )

  denovo <- tidytable::tidytable(mappingFeatureId = NA_character_)
  if (!is.null(fnames$denovo) && any(grepl(fnames$denovo, files$Name))) {
    denovo <- read_from_sirius_zip(input_directory, file = fnames$denovo)
  }

  spectral <- tidytable::tidytable(mappingFeatureId = NA_character_)
  if (!is.null(fnames$spectral)) {
    spectral_files <- files$Name[grepl(fnames$spectral, files$Name)]
    if (length(spectral_files) > 0L) {
      spectral <- purrr::map(
        spectral_files,
        ~ read_sirius_internal_file(input_directory, .x) |>
          tidytable::mutate(.sirius_source_file = .x)
      ) |>
        tidytable::bind_rows()
    }
  }

  log_debug(
    "Loaded SIRIUS tables: CANOPUS=%d, formulas=%d, structures=%d, denovo=%d, spectral=%d rows",
    nrow(canopus),
    nrow(formulas),
    nrow(structures),
    nrow(denovo),
    nrow(spectral)
  )

  list(
    canopus = canopus,
    formulas = formulas,
    structures = structures,
    denovo = denovo,
    spectral = spectral
  )
}

#' Load SIRIUS Structure Summaries
#'
#' @description Internal helper to load per-feature structure candidate
#'     summaries.
#'
#' @param input_directory [character] Character path to SIRIUS output.
#'
#' @return Data frame with structure summaries (bound rows), or empty tidytable.
#' @keywords internal
load_sirius_summaries <- function(input_directory) {
  zip_list <- tryCatch(
    utils::unzip(zipfile = input_directory, list = TRUE),
    error = function(e) {
      invisible(e)
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
    log_debug("No structure candidate summaries found")
    return(tidytable::tidytable())
  }
  base_name <- basename(input_directory) |>
    gsub(pattern = ".zip", replacement = "", fixed = TRUE)
  summary_files <- summary_files |>
    gsub(pattern = base_name, replacement = "") |>
    gsub(pattern = "^/", replacement = "")
  log_debug("Loading %d structure summary files", length(summary_files))
  summaries <- purrr::map(
    summary_files,
    ~ read_from_sirius_zip(input_directory, file = .x)
  )
  names(summaries) <- summary_files |>
    pre_harmonize_names_sirius() |>
    harmonize_names_sirius()
  summaries <- summaries[purrr::map_int(.x = summaries, .f = nrow) > 0L]
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
#' @param table [data.frame] Data frame with combined SIRIUS results.
#'
#' @return Named list with keys: canopus, formula, structures.
#' @keywords internal
split_sirius_results <- function(table) {
  model <- columns_model()
  canopus <- table |>
    tidytable::select(
      tidyselect::any_of(
        x = c(
          model$features_columns,
          model$features_calculated_columns
        )
      )
    ) |>
    tidytable::filter(
      !is.na(!!as.name(model$features_calculated_columns[5]))
    ) |>
    tidytable::filter(!is.na(!!as.name(model$features_columns[1]))) |>
    tidytable::distinct()
  formula <- table |>
    tidytable::select(
      tidyselect::any_of(
        x = c(
          model$features_columns,
          model$candidates_sirius_for_columns
        )
      )
    ) |>
    tidytable::filter(
      !is.na(!!as.name(model$candidates_sirius_for_columns[2]))
    ) |>
    tidytable::filter(!is.na(!!as.name(model$features_columns[1]))) |>
    tidytable::distinct()
  structures <- table |>
    tidytable::select(
      tidyselect::any_of(
        x = c(
          model$features_columns,
          model$candidates_structures_columns,
          model$candidates_spectra_columns,
          model$candidates_sirius_str_columns
        )
      )
    ) |>
    tidytable::filter(!is.na(!!as.name(model$features_columns[1]))) |>
    tidytable::distinct()
  log_debug(
    "Split results: CANOPUS=%d, formulas=%d, structures=%d rows",
    nrow(canopus),
    nrow(formula),
    nrow(structures)
  )
  list(canopus = canopus, formula = formula, structures = structures)
}

#' Merge SIRIUS structure candidates with spectral match scores
#'
#' @description Joins spectral match results into structure candidate rows by
#'     `feature_id` + `candidate_structure_smiles_no_stereo`.
#'     A candidate that appears in both gets all SIRIUS structure scores AND
#'     the spectral similarity score in a single row.  Spectral hits whose
#'     SMILES does not appear in the structure table are kept as standalone
#'     candidate rows.
#'
#' @param structures_prepared [data.frame] Standardized structure candidates.
#' @param spectral_prepared [data.frame] Standardized spectral-match candidates.
#'
#' @return data.frame with merged and unmatched rows combined.
#' @keywords internal
merge_sirius_structures_with_spectral <- function(
  structures_prepared,
  spectral_prepared,
  max_analog_abs_mz_error = 0.01
) {
  if (nrow(spectral_prepared) == 0L) {
    return(structures_prepared)
  }
  if (nrow(structures_prepared) == 0L) {
    return(spectral_prepared)
  }

  smiles_col <- "candidate_structure_smiles_no_stereo"
  if (
    !smiles_col %in% names(spectral_prepared) ||
      all(is.na(spectral_prepared[[smiles_col]]))
  ) {
    # No matchable spectral SMILES – nothing to join, keep spectral as-is
    return(
      tidytable::bind_rows(structures_prepared, spectral_prepared) |>
        tidytable::distinct()
    )
  }

  join_key <- c("feature_id", "candidate_structure_smiles_no_stereo")

  spectral_direct <- spectral_prepared |>
    tidytable::filter(candidate_library != "SIRIUS spectral (analog)")
  spectral_analogs <- spectral_prepared |>
    tidytable::filter(candidate_library == "SIRIUS spectral (analog)")

  if (is.finite(max_analog_abs_mz_error) && nrow(spectral_analogs) > 0L) {
    spectral_analogs <- spectral_analogs |>
      tidytable::mutate(
        .analog_abs_error_mz = abs(
          as.numeric(candidate_structure_error_mz)
        )
      ) |>
      tidytable::filter(
        !is.na(.analog_abs_error_mz),
        .analog_abs_error_mz <= max_analog_abs_mz_error
      ) |>
      tidytable::select(-.analog_abs_error_mz)
  }

  spectral_all <- tidytable::bind_rows(spectral_direct, spectral_analogs)

  if (!"candidate_count_similarity_peaks_matched" %in% names(spectral_all)) {
    spectral_all <- spectral_all |>
      tidytable::mutate(candidate_count_similarity_peaks_matched = NA_integer_)
  }
  if (!"candidate_spectrum_id" %in% names(spectral_all)) {
    spectral_all <- spectral_all |>
      tidytable::mutate(candidate_spectrum_id = NA_character_)
  }
  if (!"candidate_structure_name" %in% names(spectral_all)) {
    spectral_all <- spectral_all |>
      tidytable::mutate(candidate_structure_name = NA_character_)
  }

  spectral_typed <- spectral_all |>
    tidytable::mutate(
      candidate_score_similarity = as.numeric(candidate_score_similarity),
      candidate_count_similarity_peaks_matched = as.integer(
        candidate_count_similarity_peaks_matched
      )
    )

  structures_keyed <- structures_prepared |>
    tidytable::filter(
      !is.na(feature_id),
      !is.na(candidate_structure_smiles_no_stereo)
    )

  structures_no_key <- structures_prepared |>
    tidytable::filter(
      is.na(feature_id) | is.na(candidate_structure_smiles_no_stereo)
    )

  spectral_keyed <- spectral_typed |>
    tidytable::filter(
      !is.na(feature_id),
      !is.na(candidate_structure_smiles_no_stereo)
    )

  structure_keys <- structures_keyed |>
    tidytable::select(tidyselect::any_of(join_key)) |>
    tidytable::distinct()

  spectral_overlap_keyed <- spectral_keyed |>
    tidytable::semi_join(structure_keys, by = join_key)

  spectral_non_overlap_keyed <- spectral_keyed |>
    tidytable::anti_join(structure_keys, by = join_key)

  spectral_overlap_keyed <- .reduce_sirius_spectral_candidates(
    spectral_overlap_keyed,
    join_key = join_key
  )

  spectral_keyed <- tidytable::bind_rows(
    spectral_overlap_keyed,
    spectral_non_overlap_keyed
  )

  spectral_no_key <- spectral_typed |>
    tidytable::filter(
      is.na(feature_id) | is.na(candidate_structure_smiles_no_stereo)
    )

  # Full join keeps overlap rows merged and preserves non-overlap rows as
  # candidates.
  merged_keyed <- tidytable::full_join(
    structures_keyed,
    spectral_keyed,
    by = join_key
  )

  suffixed_bases <- names(merged_keyed)
  suffixed_bases <- suffixed_bases[grepl("\\.x$", suffixed_bases)]
  suffixed_bases <- sub("\\.x$", "", suffixed_bases)

  for (col in suffixed_bases) {
    col_x <- paste0(col, ".x")
    col_y <- paste0(col, ".y")

    merged_keyed <- merged_keyed |>
      tidytable::mutate(
        !!as.name(col) := tidytable::coalesce(
          !!as.name(col_x),
          !!as.name(col_y)
        )
      ) |>
      tidytable::select(-tidyselect::any_of(c(col_x, col_y)))
  }

  if ("candidate_score_similarity" %in% names(merged_keyed)) {
    merged_keyed <- merged_keyed |>
      tidytable::mutate(
        candidate_score_similarity = as.numeric(candidate_score_similarity)
      )
  }

  if ("candidate_count_similarity_peaks_matched" %in% names(merged_keyed)) {
    merged_keyed <- merged_keyed |>
      tidytable::mutate(
        candidate_count_similarity_peaks_matched = as.integer(
          candidate_count_similarity_peaks_matched
        )
      )
  }

  tidytable::bind_rows(
    merged_keyed,
    structures_no_key,
    spectral_no_key
  ) |>
    tidytable::distinct()
}

.has_overlap_non_na <- function(x, y, col) {
  if (!(col %in% names(x) && col %in% names(y))) {
    return(FALSE)
  }
  xv <- unique(as.character(x[[col]]))
  yv <- unique(as.character(y[[col]]))
  xv <- xv[!is.na(xv) & nzchar(trimws(xv))]
  yv <- yv[!is.na(yv) & nzchar(trimws(yv))]
  if (length(xv) == 0L || length(yv) == 0L) {
    return(FALSE)
  }
  length(intersect(xv, yv)) > 0L
}

.pick_sirius_join_keys <- function(x, y, preferred_keys) {
  # Always anchor on feature_id when available.
  keys <- character(0)
  if ("feature_id" %in% names(x) && "feature_id" %in% names(y)) {
    keys <- "feature_id"
  }

  # Add optional keys only if they can actually match (non-NA overlap).
  optional_keys <- setdiff(preferred_keys, "feature_id")
  optional_keys <- optional_keys[
    optional_keys %in% names(x) & optional_keys %in% names(y)
  ]

  keys <- c(
    keys,
    Filter(
      function(k) .has_overlap_non_na(x, y, k),
      optional_keys
    )
  )

  unique(keys)
}

.left_join_new_sirius_columns <- function(x, y, by) {
  if (nrow(x) == 0L || nrow(y) == 0L || length(by) == 0L) {
    return(x)
  }

  new_cols <- setdiff(names(y), names(x))
  if (length(new_cols) == 0L) {
    return(x)
  }

  y_reduced <- y |>
    tidytable::select(tidyselect::any_of(c(by, new_cols))) |>
    tidytable::distinct()

  # Prevent join fan-out when payload has multiple rows for the same key.
  y_reduced <- y_reduced |>
    tidytable::distinct(tidyselect::any_of(by), .keep_all = TRUE)

  tidytable::left_join(x, y_reduced, by = by)
}

.reduce_sirius_spectral_candidates <- function(spectral_keyed, join_key) {
  if (nrow(spectral_keyed) == 0L) {
    return(spectral_keyed)
  }

  join_syms <- rlang::syms(join_key)

  spectral_keyed |>
    tidytable::mutate(
      .sim_rank = tidytable::if_else(
        is.na(candidate_score_similarity),
        -Inf,
        candidate_score_similarity
      ),
      .peak_rank = tidytable::if_else(
        is.na(candidate_count_similarity_peaks_matched),
        -Inf,
        as.numeric(candidate_count_similarity_peaks_matched)
      )
    ) |>
    tidytable::arrange(
      !!!join_syms,
      tidytable::desc(.sim_rank),
      tidytable::desc(.peak_rank)
    ) |>
    tidytable::distinct(
      !!!join_syms,
      .keep_all = TRUE
    ) |>
    tidytable::select(-.sim_rank, -.peak_rank)
}

.build_formula_error_fallback <- function(formulas_prepared) {
  formulas_prepared |>
    tidytable::filter(!is.na(feature_id)) |>
    tidytable::mutate(
      candidate_structure_error_mz = as.numeric(candidate_structure_error_mz)
    ) |>
    tidytable::filter(!is.na(candidate_structure_error_mz)) |>
    tidytable::arrange(feature_id, abs(candidate_structure_error_mz)) |>
    tidytable::distinct(feature_id, .keep_all = TRUE) |>
    tidytable::select(
      feature_id,
      candidate_structure_error_mz_fallback = candidate_structure_error_mz
    )
}

.apply_candidate_error_mz_fallback <- function(df, formula_error_fallback) {
  if (!"candidate_structure_error_mz" %in% names(df)) {
    df <- df |>
      tidytable::mutate(candidate_structure_error_mz = NA_real_)
  }

  df |>
    tidytable::left_join(formula_error_fallback, by = "feature_id") |>
    tidytable::mutate(
      candidate_structure_error_mz = ifelse(
        is.na(as.numeric(candidate_structure_error_mz)),
        candidate_structure_error_mz_fallback,
        as.numeric(candidate_structure_error_mz)
      )
    ) |>
    tidytable::select(-candidate_structure_error_mz_fallback)
}

join_sirius_annotation_tables <- function(
  structures_prepared,
  formulas_prepared,
  canopus_prepared,
  denovo_prepared
) {
  formula_keys <- .pick_sirius_join_keys(
    structures_prepared,
    formulas_prepared,
    c("feature_id", "candidate_adduct", "candidate_structure_molecular_formula")
  )
  canopus_keys <- .pick_sirius_join_keys(
    structures_prepared,
    canopus_prepared,
    c("feature_id", "candidate_adduct", "candidate_structure_molecular_formula")
  )
  denovo_keys <- .pick_sirius_join_keys(
    structures_prepared,
    denovo_prepared,
    c(
      "feature_id",
      "candidate_adduct",
      "candidate_structure_smiles_no_stereo",
      "candidate_structure_molecular_formula"
    )
  )

  structures_enriched <- structures_prepared |>
    .left_join_new_sirius_columns(formulas_prepared, by = formula_keys) |>
    .left_join_new_sirius_columns(canopus_prepared, by = canopus_keys) |>
    .left_join_new_sirius_columns(denovo_prepared, by = denovo_keys)

  # Keep de novo candidates that do not exist in structure_identifications.
  denovo_match_keys <- c(
    "feature_id",
    "candidate_structure_smiles_no_stereo",
    "candidate_structure_molecular_formula",
    "candidate_adduct"
  )
  denovo_match_keys <- denovo_match_keys[
    denovo_match_keys %in%
      names(structures_prepared) &
      denovo_match_keys %in% names(denovo_prepared)
  ]

  denovo_only <- if (
    nrow(denovo_prepared) == 0L ||
      !"feature_id" %in% denovo_match_keys ||
      length(denovo_match_keys) < 2L
  ) {
    denovo_prepared
  } else {
    denovo_prepared |>
      tidytable::anti_join(
        structures_prepared |>
          tidytable::distinct(tidyselect::any_of(denovo_match_keys)),
        by = denovo_match_keys
      )
  }

  denovo_only_enriched <- denovo_only |>
    .left_join_new_sirius_columns(
      formulas_prepared,
      by = .pick_sirius_join_keys(
        denovo_only,
        formulas_prepared,
        c(
          "feature_id",
          "candidate_structure_molecular_formula"
        )
      )
    ) |>
    .left_join_new_sirius_columns(
      canopus_prepared,
      by = .pick_sirius_join_keys(
        denovo_only,
        canopus_prepared,
        c(
          "feature_id",
          "candidate_adduct",
          "candidate_structure_molecular_formula"
        )
      )
    )

  # Build fallback mz error from formula-level data (best match per feature)
  # This is used for both de novo and structure candidates that lack mz_error.
  formula_error_fallback <- .build_formula_error_fallback(formulas_prepared)

  structures_with_fallback <- .apply_candidate_error_mz_fallback(
    structures_enriched,
    formula_error_fallback
  )

  # Some de novo candidates can miss adduct/formula keys and therefore bypass
  # strict formula joins; backfill mz error from the formula-level feature row.
  denovo_only_enriched <- .apply_candidate_error_mz_fallback(
    denovo_only_enriched,
    formula_error_fallback
  )

  tidytable::bind_rows(structures_with_fallback, denovo_only_enriched) |>
    tidytable::distinct()
}
