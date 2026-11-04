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
    fr_can <- suppressWarnings(as.integer(canopus$formulaRank))
    canopus <- canopus[which(!is.na(fr_can) & fr_can == 1L), ]
  }

  formulas <- read_from_sirius_zip(input_directory, file = fnames$formulas)
  if (
    version == "6" && nrow(formulas) > 0L && "formulaRank" %in% names(formulas)
  ) {
    fr_for <- suppressWarnings(as.integer(formulas$formulaRank))
    formulas <- formulas[which(!is.na(fr_for) & fr_for == 1L), ]
  }

  structures <- read_from_sirius_zip(input_directory, file = fnames$structures)

  files <- tryCatch(
    list(Name = .get_sirius_zip_names(input_directory)),
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
      spectral <- lapply(
        spectral_files,
        function(x) {
          read_sirius_internal_file(input_directory, x) |>
            tidytable::mutate(.sirius_source_file = x)
        }
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
    list(Name = .get_sirius_zip_names(input_directory)),
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
  summaries <- lapply(
    summary_files,
    function(x) read_from_sirius_zip(input_directory, file = x)
  )
  names(summaries) <- summary_files |>
    pre_harmonize_names_sirius() |>
    harmonize_names_sirius()
  summaries <- summaries[vapply(summaries, nrow, integer(1L)) > 0L]
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

.fast_distinct_with_key_guard <- function(df, key_cols) {
  n <- nrow(df)
  if (n <= 1L) {
    return(df)
  }
  present_keys <- intersect(key_cols, names(df))
  if (length(present_keys) == 0L) {
    return(tidytable::distinct(df))
  }
  k1 <- df[[present_keys[[1L]]]]
  u1 <- unique(k1)
  id1 <- if (anyNA(u1)) match(k1, u1) else fastmatch::fmatch(k1, u1)
  if (length(present_keys) == 1L) {
    comp_key <- id1
  } else {
    comp_key <- as.double(id1 - 1L)
    total_space <- as.double(length(u1) + 1L)
    use_paste <- FALSE
    ids <- list(id1)
    for (i in 2:length(present_keys)) {
      ki <- df[[present_keys[[i]]]]
      ui <- unique(ki)
      idi <- if (anyNA(ui)) match(ki, ui) else fastmatch::fmatch(ki, ui)
      ids[[i]] <- idi
      bi <- as.double(length(ui) + 1L)
      total_space <- total_space * bi
      if (total_space < 4503599627370496) {
        comp_key <- comp_key * bi + (idi - 1L)
      } else {
        use_paste <- TRUE
      }
    }
    if (use_paste) {
      comp_key <- do.call(paste, c(ids, list(sep = ":")))
    }
  }

  dup_fwd <- duplicated(comp_key)
  if (!any(dup_fwd)) {
    return(df)
  }
  is_dup_key <- dup_fwd | duplicated(comp_key, fromLast = TRUE)
  dup_idx <- which(is_dup_key)
  sub_dups <- duplicated(df[dup_idx, ])
  if (!any(sub_dups)) {
    return(df)
  }
  df[-dup_idx[sub_dups], ]
}

.fast_match_rows <- function(x, y, by) {
  nx <- nrow(x)
  ny <- nrow(y)
  if (nx == 0L || ny == 0L || length(by) == 0L) {
    return(rep(NA_integer_, nx))
  }
  if (length(by) == 1L) {
    k <- by[[1L]]
    yk <- y[[k]]
    if (!is.character(yk)) {
      yk <- as.character(yk)
    }
    keep_y <- !duplicated(yk)
    yk_u <- yk[keep_y]
    y_first <- which(keep_y)
    xk <- x[[k]]
    if (!is.character(xk)) {
      xk <- as.character(xk)
    }
    m <- if (anyNA(yk_u) || anyNA(xk)) {
      match(xk, yk_u)
    } else {
      fastmatch::fmatch(xk, yk_u)
    }
    return(y_first[m])
  }

  y_ids <- vector("list", length(by))
  x_ids <- vector("list", length(by))
  bases <- integer(length(by))
  valid_x <- rep(TRUE, nx)

  for (i in seq_along(by)) {
    k <- by[[i]]
    yk <- y[[k]]
    if (!is.character(yk)) {
      yk <- as.character(yk)
    }
    uk <- unique(yk)
    xk <- x[[k]]
    if (!is.character(xk)) {
      xk <- as.character(xk)
    }
    if (anyNA(uk) || anyNA(xk)) {
      yi <- match(yk, uk)
      xi <- match(xk, uk)
    } else {
      yi <- fastmatch::fmatch(yk, uk)
      xi <- fastmatch::fmatch(xk, uk)
    }
    valid_x <- valid_x & !is.na(xi)
    y_ids[[i]] <- yi
    x_ids[[i]] <- xi
    bases[[i]] <- length(uk) + 1L
  }

  if (!any(valid_x)) {
    return(rep(NA_integer_, nx))
  }

  vx_idx <- which(valid_x)
  total_space <- prod(as.double(bases))
  if (total_space < 4503599627370496) {
    y_key <- as.double(y_ids[[1L]] - 1L)
    x_key <- as.double(x_ids[[1L]][vx_idx] - 1L)
    for (i in 2:length(by)) {
      b <- as.double(bases[[i]])
      y_key <- y_key * b + (y_ids[[i]] - 1L)
      x_key <- x_key * b + (x_ids[[i]][vx_idx] - 1L)
    }
  } else {
    y_key <- do.call(paste, c(y_ids, list(sep = ":")))
    x_sub <- lapply(x_ids, `[`, vx_idx)
    x_key <- do.call(paste, c(x_sub, list(sep = ":")))
  }

  keep_y <- !duplicated(y_key)
  u_y_key <- y_key[keep_y]
  y_first <- which(keep_y)
  m_vx <- fastmatch::fmatch(x_key, u_y_key)
  out <- rep(NA_integer_, nx)
  out[vx_idx] <- y_first[m_vx]
  out
}

#' Split SIRIUS Results into Output Tables
#'
#' @description Internal helper to split combined SIRIUS table into CANOPUS,
#'     formula, and structure-specific tables based on column model.
#'
#' @include columns_utils.R
#' @include logs_utils.R
#' @include validations_utils.R
#' @include predicates_utils.R
#' @include harmonize_names_sirius.R
#' @include pre_harmonize_names_sirius.R
#' @include read_from_sirius_zip.R
#' @include prepare_annotations_sirius.R
#' @include validations_params.R
#'
#' @param table [data.frame] Data frame with combined SIRIUS results.
#'
#' @return Named list with keys: canopus, formula, structures.
#' @keywords internal
split_sirius_results <- function(table) {
  model <- columns_model()
  tbl_names <- names(table)
  fid_col <- model$features_columns[1]
  has_fid <- fid_col %in% tbl_names
  valid_fid <- if (has_fid) !is.na(table[[fid_col]]) else rep(TRUE, nrow(table))

  can_cols <- intersect(
    c(model$features_columns, model$features_calculated_columns),
    tbl_names
  )
  can_filter_col <- model$features_calculated_columns[5]
  if (can_filter_col %in% tbl_names) {
    keep_can <- which(valid_fid & !is.na(table[[can_filter_col]]))
    canopus <- tidytable::distinct(
      tidytable::as_tidytable(as.list(table)[can_cols])[keep_can, ]
    )
  } else {
    canopus <- tidytable::distinct(
      tidytable::as_tidytable(as.list(table)[can_cols])[which(valid_fid), ]
    )
  }

  for_cols <- intersect(
    c(model$features_columns, model$candidates_sirius_for_columns),
    tbl_names
  )
  for_filter_col <- model$candidates_sirius_for_columns[2]
  if (for_filter_col %in% tbl_names) {
    keep_for <- which(valid_fid & !is.na(table[[for_filter_col]]))
    formula <- tidytable::distinct(
      tidytable::as_tidytable(as.list(table)[for_cols])[keep_for, ]
    )
  } else {
    formula <- tidytable::distinct(
      tidytable::as_tidytable(as.list(table)[for_cols])[which(valid_fid), ]
    )
  }

  str_cols <- intersect(
    c(
      model$features_columns,
      model$candidates_structures_columns,
      model$candidates_spectra_columns,
      model$candidates_sirius_str_columns
    ),
    tbl_names
  )
  str_sub <- tidytable::as_tidytable(as.list(table)[str_cols])
  if (!all(valid_fid)) {
    str_sub <- str_sub[which(valid_fid), ]
  }
  structures <- .fast_distinct_with_key_guard(
    str_sub,
    c(
      "feature_id",
      "candidate_structure_smiles_no_stereo",
      "candidate_adduct",
      "candidate_library"
    )
  )

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
  lib_col <- spectral_prepared$candidate_library
  dir_idx <- which(!is.na(lib_col) & lib_col != "SIRIUS spectral (analog)")
  ana_idx <- which(!is.na(lib_col) & lib_col == "SIRIUS spectral (analog)")
  if (is.finite(max_analog_abs_mz_error) && length(ana_idx) > 0L) {
    err_ana <- abs(
      as.numeric(spectral_prepared$candidate_structure_error_mz[ana_idx])
    )
    ana_idx <- ana_idx[!is.na(err_ana) & err_ana <= max_analog_abs_mz_error]
  }
  spectral_all <- tidytable::as_tidytable(
    as.list(spectral_prepared[c(dir_idx, ana_idx), ])
  )

  n_sp <- nrow(spectral_all)
  if (!"candidate_count_similarity_peaks_matched" %in% names(spectral_all)) {
    data.table::set(
      spectral_all,
      j = "candidate_count_similarity_peaks_matched",
      value = rep.int(NA_integer_, n_sp)
    )
  } else {
    data.table::set(
      spectral_all,
      j = "candidate_count_similarity_peaks_matched",
      value = as.integer(spectral_all$candidate_count_similarity_peaks_matched)
    )
  }
  if (!"candidate_spectrum_id" %in% names(spectral_all)) {
    data.table::set(
      spectral_all,
      j = "candidate_spectrum_id",
      value = rep.int(NA_character_, n_sp)
    )
  }
  if (!"candidate_structure_name" %in% names(spectral_all)) {
    data.table::set(
      spectral_all,
      j = "candidate_structure_name",
      value = rep.int(NA_character_, n_sp)
    )
  }
  data.table::set(
    spectral_all,
    j = "candidate_score_similarity",
    value = as.numeric(spectral_all$candidate_score_similarity)
  )

  st_valid <- !is.na(structures_prepared$feature_id) &
    !is.na(structures_prepared$candidate_structure_smiles_no_stereo)
  structures_keyed <- tidytable::as_tidytable(
    as.list(structures_prepared[which(st_valid), ])
  )
  structures_no_key <- structures_prepared[which(!st_valid), ]

  sp_valid <- !is.na(spectral_all$feature_id) &
    !is.na(spectral_all$candidate_structure_smiles_no_stereo)
  spectral_keyed <- spectral_all[which(sp_valid), ]
  spectral_no_key <- spectral_all[which(!sp_valid), ]

  in_struct <- !is.na(
    .fast_match_rows(spectral_keyed, structures_keyed, by = join_key)
  )
  spectral_overlap_keyed <- .reduce_sirius_spectral_candidates(
    spectral_keyed[which(in_struct), ],
    join_key = join_key
  )
  spectral_non_overlap_keyed <- spectral_keyed[which(!in_struct), ]

  st_orig_cols <- names(structures_keyed)
  sp_orig_cols <- names(spectral_keyed)
  suffixed_bases <- intersect(
    setdiff(st_orig_cols, join_key),
    setdiff(sp_orig_cols, join_key)
  )
  st_only_cols <- setdiff(st_orig_cols, c(join_key, suffixed_bases))
  sp_only_cols <- setdiff(sp_orig_cols, c(join_key, suffixed_bases))

  if (nrow(spectral_overlap_keyed) > 0L) {
    m_ov <- .fast_match_rows(
      structures_keyed,
      spectral_overlap_keyed,
      by = join_key
    )
    has_ov <- which(!is.na(m_ov))
    ov_rows <- m_ov[has_ov]
    sp_cols <- setdiff(names(spectral_overlap_keyed), join_key)
    for (col in sp_cols) {
      if (col %in% st_orig_cols) {
        cur <- structures_keyed[[col]]
        sp_v <- spectral_overlap_keyed[[col]][ov_rows]
        if (!identical(typeof(cur), typeof(sp_v))) {
          full_sp <- spectral_overlap_keyed[[col]][m_ov]
          cur <- tidytable::coalesce(cur, full_sp)
          data.table::set(structures_keyed, j = col, value = cur)
        } else {
          na_cur <- is.na(cur[has_ov])
          if (any(na_cur)) {
            cur[has_ov[na_cur]] <- sp_v[na_cur]
            data.table::set(structures_keyed, j = col, value = cur)
          }
        }
      } else {
        data.table::set(
          structures_keyed,
          j = col,
          value = spectral_overlap_keyed[[col]][m_ov]
        )
      }
    }
  } else {
    for (col in sp_only_cols) {
      data.table::set(
        structures_keyed,
        j = col,
        value = spectral_keyed[[col]][rep(NA_integer_, nrow(structures_keyed))]
      )
    }
  }

  target_order <- c(
    intersect(join_key, names(structures_keyed)),
    st_only_cols,
    sp_only_cols,
    suffixed_bases
  )
  target_order <- intersect(target_order, names(structures_keyed))
  if (length(target_order) > 0L) {
    data.table::setcolorder(structures_keyed, target_order)
  }

  merged_keyed <- tidytable::bind_rows(
    structures_keyed,
    spectral_non_overlap_keyed
  )

  if ("candidate_score_similarity" %in% names(merged_keyed)) {
    merged_keyed$candidate_score_similarity <- as.numeric(
      merged_keyed$candidate_score_similarity
    )
  }

  if ("candidate_count_similarity_peaks_matched" %in% names(merged_keyed)) {
    merged_keyed$candidate_count_similarity_peaks_matched <- as.integer(
      merged_keyed$candidate_count_similarity_peaks_matched
    )
  }

  result <- .fast_distinct_with_key_guard(
    tidytable::bind_rows(
      merged_keyed,
      structures_no_key,
      spectral_no_key
    ),
    c(
      "feature_id",
      "candidate_structure_smiles_no_stereo",
      "candidate_adduct",
      "candidate_library"
    )
  )

  rm(
    structures_keyed,
    spectral_keyed,
    spectral_all,
    merged_keyed,
    structures_no_key,
    spectral_no_key
  )
  result
}

.has_overlap_non_na <- function(x, y, col) {
  if (!(col %in% names(x) && col %in% names(y))) {
    return(FALSE)
  }
  xv <- unique(as.character(x[[col]]))
  yv <- unique(as.character(y[[col]]))
  xv <- xv[!is.na(xv) & nzchar(xv)]
  yv <- yv[!is.na(yv) & nzchar(yv)]
  if (length(xv) == 0L || length(yv) == 0L) {
    return(FALSE)
  }
  ws_x <- grep("^[ \t\r\n]|[ \t\r\n]$", xv)
  if (length(ws_x) > 0L) {
    tx <- xv
    tx[ws_x] <- trimws(xv[ws_x])
    xv <- xv[nzchar(tx)]
  }
  ws_y <- grep("^[ \t\r\n]|[ \t\r\n]$", yv)
  if (length(ws_y) > 0L) {
    ty <- yv
    ty[ws_y] <- trimws(yv[ws_y])
    yv <- yv[nzchar(ty)]
  }
  if (length(xv) == 0L || length(yv) == 0L) {
    return(FALSE)
  }
  any(fastmatch::fmatch(xv, yv, nomatch = 0L) > 0L)
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

  # Prevent join fan-out when payload has multiple rows for the same key.
  # Keep only one row per key, preferring the first occurrence
  m <- .fast_match_rows(x, y, by = by)
  out <- tidytable::as_tidytable(as.list(x))
  for (col in new_cols) {
    data.table::set(out, j = col, value = y[[col]][m])
  }
  out
}

.reduce_sirius_spectral_candidates <- function(spectral_keyed, join_key) {
  n <- nrow(spectral_keyed)
  if (n <= 1L) {
    return(spectral_keyed)
  }

  sim <- as.numeric(spectral_keyed$candidate_score_similarity)
  sim[is.na(sim)] <- -Inf
  pk <- as.numeric(spectral_keyed$candidate_count_similarity_peaks_matched)
  pk[is.na(pk)] <- -Inf
  f_col <- spectral_keyed[[join_key[[1L]]]]
  s_col <- spectral_keyed[[join_key[[2L]]]]
  ord <- order(f_col, s_col, -sim, -pk, method = "radix")
  f_ord <- f_col[ord]
  s_ord <- s_col[ord]
  keep <- c(TRUE, f_ord[-1L] != f_ord[-n] | s_ord[-1L] != s_ord[-n])
  spectral_keyed[ord[keep], ]
}

.build_formula_error_fallback <- function(formulas_prepared) {
  if (
    nrow(formulas_prepared) == 0L ||
      !"feature_id" %in% names(formulas_prepared) ||
      !"candidate_structure_error_mz" %in% names(formulas_prepared)
  ) {
    return(tidytable::tidytable(
      feature_id = character(),
      candidate_structure_error_mz_fallback = numeric()
    ))
  }
  fid <- formulas_prepared$feature_id
  err <- as.numeric(formulas_prepared$candidate_structure_error_mz)
  ok <- which(!is.na(fid) & !is.na(err))
  if (length(ok) == 0L) {
    return(tidytable::tidytable(
      feature_id = character(),
      candidate_structure_error_mz_fallback = numeric()
    ))
  }
  fid_ok <- fid[ok]
  err_ok <- err[ok]
  ord <- order(fid_ok, abs(err_ok), method = "radix")
  keep <- ord[!duplicated(fid_ok[ord])]
  tidytable::tidytable(
    feature_id = fid_ok[keep],
    candidate_structure_error_mz_fallback = err_ok[keep]
  )
}

.apply_candidate_error_mz_fallback <- function(df, formula_error_fallback) {
  out <- tidytable::as_tidytable(as.list(df))
  err <- if ("candidate_structure_error_mz" %in% names(out)) {
    as.numeric(out$candidate_structure_error_mz)
  } else {
    rep(NA_real_, nrow(out))
  }
  na_idx <- which(is.na(err))
  if (length(na_idx) > 0L && nrow(formula_error_fallback) > 0L) {
    fb_f <- formula_error_fallback$feature_id
    q_f <- out$feature_id[na_idx]
    m <- if (anyNA(fb_f) || anyNA(q_f)) {
      match(q_f, fb_f)
    } else {
      fastmatch::fmatch(q_f, fb_f)
    }
    err[na_idx] <- formula_error_fallback$candidate_structure_error_mz_fallback[
      m
    ]
  }
  data.table::set(out, j = "candidate_structure_error_mz", value = err)
  out
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
    m_dn <- .fast_match_rows(
      denovo_prepared,
      structures_prepared,
      by = denovo_match_keys
    )
    denovo_prepared[which(is.na(m_dn)), ]
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

  .fast_distinct_with_key_guard(
    tidytable::bind_rows(structures_with_fallback, denovo_only_enriched),
    c(
      "feature_id",
      "candidate_structure_smiles_no_stereo",
      "candidate_adduct",
      "candidate_structure_molecular_formula"
    )
  )
}
