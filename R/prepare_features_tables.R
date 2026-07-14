#' @title Prepare features table
#'
#' @description Prepares LC-MS feature tables by standardizing column names,
#' filtering to top-intensity samples per feature, and formatting for
#' downstream analysis. Supports multiple formats (mzmine, SLAW, SIRIUS).
#'
#' @include get_params.R
#' @include rts_utils.R
#'
#' @param features [character] Path to raw features file (CSV/TSV).
#' @param output [character] Path where prepared features should be saved.
#' @param candidates [integer] Number of top-intensity samples to retain per
#' feature (default: from params; recommended <=5 to balance data size and
#'     coverage).
#' @param name_adduct [character] Name of the adduct column in input.
#' @param name_features [character] Name of the feature ID column in input.
#' @param name_rt [character] Name of the retention time column in input.
#' @param name_mz [character] Name of the m/z column in input.
#'
#' @return character(1) Path to the prepared feature table (invisibly).
#'
#' @family preparation
#'
#' @export
#'
#' @examples
#' \dontrun{
#' copy_backbone()
#' go_to_cache()
#' get_file(
#'   url = get_default_paths()$urls$examples$features,
#'   export = get_params(step = "prepare_features_tables")$files$features$raw
#' )
#' prepare_features_tables()
#' unlink("data", recursive = TRUE)
#' }
prepare_features_tables <- function(
  features = get_params(step = "prepare_features_tables")$files$features$raw,
  output = get_params(
    step = "prepare_features_tables"
  )$files$features$prepared,
  candidates = get_params(
    step = "prepare_features_tables"
  )$annotations$canidates$samples,
  name_adduct = get_params(step = "prepare_features_tables")$names$adduct,
  name_features = get_params(step = "prepare_features_tables")$names$features,
  name_rt = get_params(step = "prepare_features_tables")$names$rt$features,
  name_mz = get_params(step = "prepare_features_tables")$names$precursor
) {
  ctx <- log_operation(
    "prepare_features_tables",
    input = features,
    candidates = candidates
  )

  # Input Validation ----
  .validate_prepare_features_inputs(
    features = features,
    output = output,
    candidates = candidates,
    name_adduct = name_adduct,
    name_features = name_features,
    name_rt = name_rt,
    name_mz = name_mz
  )

  # Load Features Table ----
  log_debug("Retaining top %d intensity samples per feature", candidates)

  features_raw <- .load_features_file(features)
  log_debug("Loaded %d features", nrow(features_raw))

  # Resolve configured column names against common aliases (e.g. mzTab fields).
  resolved_names <- .resolve_feature_column_names(
    tbl = features_raw,
    name_features = name_features,
    name_rt = name_rt,
    name_mz = name_mz,
    name_adduct = name_adduct
  )
  name_features <- resolved_names$name_features
  name_rt <- resolved_names$name_rt
  name_mz <- resolved_names$name_mz
  name_adduct <- resolved_names$name_adduct

  # Format and Filter ----
  features_selected <- .select_intensity_columns(
    features_raw,
    name_features = name_features,
    name_rt = name_rt,
    name_mz = name_mz,
    name_adduct = name_adduct
  )

  # Normalize RT to minutes
  features_selected <- .normalize_rt_column(
    features_selected,
    name_rt = name_rt
  )

  # Standardize column names (remove format-specific prefixes/suffixes)
  features_std <- .standardize_column_names(features_selected)

  # Filter to top intensity samples per feature
  features_prepared <- .filter_top_intensity_samples(
    features_std,
    candidates = candidates,
    name_features = name_features,
    name_rt = name_rt,
    name_mz = name_mz,
    name_adduct = name_adduct
  )

  log_info("Prepared %d feature-sample pairs", nrow(features_prepared))

  # Export Results ----
  log_complete(ctx, n_features = nrow(features_prepared))

  export_params(
    parameters = get_params(step = "prepare_features_tables"),
    step = "prepare_features_tables"
  )
  export_output(x = features_prepared, file = output)

  invisible(output)
}

# --- Internal Helpers -------------------------------------------------------

#' Validate inputs for prepare_features_tables
#' @keywords internal
.validate_prepare_features_inputs <- function(
  features,
  output,
  candidates,
  name_adduct,
  name_features,
  name_rt,
  name_mz
) {
  # Validate file paths
  validate_character(features, param_name = "features", allow_empty = FALSE)
  validate_file_exists(
    path = features,
    file_type = "features file",
    param_name = "features"
  )
  validate_character(output, param_name = "output", allow_empty = FALSE)

  # Validate candidates (allow NULL or integer in range)
  if (!is.null(candidates)) {
    if (
      !is.numeric(candidates) || length(candidates) != 1L || is.na(candidates)
    ) {
      cli::cli_abort(
        "candidates must be a single numeric value or NULL",
        class = c("tima_validation_error", "tima_error"),
        call = NULL
      )
    }
    if (candidates < 1L || candidates > 100L) {
      cli::cli_abort(
        "candidates must be between 1 and 100, got {.val {candidates}}",
        class = c("tima_validation_error", "tima_error"),
        call = NULL
      )
    }
  }

  # Validate column name parameters (all must be single character strings)
  col_params <- list(
    name_adduct = name_adduct,
    name_features = name_features,
    name_rt = name_rt,
    name_mz = name_mz
  )
  # Validate all parameters
  purrr::iwalk(col_params, .validate_column_name_param)

  invisible(NULL)
}

#' Validate column name parameter
#' @keywords internal
#' @noRd
.validate_column_name_param <- function(val, param_name) {
  if (!is.character(val) || length(val) != 1L || is.na(val) || !nzchar(val)) {
    msg <- sprintf(
      "`%s` must be a single non-empty character string",
      param_name
    )
    cli::cli_abort(
      msg,
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }
}

#' Load features file with error handling
#' @keywords internal
.load_features_file <- function(features) {
  tbl <- safe_fread(
    file = features,
    file_type = "features table",
    na.strings = c("", "NA"),
    colClasses = "character"
  )

  if (nrow(tbl) == 0L) {
    cli::cli_abort(
      c(
        "features file is empty",
        "x" = features
      ),
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }

  tbl
}

#' Select intensity columns and metadata (auto-detect format)
#' @keywords internal
.select_intensity_columns <- function(
  tbl,
  name_features,
  name_rt,
  name_mz,
  name_adduct
) {
  core_cols <- c(name_features, name_rt, name_mz, name_adduct)

  # Detect and select columns for mzmine (Peak area/height), SLAW (quant_),
  # SIRIUS
  selected <- tbl |>
    tidytable::select(
      tidyselect::any_of(x = core_cols),
      tidyselect::matches(match = " Peak area"),
      tidyselect::matches(match = ":area"),
      tidyselect::matches(match = "quant_"),
      tidyselect::matches(match = " Peak height")
    ) |>
    tidytable::select(-tidyselect::matches(match = "quant_peaktable"))

  # Fallback for mzTab-derived tables where sample columns are plain names.
  selected_non_core <- setdiff(colnames(selected), core_cols)
  if (length(selected_non_core) == 0L) {
    candidate_cols <- setdiff(colnames(tbl), core_cols)

    is_numeric_like <- function(x) {
      vals <- as.character(x)
      vals <- vals[!is.na(vals) & nzchar(vals)]
      if (length(vals) == 0L) {
        return(FALSE)
      }
      ratio_numeric <- mean(!is.na(suppressWarnings(as.numeric(vals))))
      ratio_numeric >= 0.8
    }

    numeric_like_cols <- candidate_cols[vapply(
      X = tbl[, ..candidate_cols],
      FUN = is_numeric_like,
      FUN.VALUE = logical(1L)
    )]

    if (length(numeric_like_cols) > 0L) {
      log_info(
        "No standard intensity patterns found; using %d numeric-like column(s) as intensities",
        length(numeric_like_cols)
      )
      selected <- tbl |>
        tidytable::select(
          tidyselect::any_of(x = core_cols),
          tidyselect::all_of(numeric_like_cols)
        )
    }
  }

  # When both Peak area and Peak height are present for the same samples,
  # prefer Peak area and drop Peak height duplicates
  area_cols <- grep(" Peak area$", colnames(selected), value = TRUE)
  height_cols <- grep(" Peak height$", colnames(selected), value = TRUE)

  if (length(area_cols) > 0L && length(height_cols) > 0L) {
    # Derive sample stems by stripping suffixes
    area_stems <- sub(" Peak area$", "", area_cols)
    height_stems <- sub(" Peak height$", "", height_cols)
    redundant_heights <- height_cols[height_stems %in% area_stems]

    if (length(redundant_heights) > 0L) {
      log_info(
        "Both Peak area and Peak height detected for %d sample(s); using Peak area",
        length(redundant_heights)
      )
      selected <- selected |>
        tidytable::select(-tidyselect::all_of(redundant_heights))
    }
  }

  selected
}

#' Normalize RT column to minutes (auto-detect unit if possible)
#' @keywords internal
.normalize_rt_column <- function(tbl, name_rt) {
  if (name_rt %in% colnames(tbl)) {
    tbl[[name_rt]] <- normalize_rt_to_minutes(
      tbl[[name_rt]],
      unit = "auto",
      quiet = FALSE
    )
  }
  tbl
}

#' Standardize column names by removing format-specific prefixes/suffixes
#' @keywords internal
.standardize_column_names <- function(tbl) {
  # Pattern replacements: format markers -> clean names
  replacements <- c(
    " Peak area" = "",
    ":area" = "",
    "datafile:" = "",
    "quant_" = "",
    " Peak height" = ""
  )

  # Sequential replacements
  col_names_clean <- Reduce(
    function(names_vec, pattern) {
      stringi::stri_replace_all_fixed(
        str = names_vec,
        pattern = pattern,
        replacement = replacements[[pattern]],
        vectorize_all = FALSE
      )
    },
    names(replacements),
    init = colnames(tbl)
  )

  # Safety guard: if stripping suffixes produced duplicate names (e.g. both

  # Peak area and Peak height survived), keep the first occurrence (area is
  # selected before height by .select_intensity_columns)
  dup_idx <- duplicated(col_names_clean)
  if (any(dup_idx)) {
    dup_names <- unique(col_names_clean[dup_idx])
    log_warn(
      "Duplicate column names after standardization (%s); keeping first occurrence (Peak area preferred)",
      paste(dup_names, collapse = ", ")
    )
    tbl <- tbl[, !dup_idx, with = FALSE]
    col_names_clean <- col_names_clean[!dup_idx]
  }

  data.table::setnames(tbl, col_names_clean)
  tbl
}

#' Filter to top N intensity samples per feature
#' @keywords internal
.filter_top_intensity_samples <- function(
  tbl,
  candidates,
  name_features,
  name_rt,
  name_mz,
  name_adduct
) {
  candidates <- if (is.null(candidates)) 1L else as.integer(candidates)
  candidates <- max(candidates, 1L)

  core_cols <- c(name_features, name_rt, name_mz, name_adduct)
  intensity_cols <- setdiff(colnames(tbl), core_cols)
  if (length(intensity_cols) == 0L) {
    empty_cols <- c("feature_id", "sample")
    empty_tbl <- data.frame(
      feature_id = character(0),
      sample = character(0),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    if (name_rt %in% names(tbl)) {
      empty_tbl$rt <- vector("list", 0L)
    }
    if (name_mz %in% names(tbl)) {
      empty_tbl$mz <- vector("list", 0L)
    }
    if (name_adduct %in% names(tbl)) {
      empty_tbl$adduct <- vector("list", 0L)
    }
    return(empty_tbl)
  }

  tbl_df <- as.data.frame(tbl)
  feature_values <- tbl_df[[name_features]]
  feature_ids <- as.character(feature_values)
  feature_order <- suppressWarnings(as.numeric(feature_ids))
  feature_order_is_numeric <- !all(is.na(feature_order))

  extra_cols <- c()
  if (name_rt %in% names(tbl_df)) {
    extra_cols <- c(extra_cols, "rt")
  }
  if (name_mz %in% names(tbl_df)) {
    extra_cols <- c(extra_cols, "mz")
  }
  if (name_adduct %in% names(tbl_df)) {
    extra_cols <- c(extra_cols, "adduct")
  }

  max_keep <- min(candidates, length(intensity_cols))
  if (max_keep < 1L) {
    return(tidytable::tidytable())
  }

  long_tbl <- tidytable::as_tidytable(tbl_df) |>
    tidytable::pivot_longer(
      cols = tidyselect::all_of(intensity_cols),
      names_to = "sample",
      values_to = "value"
    ) |>
    tidytable::mutate(
      feature_id = as.character(.data[[name_features]]),
      value_num = suppressWarnings(as.numeric(value)),
      value_num = ifelse(is.na(value_num) | value_num == 0, NA_real_, value_num)
    ) |>
    tidytable::filter(!is.na(value_num)) |>
    tidytable::mutate(
      rank = rank(-value_num, ties.method = "first"),
      .by = feature_id
    ) |>
    tidytable::filter(rank <= max_keep) |>
    tidytable::arrange(feature_id, rank)

  if (nrow(long_tbl) == 0L) {
    empty_tbl <- data.frame(
      feature_id = character(0),
      sample = character(0),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    if ("rt" %in% extra_cols) {
      empty_tbl$rt <- vector("list", 0L)
    }
    if ("mz" %in% extra_cols) {
      empty_tbl$mz <- vector("list", 0L)
    }
    if ("adduct" %in% extra_cols) {
      empty_tbl$adduct <- vector("list", 0L)
    }
    return(empty_tbl)
  }

  out <- data.frame(
    feature_id = long_tbl$feature_id,
    sample = long_tbl$sample,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  if ("rt" %in% extra_cols) {
    out$rt <- long_tbl[[name_rt]]
  }
  if ("mz" %in% extra_cols) {
    out$mz <- long_tbl[[name_mz]]
  }
  if ("adduct" %in% extra_cols) {
    out$adduct <- long_tbl[[name_adduct]]
  }

  if (feature_order_is_numeric) {
    order_idx <- order(
      feature_order[match(out$feature_id, feature_ids)],
      as.character(out$feature_id)
    )
  } else {
    order_idx <- order(as.character(out$feature_id))
  }
  out <- out[order_idx, , drop = FALSE]

  keep_cols <- intersect(
    c("feature_id", "rt", "mz", "adduct", "sample"),
    names(out)
  )
  out <- out[, keep_cols, drop = FALSE]

  row.names(out) <- NULL
  out
}

#' Resolve feature-table column names from configured values or aliases
#' @keywords internal
.resolve_feature_column_names <- function(
  tbl,
  name_features,
  name_rt,
  name_mz,
  name_adduct
) {
  cols <- colnames(tbl)

  resolve_one <- function(
    requested,
    aliases,
    required = TRUE,
    label = requested
  ) {
    if (requested %in% cols) {
      return(requested)
    }

    candidates <- unique(c(aliases, requested))
    matched <- candidates[candidates %in% cols]
    if (length(matched) > 0L) {
      chosen <- matched[[1L]]
      log_warn(
        "Column '%s' not found; using '%s' for %s",
        requested,
        chosen,
        label
      )
      return(chosen)
    }

    if (required) {
      cli::cli_abort(
        c(
          sprintf("Required column '%s' is missing", requested),
          i = sprintf("Tried aliases: %s", paste(candidates, collapse = ", ")),
          i = sprintf("Available columns: %s", paste(cols, collapse = ", "))
        ),
        class = c("tima_validation_error", "tima_error"),
        call = NULL
      )
    }

    requested
  }

  list(
    name_features = resolve_one(
      requested = name_features,
      aliases = c("feature_id", "row ID", "SML_ID", "SMF_ID", "id"),
      required = TRUE,
      label = "feature IDs"
    ),
    name_rt = resolve_one(
      requested = name_rt,
      aliases = c(
        "rt",
        "row retention time",
        "retention_time_in_minutes",
        "retention_time_in_seconds"
      ),
      required = FALSE,
      label = "retention time"
    ),
    name_mz = resolve_one(
      requested = name_mz,
      aliases = c("mz", "row m/z", "exp_mass_to_charge", "precursor_mz"),
      required = TRUE,
      label = "precursor m/z"
    ),
    name_adduct = resolve_one(
      requested = name_adduct,
      aliases = c("adduct", "best ion", "adduct_ion", "adduct_ions"),
      required = FALSE,
      label = "adduct"
    )
  )
}
