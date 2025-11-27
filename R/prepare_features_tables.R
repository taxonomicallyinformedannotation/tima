#' @title Prepare features table
#'
#' @description Prepares LC-MS feature tables by standardizing column names,
#' filtering to top-intensity samples per feature, and formatting for
#' downstream analysis. Supports multiple formats (MZmine, SLAW, SIRIUS).
#'
#' @include get_params.R
#' @include rts_utils.R
#'
#' @param features character(1) Path to raw features file (CSV/TSV).
#' @param output character(1) Path where prepared features should be saved.
#' @param candidates integer(1) Number of top-intensity samples to retain per
#'   feature (default: from params; recommended ≤5 to balance data size and coverage).
#' @param name_adduct character(1) Name of the adduct column in input.
#' @param name_features character(1) Name of the feature ID column in input.
#' @param name_rt character(1) Name of the retention time column in input.
#' @param name_mz character(1) Name of the m/z column in input.
#'
#' @return character(1) Path to the prepared feature table (invisibly).
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
  logger::log_info("Preparing features table from: {features}")
  logger::log_debug("Retaining top {candidates} intensity samples per feature")

  features_raw <- .load_features_file(features)
  logger::log_debug("Loaded {nrow(features_raw)} features")

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

  logger::log_info("Prepared {nrow(features_prepared)} feature-sample pairs")

  # Export Results ----
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
  if (!is.character(features) || length(features) != 1L || is.na(features)) {
    stop("features must be a single non-NA character string", call. = FALSE)
  }
  if (!file.exists(features)) {
    stop("Features file not found: ", features, call. = FALSE)
  }
  if (!is.character(output) || length(output) != 1L || is.na(output)) {
    stop("output must be a single non-NA character string", call. = FALSE)
  }

  # Validate candidates (allow NULL or integer in range)
  if (!is.null(candidates)) {
    if (
      !is.numeric(candidates) || length(candidates) != 1L || is.na(candidates)
    ) {
      stop("candidates must be a single numeric value or NULL", call. = FALSE)
    }
    if (candidates < 1L || candidates > 100L) {
      stop(
        "candidates must be between 1 and 100, got: ",
        candidates,
        call. = FALSE
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
  for (param_name in names(col_params)) {
    val <- col_params[[param_name]]
    if (!is.character(val) || length(val) != 1L || is.na(val) || !nzchar(val)) {
      stop(
        param_name,
        " must be a single non-empty character string",
        call. = FALSE
      )
    }
  }

  invisible(NULL)
}

#' Load features file with error handling
#' @keywords internal
.load_features_file <- function(features) {
  tbl <- tryCatch(
    tidytable::fread(
      features,
      na.strings = c("", "NA"),
      colClasses = "character"
    ),
    error = function(e) {
      stop(
        "Failed to read features file: ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )

  if (nrow(tbl) == 0L) {
    stop("Features file is empty: ", features, call. = FALSE)
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
  # Detect and select columns for MZmine (Peak area/height), SLAW (quant_), SIRIUS
  tbl |>
    tidytable::select(
      tidyselect::any_of(x = c(name_features, name_rt, name_mz, name_adduct)),
      tidyselect::matches(match = " Peak area"),
      tidyselect::matches(match = ":area"),
      tidyselect::matches(match = "quant_"),
      tidyselect::matches(match = " Peak height")
    ) |>
    tidytable::select(-tidyselect::matches(match = "quant_peaktable"))
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

  col_names_clean <- colnames(tbl)
  for (pattern in names(replacements)) {
    col_names_clean <- stringi::stri_replace_all_fixed(
      col_names_clean,
      pattern = pattern,
      replacement = replacements[[pattern]],
      vectorize_all = FALSE
    )
  }
  colnames(tbl) <- col_names_clean
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
  tbl |>
    tidytable::pivot_longer(
      cols = !tidyselect::any_of(
        x = c(name_features, name_rt, name_mz, name_adduct)
      ),
      names_to = "sample"
    ) |>
    tidytable::filter(value != 0) |>
    tidytable::mutate(
      rank = rank(-as.numeric(value)),
      .by = tidyselect::all_of(x = c(name_features))
    ) |>
    tidytable::filter(rank <= candidates) |>
    tidytable::select(
      tidyselect::any_of(
        x = c(
          feature_id = name_features,
          rt = name_rt,
          mz = name_mz,
          adduct = name_adduct,
          "sample"
        )
      )
    ) |>
    tidytable::arrange(feature_id |> as.numeric())
}
