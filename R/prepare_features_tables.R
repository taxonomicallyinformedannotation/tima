#' @title Prepare features table
#'
#' @description This function prepares LC-MS feature tables by standardizing
#'     column names, filtering to top intensity samples per feature, and
#'     formatting data for downstream analysis. Supports multiple input formats
#'     (MZmine, SLAW, SIRIUS).
#'
#' @include get_params.R
#'
#' @param features Character string path to raw features file
#' @param output Character string path where prepared features should be saved
#' @param candidates Integer number of top-intensity samples to retain per feature
#'     (recommended: ≤5 to reduce data size while keeping representative samples)
#' @param name_adduct Character string name of the adduct column in input
#' @param name_features Character string name of the feature ID column in input
#' @param name_rt Character string name of the retention time column in input
#' @param name_mz Character string name of the m/z column in input
#'
#' @return Character string path to the prepared feature table
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
  # Validate inputs
  if (!is.character(features) || length(features) != 1L) {
    stop("features must be a single character string")
  }

  if (!file.exists(features)) {
    stop("Features file not found: ", features)
  }

  if (!is.character(output) || length(output) != 1L) {
    stop("output must be a single character string")
  }

  if (!is.numeric(candidates) || candidates < 1L || candidates > 5L) {
    stop("candidates must be an integer between 1 and 5, got: ", candidates)
  }

  # Validate column name parameters
  col_names <- list(
    name_adduct = name_adduct,
    name_features = name_features,
    name_rt = name_rt,
    name_mz = name_mz
  )

  for (col_name in names(col_names)) {
    if (
      !is.character(col_names[[col_name]]) ||
        length(col_names[[col_name]]) != 1L
    ) {
      stop(col_name, " must be a single character string")
    }
  }

  logger::log_info("Preparing features table from: ", features)
  logger::log_debug(
    "Retaining top ",
    candidates,
    " intensity samples per feature"
  )

  # Load features table
  logger::log_trace("Loading features table")
  features_table_0 <- tryCatch(
    {
      tidytable::fread(
        features,
        na.strings = c("", "NA"),
        colClasses = "character"
      )
    },
    error = function(e) {
      stop("Failed to read features file: ", conditionMessage(e))
    }
  )

  if (nrow(features_table_0) == 0L) {
    stop("Features file is empty")
  }

  logger::log_debug("Loaded {nrow(features_table_0)} features")

  # Format feature table
  logger::log_trace("Formatting feature table")
  logger::log_trace(
    "Detecting format: MZmine ('Peak area' or ':area'), ",
    "SLAW ('quant_'), or SIRIUS ('Peak height')"
  )

  features_table <- features_table_0 |>
    tidytable::select(
      tidyselect::any_of(c(name_features, name_rt, name_mz, name_adduct)),
      tidyselect::matches(" Peak area"),
      tidyselect::matches(":area"),
      tidyselect::matches("quant_"),
      tidyselect::matches(" Peak height")
    ) |>
    tidytable::select(-tidyselect::matches("quant_peaktable"))
  rm(features_table_0)

  # Standardize column names
  logger::log_trace("Standardizing column names")
  colnames(features_table) <- colnames(features_table) |>
    stringi::stri_replace_all_fixed(
      pattern = " Peak area",
      replacement = "",
      vectorize_all = FALSE
    )
  colnames(features_table) <- colnames(features_table) |>
    stringi::stri_replace_all_fixed(
      pattern = ":area",
      replacement = "",
      vectorize_all = FALSE
    ) |>
    stringi::stri_replace_all_fixed(
      pattern = "datafile:",
      replacement = "",
      vectorize_all = FALSE
    )
  colnames(features_table) <- colnames(features_table) |>
    stringi::stri_replace_all_fixed(
      pattern = "quant_",
      replacement = "",
      vectorize_all = FALSE
    )
  colnames(features_table) <- colnames(features_table) |>
    stringi::stri_replace_all_fixed(
      pattern = " Peak height",
      replacement = "",
      vectorize_all = FALSE
    )

  logger::log_trace(
    "Filtering to top ",
    candidates,
    " intensity samples per feature"
  )
  features_prepared <- features_table |>
    tidytable::pivot_longer(
      cols = !tidyselect::any_of(c(
        name_features,
        name_rt,
        name_mz,
        name_adduct
      )),
      names_to = "sample"
    ) |>
    tidytable::filter(value != 0) |>
    tidytable::mutate(
      rank = rank(-as.numeric(value)),
      .by = tidyselect::all_of(c(name_features))
    ) |>
    tidytable::filter(rank <= candidates) |>
    tidytable::select(tidyselect::any_of(
      c(
        feature_id = name_features,
        rt = name_rt,
        mz = name_mz,
        adduct = name_adduct,
        "sample"
      )
    )) |>
    tidytable::arrange(
      feature_id |>
        as.numeric()
    )
  rm(features_table)

  logger::log_info(
    "Prepared ",
    nrow(features_prepared),
    " feature-sample pairs"
  )

  export_params(
    parameters = get_params(step = "prepare_features_tables"),
    step = "prepare_features_tables"
  )
  export_output(x = features_prepared, file = output)
  rm(features_prepared)

  return(output)
}
