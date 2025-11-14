#' @title Filter annotations
#'
#' @description This function filters initial annotations by removing MS1-only
#'     annotations that also have spectral matches, and optionally filtering by
#'     retention time tolerance when RT libraries are available.
#'
#' @include get_params.R
#'
#' @param annotations Character vector or list of paths to prepared annotation files
#' @param features Character string path to prepared features file
#' @param rts Character string path to prepared retention time library (optional)
#' @param output Character string path for filtered annotations output
#' @param tolerance_rt Numeric RT tolerance in minutes for library matching
#'
#' @return Character string path to the filtered annotations file
#'
#' @export
#'
#' @examples
#' \dontrun{
#' copy_backbone()
#' go_to_cache()
#' github <- "https://raw.githubusercontent.com/"
#' repo <- "taxonomicallyinformedannotation/tima-example-files/main/"
#' dir <- paste0(github, repo)
#' annotations <- get_params(step = "filter_annotations")$files$annotations$prepared$structural[[2]] |>
#'   gsub(pattern = ".gz", replacement = "", fixed = TRUE)
#' features <- get_params(step = "filter_annotations")$files$features$prepared |>
#'   gsub(pattern = ".gz", replacement = "", fixed = TRUE)
#' rts <- get_params(step = "filter_annotations")$files$libraries$temporal$prepared |>
#'   gsub(pattern = ".gz", replacement = "", fixed = TRUE)
#' get_file(url = paste0(dir, annotations), export = annotations)
#' get_file(url = paste0(dir, features), export = features)
#' get_file(url = paste0(dir, rts), export = rts)
#' filter_annotations(
#'   annotations = annotations,
#'   features = features,
#'   rts = rts
#' )
#' unlink("data", recursive = TRUE)
#' }
filter_annotations <- function(
  annotations = get_params(
    step = "filter_annotations"
  )$files$annotations$prepared$structural,
  features = get_params(step = "filter_annotations")$files$features$prepared,
  rts = get_params(
    step = "filter_annotations"
  )$files$libraries$temporal$prepared,
  output = get_params(step = "filter_annotations")$files$annotations$filtered,
  tolerance_rt = get_params(
    step = "filter_annotations"
  )$ms$tolerances$rt$library
) {
  # ============================================================================
  # Input Validation
  # ============================================================================

  # Validate tolerance first (cheapest check)
  if (!is.numeric(tolerance_rt) || tolerance_rt <= 0) {
    stop("tolerance_rt must be a positive number, got: ", tolerance_rt)
  }

  # Validate output path
  if (!is.character(output) || length(output) != 1L) {
    stop("output must be a single character string")
  }

  # Validate features
  if (!is.character(features) || length(features) != 1L) {
    stop("features must be a single character string")
  }

  if (!file.exists(features)) {
    stop("Features file not found: ", features)
  }

  # Validate annotations (handle both list and character vector)
  if (is.list(annotations)) {
    if (length(annotations) == 0L) {
      stop("annotations must be a non-empty list or character vector")
    }

    # Validation for list elements
    ann_vec <- unlist(annotations)
    if (!is.character(ann_vec)) {
      stop("All annotation elements must be character strings")
    }

    missing_annotations <- ann_vec[!file.exists(ann_vec)]
    if (length(missing_annotations) > 0L) {
      stop(
        "Annotation file(s) not found: ",
        paste(missing_annotations, collapse = ", ")
      )
    }
  } else {
    # Character vector
    if (!is.character(annotations) || length(annotations) == 0L) {
      stop("annotations must be a non-empty character vector or list")
    }

    missing_annotations <- annotations[!file.exists(annotations)]
    if (length(missing_annotations) > 0L) {
      stop(
        "Annotation file(s) not found: ",
        paste(missing_annotations, collapse = ", ")
      )
    }
  }

  # Validate RT files
  if (length(rts) > 0) {
    rts_exist <- purrr::map_lgl(rts, file.exists)
    if (!all(rts_exist)) {
      stop(
        "Retention time file(s) not found: ",
        paste(rts[!rts_exist], collapse = ", ")
      )
    }
  } else {
    rts <- NULL
  }

  # ============================================================================
  # Load and Process Data
  # ============================================================================

  logger::log_info("Filtering annotations")
  logger::log_debug("RT tolerance: {tolerance_rt} minutes")

  logger::log_trace("Loading features table")
  features_table <- tidytable::fread(
    file = features,
    colClasses = "character",
    na.strings = c("", "NA")
  ) |>
    tidytable::distinct(
      feature_id,
      rt,
      mz
    )

  n_features <- nrow(features_table)
  logger::log_info(
    "Processing {n_features} unique features for annotation filtering"
  )

  # ============================================================================
  # Load and Merge Annotations
  # ============================================================================

  logger::log_debug("Loading {length(annotations)} annotation file(s)")
  annotation_tables_list <- purrr::map(
    .x = annotations,
    .f = tidytable::fread,
    na.strings = c("", "NA"),
    colClasses = "character"
  )

  # Filter MS1 annotations when spectral matches exist
  if ("ms1" %in% names(annotation_tables_list)) {
    logger::log_info(
      "Removing MS1 annotations superseded by spectral matches"
    )

    # Extract spectral annotations (all non-MS1)
    spectral_names <- names(annotation_tables_list)[
      names(annotation_tables_list) != "ms1"
    ]
    annotations_tables_spectral <- annotation_tables_list[spectral_names] |>
      tidytable::bind_rows()

    n_spectral <- nrow(annotations_tables_spectral)
    logger::log_debug("Found {n_spectral} spectral annotations")

    # Create key for anti-join
    spectral_keys <- annotations_tables_spectral |>
      tidytable::distinct(
        feature_id,
        ## Comment not taking into account because of inconsistencies among tools
        # candidate_adduct,
        candidate_structure_inchikey_connectivity_layer
      )

    n_ms1_before <- nrow(annotation_tables_list[["ms1"]])

    # Remove redundant MS1 and combine
    annotation_table <- annotation_tables_list[["ms1"]] |>
      tidytable::anti_join(spectral_keys) |>
      tidytable::bind_rows(annotations_tables_spectral)

    n_ms1_removed <- n_ms1_before - (nrow(annotation_table) - n_spectral)
    logger::log_info("Removed {n_ms1_removed} redundant MS1 annotations")

    # Clean up intermediate objects
    rm(annotations_tables_spectral, spectral_keys, spectral_names)
  } else {
    logger::log_debug("No MS1 annotations to filter, combining all annotations")
    annotation_table <- annotation_tables_list |>
      tidytable::bind_rows()
  }

  n_total_annotations <- nrow(annotation_table)
  logger::log_info(
    "Total annotations before RT filtering: {n_total_annotations}"
  )

  # Clean up
  rm(annotation_tables_list)

  logger::log_trace("Loading retention time library")
  if (!is.null(rts)) {
    rt_table <- purrr::map(
      .x = rts,
      .f = tidytable::fread,
      na.strings = c("", "NA"),
      colClasses = "character"
    ) |>
      tidytable::bind_rows() |>
      tidytable::rename(rt_target = rt)

    n_rt_standards <- nrow(rt_table)
    logger::log_debug("Loaded {n_rt_standards} retention time standards")
  }

  features_annotated_table_1 <- features_table |>
    tidytable::left_join(annotation_table)
  rm(annotation_table)

  if (!is.null(rts)) {
    logger::log_info(
      "Filtering annotations outside {tolerance_rt} min RT tolerance"
    )
    features_annotated_table_2 <- features_annotated_table_1 |>
      tidytable::left_join(rt_table) |>
      tidytable::mutate(
        candidate_structure_error_rt = as.numeric(rt) -
          as.numeric(rt_target)
      ) |>
      tidytable::arrange(abs(candidate_structure_error_rt)) |>
      tidytable::distinct(
        -candidate_structure_error_rt,
        -rt_target,
        .keep_all = TRUE
      ) |>
      tidytable::filter(
        abs(candidate_structure_error_rt) <= abs(tolerance_rt) |
          is.na(candidate_structure_error_rt)
      ) |>
      tidytable::select(-rt_target, -type)
  } else {
    logger::log_debug("No RT library provided, skipping RT filtering")
    features_annotated_table_2 <- features_annotated_table_1 |>
      tidytable::mutate(candidate_structure_error_rt = NA)
  }

  n_removed_rt <- nrow(features_annotated_table_1) -
    nrow(features_annotated_table_2)
  logger::log_info(
    "Removed {n_removed_rt} annotations based on retention time tolerance"
  )
  rm(features_annotated_table_1)

  ## in case some features had a single filtered annotation
  final_table <- features_table |>
    tidytable::left_join(features_annotated_table_2)

  rm(
    features_table,
    features_annotated_table_2
  )

  export_params(
    parameters = get_params(step = "filter_annotations"),
    step = "filter_annotations"
  )
  export_output(x = final_table, file = output[[1]])

  rm(final_table)
  return(output[[1]])
}
