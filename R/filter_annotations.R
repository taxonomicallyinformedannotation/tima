#' Validate Inputs for filter_annotations
#'
#' @description Internal helper to validate all input parameters.
#'
#' @param annotations Character vector or list of annotation file paths
#' @param features Character path to features file
#' @param rts Character vector of RT library paths (optional)
#' @param output Character path for output
#' @param tolerance_rt Numeric RT tolerance
#'
#' @return NULL (stops execution if validation fails)
#' @keywords internal
validate_filter_annotations_inputs <- function(
  annotations,
  features,
  rts,
  output,
  tolerance_rt
) {
  # Validate tolerance first (cheapest check)
  if (!is.numeric(tolerance_rt) || tolerance_rt <= 0) {
    stop(
      "tolerance_rt must be a positive number, got: ",
      tolerance_rt,
      call. = FALSE
    )
  }

  # Validate output path
  if (!is.character(output) || length(output) != 1L) {
    stop("output must be a single character string", call. = FALSE)
  }

  # Validate features
  if (!is.character(features) || length(features) != 1L) {
    stop("features must be a single character string", call. = FALSE)
  }

  if (!file.exists(features)) {
    stop("Features file not found: ", features, call. = FALSE)
  }

  # Validate annotations (handle both list and character vector)
  if (is.list(annotations)) {
    if (length(annotations) == 0L) {
      stop(
        "annotations must be a non-empty list or character vector",
        call. = FALSE
      )
    }

    ann_vec <- unlist(annotations)
    if (!is.character(ann_vec)) {
      stop("All annotation elements must be character strings", call. = FALSE)
    }

    missing_annotations <- ann_vec[!file.exists(ann_vec)]
    if (length(missing_annotations) > 0L) {
      stop(
        "Annotation file(s) not found: ",
        paste(missing_annotations, collapse = ", "),
        call. = FALSE
      )
    }
  } else {
    if (!is.character(annotations) || length(annotations) == 0L) {
      stop(
        "annotations must be a non-empty character vector or list",
        call. = FALSE
      )
    }

    missing_annotations <- annotations[!file.exists(annotations)]
    if (length(missing_annotations) > 0L) {
      stop(
        "Annotation file(s) not found: ",
        paste(missing_annotations, collapse = ", "),
        call. = FALSE
      )
    }
  }

  # Validate RT files
  if (length(rts) > 0) {
    rts_exist <- purrr::map_lgl(.x = rts, .f = file.exists)
    if (!all(rts_exist)) {
      stop(
        "Retention time file(s) not found: ",
        paste(rts[!rts_exist], collapse = ", "),
        call. = FALSE
      )
    }
  }

  invisible(NULL)
}

#' Filter MS1 Annotations
#'
#' @description Internal helper to remove MS1 annotations that have spectral matches.
#'
#' @param annotation_tables_list Named list of annotation tables
#'
#' @return Data frame with filtered annotations
#' @keywords internal
filter_ms1_redundancy <- function(annotation_tables_list) {
  if (!"ms1" %in% names(annotation_tables_list)) {
    log_debug("No MS1 annotations to filter, combining all annotations")
    return(tidytable::bind_rows(annotation_tables_list))
  }

  log_info("Removing MS1 annotations superseded by spectral matches")

  # Extract spectral annotations (all non-MS1)
  spectral_names <- names(annotation_tables_list)[
    names(annotation_tables_list) != "ms1"
  ]
  annotations_tables_spectral <- annotation_tables_list[spectral_names] |>
    tidytable::bind_rows()

  n_spectral <- nrow(annotations_tables_spectral)
  log_debug("Found %d spectral annotations", n_spectral)

  # Create key for anti-join
  spectral_keys <- annotations_tables_spectral |>
    tidytable::distinct(
      feature_id,
      candidate_structure_inchikey_connectivity_layer
    )

  n_ms1_before <- nrow(annotation_tables_list[["ms1"]])

  # Remove redundant MS1 and combine
  annotation_table <- annotation_tables_list[["ms1"]] |>
    tidytable::anti_join(
      y = spectral_keys,
      by = c(
        "feature_id",
        "candidate_structure_inchikey_connectivity_layer"
      )
    ) |>
    tidytable::bind_rows(annotations_tables_spectral)

  n_ms1_removed <- n_ms1_before - (nrow(annotation_table) - n_spectral)
  log_info("Removed %d redundant MS1 annotations", n_ms1_removed)

  annotation_table
}

#' Apply RT Filtering
#'
#' @description Internal helper to filter annotations by RT tolerance.
#'
#' @param features_annotated_table Data frame with features and annotations
#' @param rt_table Data frame with RT standards
#' @param tolerance_rt Numeric RT tolerance in minutes
#'
#' @return Filtered data frame
#' @keywords internal
apply_rt_filter <- function(features_annotated_table, rt_table, tolerance_rt) {
  log_info(
    "Filtering annotations outside {tolerance_rt} min RT tolerance"
  )

  features_annotated_table |>
    tidytable::left_join(
      y = rt_table,
      by = "candidate_structure_inchikey_connectivity_layer"
    ) |>
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
      abs(candidate_structure_error_rt) <=
        abs(tolerance_rt) + .Machine$double.eps |
        is.na(candidate_structure_error_rt)
    ) |>
    tidytable::select(-tidyselect::any_of(x = c("rt_target", "type")))
}

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
  # Input Validation ----

  validate_filter_annotations_inputs(
    annotations = annotations,
    features = features,
    rts = rts,
    output = output,
    tolerance_rt = tolerance_rt
  )

  # Normalize RT input
  if (length(rts) == 0) {
    rts <- NULL
  }

  # Load and Process Data ----

  log_info("Filtering annotations")
  log_debug("RT tolerance: %f2 minutes", tolerance_rt)

  features_table <- tidytable::fread(
    file = features,
    colClasses = "character",
    na.strings = c("", "NA")
  ) |>
    tidytable::distinct(feature_id, rt, mz)

  n_features <- nrow(features_table)
  log_info(
    "Processing %d unique features for annotation filtering",
    n_features
  )

  # Load and Merge Annotations ----

  log_debug("Loading %d annotation file(s)", length(annotations))
  annotation_tables_list <- purrr::map(
    .x = annotations,
    .f = tidytable::fread,
    na.strings = c("", "NA"),
    colClasses = "character"
  )

  # Filter MS1 redundancy
  annotation_table <- filter_ms1_redundancy(annotation_tables_list)
  rm(annotation_tables_list)

  n_total_annotations <- nrow(annotation_table)
  log_info(
    "Total annotations before RT filtering: %d",
    n_total_annotations
  )

  # Apply RT Filtering if Library Available ----

  features_annotated_table_1 <- features_table |>
    tidytable::left_join(y = annotation_table, by = "feature_id")
  rm(annotation_table)

  if (!is.null(rts)) {
    rt_table <- purrr::map(
      .x = rts,
      .f = tidytable::fread,
      na.strings = c("", "NA"),
      colClasses = "character"
    ) |>
      tidytable::bind_rows()

    # Robust rename: support either 'rt' or pre-renamed 'rt_target'
    if ("rt" %in% names(rt_table)) {
      rt_table <- rt_table |>
        tidytable::rename(rt_target = rt)
    } else if (!"rt_target" %in% names(rt_table)) {
      stop(
        "Retention time library must contain column 'rt' or 'rt_target'",
        call. = FALSE
      )
    }

    n_rt_standards <- nrow(rt_table)
    log_debug("Loaded %d retention time standards", n_rt_standards)

    features_annotated_table_2 <- apply_rt_filter(
      features_annotated_table_1,
      rt_table,
      tolerance_rt
    )
  } else {
    log_debug("No RT library provided, skipping RT filtering")
    features_annotated_table_2 <- features_annotated_table_1 |>
      tidytable::mutate(candidate_structure_error_rt = NA)
  }

  n_removed_rt <- nrow(features_annotated_table_1) -
    nrow(features_annotated_table_2)
  log_info(
    "Removed %d annotations based on retention time tolerance",
    n_removed_rt
  )
  rm(features_annotated_table_1)

  ## in case some features had a single filtered annotation
  final_table <- features_table |>
    tidytable::left_join(y = features_annotated_table_2)

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
