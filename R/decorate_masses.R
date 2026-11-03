#' @title Decorate MS1 annotation results with statistics
#'
#' @description Logs summary statistics about MS1-based annotations,
#'     reporting the number of unique structures and features annotated.
#'
#' @include validations_utils.R
#' @include logs_utils.R
#' @include validations_params.R
#'
#' @param annotation_table_ms1 Data frame containing MS1 annotation results
#' with feature_id and candidate_structure_inchikey_connectivity_layer columns
#'
#' @return The input annotation table (unchanged), for pipeline compatibility
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # Log statistics and pass through
#' annotations_ms1 |>
#'   decorate_masses()
#' }
decorate_masses <- function(annotation_table_ms1) {
  ctx <- log_operation(
    "decorate_masses",
    n_annotations = nrow(annotation_table_ms1)
  )

  # Input Validation ----
  validate_dataframe(
    annotation_table_ms1,
    param_name = "annotation_table_ms1"
  )

  # Check Required Column ----
  if (
    !"candidate_structure_inchikey_connectivity_layer" %in%
      colnames(annotation_table_ms1)
  ) {
    log_warn(
      "MS1 annotation table missing
      candidate_structure_inchikey_connectivity_layer column.
      Skipping annotation summary."
    )
    return(annotation_table_ms1)
  }

  # Filter Valid Annotations & Count Statistics (vectorized, zero table copy) ----
  ik_col <- annotation_table_ms1[[
    "candidate_structure_inchikey_connectivity_layer"
  ]]
  valid_idx <- which(!is.na(ik_col) & ik_col != "notAnnotated")
  n_unique_structures <- length(unique(ik_col[valid_idx]))
  n_unique_features <- if ("feature_id" %in% colnames(annotation_table_ms1)) {
    length(unique(annotation_table_ms1[["feature_id"]][valid_idx]))
  } else {
    0L
  }

  # Log Summary ----
  log_info(
    "MS1 annotations: %d unique structures across %d features",
    n_unique_structures,
    n_unique_features
  )

  log_complete(
    ctx,
    n_structures = n_unique_structures,
    n_features = n_unique_features
  )

  annotation_table_ms1
}
