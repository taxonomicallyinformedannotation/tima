#' @title Decorate MS1 annotation results with statistics
#'
#' @description Logs summary statistics about MS1-based annotations,
#'     reporting the number of unique structures and features annotated.
#'
#' @include validations_utils.R
#'
#' @param annotation_table_ms1 Data frame containing MS1 annotation results
#'     with feature_id and candidate_structure_inchikey_connectivity_layer columns
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

  # Filter Valid Annotations ----
  valid_annotations <- annotation_table_ms1 |>
    tidytable::filter(
      !is.na(candidate_structure_inchikey_connectivity_layer) &
        candidate_structure_inchikey_connectivity_layer != "notAnnotated"
    )

  # Count Statistics ----
  n_unique_structures <- valid_annotations |>
    tidytable::distinct(candidate_structure_inchikey_connectivity_layer) |>
    nrow()

  n_unique_features <- valid_annotations |>
    tidytable::distinct(feature_id) |>
    nrow()

  # Log Summary ----
  log_info(
    "MS1 annotations: %d unique structures across %d features",
    n_unique_structures,
    n_unique_features
  )

  annotation_table_ms1
}
