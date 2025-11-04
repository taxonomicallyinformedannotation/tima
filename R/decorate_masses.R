#' @title Decorate masses
#'
#' @description This function logs summary statistics about MS1-based
#'     annotations, reporting the number of unique structures and features
#'     annotated
#'
#' @param annotation_table_ms1 Data frame containing MS1 annotation results
#'     with columns for feature_id and candidate_structure_inchikey_connectivity_layer
#'
#' @return The input annotation table (unchanged), for use in pipelines
#'
#' @examples NULL
decorate_masses <- function(
  annotation_table_ms1 = get("annotation_table_ms1", envir = parent.frame())
) {
  # Filter for valid annotations (not NA and not "notAnnotated")
  valid_annotations <- annotation_table_ms1 |>
    tidytable::filter(
      !is.na(candidate_structure_inchikey_connectivity_layer) &
        candidate_structure_inchikey_connectivity_layer != "notAnnotated"
    )

  # Count unique structures and features
  n_unique_structures <- valid_annotations |>
    tidytable::distinct(candidate_structure_inchikey_connectivity_layer) |>
    nrow()

  n_unique_features <- valid_annotations |>
    tidytable::distinct(feature_id) |>
    nrow()

  # Log summary
  logger::log_info(
    "MS1 annotation results: ",
    n_unique_structures, " unique structures annotated across ",
    n_unique_features, " features"
  )

  return(annotation_table_ms1)
}
