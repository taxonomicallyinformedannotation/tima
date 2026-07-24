#' @title Extract narrow biological score table
#'
#' @description Extracts only the narrow scoring columns from the full
#'     weight_bio output, avoiding wide table materialization.
#'
#' @include weight_bio.R
#' @include validations_utils.R
#'
#' @param weight_bio_result [data.frame] Output from weight_bio()
#'
#' @return Data frame with columns:
#'     - feature_id
#'     - candidate_structure_inchikey_connectivity_layer
#'     - score_chemical
#'     - score_weighted_chemo
#'     - score_weighted_chemo_coverage
#'
#' @keywords internal
extract_chemo_scores <- function(weight_chemo_result) {
  validate_dataframe(weight_chemo_result, param_name = "weight_chemo_result")

  if (nrow(weight_chemo_result) == 0L) {
    return(
      tidytable::tidytable(
        feature_id = character(),
        candidate_structure_inchikey_connectivity_layer = character(),
        score_chemical = numeric(),
        score_weighted_chemo = numeric(),
        score_weighted_chemo_coverage = numeric()
      )
    )
  }

  weight_chemo_result |>
    tidytable::select(
      feature_id,
      candidate_structure_inchikey_connectivity_layer,
      score_chemical,
      score_weighted_chemo,
      score_weighted_chemo_coverage
    )
}
