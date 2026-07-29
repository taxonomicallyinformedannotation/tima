#' @title Extract narrow chemical score table
#'
#' @description Extracts only the narrow scoring columns from the full
#'     weight_chemo output, avoiding wide table materialization.
#'
#' @include weight_chemo.R
#' @include validations_utils.R
#' @include validations_params.R
#'
#' @param weight_chemo_result [data.frame] Output from weight_chemo()
#'
#' @return Data frame with columns:
#'     - feature_id
#'     - candidate_structure_inchikey_connectivity_layer
#'     - score_chemical
#'     - score_weighted_chemo
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
        score_weighted_chemo = numeric()
      )
    )
  }

  weight_chemo_result |>
    tidytable::select(
      feature_id,
      candidate_structure_inchikey_connectivity_layer,
      score_chemical,
      score_weighted_chemo
    )
}
