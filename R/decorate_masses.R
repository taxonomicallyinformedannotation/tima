#' @title Decorate masses
#'
#' @description This function outputs information about MS1 annotation
#'
#' @param annotation_table_ms1 Table to decorate
#'
#' @return Message indicating the number of annotations obtained by MS1
#'
#' @examples NULL
decorate_masses <- function(
  annotation_table_ms1 = get("annotation_table_ms1", envir = parent.frame())
) {
  df_1 <- annotation_table_ms1 |>
    tidytable::filter(
      !is.na(candidate_structure_inchikey_connectivity_layer) |
        candidate_structure_inchikey_connectivity_layer != "notAnnotated"
    )
  logger::log_info(
    "MS1 annotation led to {nrow(
      df_1 |>
        tidytable::distinct(candidate_structure_inchikey_connectivity_layer)
    )} annotations on {nrow(
      df_1 |>
        tidytable::distinct(feature_id)
    )} features"
  )
  rm(df_1)
  return(annotation_table_ms1)
}
