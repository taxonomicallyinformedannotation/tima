#' @title Decorate masses
#'
#' @description This function outputs information about MS1 annotation
#'
#' @param df Table to decorate
#'
#' @return Message indicating the number of annotations obtained by MS1
#'
#' @export
#'
#' @examples NULL
decorate_masses <- function(df = annotation_table_ms1) {
  df_1 <- df |>
    dplyr::filter(score_input == 0) |>
    dplyr::filter(!is.na(structure_inchikey_2D) |
      structure_inchikey_2D != "notAnnotated")
  log_debug(
    "MS1 annotation led to \n",
    crayon::green(nrow(df_1 |>
      tidytable::distinct(structure_inchikey_2D))),
    crayon::green("annotations"),
    ", on \n",
    crayon::blue(nrow(df_1 |>
      tidytable::distinct(feature_id))),
    crayon::blue("features")
  )
}
