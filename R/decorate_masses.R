utils::globalVariables(c("annotation_table_ms1"))

#' @title Decorate masses
#'
#' @description This function outputs information about MS1 annotation
#'
#' @param annotation_table_ms1 Table to decorate
#'
#' @return Message indicating the number of annotations obtained by MS1
#'
#' @export
#'
#' @examples NULL
decorate_masses <- function(annotation_table_ms1 =
                              get("annotation_table_ms1",
                                envir = parent.frame()
                              )) {
  df_1 <- annotation_table_ms1 |>
    tidytable::filter(score_input == 0) |>
    tidytable::filter(!is.na(structure_inchikey_2D) |
      structure_inchikey_2D != "notAnnotated")
  log_debug(
    "MS1 annotation led to \n",
    crayon::green(nrow(
      df_1 |>
        tidytable::distinct(structure_inchikey_2D)
    )),
    crayon::green("annotations"),
    ", on \n",
    crayon::blue(nrow(
      df_1 |>
        tidytable::distinct(feature_id)
    )),
    crayon::blue("features")
  )
}
