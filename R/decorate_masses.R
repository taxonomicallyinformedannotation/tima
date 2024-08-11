import::from(crayon, blue, .into = environment())
import::from(crayon, green, .into = environment())
import::from(tidytable, distinct, .into = environment())
import::from(tidytable, filter, .into = environment())

#' @title Decorate masses
#'
#' @description This function outputs information about MS1 annotation
#'
#' @importFrom crayon blue
#' @importFrom crayon green
#' @importFrom tidytable distinct
#' @importFrom tidytable filter
#'
#' @param annotation_table_ms1 Table to decorate
#'
#' @return Message indicating the number of annotations obtained by MS1
#'
#' @examples NULL
decorate_masses <- function(annotation_table_ms1 =
                              get("annotation_table_ms1", envir = parent.frame())) {
  df_1 <- annotation_table_ms1 |>
    filter(
      !is.na(candidate_structure_inchikey_no_stereo) |
        candidate_structure_inchikey_no_stereo != "notAnnotated"
    )
  log_debug(
    "MS1 annotation led to \n",
    green(nrow(
      df_1 |>
        distinct(candidate_structure_inchikey_no_stereo)
    )),
    green("annotations"),
    ", on \n",
    blue(nrow(df_1 |>
      distinct(feature_id))),
    blue("features")
  )
  rm(df_1)
  return(annotation_table_ms1)
}
