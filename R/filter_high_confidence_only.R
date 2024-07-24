import::from(crayon, red, .into = environment())
import::from(tidytable, filter, .into = environment())

#' @title Filter high confidence only
#' `r lifecycle::badge("experimental")`
#'
#' @description This function filters highly confident annotations only.
#'
#' @importFrom crayon red
#' @importFrom tidytable filter
#'
#' @param df Dataframe
#' @param score_bio_min Minimal biological score. Current default to 0.85.
#' @param score_ini_min Minimal initial score. Current default to 0.75.
#'
#' @return NULL
#'
#' @export
#'
#' @examples NULL
filter_high_confidence_only <-
  function(df,
           score_bio_min = 0.85,
           score_ini_min = 0.75) {
    log_debug("Keeping high confidence candidates only...")
    before <- nrow(df)
    # TODO this is very basic for now but already massively filters.
    # TODO Later implement SIRIUS/internal library filters.
    df <- df |>
      filter(score_biological >= score_bio_min |
        candidate_score_pseudo_initial >= score_ini_min)
    after <- nrow(df)
    log_debug("Removed", red(before - after), "low confidence candidates")
    rm(before, after)
    return(df)
  }
