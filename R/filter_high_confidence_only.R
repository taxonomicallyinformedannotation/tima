#' @title Filter high confidence only
#' `r lifecycle::badge("experimental")`
#'
#' @description This function filters highly confident annotations only.
#'
#' @param df Dataframe
#' @param score_bio_min Minimal biological score. Current default to 0.85.
#' @param score_ini_min Minimal initial score. Current default to 0.95.
#' @param score_final_min Minimal final score. Current default to 0.75.
#'
#' @return A dataframe containing only high confidence annotations
#'
#' @examples NULL
filter_high_confidence_only <-
  function(
    df,
    score_bio_min = 0.85,
    score_ini_min = 0.95,
    score_final_min = 0.75
  ) {
    log_debug("Keeping high confidence candidates only...")
    before <- nrow(df)
    # TODO this is very basic for now but already massively filters.
    # TODO Later implement SIRIUS/internal library filters.
    df <- df |>
      tidytable::filter(
        score_biological >= score_bio_min |
          candidate_score_pseudo_initial >= score_ini_min |
          score_weighted_chemo >= score_final_min
      )
    after <- nrow(df)
    log_debug(
      "Removed",
      crayon::red(before - after),
      "low confidence candidates out of the",
      crayon::blue(before),
      "total ones."
    )
    log_debug(crayon::green(after), "high confidence candidates remaining.")
    rm(before, after)
    return(df)
  }
