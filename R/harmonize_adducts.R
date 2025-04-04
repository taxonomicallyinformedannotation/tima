#' @title Harmonize adducts
#'
#' @description This function annotates masses
#'
#' @param df Dataframe
#' @param adducts_colname Adducts colname
#' @param adducts_translations Adducts translations
#'
#' @return A table with harmonized adducts
#'
#' @examples NULL
harmonize_adducts <- function(
  df,
  adducts_colname = "adduct",
  adducts_translations
) {
  logger::log_trace("Trying to harmonize adducts definitions")
  df[[adducts_colname]] <- stringi::stri_replace_all_fixed(
    str = df[[adducts_colname]],
    pattern = names(adducts_translations),
    replacement = adducts_translations,
    vectorize_all = FALSE
  )
  df
}
