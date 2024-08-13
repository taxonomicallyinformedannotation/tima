import::from(stringi, stri_replace_all_fixed, .into = environment())

#' @title Harmonize adducts
#'
#' @description This function annotates masses
#'
#' @importFrom stringi stri_replace_all_fixed
#'
#' @param df Dataframe
#' @param adducts_colname Adducts colname
#'
#' @return A table with harmonized adducts
#'
#' @examples NULL
harmonize_adducts <- function(df, adducts_colname = "adduct") {
  adducts_translations <-
    c(
      "-2H" = "-H2",
      # cliqueMS
      "-3H" = "-H3",
      # cliqueMS
      "-2H2O" = "-H4O2 (2xH2O)",
      # mzmine
      "-3H2O" = "-H6O3 (3xH2O)",
      # mzmine
      "-4H2O" = "-H8O4 (4xH2O)",
      # mzmine
      "-5H2O" = "-H10O5 (5xH2O)",
      # mzmine
      "-NH3" = "+H3N",
      # mzmine
      "+2H" = "+H2",
      # mzmine
      "+2K" = "+K2",
      # cliqueMS
      "+2Na" = "+Na2",
      # mzmine
      "+3K" = "+K3",
      # cliqueMS
      "+3Na" = "+Na3",
      # cliqueMS
      "+Acetate" = "+C2H3O2",
      # mzmine
      "+ACN" = "+C2H3N",
      # mzmine
      "+CH3COO" = "+C2H3O2",
      # GNPS
      "+FA" = "+CHO2",
      # mzmine
      "+HAc" = "+C2H4O2",
      # mzmine
      "+Hac" = "+C2H4O2",
      # GNPS
      "+HFA" = "+CH2O2",
      # mzmine
      "+IsoProp" = "+C3H8O",
      # mzmine
      "+MeOH" = "+CH4O",
      # mzmine
      "+NH4" = "+H4N",
      # mzmine
      "+TFA" = "+C2HF3O2",
      # MassBank
      "[M+CH3COO]-/[M-CH3]-" = "[M+CH3COO]-"
      # weird MassBank
    )
  log_debug("Trying to harmonize adducts definitions...")
  df[[adducts_colname]] <- stri_replace_all_fixed(
    str = df[[adducts_colname]],
    pattern = names(adducts_translations),
    replacement = adducts_translations,
    vectorize_all = FALSE
  )
  df
}
