#' @title Harmonize adducts
#'
#' @description This function harmonizes adduct definitions by replacing
#'     various adduct notations with standardized forms according to a
#'     translation table.
#'
#' @param df Dataframe containing adduct column to harmonize
#' @param adducts_colname Character string name of the adduct column
#'     (default: "adduct")
#' @param adducts_translations Named character vector mapping original
#'     adduct notations (names) to standardized forms (values)
#'
#' @return The dataframe with harmonized adduct column
#'
#' @examples NULL
harmonize_adducts <- function(
  df,
  adducts_colname = "adduct",
  adducts_translations
) {
  # ============================================================================
  # Input Validation
  # ============================================================================

  # Validate dataframe
  if (!is.data.frame(df) && !inherits(df, "tbl")) {
    stop("Input 'df' must be a data frame or tibble")
  }

  # Early exit if column doesn't exist
  if (!adducts_colname %in% names(df)) {
    logger::log_debug(
      "Adduct column '{adducts_colname}' not found in dataframe, skipping harmonization"
    )
    return(df)
  }

  # Early exit if no translations provided
  if (missing(adducts_translations) || length(adducts_translations) == 0L) {
    # logger::log_trace("No adduct translations provided, skipping harmonization")
    return(df)
  }

  # Validate translations are named character vector
  if (
    !is.character(adducts_translations) || is.null(names(adducts_translations))
  ) {
    stop("adducts_translations must be a named character vector")
  }

  # ============================================================================
  # Harmonize Adducts
  # ============================================================================

  # Perform string replacement
  n_before <- length(unique(df[[adducts_colname]]))

  df[[adducts_colname]] <- stringi::stri_replace_all_fixed(
    str = df[[adducts_colname]],
    pattern = names(adducts_translations),
    replacement = adducts_translations,
    vectorize_all = FALSE
  )

  n_after <- length(unique(df[[adducts_colname]]))

  if (n_before != n_after) {
    # logger::log_trace(
    #  "Harmonized adducts: {n_before} unique forms -> {n_after} unique forms"
    #)
  }

  df
}
