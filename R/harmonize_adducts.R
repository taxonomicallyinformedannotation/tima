#' @title Harmonize adduct notations
#'
#' @description Standardizes adduct notations in a dataframe by replacing
#'     various forms with canonical representations. Uses a translation
#'     table for efficient batch replacement.
#'
#' @details Common adduct variations like "M+H", "[M+H]", and "(M+H)+" are
#'     standardized to a consistent format (e.g., "[M+H]+"). This ensures
#'     compatibility across different MS tools and databases.
#'
#' @include validators.R
#'
#' @param df Data frame or tibble containing adduct column
#' @param adducts_colname Character string name of the adduct column
#'     (default: "adduct")
#' @param adducts_translations Named character vector mapping original
#'     adduct notations (names) to standardized forms (values).
#'     If missing, returns dataframe unchanged.
#'
#' @return Data frame with harmonized adduct column
#'
#' @export
#'
#' @examples
#' \dontrun{
#' df <- data.frame(adduct = c("M+H", "[M+Na]+", "(M-H)-"))
#' translations <- c("M+H" = "[M+H]+", "(M-H)-" = "[M-H]-")
#' harmonize_adducts(df, adducts_translations = translations)
#' }
harmonize_adducts <- function(
  df,
  adducts_colname = "adduct",
  adducts_translations
) {
  # Input Validation ----
  validate_dataframe(df, param_name = "df")
  validate_character(
    adducts_colname,
    param_name = "adducts_colname",
    allow_empty = FALSE
  )

  # Early Exits ----

  # No column to harmonize
  if (!adducts_colname %in% names(df)) {
    logger::log_debug(
      "Column '{adducts_colname}' not found, skipping harmonization"
    )
    return(df)
  }

  # No translations provided
  if (missing(adducts_translations) || length(adducts_translations) == 0L) {
    return(df)
  }

  # Validate translations
  validate_adduct_translations(adducts_translations)

  # Harmonize Adducts ----
  n_unique_before <- count_unique_values(df[[adducts_colname]])

  # Vectorized replacement (faster than loop)
  df[[adducts_colname]] <- stringi::stri_replace_all_fixed(
    str = df[[adducts_colname]],
    pattern = names(adducts_translations),
    replacement = adducts_translations,
    vectorize_all = FALSE
  )

  n_unique_after <- count_unique_values(df[[adducts_colname]])

  # Log reduction in unique forms (indicates successful harmonization)
  if (n_unique_before != n_unique_after) {
    logger::log_debug(
      "Harmonized: {n_unique_before} → {n_unique_after} unique adduct forms"
    )
  }

  df
}

# Helper Functions ----

#' Validate adduct translations structure
#' @keywords internal
validate_adduct_translations <- function(translations) {
  if (!is.character(translations)) {
    stop(
      "adducts_translations must be a character vector, got: ",
      class(translations)[1],
      call. = FALSE
    )
  }

  if (is.null(names(translations))) {
    stop(
      "adducts_translations must be a named vector ",
      "(names = original, values = replacements)",
      call. = FALSE
    )
  }

  invisible(TRUE)
}

#' Count unique non-NA values
#' @keywords internal
count_unique_values <- function(x) {
  length(unique(x[!is.na(x)]))
}
