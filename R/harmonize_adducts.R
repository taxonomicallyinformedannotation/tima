# Pre-computed formula normalization patterns (avoid re-creating each call)
.FORMULA_SUBS_PATTERNS <- c(
  "\\+NH4",
  "\\+2NH4",
  "\\+NH3",
  "\\-NH3",
  "\\+H\\-H2O\\]",
  "\\+H\\-H4O2\\]",
  "\\+H\\-H6O3\\]",
  "\\+Na\\-H2O\\]",
  "\\+K\\-H2O\\]",
  "\\-H\\-H2O\\]"
)
.FORMULA_SUBS_REPLACEMENTS <- c(
  "+H4N",
  "+2H4N",
  "+H3N",
  "-H3N",
  "-H2O+H]",
  "-H4O2+H]",
  "-H6O3+H]",
  "-H2O+Na]",
  "-H2O+K]",
  "-H2O-H]"
)

#' @title Harmonize adduct notations
#'
#' @description Standardizes adduct notations in a dataframe by replacing
#'     various forms with canonical representations. Uses a translation
#'     table for efficient batch replacement.
#'
#' @details Common adduct variations like "M+H", "\[M+H\]", and "(M+H)+" are
#'     standardized to a consistent format (e.g., "\[M+H\]+"). This ensures
#'     compatibility across different MS tools and databases.
#'
#' @include adducts_utils.R
#' @include validations_utils.R
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
#' @family mass-spectrometry
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

  do_log <- nrow(df) >= 1000L
  if (do_log) {
    ctx <- log_operation("harmonize_adducts", n_rows = nrow(df))
  }

  # Early Exits ----

  # No column to harmonize
  if (!adducts_colname %in% names(df)) {
    log_debug(
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

  # Sort them (important)
  adducts_translations <- adducts_translations[order(
    nchar(names(adducts_translations)),
    decreasing = TRUE
  )]

  # Harmonize Adducts ----
  n_unique_before <- count_unique_values(df[[adducts_colname]])

  # Normalize internal spaces (e.g., "[M + K]+" -> "[M+K]+")
  df[[adducts_colname]] <- gsub("\\s+", "", df[[adducts_colname]])

  # Fast exact-match lookup via match()
  idx <- match(df[[adducts_colname]], names(adducts_translations))
  matched <- !is.na(idx)
  if (any(matched)) {
    df[[adducts_colname]][matched] <- adducts_translations[idx[matched]]
  }

  # Second pass: substring-level formula normalization
  # Handles all combinations (dimers, losses, clusters) at once
  df[[adducts_colname]] <- stringi::stri_replace_all_regex(
    str = df[[adducts_colname]],
    pattern = .FORMULA_SUBS_PATTERNS,
    replacement = .FORMULA_SUBS_REPLACEMENTS,
    vectorize_all = FALSE
  )

  n_unique_after <- count_unique_values(df[[adducts_colname]])

  if (do_log) {
    # Log reduction in unique forms (indicates successful harmonization)
    if (n_unique_before != n_unique_after) {
      log_debug(
        "Harmonized: %d -> %d unique adduct forms",
        n_unique_before,
        n_unique_after
      )
    }

    log_complete(
      ctx,
      n_unique_before = n_unique_before,
      n_unique_after = n_unique_after
    )
  }

  df
}

# Helper Functions ----

#' Validate adduct translations structure
#' @keywords internal
validate_adduct_translations <- function(translations) {
  if (!is.character(translations)) {
    cli::cli_abort(
      c(
        "adducts_translations must be a character vector",
        "x" = class(translations)[1]
      ),
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }

  if (is.null(names(translations))) {
    cli::cli_abort(
      "adducts_translations must be a named vector (names = original, values = replacements)",
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }

  invisible(TRUE)
}

#' Count unique non-NA values
#' @keywords internal
count_unique_values <- function(x) {
  length(unique(x[!is.na(x)]))
}
