#' mzTab-M validation helpers
#'
#' @description Internal validators for parsed mzTab-M tables.
#'
#' @include mztab_schema_utils.R
#' @keywords internal
#' @name mztab_validate
NULL

#' @keywords internal
.validate_required_cols <- function(df, required, section) {
  if (nrow(df) == 0L) {
    return(invisible(TRUE))
  }

  normalize <- function(x) {
    tolower(gsub("-", "_", x, fixed = TRUE))
  }

  required_norm <- normalize(required)
  names_norm <- normalize(names(df))

  missing_idx <- which(!(required_norm %in% names_norm))
  missing <- required[missing_idx]

  if (length(missing) > 0L) {
    tima_abort(
      problem = paste0("Missing required columns in ", section, " section"),
      expected = paste(required, collapse = ", "),
      received = paste(names(df), collapse = ", "),
      fix = paste0("Add missing columns: ", paste(missing, collapse = ", ")),
      class = c("tima_validation_error", "tima_error")
    )
  }

  invisible(TRUE)
}

#' @keywords internal
validate_mztab_tables <- function(mztab_tables, strict = FALSE) {
  validate_list_or_vector(
    mztab_tables,
    param_name = "mztab_tables",
    min_length = 1
  )

  required <- get_mztab_required_columns()

  if (nrow(mztab_tables$metadata) == 0L) {
    tima_abort(
      problem = "Missing metadata section (MTD)",
      fix = "Ensure the mzTab file contains MTD lines",
      class = c("tima_validation_error", "tima_error")
    )
  }

  has_version <- any(mztab_tables$metadata$key == "mzTab-version", na.rm = TRUE)
  if (!has_version) {
    tima_abort(
      problem = "Missing required mzTab metadata field: mzTab-version",
      class = c("tima_validation_error", "tima_error")
    )
  }

  has_sml <- nrow(mztab_tables$sml) > 0L
  has_smf <- nrow(mztab_tables$smf) > 0L

  if (!has_sml && !has_smf) {
    tima_abort(
      problem = "mzTab-M file must contain SML and/or SMF rows",
      fix = "Provide at least one of the small molecule summary or feature sections",
      class = c("tima_validation_error", "tima_error")
    )
  }

  .validate_required_cols(mztab_tables$sml, required$SML, "SML")
  .validate_required_cols(mztab_tables$smf, required$SMF, "SMF")

  if (strict) {
    .validate_required_cols(mztab_tables$sme, required$SME, "SME")
  } else if (nrow(mztab_tables$sme) > 0L) {
    normalize <- function(x) {
      tolower(gsub("-", "_", x, fixed = TRUE))
    }
    required_sme_norm <- normalize(required$SME)
    names_sme_norm <- normalize(names(mztab_tables$sme))
    missing_sme <- required$SME[!(required_sme_norm %in% names_sme_norm)]
    if (length(missing_sme) > 0L) {
      log_warn(
        "SME section is present but missing some required columns: %s",
        paste(missing_sme, collapse = ", ")
      )
    }
  }

  invisible(TRUE)
}
