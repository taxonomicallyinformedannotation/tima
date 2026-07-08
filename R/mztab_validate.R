#' mzTab-M validation helpers
#'
#' @description Internal validators for parsed mzTab-M tables.
#'
#' @include mztab_schema_utils.R
#' @keywords internal
#' @name mztab_validate
NULL

#' @keywords internal
.mztab_table_or_empty <- function(x) {
  if (is.null(x)) {
    return(tidytable::tidytable())
  }
  if (is.data.frame(x)) {
    return(x)
  }
  if (is.list(x) && length(x) > 0L) {
    return(tidytable::as_tidytable(x))
  }
  tidytable::tidytable()
}

#' @keywords internal
.validate_required_cols <- function(df, required, section) {
  df <- .mztab_table_or_empty(df)
  if (nrow(df) == 0L) {
    return(invisible(TRUE))
  }

  required_norm <- .mztab_normalize_column_name(required)
  names_norm <- .mztab_normalize_column_name(names(df))

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
.mztab_get_metadata_value <- function(metadata_df, key) {
  metadata_df <- .mztab_table_or_empty(metadata_df)
  if (nrow(metadata_df) == 0L) {
    return(NA_character_)
  }
  idx <- which(metadata_df$key == key)
  if (length(idx) == 0L) {
    return(NA_character_)
  }
  as.character(metadata_df$value[[idx[[1L]]]])
}

#' @keywords internal
.mztab_parse_version <- function(version_string) {
  vv <- as.character(version_string)[[1L]]
  if (is.na(vv) || !nzchar(vv)) {
    return(NULL)
  }
  # Accept semantic mzTab-M versions such as 2.0.0-M, 2.1.0-M.
  mm <- regexec("^([0-9]+)\\.([0-9]+)\\.([0-9]+)-M$", vv, perl = TRUE)
  hit <- regmatches(vv, mm)[[1L]]
  if (length(hit) != 4L) {
    return(NULL)
  }
  c(
    major = suppressWarnings(as.integer(hit[[2L]])),
    minor = suppressWarnings(as.integer(hit[[3L]])),
    patch = suppressWarnings(as.integer(hit[[4L]]))
  )
}

#' @keywords internal
.mztab_validate_metadata_semantics <- function(metadata_df, strict) {
  metadata_df <- .mztab_table_or_empty(metadata_df)
  version <- .mztab_get_metadata_value(metadata_df, "mzTab-version")
  parsed <- .mztab_parse_version(version)
  if (is.null(parsed) || anyNA(parsed)) {
    tima_abort(
      problem = paste0("Invalid mzTab-version format: ", version),
      fix = "Use semantic mzTab-M version format, e.g. 2.1.0-M",
      class = c("tima_validation_error", "tima_error")
    )
  }
  if (parsed[["major"]] < 2L) {
    tima_abort(
      problem = paste0("Unsupported mzTab-version: ", version),
      fix = "Use mzTab-M 2.x",
      class = c("tima_validation_error", "tima_error")
    )
  }

  catalog <- .mztab_schema_catalog()
  recommended_meta <- catalog$metadata$recommended
  missing_meta <- recommended_meta[!(recommended_meta %in% metadata_df$key)]
  if (length(missing_meta) > 0L) {
    msg <- paste(
      "Missing recommended metadata fields:",
      paste(missing_meta, collapse = ", ")
    )
    log_warn("%s", msg)
  }
}

#' @keywords internal
.mztab_validate_cv_registry <- function(metadata_df, strict) {
  metadata_df <- .mztab_table_or_empty(metadata_df)
  if (!"key" %in% names(metadata_df)) {
    return(invisible(TRUE))
  }

  cv_labels <- grep("^cv\\[[0-9]+\\]-label$", metadata_df$key, value = TRUE)
  if (length(cv_labels) == 0L) {
    return(invisible(TRUE))
  }

  missing <- character(0)
  for (k in cv_labels) {
    idx <- sub("-label$", "", k)
    needed <- paste0(idx, c("-full_name", "-uri"))
    miss <- needed[!(needed %in% metadata_df$key)]
    if (length(miss) > 0L) {
      missing <- c(missing, miss)
    }
  }
  missing <- unique(missing)
  if (length(missing) == 0L) {
    return(invisible(TRUE))
  }

  msg <- paste(
    "Incomplete CV registry entries:",
    paste(missing, collapse = ", ")
  )
  if (isTRUE(strict)) {
    tima_abort(
      problem = msg,
      fix = "For each cv[n]-label, provide cv[n]-full_name and cv[n]-uri",
      class = c("tima_validation_error", "tima_error")
    )
  }
  log_warn("%s", msg)
  invisible(TRUE)
}

#' @keywords internal
.mztab_missing_ref_ids <- function(ref_values, known_ids) {
  if (length(ref_values) == 0L || length(known_ids) == 0L) {
    return(character(0))
  }
  split_refs <- unlist(
    strsplit(as.character(ref_values), "[|,]", perl = TRUE),
    use.names = FALSE
  )
  split_refs <- trimws(split_refs)
  split_refs <- split_refs[
    nzchar(split_refs) &
      split_refs != "null" &
      split_refs != "NA" &
      !is.na(split_refs)
  ]

  known <- trimws(as.character(known_ids))
  known <- known[nzchar(known) & known != "null" & !is.na(known)]
  setdiff(unique(split_refs), unique(known))
}

#' @keywords internal
.mztab_split_pipe_values <- function(x) {
  one <- as.character(x)[[1L]]
  if (is.na(one) || !nzchar(one)) {
    return("null")
  }
  parts <- strsplit(one, "|", fixed = TRUE)[[1L]]
  parts <- trimws(parts)
  parts[!nzchar(parts)] <- "null"
  parts
}

#' @keywords internal
.mztab_validate_reference_integrity <- function(mztab_tables, strict) {
  sml <- .mztab_table_or_empty(mztab_tables$sml)
  smf <- .mztab_table_or_empty(mztab_tables$smf)
  sme <- .mztab_table_or_empty(mztab_tables$sme)

  if (nrow(sml) > 0L && nrow(smf) > 0L && "SMF_ID_REFS" %in% names(sml)) {
    bad <- .mztab_missing_ref_ids(sml$SMF_ID_REFS, smf$SMF_ID)
    if (length(bad) > 0L) {
      msg <- paste(
        "SML references unknown SMF_ID(s):",
        paste(bad, collapse = ", ")
      )
      if (isTRUE(strict)) {
        tima_abort(
          problem = msg,
          fix = "Ensure SML SMF_ID_REFS only contains SMF_ID values present in SMF",
          class = c("tima_validation_error", "tima_error")
        )
      }
      log_warn("%s", msg)
    }
  }

  if (nrow(sml) > 0L && nrow(sme) > 0L && "SME_ID_REFS" %in% names(sml)) {
    bad <- .mztab_missing_ref_ids(sml$SME_ID_REFS, sme$SME_ID)
    if (length(bad) > 0L) {
      msg <- paste(
        "SML references unknown SME_ID(s):",
        paste(bad, collapse = ", ")
      )
      if (isTRUE(strict)) {
        tima_abort(
          problem = msg,
          fix = "Ensure SML SME_ID_REFS only contains SME_ID values present in SME",
          class = c("tima_validation_error", "tima_error")
        )
      }
      log_warn("%s", msg)
    }
  }

  if (nrow(smf) > 0L && nrow(sme) > 0L && "SME_ID_REFS" %in% names(smf)) {
    bad <- .mztab_missing_ref_ids(smf$SME_ID_REFS, sme$SME_ID)
    if (length(bad) > 0L) {
      msg <- paste(
        "SMF references unknown SME_ID(s):",
        paste(bad, collapse = ", ")
      )
      if (isTRUE(strict)) {
        tima_abort(
          problem = msg,
          fix = "Ensure SMF SME_ID_REFS only contains SME_ID values present in SME",
          class = c("tima_validation_error", "tima_error")
        )
      }
      log_warn("%s", msg)
    }
  }

  if (
    nrow(sml) > 0L &&
      all(
        c(
          "database_identifier",
          "chemical_formula",
          "smiles",
          "inchi",
          "chemical_name",
          "uri",
          "adduct_ions"
        ) %in%
          names(sml)
      )
  ) {
    aligned_cols <- c(
      "database_identifier",
      "chemical_formula",
      "smiles",
      "inchi",
      "chemical_name",
      "uri",
      "adduct_ions"
    )

    bad_rows <- which(vapply(
      X = seq_len(nrow(sml)),
      FUN = function(i) {
        n_parts <- vapply(
          X = aligned_cols,
          FUN = function(col) {
            length(.mztab_split_pipe_values(sml[[col]][[i]]))
          },
          FUN.VALUE = integer(1L)
        )
        max(n_parts) > 1L && length(unique(n_parts)) > 1L
      },
      FUN.VALUE = logical(1L)
    ))

    if (length(bad_rows) > 0L) {
      msg <- paste0(
        "Invalid SML multi-candidate ambiguity alignment: ",
        "pipe-separated identifier fields must be cardinality-aligned"
      )
      if (isTRUE(strict)) {
        tima_abort(
          problem = msg,
          fix = paste0(
            "Align pipe-separated counts across ",
            paste(aligned_cols, collapse = ", ")
          ),
          class = c("tima_validation_error", "tima_error")
        )
      }
      log_warn("%s", msg)
    }
  }

  if (
    nrow(smf) > 0L &&
      "SME_ID_REFS" %in% names(smf) &&
      "SME_ID_REF_ambiguity_code" %in% names(smf)
  ) {
    refs <- as.character(smf$SME_ID_REFS)
    amb <- as.character(smf$SME_ID_REF_ambiguity_code)
    n_refs <- vapply(
      refs,
      function(one) {
        if (is.na(one) || !nzchar(one) || one == "null") {
          return(0L)
        }
        rr <- strsplit(one, "[|,]", perl = TRUE)[[1L]]
        rr <- trimws(rr)
        rr <- rr[nzchar(rr) & rr != "null"]
        length(unique(rr))
      },
      FUN.VALUE = integer(1L)
    )

    bad_multi <- which(n_refs > 1L & !(amb %in% c("1", "2", "3")))
    bad_single <- which(
      n_refs <= 1L & !(is.na(amb) | amb == "null" | amb == "NA" | !nzchar(amb))
    )

    if (length(bad_multi) > 0L || length(bad_single) > 0L) {
      msg <- paste0(
        "Invalid SME_ID_REF_ambiguity_code semantics in SMF: ",
        "multi-ref rows must use 1/2/3; single/no-ref rows must be null"
      )
      if (isTRUE(strict)) {
        tima_abort(
          problem = msg,
          fix = "Set SME_ID_REF_ambiguity_code according to SME_ID_REFS cardinality",
          class = c("tima_validation_error", "tima_error")
        )
      }
      log_warn("%s", msg)
    }
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
  metadata <- .mztab_table_or_empty(mztab_tables$metadata)
  sml <- .mztab_table_or_empty(mztab_tables$sml)
  smf <- .mztab_table_or_empty(mztab_tables$smf)
  sme <- .mztab_table_or_empty(mztab_tables$sme)

  if (nrow(metadata) == 0L) {
    tima_abort(
      problem = "Missing metadata section (MTD)",
      fix = "Ensure the mzTab file contains MTD lines",
      class = c("tima_validation_error", "tima_error")
    )
  }

  has_version <- any(metadata$key == "mzTab-version", na.rm = TRUE)
  if (!has_version) {
    tima_abort(
      problem = "Missing required mzTab metadata field: mzTab-version",
      class = c("tima_validation_error", "tima_error")
    )
  }

  has_sml <- nrow(sml) > 0L
  has_smf <- nrow(smf) > 0L

  if (!has_sml && !has_smf) {
    tima_abort(
      problem = "mzTab-M file must contain SML and/or SMF rows",
      fix = "Provide at least one of the small molecule summary or feature sections",
      class = c("tima_validation_error", "tima_error")
    )
  }

  .mztab_validate_metadata_semantics(
    metadata_df = metadata,
    strict = strict
  )
  .mztab_validate_cv_registry(
    metadata_df = metadata,
    strict = strict
  )

  .validate_required_cols(sml, required$SML, "SML")
  .validate_required_cols(smf, required$SMF, "SMF")

  if (strict) {
    .validate_required_cols(sme, required$SME, "SME")
  } else if (nrow(sme) > 0L) {
    required_sme_norm <- .mztab_normalize_column_name(required$SME)
    names_sme_norm <- .mztab_normalize_column_name(names(sme))
    missing_sme <- required$SME[!(required_sme_norm %in% names_sme_norm)]
    if (length(missing_sme) > 0L) {
      log_warn(
        "SME section is present but missing some required columns: %s",
        paste(missing_sme, collapse = ", ")
      )
    }
  }

  .mztab_validate_reference_integrity(
    mztab_tables = list(sml = sml, smf = smf, sme = sme),
    strict = strict
  )

  invisible(TRUE)
}
