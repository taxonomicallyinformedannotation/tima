#' mzTab-M parser helpers
#'
#' @description Internal parser for mzTab-M plain-text files.
#'
#' @include mztab_schema_utils.R
#' @include safe_fread.R
#' @keywords internal
#' @name mztab_parser
NULL

#' @keywords internal
.normalize_mztab_value <- function(x) {
  if (!is.character(x)) {
    return(x)
  }
  x <- trimws(x)
  # In mzTab, only `null` is the canonical missing token; keep literal "NA" values.
  x[x %in% c("", "null", "NULL")] <- NA_character_
  x
}

#' Split a comma-separated mzTab Param payload while respecting escapes.
#'
#' mzTab Param values use bracket notation: `[cvLabel, accession, name, value]`.
#' Commas can appear in fields when escaped (`\,`), so we cannot use plain
#' `strsplit(..., ",")` for robust parsing.
#'
#' @keywords internal
.mztab_split_param_fields <- function(x) {
  if (is.na(x) || !nzchar(x)) {
    return(character(0))
  }

  chars <- strsplit(x, "", fixed = TRUE)[[1L]]
  fields <- character(0)
  current <- character(0)  # Use vector accumulation instead of string
  escaped <- FALSE

  for (ch in chars) {
    if (escaped) {
      current <- c(current, ch)
      escaped <- FALSE
      next
    }
    if (identical(ch, "\\")) {
      escaped <- TRUE
      next
    }
    if (identical(ch, ",")) {
      fields <- c(fields, trimws(paste(current, collapse = "")))
      current <- character(0)
      next
    }
    current <- c(current, ch)
  }
  fields <- c(fields, trimws(paste(current, collapse = "")))

  fields
}

#' @keywords internal
.normalize_mztab_prefix_sep <- function(lines) {
  gsub(
    pattern = "^(COM|MTD|SMH|SML|SFH|SMF|SEH|SME)[[:space:]]+",
    replacement = paste0("\\1", "\t"),
    x = lines,
    perl = TRUE
  )
}

#' @keywords internal
.parse_mztab_metadata <- function(lines) {
  mtd_lines <- grep("^MTD([[:space:]]|$)", lines, value = TRUE, perl = TRUE)
  if (length(mtd_lines) == 0L) {
    return(tidytable::tidytable(key = character(0), value = character(0)))
  }

  rows <- lapply(mtd_lines, function(line) {
    parts <- strsplit(line, "\\t", fixed = FALSE)[[1L]]
    if (length(parts) < 3L) {
      return(NULL)
    }
    tidytable::tidytable(
      key = parts[[2L]],
      value = paste(parts[3:length(parts)], collapse = "\t")
    )
  })

  out <- tidytable::bind_rows(Filter(Negate(is.null), rows))
  if (nrow(out) > 0L) {
    out$value <- .normalize_mztab_value(out$value)
  }
  out
}

#' @keywords internal
.parse_mztab_section_table <- function(lines, header_prefix, row_prefix) {
  header_lines <- grep(
    paste0("^", header_prefix, "([[:space:]]|$)"),
    lines,
    value = TRUE,
    perl = TRUE
  )
  row_lines <- grep(
    paste0("^", row_prefix, "([[:space:]]|$)"),
    lines,
    value = TRUE,
    perl = TRUE
  )

  if (length(header_lines) == 0L || length(row_lines) == 0L) {
    return(tidytable::tidytable())
  }

  header_line <- gsub(
    "best_id_confidence_measurebest_id_evidence_value",
    "best_id_confidence_measure\\tbest_id_evidence_value",
    header_lines[[1L]],
    fixed = TRUE
  )
  col_names <- strsplit(header_line, "\\t", fixed = FALSE)[[1L]]
  col_names <- col_names[-1L]

  row_splits <- lapply(row_lines, function(line) {
    vals <- strsplit(line, "\\t", fixed = FALSE)[[1L]]
    vals[-1L]
  })
  max_len <- max(lengths(row_splits), 0L)
  if (max_len > length(col_names)) {
    col_names <- c(
      col_names,
      paste0("opt_global_extra_col_", seq_len(max_len - length(col_names)))
    )
  }
  col_names <- trimws(col_names)
  empty_idx <- which(is.na(col_names) | !nzchar(col_names))
  if (length(empty_idx) > 0L) {
    col_names[empty_idx] <- paste0("opt_global_unnamed_col_", empty_idx)
  }
  col_names <- make.unique(col_names, sep = "_dup")

  rows <- lapply(row_splits, function(vals) {
    # Pad short rows with NA to keep rectangular structure.
    if (length(vals) < length(col_names)) {
      vals <- c(vals, rep(NA_character_, length(col_names) - length(vals)))
    }
    vals <- vals[seq_along(col_names)]

    df <- as.list(vals)
    names(df) <- col_names
    as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)
  })

  out <- tidytable::as_tidytable(tidytable::bind_rows(rows))
  out[] <- lapply(out, .normalize_mztab_value)
  out
}

#' @keywords internal
.mztab_json_pick <- function(x, keys) {
  for (k in keys) {
    if (!is.null(x[[k]])) {
      return(x[[k]])
    }
  }
  NULL
}

#' @keywords internal
.mztab_json_param_to_string <- function(x) {
  if (!is.list(x) || length(x) == 0L) {
    return(NA_character_)
  }
  cv_label <- .mztab_json_pick(x, c("cv_label", "cvLabel"))
  accession <- .mztab_json_pick(x, c("cv_accession", "cvAccession"))
  name <- .mztab_json_pick(x, c("name"))
  value <- .mztab_json_pick(x, c("value"))
  if (
    is.null(cv_label) && is.null(accession) && is.null(name) && is.null(value)
  ) {
    return(NA_character_)
  }
  parts <- vapply(
    X = list(cv_label, accession, name, value),
    FUN = function(v) {
      if (is.null(v) || length(v) == 0L || is.na(v)) {
        ""
      } else {
        as.character(v[[1L]])
      }
    },
    FUN.VALUE = character(1L)
  )
  paste0("[", paste(parts, collapse = ", "), "]")
}

#' @keywords internal
.mztab_json_scalar <- function(x) {
  if (is.null(x) || length(x) == 0L) {
    return(NA_character_)
  }

  if (is.list(x) && !is.null(names(x))) {
    maybe_param <- .mztab_json_param_to_string(x)
    if (!is.na(maybe_param)) {
      return(maybe_param)
    }
  }

  if (is.atomic(x)) {
    if (length(x) == 1L) {
      if (is.na(x)) {
        return(NA_character_)
      }
      return(as.character(x))
    }
    return(paste(as.character(x), collapse = "|"))
  }

  if (is.list(x)) {
    vals <- vapply(x, .mztab_json_scalar, FUN.VALUE = character(1L))
    vals <- vals[!is.na(vals) & nzchar(vals)]
    if (length(vals) == 0L) {
      return(NA_character_)
    }
    return(paste(unique(vals), collapse = "|"))
  }

  as.character(x)
}

#' @keywords internal
.mztab_json_rows_to_table <- function(rows) {
  if (is.null(rows) || length(rows) == 0L) {
    return(tidytable::tidytable())
  }

  mapped <- lapply(rows, function(one) {
    if (!is.list(one) || length(one) == 0L) {
      return(NULL)
    }
    vals <- lapply(one, .mztab_json_scalar)
    out <- as.data.frame(vals, stringsAsFactors = FALSE, check.names = FALSE)
    drop_cols <- intersect(
      c("prefix", "header_prefix", "comment"),
      colnames(out)
    )
    if (length(drop_cols) > 0L) {
      out <- out[, setdiff(colnames(out), drop_cols), drop = FALSE]
    }
    out
  })
  mapped <- Filter(Negate(is.null), mapped)
  if (length(mapped) == 0L) {
    return(tidytable::tidytable())
  }

  out <- tidytable::as_tidytable(tidytable::bind_rows(mapped))

  # Normalize common identifier fields to plain-text mzTab header style.
  name_map <- c(
    sml_id = "SML_ID",
    smf_id = "SMF_ID",
    sme_id = "SME_ID",
    smf_id_refs = "SMF_ID_REFS",
    sme_id_refs = "SME_ID_REFS",
    sml_id_refs = "SML_ID_REFS"
  )
  rename_cols <- intersect(names(name_map), colnames(out))
  if (length(rename_cols) > 0L) {
    out <- out |>
      tidytable::rename(!!!stats::setNames(rename_cols, name_map[rename_cols]))
  }

  out[] <- lapply(out, .normalize_mztab_value)
  out
}

#' @keywords internal
.mztab_json_metadata_to_table <- function(meta) {
  if (is.null(meta) || !is.list(meta) || length(meta) == 0L) {
    return(tidytable::tidytable(key = character(0), value = character(0)))
  }

  keys <- names(meta)
  vals <- vapply(meta, .mztab_json_scalar, FUN.VALUE = character(1L))
  keep <- !is.na(vals) & nzchar(vals)
  keys <- keys[keep]
  vals <- vals[keep]

  if (length(keys) == 0L) {
    return(tidytable::tidytable(key = character(0), value = character(0)))
  }

  # Harmonize common JSON snake_case aliases to mzTab plain-text key names.
  keys <- gsub("^mz_tab_version$", "mzTab-version", keys)
  keys <- gsub("^mz_tab_mode$", "mzTab-mode", keys)
  keys <- gsub("^mz_tab_type$", "mzTab-type", keys)

  tidytable::tidytable(key = keys, value = vals)
}

#' @keywords internal
.read_mztab_json_tables <- function(input) {
  json <- jsonlite::fromJSON(input, simplifyVector = FALSE)

  meta <- .mztab_json_pick(json, c("metadata", "Metadata"))
  sml <- .mztab_json_pick(
    json,
    c("small_molecule_summary", "smallMoleculeSummary")
  )
  smf <- .mztab_json_pick(
    json,
    c("small_molecule_feature", "smallMoleculeFeature")
  )
  sme <- .mztab_json_pick(
    json,
    c("small_molecule_evidence", "smallMoleculeEvidence")
  )

  list(
    metadata = .mztab_json_metadata_to_table(meta),
    sml = .mztab_json_rows_to_table(sml),
    smf = .mztab_json_rows_to_table(smf),
    sme = .mztab_json_rows_to_table(sme)
  )
}

#' @keywords internal
read_mztab_tables <- function(input) {
  validate_character(input, param_name = "input")
  validate_file_exists(input, file_type = "mzTab-M file", param_name = "input")

  lines <- readLines(input, warn = FALSE, encoding = "UTF-8")
  if (length(lines) > 0L) {
    lines[[1L]] <- sub("^\ufeff", "", lines[[1L]])
  }

  first_nonblank <- ""
  if (length(lines) > 0L) {
    nonblank <- which(nzchar(trimws(lines)))
    if (length(nonblank) > 0L) {
      first_nonblank <- trimws(lines[[nonblank[[1L]]]])
    }
  }
  is_json <- grepl("\\.json$", tolower(input)) ||
    startsWith(first_nonblank, "{")
  if (is_json) {
    return(.read_mztab_json_tables(input))
  }

  lines <- .normalize_mztab_prefix_sep(lines)

  list(
    metadata = .parse_mztab_metadata(lines),
    sml = .parse_mztab_section_table(
      lines,
      MZTAB_SECTION_PREFIX$small_molecule_summary_header,
      MZTAB_SECTION_PREFIX$small_molecule_summary
    ),
    smf = .parse_mztab_section_table(
      lines,
      MZTAB_SECTION_PREFIX$small_molecule_feature_header,
      MZTAB_SECTION_PREFIX$small_molecule_feature
    ),
    sme = .parse_mztab_section_table(
      lines,
      MZTAB_SECTION_PREFIX$small_molecule_evidence_header,
      MZTAB_SECTION_PREFIX$small_molecule_evidence
    )
  )
}
