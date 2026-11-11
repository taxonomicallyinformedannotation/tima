#' mzTab-M parser helpers
#'
#' @description Internal parser for mzTab-M plain-text files.
#'
#' @include mztab_schema_utils.R
#' @include safe_fread.R
#' @include validations_utils.R
#' @include validations_params.R
#' @keywords internal
#' @name mztab_parser
NULL

#' @keywords internal
.normalize_mztab_value <- function(x) {
  if (!is.character(x)) {
    return(x)
  }
  x <- trimws(x)
  x[x %in% c("", "null", "NULL")] <- NA_character_
  x
}

#' @keywords internal
.mztab_split_tsv <- function(x) {
  if (is.null(x) || length(x) == 0L || is.na(x)) {
    return(character(0))
  }

  parts <- strsplit(as.character(x), "\t", fixed = TRUE)[[1L]]
  if (length(parts) == 1L && !nzchar(parts[[1L]])) {
    return(character(0))
  }

  parts
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

  # Split on commas that are NOT preceded by a backslash (escaped commas),
  # then unescape: a backslash makes the following character literal.
  # (A strsplit + gsub pass avoids the O(n^2) per-character c() growth of the
  # previous char-loop implementation.)
  parts <- strsplit(x, "(?<!\\\\),", perl = TRUE)[[1L]]
  vapply(
    parts,
    function(p) {
      p <- gsub("\\\\(.)", "\\1", p, perl = TRUE)
      trimws(p)
    },
    character(1L),
    USE.NAMES = FALSE
  )
}

#' @keywords internal
.normalize_mztab_prefix_sep <- function(lines) {
  if (length(lines) == 0L) {
    return(character(0))
  }

  out <- as.character(lines)
  has_bom <- !is.na(out) & startsWith(out, "\ufeff")
  if (any(has_bom)) {
    out[has_bom] <- sub("^\ufeff", "", out[has_bom])
  }

  already_ok <- !is.na(out) &
    grepl(
      "^(COM|MTD|SMH|SML|SFH|SMF|SEH|SME)\t[^[:space:]]",
      out,
      perl = TRUE
    )
  need_check <- which(!already_ok & !is.na(out) & nzchar(out))
  if (length(need_check) == 0L) {
    return(out)
  }

  sub_lines <- out[need_check]
  nonblank <- nzchar(trimws(sub_lines))
  if (!any(nonblank)) {
    return(out)
  }

  idx <- need_check[nonblank]
  cand <- out[idx]
  m <- grepl(
    "^(COM|MTD|SMH|SML|SFH|SMF|SEH|SME)([[:space:]]|$)",
    cand,
    perl = TRUE,
    ignore.case = TRUE
  )
  if (!any(m)) {
    return(out)
  }

  idx_m <- idx[m]
  cand_m <- cand[m]
  prefix <- sub(
    "^(COM|MTD|SMH|SML|SFH|SMF|SEH|SME)([[:space:]]+).*$",
    "\\1",
    cand_m,
    perl = TRUE,
    ignore.case = TRUE
  )
  rest <- sub(
    "^(COM|MTD|SMH|SML|SFH|SMF|SEH|SME)[[:space:]]+",
    "",
    cand_m,
    perl = TRUE,
    ignore.case = TRUE
  )
  has_rest <- !is.na(rest) & nzchar(rest)
  res <- prefix
  res[has_rest] <- paste0(prefix[has_rest], "\t", rest[has_rest])
  out[idx_m] <- res
  out
}

#' @keywords internal
.parse_mztab_metadata <- function(lines) {
  mtd_lines <- grep("^MTD([[:space:]]|$)", lines, value = TRUE, perl = TRUE)
  if (length(mtd_lines) == 0L) {
    return(tidytable::tidytable(key = character(0), value = character(0)))
  }

  splits <- strsplit(as.character(mtd_lines), "\t", fixed = TRUE)
  lens <- lengths(splits)
  keep <- lens >= 2L
  if (!any(keep)) {
    return(tidytable::tidytable(key = character(0), value = character(0)))
  }

  splits <- splits[keep]
  lens <- lens[keep]
  keys <- trimws(vapply(splits, `[[`, character(1L), 2L))
  valid_k <- nzchar(keys)
  if (!any(valid_k)) {
    return(tidytable::tidytable(key = character(0), value = character(0)))
  }

  splits <- splits[valid_k]
  lens <- lens[valid_k]
  keys <- keys[valid_k]
  vals <- rep(NA_character_, length(splits))
  exact_3 <- lens == 3L
  if (any(exact_3)) {
    vals[exact_3] <- vapply(splits[exact_3], `[[`, character(1L), 3L)
  }
  more_3 <- which(lens > 3L)
  if (length(more_3) > 0L) {
    vals[more_3] <- vapply(
      splits[more_3],
      function(p) paste(p[3:length(p)], collapse = "\t"),
      character(1L)
    )
  }

  vals <- .normalize_mztab_value(vals)
  tidytable::tidytable(key = keys, value = vals)
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
    "best_id_confidence_measure\tbest_id_evidence_value",
    header_lines[[1L]],
    fixed = TRUE
  )
  col_names <- .mztab_split_tsv(header_line)
  if (length(col_names) < 2L) {
    return(tidytable::tidytable())
  }
  col_names <- col_names[-1L]

  raw_splits <- strsplit(as.character(row_lines), "\t", fixed = TRUE)
  raw_lens <- lengths(raw_splits)
  max_len <- max(raw_lens - 1L, 0L)

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

  n_cols <- length(col_names)
  n_rows <- length(raw_splits)

  target_raw_len <- n_cols + 1L
  if (all(raw_lens == target_raw_len)) {
    flat <- unlist(raw_splits, use.names = FALSE)
    mat <- matrix(flat, nrow = target_raw_len, ncol = n_rows)
    out_list <- vector("list", n_cols)
    for (j in seq_len(n_cols)) {
      col_vec <- mat[j + 1L, ]
      u <- unique(col_vec)
      u_norm <- .normalize_mztab_value(u)
      out_list[[j]] <- u_norm[fastmatch::fmatch(col_vec, u)]
    }
    names(out_list) <- col_names
    return(tidytable::as_tidytable(out_list))
  }

  row_matrix <- matrix(NA_character_, nrow = n_rows, ncol = n_cols)
  for (row_idx in seq_len(n_rows)) {
    vals <- raw_splits[[row_idx]]
    if (length(vals) >= 2L) {
      take_len <- min(length(vals) - 1L, n_cols)
      row_matrix[row_idx, seq_len(take_len)] <- vals[seq.int(2L, take_len + 1L)]
    }
  }

  out <- as.data.frame(
    row_matrix,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  names(out) <- col_names
  out[] <- lapply(out, .normalize_mztab_value)
  rm(row_matrix, row_idx, vals, take_len, empty_idx, col_names, n_cols)
  tidytable::as_tidytable(out)
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

  vals <- vapply(x, .mztab_json_scalar, FUN.VALUE = character(1L))
  vals <- vals[!is.na(vals) & nzchar(vals)]
  if (length(vals) == 0L) {
    return(NA_character_)
  }
  paste(unique(vals), collapse = "|")
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
  rm(mapped)

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
  json <- tryCatch(
    jsonlite::fromJSON(input, simplifyVector = FALSE),
    error = function(e) {
      log_warn(
        "Failed to parse mzTab JSON file %s: %s",
        basename(input),
        conditionMessage(e)
      )
      NULL
    }
  )

  if (!is.list(json)) {
    return(list(
      metadata = tidytable::tidytable(key = character(0), value = character(0)),
      sml = tidytable::tidytable(),
      smf = tidytable::tidytable(),
      sme = tidytable::tidytable()
    ))
  }

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
read_mztab_tables <- function(input, include_raw_lines = FALSE) {
  validate_character(input, param_name = "input")
  validate_file_exists(input, file_type = "mzTab-M file", param_name = "input")

  lines <- tryCatch(
    readLines(input, warn = FALSE, encoding = "UTF-8"),
    error = function(e) {
      log_warn(
        "Failed to read mzTab file %s: %s",
        basename(input),
        conditionMessage(e)
      )
      character(0)
    }
  )
  lines <- .normalize_mztab_prefix_sep(lines)

  first_nonblank <- ""
  if (length(lines) > 0L) {
    for (line in lines) {
      if (!is.na(line) && nzchar(line)) {
        tr <- trimws(line)
        if (nzchar(tr)) {
          first_nonblank <- tr
          break
        }
      }
    }
  }
  is_json <- grepl("\\.json$", tolower(input)) ||
    startsWith(first_nonblank, "{")
  if (is_json) {
    return(.read_mztab_json_tables(input))
  }

  out <- list(
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
  if (isTRUE(include_raw_lines)) {
    attr(out, ".raw_lines") <- lines
  }
  out
}
