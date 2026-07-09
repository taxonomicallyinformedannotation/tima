.mztab_read_base <- function(path) {
  if (.mztab_is_json_path(path)) {
    tabs <- read_mztab_tables(path)
    return(list(
      metadata = tabs$metadata,
      sml = tabs$sml,
      smf = tabs$smf,
      sme = tabs$sme,
      passthrough = character(0)
    ))
  }

  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  if (length(lines) > 0L) {
    lines[[1L]] <- sub("^\ufeff", "", lines[[1L]])
  }

  lines_norm <- .normalize_mztab_prefix_sep(lines)
  managed <- grepl("^(MTD|SMH|SML|SFH|SMF|SEH|SME)(\\t|$)", lines_norm)

  # Preserve all non-managed non-blank lines in original order, regardless of
  # whether they occur before or after managed sections.
  passthrough_idx <- which(!managed & nzchar(lines))

  list(
    metadata = .parse_mztab_metadata(lines_norm),
    sml = .parse_mztab_section_table(lines_norm, "SMH", "SML"),
    smf = .parse_mztab_section_table(lines_norm, "SFH", "SMF"),
    sme = .parse_mztab_section_table(lines_norm, "SEH", "SME"),
    passthrough = lines[passthrough_idx]
  )
}

#' Merge TIMA-derived tables into a base mzTab
#' @keywords internal
.mztab_merge_tables <- function(base, sml_new, smf_new, sme_new) {
  smf_base <- tidytable::as_tidytable(base$smf)
  sme_base <- tidytable::as_tidytable(base$sme)
  sml_base <- tidytable::as_tidytable(base$sml)

  # Ensure canonical columns exist in base tables before merging.
  smf_base <- .mztab_ensure_cols(
    smf_base,
    c(
      "SMF_ID",
      "SML_ID_REFS",
      "SME_ID_REFS",
      "SME_ID_REF_ambiguity_code",
      "exp_mass_to_charge",
      "charge",
      "retention_time_in_seconds"
    )
  )
  sme_base <- .mztab_ensure_cols(
    sme_base,
    c(
      "SME_ID",
      "evidence_input_id",
      "database_identifier",
      "chemical_formula",
      "smiles",
      "inchi",
      "chemical_name",
      "uri",
      "derivatized_form",
      "adduct_ion",
      "exp_mass_to_charge",
      "charge",
      "theoretical_mass_to_charge",
      "spectra_ref",
      "identification_method",
      "ms_level",
      "rank",
      "id_confidence_measure[1]",
      "id_confidence_measure[2]",
      "id_confidence_measure[3]",
      "id_confidence_measure[4]"
    )
  )
  sml_base <- .mztab_ensure_cols(
    sml_base,
    c(
      "SML_ID",
      "SMF_ID_REFS",
      "SME_ID_REFS",
      "database_identifier",
      "chemical_formula",
      "smiles",
      "inchi",
      "chemical_name",
      "uri",
      "theoretical_neutral_mass",
      "adduct_ions",
      "reliability",
      "best_id_confidence_measure",
      "best_id_evidence_value"
    )
  )

  smf_new <- .mztab_ensure_cols(smf_new, colnames(smf_base))
  sme_new <- .mztab_ensure_cols(sme_new, colnames(sme_base))
  sml_new <- .mztab_ensure_cols(sml_new, colnames(sml_base))

  map_smf <- .mztab_merge_smf_rows(smf_base, smf_new)
  smf_all <- map_smf$smf
  smf_id_map <- map_smf$id_map

  map_sme <- .mztab_merge_sme_rows(sme_base, sme_new, smf_id_map)
  sme_all <- map_sme$sme
  sme_id_map <- map_sme$id_map

  map_sml <- .mztab_merge_sml_rows(sml_base, sml_new, smf_id_map, sme_id_map)
  sml_all <- map_sml$sml

  # Back-fill SMF -> SML references from final SML table.
  smf_all <- .mztab_refresh_smf_sml_refs(smf_all, sml_all)

  list(
    sml = sml_all,
    smf = smf_all,
    sme = sme_all
  )
}

#' Ensure columns exist and are character
#' @keywords internal
.mztab_ensure_cols <- function(tbl, cols) {
  tbl <- as.data.frame(tbl, stringsAsFactors = FALSE, check.names = FALSE)
  if (nrow(tbl) == 0L && ncol(tbl) == 0L) {
    tbl <- data.frame(stringsAsFactors = FALSE, check.names = FALSE)
  }
  for (col in cols) {
    if (!col %in% colnames(tbl)) {
      tbl[[col]] <- character(nrow(tbl))
    }
  }
  # Keep optional/unknown columns to avoid information loss in merge mode.
  ordered_cols <- c(cols, setdiff(colnames(tbl), cols))
  tbl <- tbl[, ordered_cols, drop = FALSE]
  tbl[] <- lapply(tbl, function(x) {
    x <- as.character(x)
    x[is.na(x) | !nzchar(x)] <- "null"
    x
  })
  tidytable::as_tidytable(tbl)
}

#' Build stable key for SMF deduplication
#' @keywords internal
.mztab_smf_key <- function(tbl) {
  tbl_df <- as.data.frame(tbl, stringsAsFactors = FALSE, check.names = FALSE)

  opt_cols <- grep("^opt_", colnames(tbl_df), value = TRUE)

  # Vectorized opt signature generation
  opt_sig <- if (length(opt_cols) > 0L) {
    # Combine all opt columns into a matrix for faster processing
    opt_matrix <- as.matrix(tbl_df[, opt_cols, drop = FALSE])
    apply(opt_matrix, 1L, function(x) {
      paste(ifelse(is.na(x) | !nzchar(x), "null", x), collapse = "|")
    })
  } else {
    rep("null", nrow(tbl_df))
  }

  paste(
    tbl_df$exp_mass_to_charge,
    tbl_df$retention_time_in_seconds,
    tbl_df$charge,
    opt_sig,
    sep = "||"
  )
}

#' Build stable key for SME deduplication
#' @keywords internal
.mztab_sme_key <- function(tbl) {
  paste(
    tbl$database_identifier,
    tbl$evidence_input_id,
    tbl$adduct_ion,
    tbl$rank,
    tbl$exp_mass_to_charge,
    tbl$charge,
    tbl$theoretical_mass_to_charge,
    tbl$identification_method,
    tbl$ms_level,
    tbl$chemical_formula,
    tbl$inchi,
    sep = "||"
  )
}

#' Build stable key for SML deduplication
#' @keywords internal
.mztab_sml_key <- function(tbl) {
  primary <- tbl$database_identifier
  fallback <- paste(
    tbl$chemical_name,
    tbl$chemical_formula,
    tbl$adduct_ions,
    sep = "||"
  )
  ifelse(primary == "null" | !nzchar(primary), fallback, primary)
}

#' Merge SMF rows and return id map from new IDs to final IDs
#' @keywords internal
.mztab_merge_smf_rows <- function(smf_base, smf_new) {
  smf_all <- smf_base
  base_keys <- if (nrow(smf_all) > 0L) .mztab_smf_key(smf_all) else character(0)
  id_map <- stats::setNames(character(0), character(0))
  next_id <- .mztab_next_numeric_id(smf_all$SMF_ID)

  # Process all new rows and collect new entries
  new_keys <- .mztab_smf_key(smf_new)
  match_idx <- match(new_keys, base_keys)

  # Map duplicates and collect new row indices
  new_row_indices <- which(is.na(match_idx))
  new_rows_count <- length(new_row_indices)

  if (new_rows_count > 0L) {
    # Process duplicates first
    dup_idx <- !is.na(match_idx)
    for (i in which(dup_idx)) {
      old_id <- smf_new$SMF_ID[[i]]
      id_map[[old_id]] <- smf_all$SMF_ID[[match_idx[[i]]]]
    }

    # Prepare new rows with batch ID assignment
    new_rows <- smf_new[new_row_indices, , drop = FALSE]
    new_rows$SMF_ID <- as.character(next_id + seq_len(new_rows_count) - 1L)
    new_rows$SML_ID_REFS <- "null"

    # Map new row IDs and append
    for (j in seq_len(new_rows_count)) {
      old_id <- smf_new$SMF_ID[[new_row_indices[j]]]
      id_map[[old_id]] <- new_rows$SMF_ID[[j]]
    }

    smf_all <- tidytable::bind_rows(smf_all, new_rows)
  } else {
    # Process duplicates only
    for (i in which(!is.na(match_idx))) {
      old_id <- smf_new$SMF_ID[[i]]
      id_map[[old_id]] <- smf_all$SMF_ID[[match_idx[[i]]]]
    }
  }

  list(smf = smf_all, id_map = id_map)
}

#' Merge SME rows and return id map from new IDs to final IDs
#' @keywords internal
.mztab_merge_sme_rows <- function(sme_base, sme_new, smf_id_map) {
  sme_all <- sme_base
  sme_new <- sme_new |>
    tidytable::mutate(
      evidence_input_id = .mztab_remap_ref_ids(evidence_input_id, smf_id_map)
    )

  base_keys <- if (nrow(sme_all) > 0L) .mztab_sme_key(sme_all) else character(0)
  id_map <- stats::setNames(character(0), character(0))
  next_id <- .mztab_next_numeric_id(sme_all$SME_ID)

  # Process all new rows and collect new entries
  new_keys <- .mztab_sme_key(sme_new)
  match_idx <- match(new_keys, base_keys)

  # Map duplicates and collect new row indices
  new_row_indices <- which(is.na(match_idx))
  new_rows_count <- length(new_row_indices)

  if (new_rows_count > 0L) {
    # Process duplicates first
    dup_idx <- !is.na(match_idx)
    for (i in which(dup_idx)) {
      old_id <- sme_new$SME_ID[[i]]
      id_map[[old_id]] <- sme_all$SME_ID[[match_idx[[i]]]]
    }

    # Prepare new rows with batch ID assignment
    new_rows <- sme_new[new_row_indices, , drop = FALSE]
    new_rows$SME_ID <- as.character(next_id + seq_len(new_rows_count) - 1L)

    # Map new row IDs and append
    for (j in seq_len(new_rows_count)) {
      old_id <- sme_new$SME_ID[[new_row_indices[j]]]
      id_map[[old_id]] <- new_rows$SME_ID[[j]]
    }

    sme_all <- tidytable::bind_rows(sme_all, new_rows)
  } else {
    # Process duplicates only
    for (i in which(!is.na(match_idx))) {
      old_id <- sme_new$SME_ID[[i]]
      id_map[[old_id]] <- sme_all$SME_ID[[match_idx[[i]]]]
    }
  }

  list(sme = sme_all, id_map = id_map)
}

#' Merge SML rows and enrich references for duplicates
#' @keywords internal
.mztab_merge_sml_rows <- function(sml_base, sml_new, smf_id_map, sme_id_map) {
  sml_all <- sml_base
  sml_new <- sml_new |>
    tidytable::mutate(
      SMF_ID_REFS = .mztab_remap_ref_ids(SMF_ID_REFS, smf_id_map),
      SME_ID_REFS = .mztab_remap_ref_ids(SME_ID_REFS, sme_id_map)
    )

  base_keys <- if (nrow(sml_all) > 0L) .mztab_sml_key(sml_all) else character(0)
  next_id <- .mztab_next_numeric_id(sml_all$SML_ID)

  # Pre-compute all keys for new rows
  new_keys <- .mztab_sml_key(sml_new)
  match_idx <- match(new_keys, base_keys)

  # Separate new entries from duplicates
  new_row_indices <- which(is.na(match_idx))
  new_rows_count <- length(new_row_indices)

  if (new_rows_count > 0L) {
    # Prepare new rows with batch ID assignment
    new_rows <- sml_new[new_row_indices, , drop = FALSE]
    new_rows$SML_ID <- as.character(next_id + seq_len(new_rows_count) - 1L)
    sml_all <- tidytable::bind_rows(sml_all, new_rows)
  }

  # Process duplicates: merge references
  dup_idx <- which(!is.na(match_idx))
  for (i in dup_idx) {
    base_idx <- match_idx[[i]]
    sml_all$SMF_ID_REFS[[base_idx]] <- .mztab_union_ref_ids(
      sml_all$SMF_ID_REFS[[base_idx]],
      sml_new$SMF_ID_REFS[[i]]
    )
    sml_all$SME_ID_REFS[[base_idx]] <- .mztab_union_ref_ids(
      sml_all$SME_ID_REFS[[base_idx]],
      sml_new$SME_ID_REFS[[i]]
    )
    sml_all$adduct_ions[[base_idx]] <- .mztab_union_ref_ids(
      sml_all$adduct_ions[[base_idx]],
      sml_new$adduct_ions[[i]]
    )
  }

  list(sml = sml_all)
}

#' Refresh SMF SML_ID_REFS from SML definitions
#' @keywords internal
.mztab_refresh_smf_sml_refs <- function(smf, sml) {
  if (nrow(smf) == 0L || nrow(sml) == 0L) {
    return(smf)
  }

  # Initialize all references as "null"
  refs <- stats::setNames(rep("null", nrow(smf)), smf$SMF_ID)

  if (nrow(sml) > 0L) {
    smf_id_lists <- lapply(seq_len(nrow(sml)), function(i) {
      .mztab_split_ref_ids(sml$SMF_ID_REFS[[i]])
    })
    row_counts <- lengths(smf_id_lists)
    if (any(row_counts > 0L)) {
      row_ids <- rep(seq_len(nrow(sml)), row_counts)
      smf_ids <- unlist(smf_id_lists, use.names = FALSE)
      valid_mask <- smf_ids %in% names(refs)
      if (any(valid_mask)) {
        valid_smf_ids <- smf_ids[valid_mask]
        valid_row_ids <- row_ids[valid_mask]
        grouped_sml_ids <- split(
          sml$SML_ID[valid_row_ids],
          factor(valid_smf_ids, levels = unique(valid_smf_ids))
        )
        grouped_refs <- vapply(
          grouped_sml_ids,
          function(ids) {
            paste(unique(ids), collapse = "|")
          },
          character(1L)
        )
        refs[names(grouped_refs)] <- vapply(
          names(grouped_refs),
          function(id) .mztab_union_ref_ids(refs[[id]], grouped_refs[[id]]),
          character(1L)
        )
      }
    }
  }

  smf$SML_ID_REFS <- unname(refs[smf$SMF_ID])
  smf
}

#' Refresh SMF SME_ID_REFS and ambiguity code from SME definitions
#' @keywords internal
.mztab_refresh_smf_sme_refs <- function(smf, sme) {
  if (nrow(smf) == 0L) {
    return(smf)
  }
  if (nrow(sme) == 0L) {
    smf$SME_ID_REFS <- rep("null", nrow(smf))
    smf$SME_ID_REF_ambiguity_code <- rep("null", nrow(smf))
    return(smf)
  }

  if (!"SME_ID_REFS" %in% colnames(smf)) {
    smf$SME_ID_REFS <- "null"
  }
  if (!"SME_ID_REF_ambiguity_code" %in% colnames(smf)) {
    smf$SME_ID_REF_ambiguity_code <- "null"
  }

  smf_df <- as.data.frame(smf, stringsAsFactors = FALSE, check.names = FALSE)
  sme_df <- as.data.frame(sme, stringsAsFactors = FALSE, check.names = FALSE)

  smf_feature_ids <- as.character(smf_df$feature_id)
  smf_feature_ids[
    is.na(smf_feature_ids) |
      !nzchar(smf_feature_ids) |
      smf_feature_ids == "null"
  ] <- NA_character_

  sme_feature_ids <- as.character(sme_df$feature_id)
  sme_feature_ids[
    is.na(sme_feature_ids) |
      !nzchar(sme_feature_ids) |
      sme_feature_ids == "null"
  ] <- NA_character_
  valid_sme_ids <- unique(sme_feature_ids[!is.na(sme_feature_ids)])

  feature_lookup <- if (length(valid_sme_ids) == 0L) {
    list()
  } else {
    valid_sme_idx <- which(!is.na(sme_feature_ids))
    groups <- split(
      valid_sme_idx,
      factor(sme_feature_ids[valid_sme_idx], levels = valid_sme_ids)
    )
    lapply(groups, function(idx) {
      refs <- unique(as.character(sme_df$SME_ID[idx]))
      refs <- refs[!is.na(refs) & nzchar(refs) & refs != "null"]

      if (length(refs) == 0L) {
        return(NULL)
      }

      db_vals <- unique(as.character(sme_df$database_identifier[idx]))
      db_vals <- db_vals[!is.na(db_vals) & nzchar(db_vals) & db_vals != "null"]
      method_vals <- unique(as.character(sme_df$identification_method[idx]))
      method_vals <- method_vals[
        !is.na(method_vals) & nzchar(method_vals) & method_vals != "null"
      ]

      list(
        refs = refs,
        refs_str = paste(refs, collapse = "|"),
        n_db = length(db_vals),
        n_method = length(method_vals)
      )
    })
  }
  names(feature_lookup) <- valid_sme_ids

  smf_df$SME_ID_REFS <- "null"
  smf_df$SME_ID_REF_ambiguity_code <- "null"

  matched_positions <- match(smf_feature_ids, names(feature_lookup))
  has_lookup <- !is.na(matched_positions)

  if (any(has_lookup)) {
    feature_rows <- feature_lookup[matched_positions[has_lookup]]
    smf_df$SME_ID_REFS[has_lookup] <- vapply(
      feature_rows,
      function(feature_data) {
        if (is.null(feature_data)) "null" else feature_data$refs_str
      },
      character(1L)
    )

    ambiguity_codes <- vapply(
      feature_rows,
      function(feature_data) {
        if (is.null(feature_data) || length(feature_data$refs) <= 1L) {
          "null"
        } else if (feature_data$n_db > 1L && feature_data$n_method > 1L) {
          "3"
        } else if (feature_data$n_db > 1L) {
          "1"
        } else if (feature_data$n_method > 1L) {
          "2"
        } else {
          "1"
        }
      },
      character(1L)
    )
    smf_df$SME_ID_REF_ambiguity_code[has_lookup] <- ambiguity_codes
  }

  tidytable::as_tidytable(smf_df)
}

#' Split reference list encoded with pipe separator
#' @keywords internal
.mztab_split_ref_ids <- function(x) {
  if (is.na(x) || !nzchar(x) || x == "null") {
    return(character(0))
  }
  vals <- strsplit(x, "|", fixed = TRUE)[[1L]]
  vals <- vals[nzchar(vals) & vals != "null"]
  unique(vals)
}

#' Union two pipe-separated reference lists
#' @keywords internal
.mztab_union_ref_ids <- function(a, b) {
  ids <- unique(c(.mztab_split_ref_ids(a), .mztab_split_ref_ids(b)))
  if (length(ids) == 0L) {
    "null"
  } else {
    paste(ids, collapse = "|")
  }
}

#' Remap pipe-separated references according to id map
#' @keywords internal
.mztab_remap_ref_ids <- function(x, id_map) {
  vapply(
    X = x,
    FUN = function(one) {
      ids <- .mztab_split_ref_ids(one)
      if (length(ids) == 0L) {
        return("null")
      }
      mapped <- vapply(
        X = ids,
        FUN = function(id) {
          if (!is.null(id_map[[id]]) && nzchar(id_map[[id]])) {
            id_map[[id]]
          } else {
            id
          }
        },
        FUN.VALUE = character(1L)
      )
      .mztab_union_ref_ids("null", paste(mapped, collapse = "|"))
    },
    FUN.VALUE = character(1L)
  )
}

#' Find next numeric ID from an ID column
#' @keywords internal
.mztab_next_numeric_id <- function(ids) {
  nums <- suppressWarnings(as.integer(ids))
  nums <- nums[!is.na(nums)]
  if (length(nums) == 0L) 1L else max(nums) + 1L
}

#' Convert a TIMA score to MSI reliability level (1-4)
#'
#' The mapping is intentionally conservative to avoid overstating evidence:
#' * 1 – confirmed by spectral library match AND score >= 0.7
#' * 2 – probable structure; spectral evidence or score >= 0.5
#' * 3 – putative annotation; score >= 0.2
#' * 4 – unambiguous compound class only
#' @keywords internal
.mztab_score_to_reliability <- function(score, library) {
  if (length(score) == 0L) {
    return(integer(0))
  }

  if (length(library) != length(score)) {
    library <- rep(library, length.out = length(score))
  }

  is_spectral <- grepl(
    "spectral|gnps|sirius|mzmine|mztab",
    library,
    ignore.case = TRUE
  )
  score <- suppressWarnings(as.numeric(score))
  is_na_score <- is.na(score)

  out <- rep(4L, length(score))
  cond1 <- is_spectral & !is_na_score & score >= 0.7
  cond2 <- is_spectral | (!is_na_score & score >= 0.5)
  cond3 <- !is_na_score & score >= 0.2

  out[cond1] <- 1L
  out[!cond1 & cond2] <- 2L
  out[!cond1 & !cond2 & cond3] <- 3L
  out
}

#' Build mzTab-M metadata table
#'
#' Returns a complete, ontology-aligned metadata table. Key design decisions:
#'
#' * All `id_confidence_measure[n]` entries use the `TIMA` user-CV namespace
#'   (`TIMA:001`–`TIMA:004`).  No PSI-MS accession exists for composite
#'   metabolomics scores.
#' * `ms_level` values in the SME section use `MS:1000579` / `MS:1000580`.
#' * `ms_run[1]-scan_polarity` is populated with `MS:1000130` (positive) or
#'   `MS:1000129` (negative) when `polarity` is known.
#' * When `xrefs_index` contains data, additional `database[n]` blocks are
#'   emitted (n >= 2) for every unique xref prefix found in the annotation set.
#' * The TIMA repository URL is registered as `software[1]-setting[1]` for
#'   machine-readable software provenance.
#' * `colunit-small_molecule_feature` declares seconds (UO:0000010).
#' * `colunit-small_molecule` declares daltons (UO:0000221) for the neutral
#'   mass column.
#' * `colunit-small_molecule_evidence` documents each `id_confidence_measure`
#'   column with its TIMA CV Param.
#'
#' @keywords internal
