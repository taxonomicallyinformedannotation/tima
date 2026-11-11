#' Read base mzTab while preserving unmanaged lines
#'
#' Collects passthrough content as every non-blank line that does not belong to
#' the managed TIMA sections (`MTD`, `SMH/SML`, `SFH/SMF`, `SEH/SME`). This
#' includes custom extension blocks and comment rows from anywhere in the file.
#' Passthrough lines are emitted before the TIMA-managed metadata section.
#'
#' @keywords internal
#' @include mztab_parser.R read_mztab.R
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

  rm(
    smf_base,
    sme_base,
    sml_base,
    smf_new,
    sme_new,
    sml_new,
    map_smf,
    map_sme,
    map_sml,
    smf_id_map,
    sme_id_map
  )

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
  # Add missing columns in one step via cbind — avoids per-column
  # data.frame copy growth (each tbl[[col]] <- … copies the entire frame)
  missing_cols <- setdiff(cols, colnames(tbl))
  if (length(missing_cols) > 0L) {
    new_cols <- rep(list(character(nrow(tbl))), length(missing_cols))
    names(new_cols) <- missing_cols
    tbl <- cbind(
      tbl,
      as.data.frame(new_cols, stringsAsFactors = FALSE, check.names = FALSE)
    )
  }
  # Keep optional/unknown columns to avoid information loss in merge mode.
  ordered_cols <- c(cols, setdiff(colnames(tbl), cols))
  tbl <- tbl[, ordered_cols, drop = FALSE]
  tbl[] <- lapply(tbl, function(x) {
    x <- as.character(x)
    x[is.na(x) | !nzchar(x)] <- "null"
    x
  })
  rm(cols, ordered_cols)
  tidytable::as_tidytable(tbl)
}

#' Build stable key for SMF deduplication
#' @keywords internal
.mztab_smf_key <- function(tbl) {
  tbl_df <- as.data.frame(tbl, stringsAsFactors = FALSE, check.names = FALSE)

  opt_cols <- grep("^opt_", colnames(tbl_df), value = TRUE)

  opt_sig <- if (length(opt_cols) > 0L) {
    opt_values <- lapply(tbl_df[, opt_cols, drop = FALSE], function(x) {
      x <- as.character(x)
      x[is.na(x) | !nzchar(x)] <- "null"
      x
    })
    result <- do.call(paste, c(opt_values, sep = "|"))
    rm(opt_values)
    result
  } else {
    rep("null", nrow(tbl_df))
  }

  result <- paste(
    tbl_df$exp_mass_to_charge,
    tbl_df$retention_time_in_seconds,
    tbl_df$charge,
    opt_sig,
    sep = "||"
  )
  rm(tbl_df, opt_cols, opt_sig)
  result
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

  new_keys <- .mztab_smf_key(smf_new)
  match_idx <- match(new_keys, base_keys)

  # Map duplicates and collect new row indices
  dup_idx <- which(!is.na(match_idx))
  new_row_indices <- which(is.na(match_idx))
  new_rows_count <- length(new_row_indices)

  if (length(dup_idx) > 0L) {
    old_ids <- smf_new$SMF_ID[dup_idx]
    mapped_ids <- smf_all$SMF_ID[match_idx[dup_idx]]
    id_map <- stats::setNames(as.list(mapped_ids), old_ids)
  }

  if (new_rows_count > 0L) {
    new_rows <- smf_new[new_row_indices, , drop = FALSE]
    new_rows$SMF_ID <- as.character(next_id + seq_len(new_rows_count) - 1L)
    new_rows$SML_ID_REFS <- "null"

    # Map new row IDs and append
    new_old_ids <- smf_new$SMF_ID[new_row_indices]
    new_mapped_ids <- new_rows$SMF_ID
    if (length(id_map) > 0L) {
      id_map <- c(id_map, stats::setNames(as.list(new_mapped_ids), new_old_ids))
    } else {
      id_map <- stats::setNames(as.list(new_mapped_ids), new_old_ids)
    }

    smf_all <- tidytable::bind_rows(smf_all, new_rows)
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

  new_keys <- .mztab_sme_key(sme_new)
  match_idx <- match(new_keys, base_keys)

  # Map duplicates and collect new row indices
  dup_idx <- which(!is.na(match_idx))
  new_row_indices <- which(is.na(match_idx))
  new_rows_count <- length(new_row_indices)

  if (length(dup_idx) > 0L) {
    old_ids <- sme_new$SME_ID[dup_idx]
    mapped_ids <- sme_all$SME_ID[match_idx[dup_idx]]
    id_map <- stats::setNames(as.list(mapped_ids), old_ids)
  }

  if (new_rows_count > 0L) {
    new_rows <- sme_new[new_row_indices, , drop = FALSE]
    new_rows$SME_ID <- as.character(next_id + seq_len(new_rows_count) - 1L)

    # Map new row IDs and append
    new_old_ids <- sme_new$SME_ID[new_row_indices]
    new_mapped_ids <- new_rows$SME_ID
    if (length(id_map) > 0L) {
      id_map <- c(id_map, stats::setNames(as.list(new_mapped_ids), new_old_ids))
    } else {
      id_map <- stats::setNames(as.list(new_mapped_ids), new_old_ids)
    }

    sme_all <- tidytable::bind_rows(sme_all, new_rows)
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
    new_rows <- sml_new[new_row_indices, , drop = FALSE]
    new_rows$SML_ID <- as.character(next_id + seq_len(new_rows_count) - 1L)
    sml_all <- tidytable::bind_rows(sml_all, new_rows)
  }

  # Process duplicates: merge references. Multiple new rows may map to the same
  # base row (same compound seen across several features); in that case the
  # references must be folded sequentially into the running union. We cannot use
  # a single mapply() over a duplicated `base_idx`, because R assignment with
  # duplicated indices keeps only the last write and would silently drop all
  # but the final new reference list (see .mztab_union_ref_ids).
  dup_idx <- which(!is.na(match_idx))
  if (length(dup_idx) > 0L) {
    base_idx <- match_idx[dup_idx]
    ref_cols <- c("SMF_ID_REFS", "SME_ID_REFS", "adduct_ions")

    # Work on a plain data.frame copy so duplicate-index assignment follows
    # ordinary R semantics; convert back to tidytable afterwards.
    sml_df <- as.data.frame(
      sml_all,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    new_vals <- as.data.frame(
      sml_new,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )

    u_bidx <- unique(base_idx)
    b_f <- factor(base_idx, levels = u_bidx)
    sml_df[u_bidx, ref_cols] <- lapply(
      ref_cols,
      function(col) {
        grouped <- split(new_vals[[col]][dup_idx], b_f)
        unlist(
          Map(
            function(cur, vals) Reduce(.mztab_union_ref_ids, vals, init = cur),
            sml_df[[col]][u_bidx],
            unname(grouped)
          ),
          use.names = FALSE
        )
      }
    )

    sml_all <- tidytable::as_tidytable(sml_df)
  }

  list(sml = sml_all)
}

#' Refresh SMF SML_ID_REFS from SML definitions
#' @keywords internal
.mztab_refresh_smf_sml_refs <- function(smf, sml) {
  if (nrow(smf) == 0L || nrow(sml) == 0L) {
    return(smf)
  }

  smf_target_ids <- as.character(smf$SMF_ID)
  u_smf_ids <- unique(smf_target_ids)
  n_u <- length(u_smf_ids)
  lookup_refs <- rep("null", n_u)

  raw_refs <- as.character(sml$SMF_ID_REFS)
  valid_rows <- which(!is.na(raw_refs) & nzchar(raw_refs) & raw_refs != "null")
  if (length(valid_rows) > 0L) {
    rv <- raw_refs[valid_rows]
    sml_ids_sub <- as.character(sml$SML_ID[valid_rows])
    has_pipe <- grepl("|", rv, fixed = TRUE)

    if (!any(has_pipe)) {
      smf_ids <- rv
      row_sml_ids <- sml_ids_sub
    } else {
      split_list <- as.list(rv)
      pipe_pos <- which(has_pipe)
      split_list[pipe_pos] <- lapply(
        strsplit(rv[pipe_pos], "|", fixed = TRUE),
        function(v) unique(v[nzchar(v) & v != "null"])
      )
      row_counts <- lengths(split_list)
      smf_ids <- unlist(split_list, use.names = FALSE)
      row_sml_ids <- rep.int(sml_ids_sub, row_counts)
    }

    m_idx <- fastmatch::fmatch(smf_ids, u_smf_ids)
    valid_mask <- !is.na(m_idx)
    if (any(valid_mask)) {
      v_m <- m_idx[valid_mask]
      v_sml <- row_sml_ids[valid_mask]
      counts <- tabulate(v_m, nbins = n_u)

      single_u <- which(counts == 1L)
      if (length(single_u) > 0L) {
        s_pos <- fastmatch::fmatch(single_u, v_m)
        lookup_refs[single_u] <- v_sml[s_pos]
      }

      if (any(counts > 1L)) {
        multi_mask <- counts[v_m] > 1L
        mm_u <- v_m[multi_mask]
        mm_sml <- v_sml[multi_mask]
        u_mm <- unique(mm_u)
        ord_mm <- order(fastmatch::fmatch(mm_u, u_mm), method = "radix")
        mm_u <- mm_u[ord_mm]
        mm_sml <- mm_sml[ord_mm]
        if (anyDuplicated(mm_sml) > 0L) {
          u_s <- unique(mm_sml)
          s_id <- fastmatch::fmatch(mm_sml, u_s)
          n_u_s <- length(u_s)
          s_key <- as.double(mm_u - 1L) * n_u_s + s_id
          keep_s <- !duplicated(s_key)
          mm_u <- mm_u[keep_s]
          mm_sml <- mm_sml[keep_s]
        }
        n_mm_all <- tabulate(mm_u, nbins = n_u)
        n_m <- length(mm_sml)
        new_fm <- if (n_m > 1L) c(TRUE, mm_u[-1L] != mm_u[-n_m]) else TRUE
        idx1_m <- which(new_fm)
        u_fid_m <- mm_u[idx1_m]
        grp_len_m <- n_mm_all[u_fid_m]
        lookup_refs <- Reduce(
          function(cur_refs, L) {
            f_pos <- which(grp_len_m == L)
            f_ids_L <- u_fid_m[f_pos]
            starts <- idx1_m[f_pos]
            cols_L <- lapply(0L:(L - 1L), function(off) mm_sml[starts + off])
            cur_refs[f_ids_L] <- do.call(paste, c(cols_L, list(sep = "|")))
            cur_refs
          },
          sort(unique(grp_len_m)),
          init = lookup_refs
        )
      }
    }
  }

  smf$SML_ID_REFS <- lookup_refs[fastmatch::fmatch(smf_target_ids, u_smf_ids)]
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

  smf_df <- as.data.frame(smf, stringsAsFactors = FALSE, check.names = FALSE)
  sme_df <- as.data.frame(sme, stringsAsFactors = FALSE, check.names = FALSE)

  smf_df$SME_ID_REFS <- rep.int("null", nrow(smf_df))
  smf_df$SME_ID_REF_ambiguity_code <- rep.int("null", nrow(smf_df))

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

  valid_sme_idx <- which(!is.na(sme_feature_ids))
  if (length(valid_sme_idx) == 0L) {
    return(tidytable::as_tidytable(smf_df))
  }

  valid_sme_fids <- sme_feature_ids[valid_sme_idx]
  valid_sme_ids <- unique(valid_sme_fids)
  n_u <- length(valid_sme_ids)

  matched_positions <- fastmatch::fmatch(smf_feature_ids, valid_sme_ids)
  has_lookup <- !is.na(matched_positions)
  if (!any(has_lookup)) {
    return(tidytable::as_tidytable(smf_df))
  }

  fid_id <- fastmatch::fmatch(valid_sme_fids, valid_sme_ids)
  counts <- tabulate(fid_id, nbins = n_u)

  sme_ids_clean <- as.character(sme_df$SME_ID[valid_sme_idx])
  sme_ids_clean[
    is.na(sme_ids_clean) | !nzchar(sme_ids_clean) | sme_ids_clean == "null"
  ] <- NA_character_

  lookup_refs <- rep.int("null", n_u)
  lookup_amb <- rep.int("null", n_u)

  single_fids <- which(counts == 1L)
  if (length(single_fids) > 0L) {
    single_pos <- fastmatch::fmatch(single_fids, fid_id)
    s_refs <- sme_ids_clean[single_pos]
    valid_s <- !is.na(s_refs)
    lookup_refs[single_fids[valid_s]] <- s_refs[valid_s]
  }

  if (any(counts > 1L)) {
    multi_mask <- counts[fid_id] > 1L
    m_pos <- which(multi_mask)
    m_fid <- fid_id[m_pos]
    ord_m <- order(m_fid, method = "radix")
    m_pos <- m_pos[ord_m]
    m_fid <- m_fid[ord_m]

    db_clean <- as.character(sme_df$database_identifier[valid_sme_idx])
    db_clean[
      is.na(db_clean) | !nzchar(db_clean) | db_clean == "null"
    ] <- NA_character_
    method_clean <- as.character(sme_df$identification_method[valid_sme_idx])
    method_clean[
      is.na(method_clean) | !nzchar(method_clean) | method_clean == "null"
    ] <- NA_character_

    # Count distinct non-NA database_identifier per feature_id
    n_db_all <- integer(n_u)
    m_db <- db_clean[m_pos]
    ok_db <- which(!is.na(m_db))
    if (length(ok_db) > 0L) {
      db_sub <- m_db[ok_db]
      fid_db <- m_fid[ok_db]
      u_db <- unique(db_sub)
      db_id <- fastmatch::fmatch(db_sub, u_db)
      n_u_db <- length(u_db)
      db_key <- as.double(fid_db - 1L) * n_u_db + db_id
      keep_db <- !duplicated(db_key)
      n_db_all <- tabulate(fid_db[keep_db], nbins = n_u)
    }

    # Count distinct non-NA identification_method per feature_id
    n_mt_all <- integer(n_u)
    m_mt <- method_clean[m_pos]
    ok_mt <- which(!is.na(m_mt))
    if (length(ok_mt) > 0L) {
      mt_sub <- m_mt[ok_mt]
      fid_mt <- m_fid[ok_mt]
      u_mt <- unique(mt_sub)
      mt_id <- fastmatch::fmatch(mt_sub, u_mt)
      n_u_mt <- length(u_mt)
      mt_key <- as.double(fid_mt - 1L) * n_u_mt + mt_id
      keep_mt <- !duplicated(mt_key)
      n_mt_all <- tabulate(fid_mt[keep_mt], nbins = n_u)
    }

    # Collapse distinct non-NA SME_IDs per feature_id
    m_refs <- sme_ids_clean[m_pos]
    ok_r <- which(!is.na(m_refs))
    if (length(ok_r) > 0L) {
      r_sub <- m_refs[ok_r]
      fid_r <- m_fid[ok_r]
      if (anyDuplicated(r_sub) > 0L) {
        u_r <- unique(r_sub)
        r_id <- fastmatch::fmatch(r_sub, u_r)
        n_u_r <- length(u_r)
        r_key <- as.double(fid_r - 1L) * n_u_r + r_id
        keep_r <- !duplicated(r_key)
        r_sub <- r_sub[keep_r]
        fid_r <- fid_r[keep_r]
      }
      n_r_all <- tabulate(fid_r, nbins = n_u)
      n_m_r <- length(r_sub)
      new_fr <- if (n_m_r > 1L) c(TRUE, fid_r[-1L] != fid_r[-n_m_r]) else TRUE
      idx1_r <- which(new_fr)
      u_fid_r <- fid_r[idx1_r]
      grp_len_r <- n_r_all[u_fid_r]
      lookup_refs <- Reduce(
        function(cur_refs, L) {
          f_pos <- which(grp_len_r == L)
          f_ids_L <- u_fid_r[f_pos]
          starts <- idx1_r[f_pos]
          cols_L <- lapply(0L:(L - 1L), function(off) r_sub[starts + off])
          cur_refs[f_ids_L] <- do.call(paste, c(cols_L, list(sep = "|")))
          cur_refs
        },
        sort(unique(grp_len_r)),
        init = lookup_refs
      )

      gt1_fids <- u_fid_r[grp_len_r > 1L]
      if (length(gt1_fids) > 0L) {
        nd <- n_db_all[gt1_fids]
        nm <- n_mt_all[gt1_fids]
        amb_gt1 <- rep.int("1", length(gt1_fids))
        amb_gt1[nd <= 1L & nm > 1L] <- "2"
        amb_gt1[nd > 1L & nm > 1L] <- "3"
        lookup_amb[gt1_fids] <- amb_gt1
      }
    }
  }

  pos_hit <- matched_positions[has_lookup]
  smf_df$SME_ID_REFS[has_lookup] <- lookup_refs[pos_hit]
  smf_df$SME_ID_REF_ambiguity_code[has_lookup] <- lookup_amb[pos_hit]

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
  if (length(x) == 0L) {
    return(character(0))
  }

  x <- as.character(x)
  valid_mask <- !is.na(x) & nzchar(x) & x != "null"
  if (!any(valid_mask)) {
    return(rep("null", length(x)))
  }

  map_keys <- names(id_map)
  if (!is.null(map_keys) && length(id_map) > 0L) {
    keep_m <- vapply(
      id_map,
      function(v) {
        !is.null(v) && length(v) == 1L && !is.na(v) && nzchar(as.character(v))
      },
      logical(1L)
    )
    map_keys <- map_keys[keep_m]
    map_vals <- as.character(unlist(id_map[keep_m], use.names = FALSE))
  } else {
    map_keys <- character(0)
    map_vals <- character(0)
  }
  has_map <- length(map_keys) > 0L

  out <- rep("null", length(x))
  valid_idx <- which(valid_mask)
  xv <- x[valid_idx]
  has_pipe <- grepl("|", xv, fixed = TRUE)

  if (!all(has_pipe)) {
    single_idx <- valid_idx[!has_pipe]
    single_vals <- xv[!has_pipe]
    if (has_map) {
      m <- fastmatch::fmatch(single_vals, map_keys)
      hit <- !is.na(m)
      if (any(hit)) {
        single_vals[hit] <- map_vals[m[hit]]
      }
    }
    out[single_idx] <- single_vals
  }

  if (any(has_pipe)) {
    pipe_idx <- valid_idx[has_pipe]
    pipe_vals <- xv[has_pipe]
    split_ids <- strsplit(pipe_vals, "|", fixed = TRUE)
    out[pipe_idx] <- vapply(
      split_ids,
      function(ids) {
        ids <- unique(ids[nzchar(ids) & ids != "null"])
        if (length(ids) == 0L) {
          return("null")
        }
        if (has_map) {
          m <- fastmatch::fmatch(ids, map_keys)
          hit <- !is.na(m)
          if (any(hit)) {
            ids[hit] <- map_vals[m[hit]]
          }
        }
        paste(ids, collapse = "|")
      },
      character(1L)
    )
  }

  out
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
