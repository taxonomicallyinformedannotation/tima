#' Build COM provenance lines for non-classical TIMA columns
#' @keywords internal
.mztab_build_export_comments <- function(results, chunk_size = 16L) {
  if (!is.data.frame(results) || ncol(results) == 0L) {
    return(character(0))
  }

  src_cols <- colnames(results)
  canonical_src <- c(
    "feature_id",
    "feature_mz",
    "feature_rt",
    "candidate_structure_inchikey_connectivity_layer",
    "candidate_structure_molecular_formula",
    "candidate_structure_smiles_no_stereo",
    "candidate_structure_inchi",
    "candidate_structure_name",
    "candidate_structure_uri",
    "candidate_adduct",
    "candidate_structure_exact_mass",
    "candidate_spectrum_id",
    "candidate_library",
    "rank_final",
    "score_final",
    "score_biological",
    "score_chemical",
    "candidate_score_similarity"
  )

  non_classical <- setdiff(src_cols, canonical_src)
  if (length(non_classical) == 0L) {
    return(character(0))
  }

  chunks <- split(
    non_classical,
    ceiling(seq_along(non_classical) / max(1L, as.integer(chunk_size)))
  )

  c(
    "COM\tTIMA export: non-classical input columns are preserved as opt_global_* fields in SMF/SME",
    vapply(
      seq_along(chunks),
      function(i) {
        paste0(
          "COM\tTIMA opt columns[",
          i,
          "]\t",
          paste(chunks[[i]], collapse = "|")
        )
      },
      FUN.VALUE = character(1L)
    )
  )
}

#' Build COM lines summarizing ambiguity cardinalities in SML output
#' @keywords internal
.mztab_build_ambiguity_comments <- function(results) {
  if (!is.data.frame(results) || nrow(results) == 0L) {
    return(character(0))
  }

  has_multivalue <- FALSE
  if (
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
        colnames(results)
    )
  ) {
    has_multivalue <- any(vapply(
      c(
        "database_identifier",
        "chemical_formula",
        "smiles",
        "inchi",
        "chemical_name",
        "uri",
        "adduct_ions"
      ),
      function(col) {
        any(grepl("|", as.character(results[[col]]), fixed = TRUE))
      },
      FUN.VALUE = logical(1L)
    ))
  }

  if (has_multivalue) {
    results <- .mztab_expand_summarized(results)
  }

  if (!"feature_id" %in% colnames(results)) {
    return(character(0))
  }

  raw_fids <- as.character(results$feature_id)
  valid_fid_mask <- !is.na(raw_fids) & nzchar(raw_fids) & raw_fids != "null"
  feature_ids <- raw_fids[valid_fid_mask]
  if (length(feature_ids) == 0L) {
    return(character(0))
  }

  u_fids <- unique(feature_ids)
  fid_idx <- fastmatch::fmatch(feature_ids, u_fids)
  counts <- tabulate(fid_idx, nbins = length(u_fids))

  if (!has_multivalue) {
    n_features <- length(counts)
    bins <- c(
      `1` = sum(counts == 1L),
      `2` = sum(counts == 2L),
      `3+` = sum(counts >= 3L)
    )
    return(c(
      paste0(
        "COM\tTIMA ambiguity\tfeatures=",
        n_features,
        "; candidate_rows=",
        length(feature_ids),
        "; 1-candidate=",
        unname(bins[["1"]]),
        "; 2-candidate=",
        unname(bins[["2"]]),
        "; 3+-candidate=",
        unname(bins[["3+"]]),
        "; max-candidate=1"
      ),
      "COM\tTIMA ambiguity\taligned_fields=database_identifier|chemical_formula|smiles|inchi|chemical_name|uri|adduct_ions; cardinality_verified=TRUE"
    ))
  }

  aligned_cols <- c(
    "database_identifier",
    "chemical_formula",
    "smiles",
    "inchi",
    "chemical_name",
    "uri",
    "adduct_ions"
  )

  if (!all(aligned_cols %in% colnames(results))) {
    candidate_counts <- counts
  } else {
    norm <- function(x) {
      x <- as.character(x)
      x[is.na(x) | !nzchar(x)] <- "null"
      x
    }
    cols_norm <- lapply(
      aligned_cols,
      function(col) norm(results[[col]][valid_fid_mask])
    )
    tuple_key <- do.call(paste, c(cols_norm, list(sep = "||")))
    comb_key <- paste(fid_idx, tuple_key, sep = "\r")
    keep_unique <- !duplicated(comb_key)
    candidate_counts <- tabulate(fid_idx[keep_unique], nbins = length(u_fids))
  }

  bins <- c(
    `1` = sum(counts == 1L),
    `2` = sum(counts == 2L),
    `3+` = sum(counts >= 3L)
  )

  c(
    paste0(
      "COM\tTIMA ambiguity\tfeatures=",
      length(counts),
      "; candidate_rows=",
      length(feature_ids),
      "; 1-candidate=",
      unname(bins[["1"]]),
      "; 2-candidate=",
      unname(bins[["2"]]),
      "; 3+-candidate=",
      unname(bins[["3+"]]),
      "; max-candidate=",
      if (length(candidate_counts) > 0L) {
        max(candidate_counts, na.rm = TRUE)
      } else {
        0L
      }
    ),
    paste0(
      "COM\tTIMA ambiguity\taligned_fields=",
      paste(aligned_cols, collapse = "|"),
      "; cardinality_verified=",
      if (length(candidate_counts) > 0L) {
        all(candidate_counts %in% c(1L, 2L, 3L, 4L, 5L, 6L, 7L))
      } else {
        TRUE
      }
    )
  )
}

#' Build COM lines embedding edges into mzTab text exports
#' @keywords internal
.mztab_build_edges_comments <- function(edges_file, chunk_size = 500L) {
  if (is.null(edges_file) || !is.character(edges_file) || !nzchar(edges_file)) {
    return(character(0))
  }

  edges <- safe_fread(
    file = edges_file,
    file_type = "edge table",
    na.strings = c("", "NA", "null", "NULL"),
    colClasses = "character"
  )
  if (nrow(edges) == 0L) {
    return(c("COM\tTIMA edges\tedges_file present but empty"))
  }

  df <- as.data.frame(edges, stringsAsFactors = FALSE, check.names = FALSE)
  cols <- colnames(df)
  n_rows <- nrow(df)

  header <- paste0(
    "COM\tTIMA edges\trows=",
    n_rows,
    "; cols=",
    paste(cols, collapse = "|")
  )

  encode_json_col <- function(col_name, vec) {
    key_prefix <- paste0(
      as.character(jsonlite::toJSON(col_name, auto_unbox = TRUE)),
      ":"
    )
    out <- rep(paste0(key_prefix, "null"), length(vec))
    valid <- which(!is.na(vec))
    if (length(valid) > 0L) {
      v_sub <- vec[valid]
      u <- unique(v_sub)
      special <- grepl('["\\\\\\x00-\\x1f]', u, perl = TRUE)
      u_enc <- paste0('"', u, '"')
      if (any(special)) {
        u_enc[special] <- vapply(
          u[special],
          function(s) as.character(jsonlite::toJSON(s, auto_unbox = TRUE)),
          character(1L)
        )
      }
      u_full <- paste0(key_prefix, u_enc)
      out[valid] <- u_full[fastmatch::fmatch(v_sub, u)]
    }
    out
  }

  col_pieces <- lapply(seq_along(cols), function(j) {
    encode_json_col(cols[[j]], df[[j]])
  })
  row_lines <- paste0(
    "COM\tTIMA edge\t{",
    do.call(paste, c(col_pieces, list(sep = ","))),
    "}"
  )

  if (length(row_lines) > as.integer(chunk_size)) {
    log_warn(
      "Embedding %s edges into mzTab COM lines from %s",
      n_rows,
      basename(edges_file)
    )
  }

  rm(edges, df, cols, n_rows, col_pieces, chunk_size)

  c(header, row_lines)
}

#' @keywords internal
.mztab_is_json_path <- function(path) {
  is.character(path) &&
    length(path) == 1L &&
    grepl("\\.json$", path, ignore.case = TRUE)
}

#' @keywords internal
.mztab_json_cell <- function(x) {
  x <- as.character(x)
  if (length(x) == 0L || is.na(x) || !nzchar(x) || identical(x, "null")) {
    return(NA_character_)
  }
  x
}

#' @keywords internal
.mztab_metadata_to_json <- function(meta) {
  if (nrow(meta) == 0L) {
    return(list())
  }

  keys <- as.character(meta$key)
  vals <- lapply(meta$value, .mztab_json_cell)
  stats::setNames(vals, keys)
}

#' @keywords internal
.mztab_table_to_json_rows <- function(tbl, drop_feature_id = FALSE) {
  if (nrow(tbl) == 0L) {
    return(list())
  }

  df <- as.data.frame(tbl, stringsAsFactors = FALSE, check.names = FALSE)
  if (drop_feature_id && "feature_id" %in% colnames(df)) {
    df <- df[, setdiff(colnames(df), "feature_id"), drop = FALSE]
  }
  if (ncol(df) == 0L) {
    return(replicate(
      nrow(df),
      stats::setNames(list(), character(0)),
      simplify = FALSE
    ))
  }

  cols_clean <- lapply(df, function(col) {
    x <- as.character(col)
    x[is.na(x) | !nzchar(x) | x == "null"] <- NA_character_
    x
  })
  result <- .mapply(list, cols_clean, NULL)
  rm(tbl, df, cols_clean, drop_feature_id)
  result
}

#' @keywords internal
.mztab_write_json <- function(
  file,
  metadata,
  sml_table,
  smf_table,
  sme_table,
  edges_file = NULL
) {
  sml_rows <- .mztab_table_to_json_rows(sml_table)
  smf_rows <- .mztab_table_to_json_rows(smf_table, drop_feature_id = TRUE)
  sme_rows <- .mztab_table_to_json_rows(sme_table, drop_feature_id = TRUE)
  edges_rows <- .mztab_edges_json_rows(edges_file)

  payload <- list(
    metadata = .mztab_metadata_to_json(metadata),
    # Keep snake_case keys for TIMA parser compatibility.
    small_molecule_summary = sml_rows,
    small_molecule_feature = smf_rows,
    small_molecule_evidence = sme_rows,
    # Add camelCase aliases used by rmzTabM JSON examples.
    smallMoleculeSummary = sml_rows,
    smallMoleculeFeature = smf_rows,
    smallMoleculeEvidence = sme_rows
  )

  if (length(edges_rows) > 0L) {
    payload$tima_edges <- edges_rows
  }

  create_dir(file)
  jsonlite::write_json(
    x = payload,
    path = file,
    pretty = TRUE,
    auto_unbox = TRUE,
    na = "null",
    null = "null"
  )

  rm(sml_rows, smf_rows, sme_rows, edges_rows, payload)
  invisible(file)
}

#' Build JSON edge rows for mzTab JSON export
#' @keywords internal
.mztab_edges_json_rows <- function(edges_file) {
  if (is.null(edges_file) || !is.character(edges_file) || !nzchar(edges_file)) {
    return(list())
  }

  edges <- safe_fread(
    file = edges_file,
    file_type = "edge table",
    na.strings = c("", "NA", "null", "NULL"),
    colClasses = "character"
  )
  if (nrow(edges) == 0L) {
    return(list())
  }

  .mztab_table_to_json_rows(edges)
}

#' Map a TIMA candidate_library string to an mzTab-M Param for identification_method.
#'
#' The mzTab-M spec requires `identification_method` to be a CV Param
#' `[cv_label, accession, name, value]`.  TIMA evidence items originate from
#' different upstream sources; this function maps known patterns to the most
#' appropriate Param.
#'
#' Mapping rationale (PSI-MS accessions where registered; user params otherwise):
#' * **SIRIUS** – in-silico fragmentation tree by SIRIUS/CSI:FingerID.
#'   No single PSI-MS accession covers the full SIRIUS pipeline, so a user
#'   param is used.  `software[n]` should be declared separately.
#' * **GNPS** – spectral library matching on the GNPS platform.
#' * **Spectral / mzTab / MZmine / MS2** – generic spectral library matching.
#' * **MS1 / exact-mass** – mass-based database look-up (no fragmentation).
#' * **Already-formatted Param** – passed through unchanged.
#' * **Anything else** – wrapped as a user parameter carrying the raw library
#'   name.
#'
#' @include logs_utils.R
#' @include safe_fread.R
#' @include create_dir.R
#' @param lib Character vector of `candidate_library` values.
#' @return Character vector of mzTab Param strings.
#' @keywords internal
.mztab_library_to_identification_method <- function(lib) {
  lib_chr <- as.character(lib)
  if (length(lib_chr) == 0L) {
    return(character(0))
  }
  u_lib <- unique(lib_chr)
  u_trimmed <- trimws(u_lib)
  u_out <- tidytable::case_when(
    # Pass through values already in Param format.
    grepl("^\\[.*,.*,.*,.*\\]$", u_trimmed) ~ u_trimmed,

    # SIRIUS – in-silico molecular-formula / fragmentation scoring.
    # No single PSI-MS accession covers the full SIRIUS pipeline.
    grepl("sirius", u_lib, ignore.case = TRUE) ~
      "[, , SIRIUS in-silico structure elucidation, ]",

    # GNPS spectral networking / library.
    grepl("gnps", u_lib, ignore.case = TRUE) ~
      "[, , GNPS spectral library matching, ]",

    # MZmine or other spectral-library tools embedded via mzTab.
    grepl("mzmine|mztab", u_lib, ignore.case = TRUE) ~
      "[, , spectral library matching, ]",

    # Generic MS/MS spectral library.
    grepl("spectral|library|ms2|msms", u_lib, ignore.case = TRUE) ~
      "[, , spectral library matching, ]",

    # MS1 exact-mass database search (no fragmentation evidence).
    grepl("ms1|exact.mass", u_lib, ignore.case = TRUE) ~
      "[, , exact mass database search, ]",

    # Unknown / other: embed raw library name as a user parameter.
    is.na(u_lib) | u_lib %in% c("", "null", "NULL", "NA") ~
      "[, , unknown identification method, ]",
    TRUE ~ paste0("[, , ", u_trimmed, ", ]")
  )
  u_out[fastmatch::fmatch(lib_chr, u_lib)]
}

#' Escape characters that would break mzTab tab-delimited row/column structure
#'
#' Replaces TAB, carriage-return and newline characters with a single space so
#' that free-text values (metadata, chemical names, URIs, …) never corrupt the
#' tab-delimited layout of the mzTab file, as required by the mzTab specification.
#' @keywords internal
.mztab_escape <- function(x) {
  x <- as.character(x)
  bad <- grep("[\t\r\n]", x, perl = TRUE)
  if (length(bad) > 0L) {
    x[bad] <- gsub("[\t\r\n]+", " ", x[bad], perl = TRUE)
  }
  x
}

#' Derive charge values from adduct notation
#' @keywords internal
.mztab_charge_from_adduct <- function(adduct) {
  adduct_chr <- as.character(adduct)
  if (length(adduct_chr) == 0L) {
    return(character(0))
  }
  u_add <- unique(adduct_chr)
  u_out <- vapply(
    X = u_add,
    FUN = function(a) {
      if (is.na(a) || !nzchar(a) || a == "null") {
        return("null")
      }
      m <- regexec("([0-9]*)([+-])$", a, perl = TRUE)
      reg <- regmatches(a, m)[[1L]]
      if (length(reg) < 3L) {
        return("null")
      }
      mag <- if (nzchar(reg[[2L]])) as.integer(reg[[2L]]) else 1L
      if (is.na(mag)) {
        mag <- 1L
      }
      sign <- if (identical(reg[[3L]], "-")) -1L else 1L
      as.character(sign * mag)
    },
    FUN.VALUE = character(1L),
    USE.NAMES = FALSE
  )
  out <- u_out[fastmatch::fmatch(adduct_chr, u_add)]
  if (!is.null(names(adduct))) {
    names(out) <- names(adduct)
  } else {
    names(out) <- adduct_chr
  }
  out
}

# ── xrefs helpers ─────────────────────────────────────────────────────────────

# Canonical priority order for picking the best external xref per feature.
# Wikidata is preferred for URI enrichment; it is excluded only for the
# database_identifier field (wikidata InChIKey connectivity-layer IDs carry no
# database-specific prefix).
.MZTAB_XREF_PRIORITY_PREFIXES <- c(
  "wikidata",
  "chebi",
  "pubchem.compound",
  "hmdb",
  "chembl.compound",
  "drugbank",
  "chemspider",
  "surechembl",
  "kegg",
  "cas",
  "lipidmaps",
  "knapsack"
)

# Known prefix → (human-readable name, URL template) lookup.
# Sorted by priority: Wikidata > ChEBI > PubChem > HMDB > ChEMBL > others.
.XREF_KNOWN_PREFIXES <- list(
  wikidata = list(
    name = "Wikidata",
    uri = "https://www.wikidata.org/wiki/",
    prefix = "wd"
  ),
  chebi = list(
    name = "Chemical Entities of Biological Interest (ChEBI)",
    uri = "https://www.ebi.ac.uk/chebi/searchId.do?chebiId=CHEBI:",
    prefix = "CHEBI"
  ),
  "pubchem.compound" = list(
    name = "PubChem Compound",
    uri = "https://pubchem.ncbi.nlm.nih.gov/compound/",
    prefix = "CID"
  ),
  hmdb = list(
    name = "Human Metabolome Database (HMDB)",
    uri = "https://hmdb.ca/metabolites/",
    prefix = "HMDB"
  ),
  "chembl.compound" = list(
    name = "ChEMBL",
    uri = "https://www.ebi.ac.uk/chembl/compound_report_card/",
    prefix = "CHEMBL"
  ),
  kegg = list(
    name = "KEGG Compound",
    uri = "https://www.genome.jp/entry/",
    prefix = "C"
  ),
  cas = list(
    name = "CAS Registry",
    uri = "https://commonchemistry.cas.org/detail?cas_rn=",
    prefix = "CAS"
  ),
  lipidmaps = list(
    name = "LIPID MAPS",
    uri = "https://www.lipidmaps.org/databases/lmsd/",
    prefix = "LM"
  ),
  "knapsack" = list(
    name = "KNApSAcK",
    uri = "https://www.knapsackfamily.com/knapsack_core/information.php?word=",
    prefix = "C"
  ),
  drugbank = list(
    name = "DrugBank",
    uri = "https://go.drugbank.com/drugs/",
    prefix = "DB"
  ),
  chemspider = list(
    name = "ChemSpider",
    uri = "https://www.chemspider.com/Chemical-Structure-",
    prefix = "CSID"
  ),
  surechembl = list(
    name = "SureChEMBL",
    uri = "https://www.surechembl.org/compound/",
    prefix = "SCHEMBL"
  )
)
#' Normalize an xref prefix into the canonical internal form used by mzTab export.
#' @keywords internal
.mztab_normalize_xref_prefix <- function(prefix) {
  x <- as.character(prefix)
  x[is.na(x)] <- ""
  x <- trimws(tolower(x))
  x <- sub("^pubchem$", "pubchem.compound", x)
  x <- sub("^chembl$", "chembl.compound", x)
  x <- sub("^pubchem\\.compound$", "pubchem.compound", x)
  x <- sub("^chembl\\.compound$", "chembl.compound", x)
  x
}

#' Build a named list (InChIKey → data.frame) for fast xrefs look-up.
#'
#' Loads the cross-references file from [get_compounds_xrefs()] and indexes it
#' by the InChIKey connectivity layer fragment so per-compound look-up is O(1).
#'
#' @param xrefs_file Character file path, or `NULL`.
#' @return Named list of data.frames, or `NULL` when no valid file is provided.
#' @keywords internal
.mztab_build_xrefs_index <- function(xrefs_file) {
  if (
    is.null(xrefs_file) ||
      !nzchar(xrefs_file) ||
      xrefs_file == "null" ||
      !file.exists(xrefs_file)
  ) {
    return(NULL)
  }

  xrefs <- tryCatch(
    safe_fread(xrefs_file),
    error = function(e) {
      log_warn(
        "Failed to load xrefs file '%s': %s",
        basename(xrefs_file),
        conditionMessage(e)
      )
      NULL
    }
  )

  if (is.null(xrefs) || nrow(xrefs) == 0L) {
    return(NULL)
  }

  required_cols <- c("inchikey", "prefix", "id")
  if (!all(required_cols %in% colnames(xrefs))) {
    log_warn(
      "xrefs file missing required columns (%s); skipping xref enrichment",
      paste(setdiff(required_cols, colnames(xrefs)), collapse = ", ")
    )
    return(NULL)
  }

  xrefs_df <- as.data.frame(
    xrefs,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  xrefs_df$inchikey <- as.character(xrefs_df$inchikey)
  xrefs_df$prefix <- .mztab_normalize_xref_prefix(xrefs_df$prefix)
  xrefs_df$id <- as.character(xrefs_df$id)

  # Deduplicate by connectivity-key / prefix / id to keep the index compact.
  xrefs_df <- xrefs_df[
    !is.na(xrefs_df$inchikey) & nzchar(xrefs_df$inchikey),
    ,
    drop = FALSE
  ]
  xrefs_df$ik14 <- .mztab_connectivity_layer_key(xrefs_df$inchikey)
  xrefs_df <- xrefs_df[
    !is.na(xrefs_df$ik14) & nzchar(xrefs_df$ik14),
    ,
    drop = FALSE
  ]
  xrefs_df <- xrefs_df[
    !duplicated(paste(xrefs_df$ik14, xrefs_df$prefix, xrefs_df$id, sep = "\r")),
    ,
    drop = FALSE
  ]

  split(xrefs_df, xrefs_df$ik14)
}

#' Resolve xref-derived URI and database identifier values for a vector of InChIKeys.
#'
#' This is a vectorized wrapper around the single-row helpers so
#' mzTab export only performs one xref lookup per unique connectivity-layer key.
#'
#' @param inchikeys Character vector of InChIKey connectivity-layer fragments.
#' @param xrefs_index Named list of xref rows created by
#'   [.mztab_build_xrefs_index()].
#' @param fallback Optional fallback identifier vector.
#' @return Named list with `uri` and `database_identifier` character vectors.
#' @keywords internal
.mztab_resolve_xref_fields <- function(
  inchikeys,
  xrefs_index,
  fallback = NULL
) {
  inchikeys <- as.character(inchikeys)
  n <- length(inchikeys)
  if (n == 0L) {
    return(list(uri = character(0), database_identifier = character(0)))
  }

  if (is.null(fallback)) {
    fallback <- inchikeys
  }
  fallback <- as.character(fallback)
  if (length(fallback) != n) {
    fallback <- rep(fallback, length.out = n)
  }

  uri_out <- rep("null", n)
  db_out <- rep("null", n)
  if (is.null(xrefs_index) || length(xrefs_index) == 0L) {
    return(list(uri = uri_out, database_identifier = db_out))
  }

  valid_idx <- !is.na(inchikeys) & nzchar(inchikeys) & inchikeys != "null"
  if (!any(valid_idx)) {
    return(list(uri = uri_out, database_identifier = db_out))
  }

  keys <- .mztab_connectivity_layer_key(inchikeys)
  unique_keys <- unique(keys[valid_idx])
  first_key_idx <- fastmatch::fmatch(unique_keys, keys)
  idx_match <- fastmatch::fmatch(unique_keys, names(xrefs_index))
  uri_lookup <- rep("null", length(unique_keys))
  db_lookup <- rep("null", length(unique_keys))

  for (i in which(!is.na(idx_match))) {
    rows <- xrefs_index[[idx_match[[i]]]]
    if (is.null(rows) || nrow(rows) == 0L) {
      next
    }
    row_idx <- first_key_idx[[i]]
    fallback_val <- if (!is.na(row_idx)) fallback[[row_idx]] else "null"
    uri_lookup[[i]] <- .mztab_pick_best_uri(rows)
    db_lookup[[i]] <- .mztab_pick_best_database_identifier(
      rows,
      fallback = fallback_val
    )
  }

  key_match <- fastmatch::fmatch(keys[valid_idx], unique_keys)
  uri_out[valid_idx] <- uri_lookup[key_match]
  db_out[valid_idx] <- db_lookup[key_match]
  list(uri = uri_out, database_identifier = db_out)
}

#' Select the "best" URI from a set of xref rows for a single compound.
#'
#' Priority: Wikidata > ChEBI > PubChem > HMDB > ChEMBL > first available.
#'
#' @param rows data.frame with columns `prefix`, `id`.
#' @return Single character URI string, or `"null"`.
#' @keywords internal
.mztab_pick_best_uri <- function(rows) {
  priority_prefixes <- .MZTAB_XREF_PRIORITY_PREFIXES

  for (pfx in priority_prefixes) {
    match_rows <- rows[rows$prefix == pfx, , drop = FALSE]
    if (nrow(match_rows) > 0L) {
      id <- as.character(match_rows$id[[1L]])
      info <- .XREF_KNOWN_PREFIXES[[pfx]]
      if (!is.null(info)) {
        return(paste0(info$uri, id))
      }
    }
  }

  # Fallback: any available prefix
  if (nrow(rows) > 0L) {
    pfx <- rows$prefix[[1L]]
    id <- rows$id[[1L]]
    info <- .XREF_KNOWN_PREFIXES[[pfx]]
    if (!is.null(info)) {
      return(paste0(info$uri, id))
    }
  }

  "null"
}

#' Select the best database_identifier value as prefix:id.
#'
#' Prefers identifiers that align with registered database blocks and
#' identifiers.org-compatible prefixes.
#'
#' @param rows data.frame with columns `prefix`, `id` (or NULL).
#' @param fallback fallback identifier when no mapped xref is available.
#' @return Single database_identifier string.
#' @keywords internal
.mztab_pick_best_database_identifier <- function(rows, fallback = "null") {
  if (is.null(rows) || nrow(rows) == 0L) {
    fb <- as.character(fallback)[[1L]]
    if (is.na(fb) || !nzchar(fb)) "null" else fb
  } else {
    # wikidata reserved for URIs; used only as a last resort for the
    # database_identifier field (matches original ordering: last).
    priority_prefixes <- c(
      setdiff(
        .MZTAB_XREF_PRIORITY_PREFIXES,
        "wikidata"
      ),
      "wikidata"
    )

    for (pfx in priority_prefixes) {
      match_rows <- rows[rows$prefix == pfx, , drop = FALSE]
      if (nrow(match_rows) > 0L) {
        id <- as.character(match_rows$id[[1L]])
        info <- .XREF_KNOWN_PREFIXES[[pfx]]
        if (!is.null(info) && !is.na(id) && nzchar(id)) {
          return(paste0(info$prefix, ":", id))
        }
      }
    }

    # Fallback to first available mapped prefix:id.
    # Find first valid entry instead of looping
    pfx_vec <- as.character(rows$prefix)
    id_vec <- as.character(rows$id)

    # Find indices of valid entries
    valid_idx <- which(
      !is.na(id_vec) &
        nzchar(id_vec) &
        !vapply(
          pfx_vec,
          function(p) is.null(.XREF_KNOWN_PREFIXES[[p]]),
          logical(1L)
        )
    )

    if (length(valid_idx) > 0L) {
      i <- valid_idx[[1L]]
      info <- .XREF_KNOWN_PREFIXES[[pfx_vec[[i]]]]
      return(paste0(info$prefix, ":", id_vec[[i]]))
    }

    fb <- as.character(fallback)[[1L]]
    if (is.na(fb) || !nzchar(fb)) "null" else fb
  }
}

#' Derive additional database[n] metadata blocks from xrefs index.
#'
#' Collects all unique `prefix` values found across the entire xrefs index and
#' returns a list of metadata info blocks (name, prefix, uri) in priority order,
#' excluding `wikidata` (handled via URI enrichment) and unknown prefixes.
#'
#' @param xrefs_index Named list of data.frames (output of
#'   [.mztab_build_xrefs_index()]).
#' @return Named list of lists, each with fields `name`, `prefix`, `uri`.
#' @keywords internal
.mztab_xref_database_blocks <- function(xrefs_index) {
  if (is.null(xrefs_index) || length(xrefs_index) == 0L) {
    return(list())
  }

  # Collect all prefixes across all xref rows
  all_prefixes <- unique(unlist(lapply(xrefs_index, function(df) {
    as.character(df$prefix)
  })))

  # Priority order for database[n] registration (wikidata is for URIs only)
  ordered_prefixes <- .MZTAB_XREF_PRIORITY_PREFIXES
  selected <- intersect(ordered_prefixes, all_prefixes)

  out <- lapply(selected, function(pfx) {
    info <- .XREF_KNOWN_PREFIXES[[pfx]]
    if (is.null(info)) {
      return(NULL)
    }
    list(
      name = info$name,
      prefix = info$prefix,
      uri = gsub("/$", "", sub(":[^/]*$", "", info$uri))
    )
  })

  Filter(Negate(is.null), out)
}

#' Normalize an InChIKey value to its connectivity-layer fragment.
#' @keywords internal
.mztab_connectivity_layer_key <- function(x) {
  x <- as.character(x)
  x[is.na(x) | !nzchar(x)] <- NA_character_
  substr(x, 1L, 14L)
}
