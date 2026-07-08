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

  results <- .mztab_expand_summarized(results)
  if (!"feature_id" %in% colnames(results)) {
    return(character(0))
  }

  feature_ids <- as.character(results$feature_id)
  feature_ids <- feature_ids[!is.na(feature_ids) & nzchar(feature_ids)]
  if (length(feature_ids) == 0L) {
    return(character(0))
  }

  counts <- table(feature_ids)
  aligned_cols <- c(
    "database_identifier",
    "chemical_formula",
    "smiles",
    "inchi",
    "chemical_name",
    "uri",
    "adduct_ions"
  )

  candidate_counts <- vapply(
    names(counts),
    function(fid) {
      idx <- which(feature_ids == fid)
      if (length(idx) == 0L) {
        return(0L)
      }
      grp <- results[idx, , drop = FALSE]
      if (!all(aligned_cols %in% colnames(grp))) {
        return(length(idx))
      }

      norm <- function(x) {
        x <- as.character(x)
        x[is.na(x) | !nzchar(x)] <- "null"
        x
      }
      tuple_key <- do.call(
        paste,
        c(
          lapply(aligned_cols, function(col) norm(grp[[col]])),
          list(sep = "||")
        )
      )
      length(unique(tuple_key))
    },
    FUN.VALUE = integer(1L)
  )

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

  header <- paste0(
    "COM\tTIMA edges\trows=",
    nrow(df),
    "; cols=",
    paste(cols, collapse = "|")
  )

  # Serialize rows as compact JSON objects for robust parsing of arbitrary values.
  row_lines <- vapply(
    seq_len(nrow(df)),
    function(i) {
      payload <- jsonlite::toJSON(
        as.list(df[i, , drop = FALSE]),
        auto_unbox = TRUE,
        null = "null",
        na = "null"
      )
      paste0("COM\tTIMA edge\t", as.character(payload))
    },
    FUN.VALUE = character(1L)
  )

  if (length(row_lines) > as.integer(chunk_size)) {
    log_warn(
      "Embedding %s edges into mzTab COM lines from %s",
      nrow(df),
      basename(edges_file)
    )
  }

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

  lapply(seq_len(nrow(df)), function(i) {
    row <- lapply(df[i, , drop = FALSE], function(v) .mztab_json_cell(v[[1L]]))
    names(row) <- colnames(df)
    row
  })
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
#' @param lib Character vector of `candidate_library` values.
#' @return Character vector of mzTab Param strings.
#' @keywords internal
.mztab_library_to_identification_method <- function(lib) {
  tidytable::case_when(
    # Pass through values already in Param format.
    grepl("^\\[.*,.*,.*,.*\\]$", trimws(lib)) ~ trimws(lib),

    # SIRIUS – in-silico molecular-formula / fragmentation scoring.
    # No single PSI-MS accession covers the full SIRIUS pipeline.
    grepl("sirius", lib, ignore.case = TRUE) ~
      "[, , SIRIUS in-silico structure elucidation, ]",

    # GNPS spectral networking / library.
    grepl("gnps", lib, ignore.case = TRUE) ~
      "[, , GNPS spectral library matching, ]",

    # MZmine or other spectral-library tools embedded via mzTab.
    grepl("mzmine|mztab", lib, ignore.case = TRUE) ~
      "[, , spectral library matching, ]",

    # Generic MS/MS spectral library.
    grepl("spectral|library|ms2|msms", lib, ignore.case = TRUE) ~
      "[, , spectral library matching, ]",

    # MS1 exact-mass database search (no fragmentation evidence).
    grepl("ms1|exact.mass", lib, ignore.case = TRUE) ~
      "[, , exact mass database search, ]",

    # Unknown / other: embed raw library name as a user parameter.
    is.na(lib) | !nzchar(lib) ~ "[, , unknown identification method, ]",
    TRUE ~ paste0("[, , ", trimws(lib), ", ]")
  )
}

#' Escape tab characters in free-text fields
#' @keywords internal
.mztab_escape <- function(x) {
  gsub("\t", " ", x, fixed = TRUE)
}

#' Derive charge values from adduct notation
#' @keywords internal
.mztab_charge_from_adduct <- function(adduct) {
  vapply(
    X = as.character(adduct),
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
    FUN.VALUE = character(1L)
  )
}

# ── xrefs helpers ─────────────────────────────────────────────────────────────

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
    uri = "http://www.knapsackfamily.com/knapsack_core/information.php?word=",
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
    !duplicated(xrefs_df[, c("ik14", "prefix", "id")]),
    ,
    drop = FALSE
  ]

  split(xrefs_df, xrefs_df$ik14)
}

#' Resolve xref-derived URI and database identifier values for a vector of InChIKeys.
#'
#' This is a memoized, vectorized wrapper around the single-row helpers so
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
  uri_lookup <- stats::setNames(rep("null", length(unique_keys)), unique_keys)
  db_lookup <- stats::setNames(rep("null", length(unique_keys)), unique_keys)

  for (key in unique_keys) {
    rows <- xrefs_index[[key]]
    if (is.null(rows) || nrow(rows) == 0L) {
      next
    }
    row_idx <- which(keys == key)[[1L]]
    fallback_val <- if (length(row_idx) > 0L) fallback[[row_idx]] else "null"
    uri_lookup[[key]] <- .mztab_pick_best_uri(rows)
    db_lookup[[key]] <- .mztab_pick_best_database_identifier(
      rows,
      fallback = fallback_val
    )
  }

  uri_out[valid_idx] <- uri_lookup[match(keys[valid_idx], unique_keys)]
  db_out[valid_idx] <- db_lookup[match(keys[valid_idx], unique_keys)]
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
  priority_prefixes <- c(
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
    priority_prefixes <- c(
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
      "knapsack",
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
  ordered_prefixes <- c(
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
