#' @title Complement structural metadata
#'
#' @description Complements structural metadata by joining stereochemistry,
#'     metadata, and chemical taxonomy from reference libraries.
#'     Enriches annotation results with comprehensive structure information.
#'
#' @include columns_utils.R
#' @include validations_utils.R
#' @include safe_fread.R
#' @include logs_utils.R
#' @include adduct_universe.R
#' @include validations_params.R
#'
#' @param df Data frame with structural metadata to complement
#' @param str_stereo Path to structure stereochemistry file (includes name, tag,
#'     xlogp)
#' @param str_met Path to structure metadata file (formula, mass)
#' @param str_tax_cla Path to ClassyFire taxonomy file (keyed by inchikey)
#' @param str_tax_npc Path to NPClassifier taxonomy file (keyed by smiles with
#'     stereo)
#'
#' @return Data frame with enriched structural metadata
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' enriched <- complement_metadata_structures(
#'   df = annotations,
#'   str_stereo = "data/str_stereo.tsv",
#'   str_met = "data/str_metadata.tsv",
#'   str_tax_cla = "data/str_tax_classyfire.tsv",
#'   str_tax_npc = "data/str_tax_npclassifier.tsv"
#' )
#' }
# Process-level cache: keyed on (str_stereo, str_met, str_tax_cla, str_tax_npc).
# Avoids re-reading the large reference tables on every annotator call.
complement_metadata_structures <- function(
  df,
  str_stereo,
  str_met,
  str_tax_cla,
  str_tax_npc
) {
  ctx <- log_operation("complement_metadata", n_input = nrow(df))

  validate_dataframe(df, param_name = "df")

  # Early exit for empty input
  if (nrow(df) == 0L) {
    log_warn("Empty data frame provided")
    return(df)
  }

  validate_file_existence(list(
    str_stereo = str_stereo,
    str_met = str_met,
    str_tax_cla = str_tax_cla,
    str_tax_npc = str_tax_npc
  ))

  log_debug("Complementing metadata for %d rows", nrow(df))

  keys <- .extract_structure_keys(df)
  if (length(keys$inchikey) == 0L && length(keys$smiles) == 0L) {
    log_debug("No structure keys found in input; returning input unchanged")
    return(.ensure_structure_placeholders(df))
  }

  # Load (or retrieve from cache) all four reference tables once per session.
  refs <- .complement_metadata_ref_cache$load_refs(
    str_stereo,
    str_met,
    str_tax_cla,
    str_tax_npc
  )

  # Filter stereo to keys present in this annotation batch.
  sf <- refs$stereo_full
  rows_cl <- .query_csr_index(refs$cl_csr, keys$inchikey)
  rows_sm <- .query_csr_index(refs$sm_csr, keys$smiles)
  idx_keep <- sort(unique(c(rows_cl, rows_sm)))
  log_debug("Stereo loaded: %d rows after filtering", length(idx_keep))

  ik_keep <- sf$structure_inchikey[idx_keep]
  sm_keep <- sf$structure_smiles[idx_keep]
  cl_keep <- sf$structure_inchikey_connectivity_layer[idx_keep]
  ns_keep <- sf$structure_inchikey_no_stereo[idx_keep]
  sm_ns_keep <- sf$structure_smiles_no_stereo[idx_keep]

  k_sel <- !is.na(ik_keep) & !duplicated(ik_keep)
  stereo_k <- tidytable::tidytable(
    structure_inchikey = ik_keep[k_sel],
    structure_smiles = sm_keep[k_sel],
    structure_inchikey_connectivity_layer = cl_keep[k_sel],
    structure_inchikey_no_stereo = ns_keep[k_sel],
    structure_smiles_no_stereo = sm_ns_keep[k_sel]
  )

  u_ns_keep <- unique(ns_keep)
  keys$inchikey_no_stereo <- u_ns_keep[!is.na(u_ns_keep)]

  s_sel <- !is.na(sm_ns_keep) & !duplicated(sm_ns_keep)
  stereo_s <- tidytable::tidytable(
    candidate_structure_inchikey_connectivity_layer_s = cl_keep[s_sel],
    candidate_structure_inchikey_no_stereo_s = ns_keep[s_sel],
    candidate_structure_smiles_no_stereo = sm_ns_keep[s_sel]
  )

  # Fallback: stereo_i_conn for cases where inchikey_no_stereo is unavailable
  # (e.g., SIRIUS annotations). Keyed by connectivity_layer only.
  ic_sel <- !is.na(cl_keep) & !duplicated(cl_keep)
  stereo_i_conn <- tidytable::tidytable(
    candidate_structure_inchikey_connectivity_layer = cl_keep[ic_sel],
    candidate_structure_smiles_no_stereo_ic = sm_ns_keep[ic_sel],
    candidate_structure_inchikey_no_stereo_ic = ns_keep[ic_sel]
  )
  rm(
    idx_keep,
    ik_keep,
    sm_keep,
    cl_keep,
    ns_keep,
    sm_ns_keep,
    k_sel,
    s_sel,
    ic_sel
  )

  # Load metadata (formula + mass only, keyed by inchikey_no_stereo)
  if (!"structure_inchikey_no_stereo" %in% refs$met_selected_cols) {
    log_warn(
      paste(
        "structure metadata has no structure keys;",
        "skipping key-based metadata enrichment"
      )
    )
  }

  met_lookup <- refs$met_lookup_full
  nam_lookup <- refs$nam_lookup_full
  tax_cla <- refs$tax_cla_lookup_full
  tax_npc <- refs$tax_npc_lookup_full
  log_debug("Names/tag/xlogp lookup: %d unique keys", nrow(nam_lookup))
  log_debug("ClassyFire taxonomy: %d rows", nrow(tax_cla))
  log_debug("NPClassifier taxonomy: %d rows", nrow(tax_npc))
  log_debug("Metadata lookup: %d unique keys", nrow(met_lookup))

  # Track original input columns so we only scan existing values for columns
  # actually supplied by the caller.
  orig_cols <- names(df)
  df <- tidytable::as_tidytable(as.list(df))
  missing_id_cols <- setdiff(
    c(
      "candidate_structure_inchikey_connectivity_layer",
      "candidate_structure_inchikey_no_stereo",
      "candidate_structure_smiles_no_stereo"
    ),
    names(df)
  )
  if (length(missing_id_cols) > 0L) {
    data.table::set(
      df,
      j = missing_id_cols,
      value = rep.int(NA_character_, nrow(df))
    )
  }
  log_debug("Starting key resolution joins on %d rows", nrow(df))

  # Derive candidate_structure_inchikey_no_stereo in df.
  # All structural identifiers (SMILES, InChIKey variants) are strictly
  # taken from the stereo reference file when available, ensuring
  # consistency with process_smiles() output.
  df <- .resolve_structure_identifiers(
    df = df,
    stereo_i_conn = stereo_i_conn,
    stereo_s = stereo_s
  )

  # Build the bridge from annotations (which have inchikey_no_stereo or
  # connectivity_layer) to the full inchikey needed for tax_cla and
  # the stereo smiles needed for tax_npc.
  # stereo_k provides: inchikey ->
  #   (smiles, connectivity_layer, inchikey_no_stereo,
  #    smiles_no_stereo)

  # Build unique key combinations for enrichment lookup.
  # Use only (inchikey_no_stereo, connectivity_layer) — NOT smiles_no_stereo.
  # Including smiles_no_stereo would fragment the lookup when tautomers
  # produce different SMILES for the same InChIKey (e.g., RWQNBRDOKXIBIV
  # glutamine amide vs enol form), leading to missed enrichments.
  structure_keys <- .build_structure_keys(df)
  row_to_sk <- attr(structure_keys, "row_to_sk")
  log_debug("Structure keys: %d unique combinations", nrow(structure_keys))

  stereo_bridge <- .build_stereo_bridge(stereo_k)

  key_lookup <- .build_structure_key_lookup_fast(
    structure_keys = structure_keys,
    met_lookup = met_lookup,
    nam_lookup = nam_lookup,
    stereo_bridge = stereo_bridge,
    tax_cla = tax_cla,
    tax_npc = tax_npc,
    allowed_inchikey_no_stereo = keys$inchikey_no_stereo
  )

  log_debug("Key lookup: %d rows; starting final join", nrow(key_lookup))
  table_final <- .apply_structure_enrichment_fast(
    df = df,
    key_lookup = key_lookup,
    row_to_sk = row_to_sk,
    orig_cols = orig_cols
  )
  table_final <- .ensure_structure_placeholders(table_final)
  rm(
    structure_keys,
    met_lookup,
    nam_lookup,
    key_lookup,
    stereo_k,
    stereo_i_conn,
    stereo_s,
    stereo_bridge,
    tax_cla,
    tax_npc
  )

  log_complete(ctx, n_enriched = nrow(table_final))

  table_final
}

.complement_metadata_ref_cache <- local({
  .cache <- new.env(parent = emptyenv())

  load_refs <- function(str_stereo, str_met, str_tax_cla, str_tax_npc) {
    # Include file modification time so cache invalidates when any reference file is replaced.
    mtime_key <- paste(
      vapply(
        c(
          str_stereo[[1L]],
          str_met[[1L]],
          str_tax_cla[[1L]],
          str_tax_npc[[1L]]
        ),
        function(f) {
          mt <- tryCatch(
            as.character(file.info(f)$mtime),
            error = function(.e) "?"
          )
          mt %||% "?"
        },
        character(1L)
      ),
      collapse = "|"
    )
    cache_key <- paste(
      str_stereo,
      str_met,
      str_tax_cla,
      str_tax_npc,
      mtime_key,
      sep = "|"
    )
    cached <- .cache[[cache_key]]
    if (!is.null(cached)) {
      return(cached)
    }

    stereo_cols <- .schema_stereo_cols()
    stereo_full <- .safe_fread_selected(
      file = str_stereo,
      file_type = "stereochemistry data",
      selected_cols = stereo_cols
    ) |>
      .ensure_columns(cols = stereo_cols)

    if (
      all(is.na(stereo_full$structure_inchikey_no_stereo)) &&
        nrow(stereo_full) > 0L
    ) {
      stereo_full <- stereo_full |>
        tidytable::mutate(
          structure_inchikey_no_stereo = tidytable::if_else(
            !is.na(structure_inchikey) & nchar(structure_inchikey) >= 27L,
            paste0(
              stri_sub(str = structure_inchikey, from = 1L, to = 14L),
              "-",
              stri_sub(str = structure_inchikey, from = -1L, to = -1L)
            ),
            NA_character_
          )
        )
    }

    met_cols <- .schema_metadata_cols()
    met_full <- .safe_fread_selected(
      file = str_met,
      file_type = "structure metadata",
      selected_cols = met_cols
    )
    met_selected_cols <- attr(met_full, "selected_cols")
    met_full <- .ensure_columns(met_full, cols = met_cols)

    tax_cla_full <- safe_fread(
      file = str_tax_cla,
      file_type = "ClassyFire taxonomy",
      na.strings = c("", "NA"),
      colClasses = "character",
      select = .schema_classyfire_cols()
    ) |>
      .ensure_columns(cols = .schema_classyfire_cols())

    tax_npc_full <- safe_fread(
      file = str_tax_npc,
      file_type = "NPClassifier taxonomy",
      na.strings = c("", "NA"),
      colClasses = "character",
      select = .schema_npc_cols()
    ) |>
      .ensure_columns(cols = .schema_npc_cols())

    # Precompute expensive per-reference lookups once and memoize them.
    keep_met <- which(
      !is.na(met_full$structure_inchikey_no_stereo) &
        !duplicated(met_full$structure_inchikey_no_stereo)
    )
    met_lookup_full <- tidytable::tidytable(
      candidate_structure_inchikey_no_stereo = met_full$structure_inchikey_no_stereo[
        keep_met
      ],
      .lk_molecular_formula = met_full$structure_molecular_formula[keep_met],
      .lk_exact_mass = met_full$structure_exact_mass[keep_met]
    )
    rm(met_full, keep_met)

    keep_cla <- which(!duplicated(tax_cla_full$structure_inchikey))
    tax_cla_lookup_full <- tidytable::tidytable(
      candidate_structure_inchikey = tax_cla_full$structure_inchikey[keep_cla],
      candidate_structure_tax_cla_chemontid_i = normalize_chemontid(
        tax_cla_full$structure_tax_cla_chemontid[keep_cla]
      ),
      candidate_structure_tax_cla_01kin_i = tax_cla_full$structure_tax_cla_01kin[
        keep_cla
      ],
      candidate_structure_tax_cla_02sup_i = tax_cla_full$structure_tax_cla_02sup[
        keep_cla
      ],
      candidate_structure_tax_cla_03cla_i = tax_cla_full$structure_tax_cla_03cla[
        keep_cla
      ],
      candidate_structure_tax_cla_04dirpar_i = tax_cla_full$structure_tax_cla_04dirpar[
        keep_cla
      ]
    )
    rm(tax_cla_full, keep_cla)

    keep_npc <- which(!duplicated(tax_npc_full$structure_smiles))
    tax_npc_lookup_full <- tidytable::tidytable(
      candidate_structure_smiles = tax_npc_full$structure_smiles[keep_npc],
      candidate_structure_tax_npc_01pat_s = tax_npc_full$structure_tax_npc_01pat[
        keep_npc
      ],
      candidate_structure_tax_npc_02sup_s = tax_npc_full$structure_tax_npc_02sup[
        keep_npc
      ],
      candidate_structure_tax_npc_03cla_s = tax_npc_full$structure_tax_npc_03cla[
        keep_npc
      ]
    )
    rm(tax_npc_full, keep_npc)

    # Validate stereo completeness at load time before caching.
    if (
      nrow(stereo_full) > 0L &&
        (!"structure_smiles_no_stereo" %in% colnames(stereo_full) ||
          all(is.na(stereo_full$structure_smiles_no_stereo)))
    ) {
      cli::cli_abort(
        paste(
          "stereochemistry data must contain",
          "structure_inchikey_connectivity_layer and structure_smiles_no_stereo"
        ),
        class = c("tima_validation_error", "tima_error"),
        call = NULL
      )
    }

    nam_lookup_full <- .build_name_lookup_from_stereo(stereo_full)
    stereo_full <- tidytable::tidytable(
      structure_inchikey = stereo_full$structure_inchikey,
      structure_smiles = stereo_full$structure_smiles,
      structure_inchikey_connectivity_layer = stereo_full$structure_inchikey_connectivity_layer,
      structure_inchikey_no_stereo = stereo_full$structure_inchikey_no_stereo,
      structure_smiles_no_stereo = stereo_full$structure_smiles_no_stereo
    )
    cl_csr <- .build_csr_index(
      stereo_full$structure_inchikey_connectivity_layer
    )
    sm_csr <- .build_csr_index(stereo_full$structure_smiles_no_stereo)

    # Pre-normalize all lookup columns once so repeated batches skip text normalization
    norm_set_cols <- function(tbl, cols) {
      cols <- intersect(cols, names(tbl))
      data.table::set(
        tbl,
        j = cols,
        value = lapply(cols, function(col) {
          v <- .normalize_enrichment_text(tbl[[col]])
          attr(v, "normalized") <- TRUE
          v
        })
      )
    }
    norm_set_cols(
      met_lookup_full,
      c(".lk_molecular_formula", ".lk_exact_mass")
    )
    norm_set_cols(nam_lookup_full, c(".lk_name", ".lk_tag", ".lk_xlogp"))
    norm_set_cols(
      tax_cla_lookup_full,
      setdiff(names(tax_cla_lookup_full), "candidate_structure_inchikey")
    )
    norm_set_cols(
      tax_npc_lookup_full,
      setdiff(names(tax_npc_lookup_full), "candidate_structure_smiles")
    )

    # Pre-warm fastmatch hash tables in the cached lookup tables
    if (nrow(met_lookup_full) > 0L) {
      invisible(fastmatch::fmatch(
        "",
        met_lookup_full$candidate_structure_inchikey_no_stereo
      ))
    }
    if (nrow(nam_lookup_full) > 0L) {
      invisible(fastmatch::fmatch(
        "",
        nam_lookup_full$candidate_structure_inchikey_no_stereo
      ))
    }
    if (nrow(tax_cla_lookup_full) > 0L) {
      invisible(fastmatch::fmatch(
        "",
        tax_cla_lookup_full$candidate_structure_inchikey
      ))
    }
    if (nrow(tax_npc_lookup_full) > 0L) {
      invisible(fastmatch::fmatch(
        "",
        tax_npc_lookup_full$candidate_structure_smiles
      ))
    }

    result <- list(
      stereo_full = stereo_full,
      cl_csr = cl_csr,
      sm_csr = sm_csr,
      met_selected_cols = met_selected_cols,
      met_lookup_full = met_lookup_full,
      nam_lookup_full = nam_lookup_full,
      tax_cla_lookup_full = tax_cla_lookup_full,
      tax_npc_lookup_full = tax_npc_lookup_full
    )
    .cache[[cache_key]] <- result
    result
  }

  list(load_refs = load_refs)
})

.build_csr_index <- function(vec) {
  ok <- which(!is.na(vec))
  if (length(ok) == 0L) {
    return(list(
      u_val = character(0),
      starts = integer(0),
      ends = integer(0),
      rows = integer(0),
      is_unique = TRUE
    ))
  }
  v_ok <- vec[ok]
  if (anyDuplicated(v_ok) == 0L) {
    invisible(fastmatch::fmatch("", v_ok))
    return(list(
      u_val = v_ok,
      rows = ok,
      is_unique = TRUE
    ))
  }
  ord <- order(v_ok, method = "radix")
  v_s <- v_ok[ord]
  r_s <- ok[ord]
  n_s <- length(v_s)
  new_g <- c(TRUE, v_s[-1L] != v_s[-n_s])
  starts <- which(new_g)
  ends <- c(starts[-1L] - 1L, n_s)
  u_val <- v_s[starts]
  invisible(fastmatch::fmatch("", u_val))
  list(
    u_val = u_val,
    starts = starts,
    ends = ends,
    rows = r_s,
    is_unique = FALSE
  )
}

.query_csr_index <- function(csr, keys) {
  if (length(keys) == 0L || length(csr$u_val) == 0L) {
    return(integer(0))
  }
  m <- fastmatch::fmatch(keys, csr$u_val)
  m <- m[!is.na(m)]
  if (length(m) == 0L) {
    return(integer(0))
  }
  if (csr$is_unique) {
    return(csr$rows[m])
  }
  st <- csr$starts[m]
  en <- csr$ends[m]
  lens <- en - st + 1L
  if (all(lens == 1L)) {
    return(csr$rows[st])
  }
  csr$rows[sequence(lens, from = st)]
}

.extract_structure_keys <- function(df) {
  inchikey_cols <- c(
    "candidate_structure_inchikey_connectivity_layer",
    "structure_inchikey_connectivity_layer"
  )
  smiles_cols <- c(
    "candidate_structure_smiles_no_stereo",
    "structure_smiles_no_stereo"
  )

  present_inchikey_cols <- intersect(inchikey_cols, names(df))
  present_smiles_cols <- intersect(smiles_cols, names(df))

  inchikey <- unique(unlist(
    lapply(present_inchikey_cols, function(col) {
      u <- unique(df[[col]])
      u[!is.na(u)]
    }),
    use.names = FALSE
  )) %||%
    character()

  smiles <- unique(unlist(
    lapply(present_smiles_cols, function(col) {
      u <- unique(df[[col]])
      u[!is.na(u)]
    }),
    use.names = FALSE
  )) %||%
    character()

  list(inchikey = inchikey, smiles = smiles)
}

.resolve_structure_identifiers <- function(df, stereo_i_conn, stereo_s) {
  has_ic <- "candidate_structure_inchikey_connectivity_layer" %in%
    names(df) &&
    nrow(stereo_i_conn) > 0L
  has_s <- "candidate_structure_smiles_no_stereo" %in%
    names(df) &&
    nrow(stereo_s) > 0L

  if (has_ic) {
    ic_col <- df$candidate_structure_inchikey_connectivity_layer
    u_ic <- unique(ic_col)
    u_ic_idx <- fastmatch::fmatch(
      u_ic,
      stereo_i_conn$candidate_structure_inchikey_connectivity_layer
    )
    idx <- u_ic_idx[fastmatch::fmatch(ic_col, u_ic)]
    mapped_smiles <- stereo_i_conn$candidate_structure_smiles_no_stereo_ic[idx]
    mapped_inchikey <- stereo_i_conn$candidate_structure_inchikey_no_stereo_ic[
      idx
    ]

    miss_smiles <- is.na(mapped_smiles)
    if (any(miss_smiles)) {
      mapped_smiles[miss_smiles] <- df$candidate_structure_smiles_no_stereo[
        miss_smiles
      ]
    }
    miss_inchikey <- is.na(mapped_inchikey)
    if (any(miss_inchikey)) {
      mapped_inchikey[
        miss_inchikey
      ] <- df$candidate_structure_inchikey_no_stereo[miss_inchikey]
    }

    df$candidate_structure_smiles_no_stereo <- mapped_smiles
    df$candidate_structure_inchikey_no_stereo <- mapped_inchikey
  }

  if (has_s) {
    sm_col <- df$candidate_structure_smiles_no_stereo
    u_sm <- unique(sm_col)
    u_sm_idx <- fastmatch::fmatch(
      u_sm,
      stereo_s$candidate_structure_smiles_no_stereo
    )
    idx <- u_sm_idx[fastmatch::fmatch(sm_col, u_sm)]
    mapped_conn <- stereo_s$candidate_structure_inchikey_connectivity_layer_s[
      idx
    ]
    mapped_inchikey <- stereo_s$candidate_structure_inchikey_no_stereo_s[idx]

    miss_conn <- is.na(mapped_conn)
    if (any(miss_conn)) {
      mapped_conn[
        miss_conn
      ] <- df$candidate_structure_inchikey_connectivity_layer[miss_conn]
    }
    miss_inchikey <- is.na(mapped_inchikey)
    if (any(miss_inchikey)) {
      mapped_inchikey[
        miss_inchikey
      ] <- df$candidate_structure_inchikey_no_stereo[miss_inchikey]
    }

    df$candidate_structure_inchikey_connectivity_layer <- mapped_conn
    df$candidate_structure_inchikey_no_stereo <- mapped_inchikey
  }

  df
}

.build_structure_keys <- function(df) {
  if (nrow(df) == 0L) {
    return(tidytable::tidytable(
      candidate_structure_inchikey_no_stereo = character(0),
      candidate_structure_inchikey_connectivity_layer = character(0)
    ))
  }

  ns_col <- df$candidate_structure_inchikey_no_stereo
  cl_col <- df$candidate_structure_inchikey_connectivity_layer
  u_ns <- unique(ns_col)
  u_cl <- unique(cl_col)
  ns_id <- fastmatch::fmatch(ns_col, u_ns)
  cl_id <- fastmatch::fmatch(cl_col, u_cl)
  pair_code <- (as.double(ns_id) - 1) *
    as.double(max(1L, length(u_cl))) +
    as.double(cl_id)
  u_pair_code <- unique(pair_code)
  first_pos <- fastmatch::fmatch(u_pair_code, pair_code)
  row_to_sk <- fastmatch::fmatch(pair_code, u_pair_code)

  out <- tidytable::tidytable(
    candidate_structure_inchikey_no_stereo = ns_col[first_pos],
    candidate_structure_inchikey_connectivity_layer = cl_col[first_pos]
  )
  attr(out, "row_to_sk") <- row_to_sk
  out
}

.build_stereo_bridge <- function(stereo_k) {
  cl <- stereo_k$structure_inchikey_connectivity_layer
  keep <- !is.na(cl) & !duplicated(cl)
  tidytable::tidytable(
    candidate_structure_inchikey_connectivity_layer = cl[keep],
    .bridge_inchikey = stereo_k$structure_inchikey[keep],
    .bridge_smiles = stereo_k$structure_smiles[keep]
  )
}

.make_structure_lookup_key <- function(no_stereo, connectivity_layer) {
  no_stereo <- as.character(no_stereo)
  connectivity_layer <- as.character(connectivity_layer)
  key_sep <- "\u001f"

  inv_ns <- is.na(no_stereo) |
    !nzchar(no_stereo) |
    no_stereo == "NA" |
    no_stereo == "null"
  inv_cl <- is.na(connectivity_layer) |
    !nzchar(connectivity_layer) |
    connectivity_layer == "NA" |
    connectivity_layer == "null"

  res <- paste0(no_stereo, key_sep, connectivity_layer)
  res[inv_ns | inv_cl] <- NA_character_
  res
}

.build_structure_key_lookup_fast <- function(
  structure_keys,
  met_lookup,
  nam_lookup,
  stereo_bridge,
  tax_cla,
  tax_npc,
  allowed_inchikey_no_stereo = NULL
) {
  if (nrow(structure_keys) == 0L) {
    return(tidytable::tidytable(
      candidate_structure_inchikey_no_stereo = character(0),
      candidate_structure_inchikey_connectivity_layer = character(0),
      .structure_key = character(0)
    ))
  }

  sk_inchikey_no_stereo <- structure_keys$candidate_structure_inchikey_no_stereo
  sk_connectivity <- structure_keys$candidate_structure_inchikey_connectivity_layer
  sk_ns_lookup <- if (!is.null(allowed_inchikey_no_stereo)) {
    in_allowed <- fastmatch::fmatch(
      sk_inchikey_no_stereo,
      allowed_inchikey_no_stereo,
      nomatch = 0L
    ) >
      0L
    ifelse(in_allowed, sk_inchikey_no_stereo, NA_character_)
  } else {
    sk_inchikey_no_stereo
  }

  lookup_tbl <- tidytable::tidytable(
    candidate_structure_inchikey_no_stereo = sk_inchikey_no_stereo,
    candidate_structure_inchikey_connectivity_layer = sk_connectivity,
    .structure_key = .make_structure_lookup_key(
      sk_inchikey_no_stereo,
      sk_connectivity
    )
  )

  .slice_lk_col <- function(col_vec, idx, n_out) {
    if (is.null(col_vec)) {
      res <- rep.int(NA_character_, n_out)
      attr(res, "normalized") <- TRUE
      return(res)
    }
    res <- col_vec[idx]
    if (isTRUE(attr(col_vec, "normalized"))) {
      attr(res, "normalized") <- TRUE
    }
    res
  }
  n_sk <- nrow(lookup_tbl)
  na_norm <- rep.int(NA_character_, n_sk)
  attr(na_norm, "normalized") <- TRUE

  # Metabolite lookup (formula + exact mass) - keyed by inchikey_no_stereo
  if (nrow(met_lookup) > 0L) {
    idx <- fastmatch::fmatch(
      sk_ns_lookup,
      met_lookup$candidate_structure_inchikey_no_stereo
    )
    lookup_tbl$.enr_candidate_structure_molecular_formula <- .slice_lk_col(
      met_lookup$.lk_molecular_formula,
      idx,
      n_sk
    )
    lookup_tbl$.enr_candidate_structure_exact_mass <- .slice_lk_col(
      met_lookup$.lk_exact_mass,
      idx,
      n_sk
    )
  } else {
    lookup_tbl$.enr_candidate_structure_molecular_formula <- na_norm
    lookup_tbl$.enr_candidate_structure_exact_mass <- na_norm
  }

  # Name/tag/xlogp lookup - keyed by inchikey_no_stereo
  if (nrow(nam_lookup) > 0L) {
    idx <- fastmatch::fmatch(
      sk_ns_lookup,
      nam_lookup$candidate_structure_inchikey_no_stereo
    )
    lookup_tbl$.enr_candidate_structure_name <- .slice_lk_col(
      nam_lookup$.lk_name,
      idx,
      n_sk
    )
    lookup_tbl$.enr_candidate_structure_tag <- .slice_lk_col(
      nam_lookup$.lk_tag,
      idx,
      n_sk
    )
    lookup_tbl$.enr_candidate_structure_xlogp <- .slice_lk_col(
      nam_lookup$.lk_xlogp,
      idx,
      n_sk
    )
  } else {
    lookup_tbl$.enr_candidate_structure_name <- na_norm
    lookup_tbl$.enr_candidate_structure_tag <- na_norm
    lookup_tbl$.enr_candidate_structure_xlogp <- na_norm
  }

  # Stereo bridge - keyed by connectivity_layer
  if (nrow(stereo_bridge) > 0L) {
    idx <- fastmatch::fmatch(
      sk_connectivity,
      stereo_bridge$candidate_structure_inchikey_connectivity_layer
    )
    lookup_tbl$.bridge_inchikey <- stereo_bridge$.bridge_inchikey[idx]
    lookup_tbl$.bridge_smiles <- stereo_bridge$.bridge_smiles[idx]
  } else {
    lookup_tbl$.bridge_inchikey <- NA_character_
    lookup_tbl$.bridge_smiles <- NA_character_
  }

  # ClassyFire taxonomy - keyed by full inchikey (via .bridge_inchikey)
  if (
    nrow(tax_cla) > 0L && "candidate_structure_inchikey" %in% names(tax_cla)
  ) {
    idx <- fastmatch::fmatch(
      lookup_tbl$.bridge_inchikey,
      tax_cla$candidate_structure_inchikey
    )
    lookup_tbl$.enr_candidate_structure_tax_cla_chemontid <- .slice_lk_col(
      tax_cla$candidate_structure_tax_cla_chemontid_i,
      idx,
      n_sk
    )
    lookup_tbl$.enr_candidate_structure_tax_cla_01kin <- .slice_lk_col(
      tax_cla$candidate_structure_tax_cla_01kin_i,
      idx,
      n_sk
    )
    lookup_tbl$.enr_candidate_structure_tax_cla_02sup <- .slice_lk_col(
      tax_cla$candidate_structure_tax_cla_02sup_i,
      idx,
      n_sk
    )
    lookup_tbl$.enr_candidate_structure_tax_cla_03cla <- .slice_lk_col(
      tax_cla$candidate_structure_tax_cla_03cla_i,
      idx,
      n_sk
    )
    lookup_tbl$.enr_candidate_structure_tax_cla_04dirpar <- .slice_lk_col(
      tax_cla$candidate_structure_tax_cla_04dirpar_i,
      idx,
      n_sk
    )
  } else {
    lookup_tbl$.enr_candidate_structure_tax_cla_chemontid <- na_norm
    lookup_tbl$.enr_candidate_structure_tax_cla_01kin <- na_norm
    lookup_tbl$.enr_candidate_structure_tax_cla_02sup <- na_norm
    lookup_tbl$.enr_candidate_structure_tax_cla_03cla <- na_norm
    lookup_tbl$.enr_candidate_structure_tax_cla_04dirpar <- na_norm
  }

  # NPClassifier taxonomy - keyed by smiles (via .bridge_smiles)
  if (nrow(tax_npc) > 0L && "candidate_structure_smiles" %in% names(tax_npc)) {
    idx <- fastmatch::fmatch(
      lookup_tbl$.bridge_smiles,
      tax_npc$candidate_structure_smiles
    )
    lookup_tbl$.enr_candidate_structure_tax_npc_01pat <- .slice_lk_col(
      tax_npc$candidate_structure_tax_npc_01pat_s,
      idx,
      n_sk
    )
    lookup_tbl$.enr_candidate_structure_tax_npc_02sup <- .slice_lk_col(
      tax_npc$candidate_structure_tax_npc_02sup_s,
      idx,
      n_sk
    )
    lookup_tbl$.enr_candidate_structure_tax_npc_03cla <- .slice_lk_col(
      tax_npc$candidate_structure_tax_npc_03cla_s,
      idx,
      n_sk
    )
  } else {
    lookup_tbl$.enr_candidate_structure_tax_npc_01pat <- na_norm
    lookup_tbl$.enr_candidate_structure_tax_npc_02sup <- na_norm
    lookup_tbl$.enr_candidate_structure_tax_npc_03cla <- na_norm
  }

  lookup_tbl
}

.apply_structure_enrichment_fast <- function(
  df,
  key_lookup,
  row_to_sk = NULL,
  orig_cols = NULL
) {
  if (nrow(df) == 0L) {
    return(df)
  }

  if (
    !is.null(row_to_sk) &&
      length(row_to_sk) == nrow(df) &&
      nrow(key_lookup) > 0L
  ) {
    sk_canon <- match(key_lookup$.structure_key, key_lookup$.structure_key)
    idx <- if (identical(sk_canon, seq_len(nrow(key_lookup)))) {
      row_to_sk
    } else {
      sk_canon[row_to_sk]
    }
  } else {
    row_keys <- .make_structure_lookup_key(
      df$candidate_structure_inchikey_no_stereo,
      df$candidate_structure_inchikey_connectivity_layer
    )
    idx <- match(row_keys, key_lookup$.structure_key)
  }

  out <- if (data.table::is.data.table(df)) {
    df
  } else {
    tidytable::as_tidytable(df)
  }
  enrichment_map <- list(
    candidate_structure_molecular_formula = c(
      ".enr_candidate_structure_molecular_formula",
      "candidate_structure_molecular_formula"
    ),
    candidate_structure_exact_mass = c(
      ".enr_candidate_structure_exact_mass",
      "candidate_structure_exact_mass"
    ),
    candidate_structure_xlogp = c(
      ".enr_candidate_structure_xlogp",
      "candidate_structure_xlogp"
    ),
    candidate_structure_tag = c(
      ".enr_candidate_structure_tag",
      "candidate_structure_tag"
    ),
    candidate_structure_name = c(
      ".enr_candidate_structure_name",
      "candidate_structure_name"
    ),
    candidate_structure_tax_npc_01pat = c(
      ".enr_candidate_structure_tax_npc_01pat",
      "candidate_structure_tax_npc_01pat"
    ),
    candidate_structure_tax_npc_02sup = c(
      ".enr_candidate_structure_tax_npc_02sup",
      "candidate_structure_tax_npc_02sup"
    ),
    candidate_structure_tax_npc_03cla = c(
      ".enr_candidate_structure_tax_npc_03cla",
      "candidate_structure_tax_npc_03cla"
    ),
    candidate_structure_tax_cla_chemontid = c(
      ".enr_candidate_structure_tax_cla_chemontid",
      "candidate_structure_tax_cla_chemontid"
    ),
    candidate_structure_tax_cla_01kin = c(
      ".enr_candidate_structure_tax_cla_01kin",
      "candidate_structure_tax_cla_01kin"
    ),
    candidate_structure_tax_cla_02sup = c(
      ".enr_candidate_structure_tax_cla_02sup",
      "candidate_structure_tax_cla_02sup"
    ),
    candidate_structure_tax_cla_03cla = c(
      ".enr_candidate_structure_tax_cla_03cla",
      "candidate_structure_tax_cla_03cla"
    ),
    candidate_structure_tax_cla_04dirpar = c(
      ".enr_candidate_structure_tax_cla_04dirpar",
      "candidate_structure_tax_cla_04dirpar"
    )
  )

  target_cols <- names(enrichment_map)
  data.table::set(
    out,
    j = target_cols,
    value = lapply(target_cols, function(target_col) {
      enrich_col <- enrichment_map[[target_col]][[1L]]
      existing_col <- enrichment_map[[target_col]][[2L]]
      enr_norm <- .normalize_enrichment_text(key_lookup[[enrich_col]])
      res <- enr_norm[idx]
      if (is.null(orig_cols) || existing_col %in% orig_cols) {
        existing_vec <- out[[existing_col]]
        if (!is.null(existing_vec)) {
          na_pos <- which(is.na(res) & !is.na(existing_vec))
          if (length(na_pos) > 0L) {
            res[na_pos] <- .normalize_enrichment_text(existing_vec[na_pos])
          }
        }
      }
      res
    })
  )

  out
}

.ensure_structure_placeholders <- function(df) {
  placeholder_candidate_cols <- .schema_candidate_structure_placeholders()
  placeholder_structure_cols <- .schema_structure_placeholders()

  needed <- setdiff(
    c(placeholder_candidate_cols, placeholder_structure_cols),
    names(df)
  )
  if (length(needed) == 0L) {
    return(df)
  }

  if (!data.table::is.data.table(df)) {
    df <- tidytable::as_tidytable(df)
  }
  data.table::set(df, j = needed, value = rep.int(NA_character_, nrow(df)))
  df
}

.schema_stereo_cols <- function() {
  c(
    "structure_inchikey",
    "structure_smiles",
    "structure_inchikey_connectivity_layer",
    "structure_inchikey_no_stereo",
    "structure_smiles_no_stereo",
    "structure_xlogp",
    "structure_name",
    "structure_tag"
  )
}

.schema_metadata_cols <- function() {
  c(
    "structure_inchikey_no_stereo",
    "structure_exact_mass",
    "structure_molecular_formula"
  )
}

.schema_classyfire_cols <- function() {
  c(
    "structure_inchikey",
    "structure_tax_cla_chemontid",
    "structure_tax_cla_01kin",
    "structure_tax_cla_02sup",
    "structure_tax_cla_03cla",
    "structure_tax_cla_04dirpar"
  )
}

.schema_npc_cols <- function() {
  c(
    "structure_smiles",
    "structure_tax_npc_01pat",
    "structure_tax_npc_02sup",
    "structure_tax_npc_03cla"
  )
}

.schema_candidate_structure_placeholders <- function() {
  c(
    "candidate_structure_molecular_formula",
    "candidate_structure_exact_mass",
    "candidate_structure_xlogp",
    "candidate_structure_tag",
    "candidate_structure_name",
    "candidate_structure_inchikey_connectivity_layer",
    "candidate_structure_inchikey_no_stereo",
    "candidate_structure_smiles_no_stereo",
    "candidate_structure_tax_npc_01pat",
    "candidate_structure_tax_npc_02sup",
    "candidate_structure_tax_npc_03cla",
    "candidate_structure_tax_cla_chemontid",
    "candidate_structure_tax_cla_01kin",
    "candidate_structure_tax_cla_02sup",
    "candidate_structure_tax_cla_03cla",
    "candidate_structure_tax_cla_04dirpar"
  )
}

.schema_structure_placeholders <- function() {
  c(
    "structure_molecular_formula",
    "structure_exact_mass",
    "structure_xlogp",
    "structure_tag",
    "structure_name",
    "structure_inchikey_connectivity_layer",
    "structure_smiles_no_stereo"
  )
}

.safe_fread_selected <- function(file, file_type, selected_cols) {
  header <- names(safe_fread(
    file = file,
    file_type = file_type,
    nrows = 0L,
    showProgress = FALSE
  ))
  selected <- intersect(selected_cols, header)

  if (length(selected) == 0L) {
    out <- tidytable::tidytable()
    attr(out, "selected_cols") <- character()
    return(out)
  }

  out <- safe_fread(
    file = file,
    file_type = file_type,
    na.strings = c("", "NA"),
    colClasses = "character",
    select = selected
  )
  attr(out, "selected_cols") <- selected
  out
}

.ensure_columns <- function(df, cols) {
  missing_cols <- setdiff(cols, names(df))
  if (length(missing_cols) == 0L) {
    return(df)
  }

  df |>
    tidytable::mutate(
      !!!stats::setNames(
        rep(list(NA_character_), length(missing_cols)),
        missing_cols
      )
    )
}

.trim_non_empty_fast <- function(x) {
  res <- as.character(x)
  if (length(res) == 0L) {
    return(res)
  }
  u_x <- unique(res)
  u_t <- trimws(u_x)
  u_t[is.na(u_t) | !nzchar(u_t)] <- NA_character_
  if (identical(u_t, u_x)) {
    return(res)
  }
  u_t[fastmatch::fmatch(res, u_x)]
}

.collapse_delimited_lookup <- function(
  grp_id,
  n_grp,
  vals,
  ignore_case = FALSE,
  sep = " $ "
) {
  out <- rep(NA_character_, n_grp)
  ok <- which(!is.na(vals))
  if (length(ok) == 0L) {
    return(out)
  }
  v_ok <- as.character(vals[ok])
  u_v <- unique(v_ok)
  u_trim <- trimws(u_v)
  v_trim <- if (identical(u_trim, u_v)) {
    v_ok
  } else {
    u_trim[fastmatch::fmatch(v_ok, u_v)]
  }
  nz <- nzchar(v_trim)
  if (!any(nz)) {
    return(out)
  }
  g_ok <- grp_id[ok[nz]]
  v_trim <- v_trim[nz]

  dedup_val <- if (ignore_case) {
    u_t <- unique(v_trim)
    u_low <- tolower(u_t)
    u_low[fastmatch::fmatch(v_trim, u_t)]
  } else {
    v_trim
  }
  u_dv <- unique(dedup_val)
  dv_id <- fastmatch::fmatch(dedup_val, u_dv)
  n_dv <- length(u_dv)
  gv_key <- as.double(g_ok - 1L) * n_dv + dv_id
  keep <- !duplicated(gv_key)
  g_ok <- g_ok[keep]
  v_trim <- v_trim[keep]

  if (!is.unsorted(g_ok)) {
    g_ord <- g_ok
    v_ord <- v_trim
  } else {
    ord <- order(g_ok, method = "radix")
    g_ord <- g_ok[ord]
    v_ord <- v_trim[ord]
  }

  n_m <- length(g_ord)
  new_g <- if (n_m > 1L) c(TRUE, g_ord[-1L] != g_ord[-n_m]) else TRUE
  r_m <- seq_len(n_m) - cummax(ifelse(new_g, seq_len(n_m), 0L)) + 1L
  max_r <- max(r_m)

  idx1 <- which(r_m == 1L)
  u_g <- g_ord[idx1]
  acc <- v_ord[idx1]
  if (max_r > 1L) {
    acc <- Reduce(
      function(cur_acc, k) {
        idx_k <- which(r_m == k)
        pos_k <- fastmatch::fmatch(g_ord[idx_k], u_g)
        cur_acc[pos_k] <- paste(cur_acc[pos_k], v_ord[idx_k], sep = sep)
        cur_acc
      },
      2L:max_r,
      init = acc
    )
  }
  out[u_g] <- acc
  out
}

.resolve_numeric_lookup <- function(grp_id, n_grp, vals, tolerance = 1e-8) {
  out <- rep(NA_character_, n_grp)
  ok_str <- which(!is.na(vals))
  if (length(ok_str) == 0L) {
    return(out)
  }
  v_chr <- as.character(vals[ok_str])
  u_chr <- unique(v_chr)
  u_num <- suppressWarnings(as.numeric(u_chr))
  nums <- u_num[fastmatch::fmatch(v_chr, u_chr)]
  fin <- which(!is.na(nums) & is.finite(nums))
  if (length(fin) == 0L) {
    return(out)
  }
  g_ok <- grp_id[ok_str[fin]]
  v_ok <- nums[fin]

  first_pos <- which(!duplicated(g_ok))
  first_g <- g_ok[first_pos]
  first_v <- v_ok[first_pos]
  ref_v <- rep(NA_real_, n_grp)
  ref_v[first_g] <- first_v

  diff_mask <- v_ok != ref_v[g_ok]
  if (any(diff_mask)) {
    conf_grps <- unique(g_ok[diff_mask])
    in_conf <- !is.na(fastmatch::fmatch(g_ok, conf_grps))
    g_c <- g_ok[in_conf]
    v_c <- v_ok[in_conf]
    ord <- order(g_c, v_c, method = "radix")
    g_s <- g_c[ord]
    v_s <- v_c[ord]
    n_s <- length(g_s)
    starts <- which(if (n_s > 1L) c(TRUE, g_s[-1L] != g_s[-n_s]) else TRUE)
    ends <- c(starts[-1L] - 1L, n_s)
    bad_g <- g_s[starts[(v_s[ends] - v_s[starts]) > tolerance]]
    if (length(bad_g) > 0L) {
      keep_first <- is.na(fastmatch::fmatch(first_g, bad_g))
      first_g <- first_g[keep_first]
      first_v <- first_v[keep_first]
    }
  }
  if (length(first_g) > 0L) {
    u_fv <- unique(first_v)
    u_fv_chr <- as.character(u_fv)
    out[first_g] <- u_fv_chr[fastmatch::fmatch(first_v, u_fv)]
  }
  out
}

.build_name_lookup_from_stereo <- function(stereo) {
  valid <- !is.na(stereo$structure_inchikey_no_stereo)
  if (!any(valid)) {
    return(tidytable::tidytable(
      candidate_structure_inchikey_no_stereo = character(0),
      .lk_name = character(0),
      .lk_tag = character(0),
      .lk_xlogp = character(0)
    ))
  }

  keys <- stereo$structure_inchikey_no_stereo[valid]
  names_vec <- stereo$structure_name[valid]
  tags_vec <- stereo$structure_tag[valid]
  xlogp_vec <- stereo$structure_xlogp[valid]

  dup_keys <- unique(keys[duplicated(keys)])
  is_multi <- if (length(dup_keys) > 0L) {
    fastmatch::fmatch(keys, dup_keys, nomatch = 0L) > 0L
  } else {
    logical(length(keys))
  }

  s_idx <- !is_multi
  s_name <- .trim_non_empty_fast(names_vec[s_idx])
  s_tag <- .trim_non_empty_fast(tags_vec[s_idx])
  s_xlogp <- .trim_non_empty_fast(xlogp_vec[s_idx])

  singletons_nam <- tidytable::tidytable(
    candidate_structure_inchikey_no_stereo = keys[s_idx],
    .lk_name = s_name,
    .lk_tag = s_tag,
    .lk_xlogp = s_xlogp
  )

  if (any(is_multi)) {
    m_keys <- keys[is_multi]
    u_m_keys <- unique(m_keys)
    n_m <- length(u_m_keys)
    grp_id <- fastmatch::fmatch(m_keys, u_m_keys)

    multi_nam <- tidytable::tidytable(
      candidate_structure_inchikey_no_stereo = u_m_keys,
      .lk_name = .collapse_delimited_lookup(
        grp_id = grp_id,
        n_grp = n_m,
        vals = names_vec[is_multi],
        ignore_case = TRUE
      ),
      .lk_tag = .collapse_delimited_lookup(
        grp_id = grp_id,
        n_grp = n_m,
        vals = tags_vec[is_multi],
        ignore_case = FALSE
      ),
      .lk_xlogp = .resolve_numeric_lookup(
        grp_id = grp_id,
        n_grp = n_m,
        vals = xlogp_vec[is_multi]
      )
    )
    tidytable::bind_rows(singletons_nam, multi_nam)
  } else {
    singletons_nam
  }
}

.normalize_enrichment_text <- function(x) {
  if (isTRUE(attr(x, "normalized"))) {
    return(x)
  }
  n <- length(x)
  if (n == 0L) {
    return(character(0))
  }
  if (n > 1024L) {
    ux <- unique(x)
    if (length(ux) * 2L < n) {
      unorm <- .normalize_enrichment_text(ux)
      if (is.character(x) && identical(unorm, ux)) {
        return(x)
      }
      return(unorm[fastmatch::fmatch(x, ux)])
    }
  }
  vals <- as.character(x)
  na_or_empty <- is.na(vals) | !nzchar(vals)
  c1 <- substr(vals, 1L, 1L)
  cand <- which(
    !na_or_empty &
      (c1 == " " |
        c1 == "\t" |
        c1 == "\r" |
        c1 == "\n" |
        c1 == "n" |
        c1 == "N" |
        c1 == "e" |
        c1 == "E")
  )
  if (any(na_or_empty)) {
    vals[na_or_empty] <- NA_character_
  }
  if (length(cand) > 0L) {
    trimmed <- trimws(vals[cand])
    low <- tolower(trimmed)
    inv_cand <- !nzchar(trimmed) |
      low %in% c("na", "n/a", "null", "notclassified", "empty")
    if (any(inv_cand)) {
      vals[cand[inv_cand]] <- NA_character_
    }
  }
  vals
}
