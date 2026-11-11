#' @title Split structure-organism pairs table
#'
#' @description Splits a concatenated structure-organism pairs (SOP) table into
#'     separate normalized tables for structures, organisms, and their
#'     relationships. Processes SMILES strings and creates standardized
#'     reference tables.
#'
#' @include columns_utils.R
#' @include process_smiles.R
#' @include logs_utils.R
#' @include validations_utils.R
#' @include validations_params.R
#'
#' @param table [data.frame] Data frame containing combined structure-organism
#'     pair data with
#'     columns for structures (SMILES, InChI, names), organisms, and references
#' @param cache [character] Path to cache file for previously processed SMILES,
#'     or NULL to
#'     skip caching
#'
#' @return List of normalized data frames:
#'   \item{key}{Structure-organism pairs with reference DOIs}
#'   \item{org_tax_ott}{Organism taxonomy information}
#'   \item{str_can}{Canonicalization mapping (smiles_initial -> smiles)}
#'   \item{str_stereo}{Structure stereochemistry with name, tag, xlogp}
#'   \item{str_met}{Stereo-invariant physical properties (formula, mass)}
#'   \item{str_tax_cla}{ClassyFire taxonomy keyed by inchikey}
#'   \item{str_tax_npc}{NPClassifier taxonomy keyed by smiles (with stereo)}
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # Split concatenated library
#' result <- split_tables_sop(
#'   table = combined_sop_data,
#'   cache = "data/processed_smiles.tsv"
#' )
#'
#' # Access individual tables
#' structures <- result$str_stereo
#' organisms <- result$org_tax_ott
#' }
split_tables_sop <- function(table, cache) {
  validate_dataframe(table, param_name = "table")

  # Early exit for empty input
  if (nrow(table) == 0L) {
    log_warn("Empty table provided")
    return(create_empty_sop_tables())
  }

  log_info("Splitting SOP library into standardized components")
  log_debug("Input: %d rows", nrow(table))

  smi_init <- table$structure_smiles
  na_smi <- is.na(smi_init)
  if (any(na_smi) && "structure_smiles_no_stereo" %in% names(table)) {
    smi_init[na_smi] <- table$structure_smiles_no_stereo[na_smi]
  }
  table <- tidytable::as_tidytable(as.list(table))
  data.table::set(table, j = "structure_smiles_initial", value = smi_init)
  rm(smi_init, na_smi)

  org_cols <- grep("organism", names(table), fixed = TRUE, value = TRUE)
  keep_org <- which(!is.na(table$organism_name))
  table_organisms <- tidytable::distinct(
    tidytable::as_tidytable(as.list(table)[org_cols])[keep_org, ]
  )
  rm(org_cols, keep_org)

  log_debug("Joining structural data with computed properties")
  struct_all_cols <- grep("structure", names(table), fixed = TRUE, value = TRUE)
  keep_struct_cols <- unique(c(
    "structure_smiles_initial",
    intersect("structure_name", struct_all_cols),
    grep("_tag|_tax", struct_all_cols, value = TRUE)
  ))
  table_structural_initial <- tidytable::distinct(
    tidytable::as_tidytable(as.list(table)[struct_all_cols])
  )
  table_structural_standardized <- table_structural_initial |>
    process_smiles(cache = cache) |>
    tidytable::distinct(structure_smiles_initial, .keep_all = TRUE)
  table_structural <- tidytable::distinct(
    tidytable::as_tidytable(as.list(table_structural_initial)[keep_struct_cols])
  )
  rm(table_structural_initial)

  if ("structure_inchikey" %in% names(table_structural_standardized)) {
    ik_std <- table_structural_standardized$structure_inchikey
    valid_ik_std <- !is.na(ik_std) & nchar(ik_std) >= 27L
    ik_ns_std <- rep(NA_character_, length(ik_std))
    if (any(valid_ik_std)) {
      ik_ns_std[valid_ik_std] <- paste0(
        stri_sub(str = ik_std[valid_ik_std], from = 1L, to = 14L),
        "-",
        stri_sub(str = ik_std[valid_ik_std], from = -1L, to = -1L)
      )
    }
    table_structural_standardized$structure_inchikey_no_stereo <- ik_ns_std
  }

  std_cols <- setdiff(
    names(table_structural_standardized),
    "structure_smiles_initial"
  )
  if (length(std_cols) > 0L) {
    s_idx <- fastmatch::fmatch(
      table_structural$structure_smiles_initial,
      table_structural_standardized$structure_smiles_initial
    )
    for (col in std_cols) {
      data.table::set(
        table_structural,
        j = col,
        value = table_structural_standardized[[col]][s_idx]
      )
    }
  }
  table_structural <- tidytable::distinct(table_structural)
  log_debug(
    "table_structural: %s rows x %d cols",
    format_count(nrow(table_structural)),
    ncol(table_structural)
  )

  log_debug("Building structure-organism-reference table")
  t_idx <- fastmatch::fmatch(
    table$structure_smiles_initial,
    table_structural_standardized$structure_smiles_initial
  )
  ik_vec <- table_structural_standardized$structure_inchikey[t_idx]
  sm2d_vec <- table_structural_standardized$structure_smiles_no_stereo[t_idx]
  rm(table_structural_standardized)

  org_vec <- table$organism_name %||% rep(NA_character_, nrow(table))
  doi_vec <- table$reference_doi %||% rep(NA_character_, nrow(table))
  rm(table)

  keep_key <- !is.na(ik_vec) & !is.na(sm2d_vec) & !is.na(org_vec)
  table_keys <- tidytable::tidytable(
    structure_inchikey = ik_vec[keep_key],
    structure_smiles_no_stereo = sm2d_vec[keep_key],
    organism_name = org_vec[keep_key],
    reference_doi = doi_vec[keep_key]
  ) |>
    tidytable::distinct()
  rm(ik_vec, sm2d_vec, org_vec, doi_vec, keep_key)

  na_doi <- is.na(table_keys$reference_doi)
  if (any(na_doi) && nrow(table_keys) > 1L) {
    u_ik_k <- unique(table_keys$structure_inchikey)
    u_org_k <- unique(table_keys$organism_name)
    ik_id_k <- fastmatch::fmatch(table_keys$structure_inchikey, u_ik_k)
    org_id_k <- fastmatch::fmatch(table_keys$organism_name, u_org_k)
    n_org_k <- length(u_org_k)
    io_key <- as.double(ik_id_k - 1L) * n_org_k + org_id_k
    is_dup_io <- duplicated(io_key)
    dup_io <- unique(io_key[is_dup_io])
    if (length(dup_io) > 0L) {
      drop_na <- na_doi & !is.na(fastmatch::fmatch(io_key, dup_io))
      if (any(drop_na)) {
        table_keys <- table_keys[!drop_na, ]
      }
    }
    rm(u_ik_k, u_org_k, ik_id_k, org_id_k, io_key, dup_io)
  }
  rm(na_doi)
  log_with_count("Referenced structure-organism pairs", n = nrow(table_keys))

  # str_can: Canonicalization mapping (smiles_initial -> smiles with stereo)
  keep_can <- which(
    !is.na(table_structural$structure_smiles_initial) &
      !is.na(table_structural$structure_smiles)
  )
  u_can <- keep_can[
    !duplicated(table_structural$structure_smiles_initial[keep_can])
  ]
  table_structures_canonical <- tidytable::tidytable(
    structure_smiles_initial = table_structural$structure_smiles_initial[u_can],
    structure_smiles = table_structural$structure_smiles[u_can]
  )

  # str_stereo: keyed by full inchikey, includes name, tag, xlogp
  keep_st <- which(
    !is.na(table_structural$structure_inchikey) &
      (!is.na(table_structural$structure_smiles) |
        !is.na(table_structural$structure_smiles_no_stereo)) &
      !is.na(table_structural$structure_inchikey_connectivity_layer)
  )
  table_structures_stereo <- table_structural[keep_st, ]

  # Calculate structure statistics in one pass over unique InChIKeys
  u_ik <- unique(table_structures_stereo$structure_inchikey)
  no_stereo_mask <- grepl(INCHI_NO_STEREO_PATTERN, u_ik, fixed = TRUE)
  n_no_stereo <- sum(no_stereo_mask)
  n_stereoisomers <- length(u_ik) - n_no_stereo
  n_constitutional <- length(unique(
    table_structures_stereo$structure_inchikey_connectivity_layer
  ))
  rm(u_ik, no_stereo_mask)

  log_info(
    "Structures: %s stereoisomers, %s without stereochemistry, %s constitutional isomers",
    format_count(n_stereoisomers),
    format_count(n_no_stereo),
    format_count(n_constitutional)
  )

  # Collapse name, tag, xlogp per inchikey
  # xlogp is deterministic from SMILES (computed by process_smiles), so
  # distinct() is sufficient for it. Only name and tag need collapsing
  # across multiple library sources.
  if (!"structure_xlogp" %in% names(table_structures_stereo)) {
    table_structures_stereo$structure_xlogp <- NA_character_
  }
  # Keep one type so the per-group collapse below never mixes double and
  # character results
  table_structures_stereo$structure_xlogp <- as.character(
    table_structures_stereo$structure_xlogp
  )
  if (!"structure_tag" %in% names(table_structures_stereo)) {
    table_structures_stereo$structure_tag <- NA_character_
  }
  if (!"structure_name" %in% names(table_structures_stereo)) {
    table_structures_stereo$structure_name <- NA_character_
  }
  stereo_cols <- c(
    "structure_inchikey",
    "structure_smiles",
    "structure_inchikey_connectivity_layer",
    "structure_inchikey_no_stereo",
    "structure_smiles_no_stereo",
    "structure_xlogp",
    "structure_name",
    "structure_tag"
  )
  table_structures_stereo <- tidytable::distinct(
    tidytable::as_tidytable(as.list(table_structures_stereo)[stereo_cols])
  )
  rm(stereo_cols)

  # Optimization: most inchikeys map to a single row after distinct().
  # Only multi-row groups need collapsing. Split into singletons and
  # duplicates without per-group .by overhead, and collapse vectorially.
  .collapse_delimited_col <- function(
    grp_id,
    n_grp,
    vals,
    ignore_case = FALSE,
    na_default = NA_character_
  ) {
    out <- rep(na_default, n_grp)
    ok <- which(!is.na(vals))
    if (length(ok) == 0L) {
      return(out)
    }
    v_ok <- as.character(vals[ok])
    u_v <- unique(v_ok)
    u_trim <- trimws(u_v)
    v_trim <- u_trim[fastmatch::fmatch(v_ok, u_v)]
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
      for (k in 2L:max_r) {
        idx_k <- which(r_m == k)
        pos_k <- fastmatch::fmatch(g_ord[idx_k], u_g)
        acc[pos_k] <- paste(acc[pos_k], v_ord[idx_k], sep = " $ ")
      }
    }
    out[u_g] <- acc
    out
  }

  ik_st <- table_structures_stereo$structure_inchikey
  dup_ik_st <- unique(ik_st[duplicated(ik_st)])
  if (length(dup_ik_st) > 0L) {
    is_multi_st <- !is.na(fastmatch::fmatch(ik_st, dup_ik_st))
    singletons <- table_structures_stereo[!is_multi_st, ]
    multi <- table_structures_stereo[is_multi_st, ]
    log_debug(
      "Collapsing names/tags for %d multi-source inchikeys (%d rows)",
      length(dup_ik_st),
      nrow(multi)
    )
    u_ik_m <- unique(multi$structure_inchikey)
    n_grp_m <- length(u_ik_m)
    grp_id_m <- fastmatch::fmatch(multi$structure_inchikey, u_ik_m)
    first_idx_m <- which(!duplicated(grp_id_m))

    ok_xl <- which(!is.na(multi$structure_xlogp))
    first_xl <- ok_xl[!duplicated(grp_id_m[ok_xl])]
    xl_out <- rep(NA_character_, n_grp_m)
    xl_out[grp_id_m[first_xl]] <- multi$structure_xlogp[first_xl]

    multi <- tidytable::tidytable(
      structure_inchikey = u_ik_m,
      structure_smiles = multi$structure_smiles[first_idx_m],
      structure_inchikey_connectivity_layer = multi$structure_inchikey_connectivity_layer[
        first_idx_m
      ],
      structure_inchikey_no_stereo = multi$structure_inchikey_no_stereo[
        first_idx_m
      ],
      structure_smiles_no_stereo = multi$structure_smiles_no_stereo[
        first_idx_m
      ],
      structure_xlogp = xl_out,
      structure_name = .collapse_delimited_col(
        grp_id_m,
        n_grp_m,
        multi$structure_name,
        ignore_case = TRUE,
        na_default = NA_character_
      ),
      structure_tag = .collapse_delimited_col(
        grp_id_m,
        n_grp_m,
        multi$structure_tag,
        ignore_case = FALSE,
        na_default = NA_character_
      )
    )
    table_structures_stereo <- tidytable::bind_rows(singletons, multi)
    rm(
      is_multi_st,
      singletons,
      multi,
      u_ik_m,
      grp_id_m,
      first_idx_m,
      ok_xl,
      first_xl,
      xl_out
    )
  }
  rm(ik_st, dup_ik_st)

  # str_met: Stereo-invariant properties keyed by inchikey_no_stereo
  ## formula and exact_mass are deterministically derived from SMILES via
  ## process_smiles() and are stereo-invariant (same connectivity + protonation
  ## = same formula = same mass). No group_by + summarize needed — distinct()
  ## is sufficient since values are deterministic.
  keep_met <- which(
    !is.na(table_structural$structure_inchikey) &
      !is.na(table_structural$structure_molecular_formula) &
      !is.na(table_structural$structure_exact_mass)
  )
  u_met <- keep_met[
    !duplicated(table_structural$structure_inchikey_no_stereo[keep_met])
  ]
  table_structures_metadata <- tidytable::tidytable(
    structure_inchikey_no_stereo = table_structural$structure_inchikey_no_stereo[
      u_met
    ],
    structure_molecular_formula = table_structural$structure_molecular_formula[
      u_met
    ],
    structure_exact_mass = table_structural$structure_exact_mass[u_met]
  )

  # str_tax_npc: keyed by canonical SMILES with stereo
  npc_cols <- c(
    "structure_tax_npc_01pat",
    "structure_tax_npc_02sup",
    "structure_tax_npc_03cla"
  )
  keep_npc <- which(
    !is.na(table_structural$structure_smiles) &
      (!is.na(table_structural$structure_tax_npc_01pat) |
        !is.na(table_structural$structure_tax_npc_02sup) |
        !is.na(table_structural$structure_tax_npc_03cla))
  )
  table_structures_taxonomy_npc <- tidytable::distinct(
    tidytable::as_tidytable(
      as.list(table_structural)[c("structure_smiles", npc_cols)]
    )[keep_npc, ]
  )
  rm(keep_npc)

  # Fast collapse: most groups have 1 row; only collapse multi-row groups
  sm_npc <- table_structures_taxonomy_npc$structure_smiles
  dup_sm_npc <- unique(sm_npc[duplicated(sm_npc)])
  if (length(dup_sm_npc) > 0L) {
    is_multi_npc <- !is.na(fastmatch::fmatch(sm_npc, dup_sm_npc))
    npc_single <- table_structures_taxonomy_npc[!is_multi_npc, ]
    npc_multi <- table_structures_taxonomy_npc[is_multi_npc, ]
  } else {
    npc_single <- table_structures_taxonomy_npc
    npc_multi <- table_structures_taxonomy_npc[0L, ]
  }
  for (col in npc_cols) {
    v_col <- as.character(npc_single[[col]])
    v_col[is.na(v_col)] <- "notClassified"
    data.table::set(npc_single, j = col, value = v_col)
  }
  if (nrow(npc_multi) > 0L) {
    u_sm_m <- unique(npc_multi$structure_smiles)
    n_npc_m <- length(u_sm_m)
    grp_npc_m <- fastmatch::fmatch(npc_multi$structure_smiles, u_sm_m)
    npc_multi_list <- list(structure_smiles = u_sm_m)
    for (col in npc_cols) {
      npc_multi_list[[col]] <- .collapse_delimited_col(
        grp_npc_m,
        n_npc_m,
        npc_multi[[col]],
        ignore_case = FALSE,
        na_default = "notClassified"
      )
    }
    npc_multi <- tidytable::as_tidytable(npc_multi_list)
    table_structures_taxonomy_npc <- tidytable::bind_rows(
      npc_single,
      npc_multi
    )
    rm(u_sm_m, grp_npc_m, npc_multi_list)
  } else {
    table_structures_taxonomy_npc <- npc_single
  }
  rm(sm_npc, dup_sm_npc, npc_single, npc_multi)

  # str_tax_cla: keyed by full inchikey
  cla_cols <- c(
    "structure_tax_cla_chemontid",
    "structure_tax_cla_01kin",
    "structure_tax_cla_02sup",
    "structure_tax_cla_03cla",
    "structure_tax_cla_04dirpar"
  )
  keep_cla <- which(
    !is.na(table_structural$structure_inchikey) &
      !is.na(table_structural$structure_tax_cla_chemontid)
  )
  cla_sub <- tidytable::as_tidytable(
    as.list(table_structural)[c("structure_inchikey", cla_cols)]
  )[keep_cla, ]
  data.table::set(
    cla_sub,
    j = "structure_tax_cla_chemontid",
    value = normalize_chemontid(cla_sub$structure_tax_cla_chemontid)
  )
  table_structures_taxonomy_cla <- tidytable::distinct(cla_sub)
  rm(keep_cla, cla_sub)

  # Fast collapse: most groups have 1 row; only collapse multi-row groups
  ik_cla <- table_structures_taxonomy_cla$structure_inchikey
  dup_ik_cla <- unique(ik_cla[duplicated(ik_cla)])
  if (length(dup_ik_cla) > 0L) {
    is_multi_cla <- !is.na(fastmatch::fmatch(ik_cla, dup_ik_cla))
    cla_single <- table_structures_taxonomy_cla[!is_multi_cla, ]
    cla_multi <- table_structures_taxonomy_cla[is_multi_cla, ]
  } else {
    cla_single <- table_structures_taxonomy_cla
    cla_multi <- table_structures_taxonomy_cla[0L, ]
  }
  for (col in cla_cols) {
    v_col <- as.character(cla_single[[col]])
    v_col[is.na(v_col)] <- "notClassified"
    data.table::set(cla_single, j = col, value = v_col)
  }
  if (nrow(cla_multi) > 0L) {
    u_ik_c <- unique(cla_multi$structure_inchikey)
    n_cla_m <- length(u_ik_c)
    grp_cla_m <- fastmatch::fmatch(cla_multi$structure_inchikey, u_ik_c)
    cla_multi_list <- list(structure_inchikey = u_ik_c)
    for (col in cla_cols) {
      cla_multi_list[[col]] <- .collapse_delimited_col(
        grp_cla_m,
        n_cla_m,
        cla_multi[[col]],
        ignore_case = FALSE,
        na_default = "notClassified"
      )
    }
    cla_multi <- tidytable::as_tidytable(cla_multi_list)
    table_structures_taxonomy_cla <- tidytable::bind_rows(
      cla_single,
      cla_multi
    )
    rm(u_ik_c, grp_cla_m, cla_multi_list)
  } else {
    table_structures_taxonomy_cla <- cla_single
  }
  rm(ik_cla, dup_ik_cla, cla_single, cla_multi)

  log_with_count(
    "Unique organisms",
    n = length(unique(table_organisms$organism_name))
  )

  keep_ott <- which(!is.na(table_organisms$organism_taxonomy_ottid))
  table_org_tax_ott <- table_organisms[keep_ott, ]
  for (col in names(table_org_tax_ott)) {
    v_col <- table_org_tax_ott[[col]]
    if (is.character(v_col) && anyNA(v_col)) {
      v_col[is.na(v_col)] <- "notClassified"
      data.table::set(table_org_tax_ott, j = col, value = v_col)
    }
  }
  rm(table_organisms)

  tables <-
    list(
      "key" = table_keys,
      # "org_nam" = table_organisms_names,
      "org_tax_ott" = table_org_tax_ott,
      "str_can" = table_structures_canonical,
      "str_stereo" = table_structures_stereo,
      "str_met" = table_structures_metadata,
      "str_tax_cla" = table_structures_taxonomy_cla,
      "str_tax_npc" = table_structures_taxonomy_npc
    )

  rm(
    table_keys,
    # table_organisms_names,
    table_org_tax_ott,
    table_structures_canonical,
    table_structures_stereo,
    table_structures_metadata,
    table_structures_taxonomy_cla,
    table_structures_taxonomy_npc
  )
  tables
}


#' Create empty SOP tables structure
#' @keywords internal
create_empty_sop_tables <- function() {
  list(
    "key" = tidytable::tidytable(),
    "org_tax_ott" = tidytable::tidytable(),
    "str_can" = tidytable::tidytable(),
    "str_stereo" = tidytable::tidytable(),
    "str_met" = tidytable::tidytable(),
    "str_tax_cla" = tidytable::tidytable(),
    "str_tax_npc" = tidytable::tidytable()
  )
}
