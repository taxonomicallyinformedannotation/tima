#' Convert numeric vector to character via unique values
#' @noRd
.fast_num_to_char <- function(x) {
  if (!is.numeric(x) || length(x) == 0L) {
    return(as.character(x))
  }
  u <- unique(x)
  u_chr <- as.character(u)
  m <- if (anyNA(u)) match(x, u) else fastmatch::fmatch(x, u)
  u_chr[m]
}

#' Decorate annotation rows with structural / taxonomic metadata
#' @keywords internal
enrich_with_structure_metadata <- function(combined, structure_table) {
  if (nrow(combined) == 0L) {
    return(
      tidytable::tidytable(
        feature_id = character(),
        candidate_structure_error_mz = numeric(),
        candidate_structure_name = character(),
        candidate_structure_inchikey_connectivity_layer = character(),
        candidate_structure_smiles_no_stereo = character(),
        candidate_library = character(),
        candidate_adduct = character()
      )
    )
  }
  struct_cols <- c(
    "structure_name",
    "structure_inchikey_connectivity_layer",
    "structure_inchikey_no_stereo",
    "structure_smiles_no_stereo",
    "structure_molecular_formula",
    "structure_exact_mass",
    "structure_xlogp"
  )
  tax_cols <- c(
    "structure_inchikey_connectivity_layer",
    "structure_smiles_no_stereo",
    "structure_tax_npc_01pat",
    "structure_tax_npc_02sup",
    "structure_tax_npc_03cla",
    "structure_tax_cla_chemontid",
    "structure_tax_cla_01kin",
    "structure_tax_cla_02sup",
    "structure_tax_cla_03cla",
    "structure_tax_cla_04dirpar"
  )

  comb_em_chr <- if ("structure_exact_mass" %in% colnames(combined)) {
    .fast_num_to_char(combined$structure_exact_mass)
  } else {
    NULL
  }

  # Pre-filter structure_table to exact masses present in combined when
  # structure_exact_mass is the sole join key from combined
  shared_struct_tax <- intersect(
    colnames(combined),
    intersect(c(struct_cols, tax_cols), colnames(structure_table))
  )
  if (
    identical(shared_struct_tax, "structure_exact_mass") &&
      nrow(structure_table) > 0L
  ) {
    u_comb_em <- unique(comb_em_chr[!is.na(comb_em_chr)])
    st_em_chr <- .fast_num_to_char(structure_table$structure_exact_mass)
    keep_st <- !is.na(fastmatch::fmatch(st_em_chr, u_comb_em))
    structure_table <- structure_table[which(keep_st), , drop = FALSE]
    if (nrow(structure_table) > 0L) {
      structure_table$structure_exact_mass <- st_em_chr[keep_st]
    } else {
      structure_table$structure_exact_mass <- character(0L)
    }
  }

  has_tax_cols <- any(tax_cols %in% colnames(structure_table))
  avail_struct_cols <- intersect(struct_cols, colnames(structure_table))
  avail_tax_cols <- intersect(tax_cols, colnames(structure_table))
  tax_keys_early <- intersect(avail_struct_cols, avail_tax_cols)
  tax_keys_in_comb <- intersect(colnames(combined), avail_tax_cols)
  can_join_tax_early <- has_tax_cols &&
    length(tax_keys_early) > 0L &&
    length(setdiff(tax_keys_in_comb, avail_struct_cols)) == 0L

  # Fast path when each (inchikey, smiles) in filtered structure_table is unique:
  # include tax_cols directly when deduplicating struct_unique instead of
  # splitting and re-joining taxonomy_table.
  if (
    can_join_tax_early &&
      nrow(structure_table) > 0L &&
      anyDuplicated(
        tidytable::as_tidytable(as.list(structure_table)[tax_keys_early])
      ) ==
        0L
  ) {
    all_st_cols <- union(avail_struct_cols, avail_tax_cols)
    struct_unique <- tidytable::as_tidytable(
      as.list(structure_table)[all_st_cols]
    ) |>
      tidytable::distinct(
        tidyselect::any_of(c(
          "structure_inchikey_connectivity_layer",
          "structure_molecular_formula",
          "structure_exact_mass"
        )),
        .keep_all = TRUE
      )
    for (cc in avail_struct_cols) {
      if (is.numeric(struct_unique[[cc]])) {
        struct_unique[[cc]] <- .fast_num_to_char(struct_unique[[cc]])
      }
    }
    taxonomy_table <- NULL
    tax_joined_early <- TRUE
  } else {
    struct_unique <- structure_table |>
      tidytable::distinct(tidyselect::any_of(struct_cols)) |>
      tidytable::distinct(
        tidyselect::any_of(c(
          "structure_inchikey_connectivity_layer",
          "structure_molecular_formula",
          "structure_exact_mass"
        )),
        .keep_all = TRUE
      )
    for (cc in colnames(struct_unique)) {
      if (is.numeric(struct_unique[[cc]])) {
        struct_unique[[cc]] <- .fast_num_to_char(struct_unique[[cc]])
      }
    }

    taxonomy_table <- if (has_tax_cols) {
      structure_table |>
        tidytable::distinct(tidyselect::any_of(tax_cols))
    } else {
      NULL
    }

    tax_joined_early <- FALSE
  }

  enriched <- tidytable::as_tidytable(combined)
  if (!is.null(comb_em_chr)) {
    enriched$structure_exact_mass <- comb_em_chr
  }
  enriched$candidate_library <- rep("TIMA MS1", nrow(enriched))
  enriched$candidate_structure_error_mz <- as.numeric(enriched$error_mz)
  enriched <- enriched |> tidytable::distinct()

  join_keys <- intersect(colnames(enriched), colnames(struct_unique))
  if (length(join_keys) > 0L) {
    enriched <- enriched |>
      tidytable::left_join(struct_unique, by = join_keys)
  }

  if (!is.null(taxonomy_table) && !tax_joined_early) {
    tax_keys <- intersect(colnames(enriched), colnames(taxonomy_table))
    if (length(tax_keys) > 0L) {
      enriched <- enriched |>
        tidytable::left_join(taxonomy_table, by = tax_keys)
    }
  }

  enriched <- enriched |>
    tidytable::rename(
      candidate_structure_name = tidyselect::any_of("structure_name"),
      candidate_structure_inchikey_connectivity_layer = tidyselect::any_of(
        "structure_inchikey_connectivity_layer"
      ),
      candidate_structure_inchikey_no_stereo = tidyselect::any_of(
        "structure_inchikey_no_stereo"
      ),
      candidate_structure_smiles_no_stereo = tidyselect::any_of(
        "structure_smiles_no_stereo"
      ),
      candidate_structure_molecular_formula = tidyselect::any_of(
        "structure_molecular_formula"
      ),
      candidate_structure_xlogp = tidyselect::any_of("structure_xlogp"),
      candidate_structure_tax_npc_01pat = tidyselect::any_of(
        "structure_tax_npc_01pat"
      ),
      candidate_structure_tax_npc_02sup = tidyselect::any_of(
        "structure_tax_npc_02sup"
      ),
      candidate_structure_tax_npc_03cla = tidyselect::any_of(
        "structure_tax_npc_03cla"
      ),
      candidate_structure_tax_cla_chemontid = tidyselect::any_of(
        "structure_tax_cla_chemontid"
      ),
      candidate_structure_tax_cla_01kin = tidyselect::any_of(
        "structure_tax_cla_01kin"
      ),
      candidate_structure_tax_cla_02sup = tidyselect::any_of(
        "structure_tax_cla_02sup"
      ),
      candidate_structure_tax_cla_03cla = tidyselect::any_of(
        "structure_tax_cla_03cla"
      ),
      candidate_structure_tax_cla_04dirpar = tidyselect::any_of(
        "structure_tax_cla_04dirpar"
      )
    )
  if (!tax_joined_early) {
    enriched <- enriched |> tidytable::distinct()
  }
  enriched
}

#' Extract per-element atom requirements implied by a loss formula token
#'
#' Parses loss tokens (e.g. `H2O`, `2C6H10O5`, `C6H10O5 (hexose)`) and returns
#' a named integer vector with required atom counts.
#' @keywords internal
loss_term_atom_requirements <- function(loss_term) {
  if (is.null(loss_term) || is.na(loss_term) || !nzchar(loss_term)) {
    return(integer())
  }
  loss_term <- gsub("\n", " ", as.character(loss_term))
  loss_term <- trimws(sub(" .*", "", loss_term))
  coef_match <- regmatches(
    loss_term,
    regexpr("^[0-9]+", loss_term, perl = TRUE)
  )
  coef <- if (length(coef_match) == 0L || !nzchar(coef_match)) {
    1L
  } else {
    as.integer(coef_match)
  }
  formula <- sub("^[0-9]+", "", loss_term, perl = TRUE)
  parsed <- tryCatch(
    parse_atomic_formula(formula),
    error = function(.err) {
      invisible(.err)
      NULL
    }
  )
  if (is.null(parsed) || length(parsed) == 0L) {
    return(integer())
  }
  stats::setNames(as.integer(parsed * coef), names(parsed))
}

#' Backwards-compatible alias for older tests / callers
#' @keywords internal
adduct_loss_atom_requirements <- function(adduct) {
  if (is.null(adduct) || is.na(adduct) || !nzchar(adduct)) {
    return(integer())
  }
  adduct_norm <- tryCatch(
    normalize_adduct_string(adduct),
    error = function(.err) {
      invisible(.err)
      as.character(adduct)
    }
  )
  inner <- sub("^\\[(.*)\\][0-9]*[+-]+$", "\\1", adduct_norm, perl = TRUE)
  if (!nzchar(inner) || identical(inner, adduct_norm)) {
    return(loss_term_atom_requirements(adduct_norm))
  }
  toks <- regmatches(
    inner,
    gregexpr("[+-][0-9]*[A-Za-z][A-Za-z0-9]*", inner, perl = TRUE)
  )[[1L]]
  toks <- toks[nzchar(toks)]
  if (length(toks) == 0L) {
    return(integer())
  }
  neg_toks <- substring(toks[startsWith(toks, "-")], 2L)
  all_reqs <- unlist(lapply(neg_toks, loss_term_atom_requirements))
  if (length(all_reqs) == 0L) {
    return(integer())
  }
  els <- names(all_reqs)
  u_els <- unique(els)
  vapply(
    split(unname(all_reqs), factor(els, levels = u_els)),
    sum,
    integer(1L)
  )
}

#' Check whether formula contains all atoms required by adduct losses
#' @keywords internal
formula_satisfies_loss_requirements <- function(formula, loss_requirements) {
  if (length(loss_requirements) == 0L) {
    return(TRUE)
  }
  if (is.null(formula) || is.na(formula) || !nzchar(formula)) {
    return(NA)
  }
  parsed <- tryCatch(
    parse_atomic_formula(formula),
    error = function(.err) {
      invisible(.err)
      NULL
    }
  )
  if (is.null(parsed) || length(parsed) == 0L) {
    return(NA)
  }
  m_el <- match(names(loss_requirements), names(parsed), nomatch = 0L)
  !any(m_el == 0L) && all(parsed[m_el] >= loss_requirements)
}

#' Demote structural matches whose adduct-loss atoms are absent from formula
#'
#' Rows that fail this check keep the adduct hypothesis but lose structure-level
#' fields (treated as unmatched).
#' @keywords internal
enforce_loss_formula_compatibility <- function(annotations) {
  if (nrow(annotations) == 0L) {
    return(annotations)
  }
  required_cols <- c(
    "adduct",
    "candidate_structure_molecular_formula",
    "candidate_structure_error_mz"
  )
  if (!all(required_cols %in% colnames(annotations))) {
    return(annotations)
  }

  has_source <- "source" %in% colnames(annotations)
  has_loss_term <- "loss_term" %in% colnames(annotations)

  # Pre-evaluate loss requirements for all unique adducts and loss_terms (<100 unique strings)
  u_add <- unique(annotations$adduct[!is.na(annotations$adduct)])
  add_req_list <- lapply(u_add, adduct_loss_atom_requirements)
  add_has_req <- lengths(add_req_list) > 0L

  u_lt <- if (has_loss_term) {
    unique(annotations$loss_term[!is.na(annotations$loss_term)])
  } else {
    character(0L)
  }
  lt_req_list <- lapply(u_lt, loss_term_atom_requirements)
  lt_has_req <- lengths(lt_req_list) > 0L

  if (!any(add_has_req) && !any(lt_has_req)) {
    return(annotations)
  }

  has_lt_row <- if (has_loss_term) {
    !is.na(annotations$loss_term)
  } else {
    logical(nrow(annotations))
  }
  is_loss_src <- if (has_source) {
    !is.na(annotations$source) & annotations$source == "loss"
  } else {
    logical(nrow(annotations))
  }
  idx_check <- which(
    !is.na(annotations$candidate_structure_error_mz) &
      !is.na(annotations$adduct) &
      (is_loss_src | has_lt_row)
  )
  if (length(idx_check) == 0L) {
    return(annotations)
  }

  use_loss_term <- if (has_loss_term) {
    !is.na(annotations$loss_term[idx_check])
  } else {
    logical(length(idx_check))
  }

  # Map each row in idx_check to an integer requirement index in combined req_pool
  # 1..length(u_lt) = loss_term requirements; length(u_lt) + 1..length(u_add) = adduct requirements
  req_pool <- c(lt_req_list, add_req_list)
  req_pool_nonempty <- c(lt_has_req, add_has_req)
  n_lt <- length(u_lt)

  req_id <- integer(length(idx_check))
  if (any(use_loss_term)) {
    w_lt <- which(use_loss_term)
    req_id[w_lt] <- fastmatch::fmatch(
      annotations$loss_term[idx_check[w_lt]],
      u_lt
    )
  }
  if (!all(use_loss_term)) {
    w_ad <- which(!use_loss_term)
    req_id[w_ad] <- n_lt +
      fastmatch::fmatch(annotations$adduct[idx_check[w_ad]], u_add)
  }

  # Filter idx_check to rows whose requirement is actually non-empty
  active_mask <- req_pool_nonempty[req_id]
  if (!any(active_mask)) {
    return(annotations)
  }
  idx_check <- idx_check[active_mask]
  req_id <- req_id[active_mask]

  formulas_check <- annotations$candidate_structure_molecular_formula[idx_check]
  u_fm <- unique(formulas_check)
  n_fm <- max(1L, length(u_fm))
  fm_id <- if (anyNA(u_fm)) {
    match(formulas_check, u_fm)
  } else {
    fastmatch::fmatch(formulas_check, u_fm)
  }

  pair_code <- (req_id - 1L) * n_fm + fm_id
  u_pair_idx <- which(!duplicated(pair_code))
  u_pair_codes <- pair_code[u_pair_idx]
  u_keep <- vapply(
    X = u_pair_idx,
    FUN = function(j) {
      req <- req_pool[[req_id[[j]]]]
      fs <- formula_satisfies_loss_requirements(
        formula = formulas_check[[j]],
        loss_requirements = req
      )
      if (is.na(fs)) TRUE else fs
    },
    FUN.VALUE = logical(1L)
  )
  keep_struct <- u_keep[fastmatch::fmatch(pair_code, u_pair_codes)]
  idx_drop <- idx_check[!keep_struct]
  if (length(idx_drop) == 0L) {
    return(annotations)
  }

  struct_cols <- unique(c(
    grep("^candidate_structure_", colnames(annotations), value = TRUE),
    intersect(
      colnames(annotations),
      c("candidate_library", "structure_exact_mass", "error_mz")
    )
  ))

  out_list <- as.list(annotations)
  for (cc in struct_cols) {
    out_list[[cc]][idx_drop] <- NA
  }
  out <- tidytable::distinct(tidytable::as_tidytable(out_list))

  log_info(
    paste0(
      "Loss/formula filter demoted %d structural match row(s) where the actual ",
      "loss-edge term was not contained in the candidate formula."
    ),
    length(idx_drop)
  )
  out
}

#' Keep only annotation adducts that agree with edge evidence
#'
#' Agreement is evaluated per feature. For pair-derived adducts,
#' `(feature_id, adduct)` must be present in the adduct-edge endpoint set.
#' Loss/cluster-derived hypotheses are considered edge-backed by construction
#' (their `source` originates from those edge tables). Explicit user-guided
#' hypotheses are also preserved (`baseline`, `preassigned`,
#' `preassigned_propagated`).
#' @keywords internal
enforce_annotation_edge_adduct_agreement <- function(
  annotations,
  adduct_edges,
  baseline_adduct = NULL
) {
  n_in <- nrow(annotations)
  if (n_in == 0L) {
    return(annotations)
  }
  required <- c("feature_id", "adduct")
  if (!all(required %in% colnames(annotations))) {
    return(annotations)
  }

  adduct_key_map <- build_adduct_state_key_map(unique(c(
    annotations$adduct,
    adduct_edges$adduct,
    adduct_edges$adduct_dest
  )))
  sm_add <- as.character(adduct_key_map$adduct)
  sm_key <- as.character(adduct_key_map$state_key)

  edge_supported_vec <- logical(n_in)
  feature_has_edge_support <- logical(n_in)

  if (nrow(adduct_edges) > 0L) {
    e_f <- c(adduct_edges$feature_id, adduct_edges$feature_id_dest)
    e_a <- c(adduct_edges$adduct, adduct_edges$adduct_dest)
    e_k <- sm_key[fastmatch::fmatch(e_a, sm_add)]
    valid_e <- !is.na(e_k)
    if (any(valid_e)) {
      e_f <- e_f[valid_e]
      e_k <- e_k[valid_e]
      supp_feats <- unique(e_f)
      feature_has_edge_support <- !is.na(fastmatch::fmatch(
        annotations$feature_id,
        supp_feats
      ))
      if (any(feature_has_edge_support)) {
        idx_ef <- which(feature_has_edge_support)
        row_sk_ef <- sm_key[fastmatch::fmatch(
          annotations$adduct[idx_ef],
          sm_add
        )]
        u_sk <- unique(c(e_k, row_sk_ef[!is.na(row_sk_ef)]))
        n_sk <- max(1L, length(u_sk))
        ef_id <- fastmatch::fmatch(e_f, supp_feats)
        ek_id <- fastmatch::fmatch(e_k, u_sk)
        e_pair_int <- unique((ef_id - 1L) * n_sk + ek_id)

        rf_id <- fastmatch::fmatch(annotations$feature_id[idx_ef], supp_feats)
        rk_id <- fastmatch::fmatch(row_sk_ef, u_sk)
        r_pair_int <- (rf_id - 1L) * n_sk + rk_id
        edge_supported_vec[idx_ef] <- !is.na(rk_id) &
          !is.na(fastmatch::fmatch(r_pair_int, e_pair_int))
        # Recompute feature_has_edge_support strictly from rows that matched edge_supported
        # in case a feature in supp_feats had zero matching annotation rows
        supp_feats_in_ann <- unique(annotations$feature_id[edge_supported_vec])
        feature_has_edge_support <- !is.na(fastmatch::fmatch(
          annotations$feature_id,
          supp_feats_in_ann
        ))
      }
    }
  }

  has_source <- "source" %in% colnames(annotations)
  src_col <- if (has_source) as.character(annotations$source) else NULL
  has_is_preassigned <- "is_preassigned" %in% colnames(annotations)
  has_origin <- "candidate_adduct_origin" %in% colnames(annotations)
  preassigned_safe <- if (has_is_preassigned) {
    p_tmp <- as.logical(annotations$is_preassigned)
    p_tmp[is.na(p_tmp)] <- FALSE
    p_tmp
  } else {
    logical(n_in)
  }
  origin_safe <- if (has_origin) {
    !is.na(annotations$candidate_adduct_origin) &
      annotations$candidate_adduct_origin %in% c("enforced", "preassigned")
  } else {
    logical(n_in)
  }
  baseline_safe <- if (!is.null(baseline_adduct)) {
    !is.na(annotations$adduct) & annotations$adduct == baseline_adduct
  } else {
    logical(n_in)
  }

  is_multi_src <- if (has_source) {
    !is.na(src_col) & src_col == "multi"
  } else {
    logical(n_in)
  }
  is_loss_cluster_src <- if (has_source) {
    !is.na(src_col) & src_col %in% c("loss", "cluster")
  } else {
    logical(n_in)
  }
  is_guided_src <- if (has_source) {
    !is.na(src_col) &
      src_col %in% c("baseline", "preassigned", "preassigned_propagated")
  } else {
    logical(n_in)
  }

  keep <- edge_supported_vec |
    (!feature_has_edge_support & !is_multi_src) |
    is_loss_cluster_src |
    preassigned_safe |
    origin_safe |
    baseline_safe |
    is_guided_src

  if (!all(keep)) {
    drop_idx <- which(!keep)
    f_dr <- annotations$feature_id[drop_idx]
    a_dr <- annotations$adduct[drop_idx]
    u_fd <- unique(f_dr)
    u_ad <- unique(a_dr)
    n_ad <- max(1L, length(u_ad))
    f_i <- match(f_dr, u_fd)
    a_i <- match(a_dr, u_ad)
    dropped_n <- sum(!duplicated(as.double(f_i - 1L) * n_ad + a_i))
    log_info(
      paste0(
        "Annotation/edge adduct agreement removed %d unsupported ",
        "(feature, adduct) assignment(s)."
      ),
      dropped_n
    )
    keep_idx <- which(keep)
    return(tidytable::as_tidytable(lapply(
      as.list(annotations),
      `[`,
      keep_idx
    )))
  }

  tidytable::as_tidytable(annotations)
}

#' Build the output edges table from the three edge sets
#' @keywords internal
build_output_edges <- function(
  adduct_edges,
  loss_edges,
  cluster_edges,
  covariance_edges = NULL,
  features_table,
  name_source,
  name_target
) {
  parts <- list()
  if (nrow(adduct_edges) > 0L) {
    parts[[length(parts) + 1L]] <- adduct_edges |>
      tidytable::mutate(
        label = paste0(adduct, " _ ", adduct_dest),
        correlation = NA_real_,
        p_value = NA_real_
      ) |>
      tidytable::select(
        !!as.name(name_source) := feature_id,
        !!as.name(name_target) := feature_id_dest,
        label,
        correlation,
        p_value
      ) |>
      tidytable::distinct()
  }
  if (
    nrow(loss_edges) > 0L &&
      nrow(adduct_edges) > 0L &&
      all(c("adduct", "adduct_dest") %in% colnames(adduct_edges))
  ) {
    ae_pairs <- paste(
      adduct_edges$feature_id,
      adduct_edges$feature_id_dest,
      sep = "\r"
    )
    le_pairs <- paste(
      loss_edges$feature_id,
      loss_edges$feature_id_dest,
      sep = "\r"
    )
    hit_le <- which(!is.na(fastmatch::fmatch(le_pairs, ae_pairs)))
    if (length(hit_le) > 0L) {
      ae_by_pair <- split(seq_len(nrow(adduct_edges)), ae_pairs)
      ae_add <- adduct_edges$adduct
      ae_add_dst <- adduct_edges$adduct_dest
      drop_redundant <- logical(nrow(loss_edges))
      drop_redundant[hit_le] <- vapply(
        hit_le,
        function(i) {
          j_vec <- ae_by_pair[[le_pairs[[i]]]]
          loss_formula <- loss_edges$loss[[i]]
          any(vapply(
            j_vec,
            function(j) {
              adduct_has_explicit_loss(ae_add[[j]], loss_formula) &&
                adduct_has_explicit_loss(ae_add_dst[[j]], loss_formula)
            },
            logical(1L)
          ))
        },
        logical(1L)
      )
      if (any(drop_redundant)) {
        loss_edges <- loss_edges[!drop_redundant, ]
      }
    }
  }
  if (nrow(loss_edges) > 0L) {
    parts[[length(parts) + 1L]] <- loss_edges |>
      tidytable::mutate(
        label = paste0(loss, " loss"),
        correlation = NA_real_,
        p_value = NA_real_
      ) |>
      tidytable::select(
        # NL convention: precursor (higher mz, feature_id_dest) -> product
        !!as.name(name_source) := feature_id_dest,
        !!as.name(name_target) := feature_id,
        label,
        correlation,
        p_value
      ) |>
      tidytable::distinct()
  }
  if (nrow(cluster_edges) > 0L) {
    parts[[length(parts) + 1L]] <- cluster_edges |>
      tidytable::mutate(
        label = paste0("+", cluster, " cluster"),
        correlation = NA_real_,
        p_value = NA_real_
      ) |>
      tidytable::select(
        # cluster: lower mz -> higher mz (which carries the cluster)
        !!as.name(name_source) := feature_id,
        !!as.name(name_target) := feature_id_dest,
        label,
        correlation,
        p_value
      ) |>
      tidytable::distinct()
  }

  if (!is.null(covariance_edges) && nrow(covariance_edges) > 0L) {
    parts[[length(parts) + 1L]] <- covariance_edges |>
      tidytable::mutate(label = "covariance") |>
      tidytable::select(
        !!as.name(name_source) := feature_id,
        !!as.name(name_target) := feature_id_dest,
        label,
        correlation,
        p_value
      ) |>
      tidytable::distinct()
  }

  edges <- if (length(parts) > 0L) {
    tidytable::bind_rows(parts)
  } else {
    tidytable::tidytable(
      !!as.name(name_source) := character(),
      !!as.name(name_target) := character(),
      label = character(),
      correlation = numeric(),
      p_value = numeric()
    )
  }

  # Self-loops for isolated features so downstream graph processing keeps them
  isolated <- features_table |>
    tidytable::filter(
      !feature_id %in% edges[[name_source]] &
        !feature_id %in% edges[[name_target]]
    ) |>
    tidytable::mutate(!!as.name(name_target) := feature_id) |>
    tidytable::rename(!!as.name(name_source) := feature_id) |>
    tidytable::distinct(!!as.name(name_source), !!as.name(name_target))

  tidytable::bind_rows(edges, isolated)
}

#' Log the consistency-audit attribute attached by
#' `enforce_graph_adduct_consistency()`.
#' @keywords internal
#' @include logs_utils.R
#' @include adduct_universe_formula.R
#' @include parse_adduct.R
#' @include annotate_masses.R
#' @include annotate_masses_consistency.R
log_consistency_audit <- function(audit) {
  if (is.null(audit)) {
    return(invisible(NULL))
  }
  log_debug(
    "Adduct consistency audit: kept=%d, dropped=%d",
    audit$n_kept,
    audit$n_dropped
  )
  invisible(NULL)
}

# ============================================================
#                LOWER-LEVEL HELPERS (kept stable
