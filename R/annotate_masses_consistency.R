#' Build feature pairs inside RT windows using sweep-line algorithm.
#'
#' For every pair `(src, dest)` of features in the same RT tolerance window
#' (and same sample), with `mz_dest >= mz_src`, returns `delta = mz_dest - mz_src`
#' together with `delta_min/delta_max` widened by the ppm tolerance.
#'
#' Uses a sweep-line algorithm to avoid the O(N^2) cartesian join.
#' Time complexity: O(N log N + K) where K is the number of output pairs.
#' @keywords internal
build_feature_pairs_within_rt <- function(
  df_rt_tol,
  df_fea_min,
  tolerance_ppm
) {
  if (nrow(df_rt_tol) == 0L || nrow(df_fea_min) == 0L) {
    return(tidytable::tidytable(
      feature_id = character(),
      rt = numeric(),
      mz = numeric(),
      feature_id_dest = character(),
      mz_dest = numeric(),
      delta = numeric(),
      delta_min = numeric(),
      delta_max = numeric()
    ))
  }

  # Extract source features with RT windows
  src <- tidytable::as_tidytable(df_rt_tol)[,
    .(
      feature_id,
      rt,
      mz,
      sample,
      rt_min,
      rt_max
    )
  ]

  # Extract destination features
  dest <- tidytable::as_tidytable(df_fea_min)[,
    .(
      feature_id_dest = feature_id,
      rt_dest = rt,
      mz_dest = mz,
      sample
    )
  ]

  # Sweep-line: sort by sample, then rt_dest
  dest <- dest[order(sample, rt_dest)]
  src <- src[order(sample, rt_min)]

  # For each sample, use rolling join with pre-filtering
  # This replaces the cartesian join with allow.cartesian = TRUE
  matches <- dest[
    src,
    on = .(sample, rt_dest >= rt_min, rt_dest <= rt_max),
    nomatch = 0L,
    # Many sources can share one RT window: the result may legitimately
    # exceed nrow(x) + nrow(i), which data.table rejects without this
    allow.cartesian = TRUE
  ][
    feature_id != feature_id_dest & mz_dest >= mz
  ][,
    .(feature_id, rt, mz, feature_id_dest, mz_dest)
  ]

  if (nrow(matches) == 0L) {
    return(tidytable::tidytable(
      feature_id = character(),
      rt = numeric(),
      mz = numeric(),
      feature_id_dest = character(),
      mz_dest = numeric(),
      delta = numeric(),
      delta_min = numeric(),
      delta_max = numeric()
    ))
  }

  matches[,
    `:=`(
      delta = mz_dest - mz,
      tolerance_term = (tolerance_ppm * 1e-6 * (mz + mz_dest) / 2)
    )
  ][,
    `:=`(
      delta_min = delta - tolerance_term,
      delta_max = delta + tolerance_term,
      tolerance_term = NULL
    )
  ] |>
    tidytable::as_tidytable()
}

#' Build all ordered adduct-pair differences for single-charge adducts.
#' @keywords internal
build_adduct_pair_differences <- function(
  add_clu_table,
  tolerance_ppm,
  max_mz
) {
  if (nrow(add_clu_table) == 0L) {
    return(tidytable::tidytable(
      Distance = numeric(),
      Group1 = character(),
      Group2 = character()
    ))
  }

  min_distance <- tolerance_ppm * 1e-6 * max_mz

  add_src <- add_clu_table |>
    tidytable::distinct(adduct, adduct_mass) |>
    tidytable::rename(Group1 = adduct, mass1 = adduct_mass)

  add_dest <- add_clu_table |>
    tidytable::distinct(adduct, adduct_mass) |>
    tidytable::rename(Group2 = adduct, mass2 = adduct_mass)

  # Direct cross join
  add_src |>
    tidytable::cross_join(add_dest) |>
    tidytable::mutate(Distance = mass2 - mass1) |>
    tidytable::filter(
      Group1 != Group2 &
        mass1 < mass2 &
        Distance >= min_distance
    ) |>
    tidytable::distinct(Group1, Group2, Distance) |>
    tidytable::select(Distance, Group1, Group2)
}

#' Count, for each (feature, adduct), how many distinct neighbors in the
#' adduct graph independently support that label.
#'
#' When `covariance_edges` (from compute_intensity_covariance_edges()) are
#' given, a neighbor whose intensities co-vary significantly with the feature
#' across samples counts twice: the adduct relation is then backed by both the
#' mass difference and the intensity profile. Covariance never removes support.
#' @param df_add Adduct edges.
#' @param covariance_edges Optional covariance edges (`feature_id`,
#'   `feature_id_dest`).
#' @keywords internal
compute_feature_adduct_support <- function(df_add, covariance_edges = NULL) {
  if (nrow(df_add) == 0L) {
    return(tidytable::tidytable(
      feature_id = character(),
      adduct = character(),
      adduct_support = integer()
    ))
  }

  f_vec <- c(df_add$feature_id, df_add$feature_id_dest)
  a_vec <- c(df_add$adduct, df_add$adduct_dest)
  nb_vec <- c(df_add$feature_id_dest, df_add$feature_id)

  u_f <- unique(f_vec)
  u_a <- unique(a_vec)
  n_f <- length(u_f)
  n_a <- length(u_a)

  f_id <- if (anyNA(u_f)) match(f_vec, u_f) else fastmatch::fmatch(f_vec, u_f)
  nb_id <- if (anyNA(u_f)) {
    match(nb_vec, u_f)
  } else {
    fastmatch::fmatch(nb_vec, u_f)
  }
  a_id <- if (anyNA(u_a)) match(a_vec, u_a) else fastmatch::fmatch(a_vec, u_a)

  fa_id <- (f_id - 1L) * n_a + a_id
  if (as.double(n_f) * n_a * n_f < .Machine$integer.max) {
    fan_key <- (fa_id - 1L) * n_f + nb_id
    keep_fan <- !duplicated(fan_key)
  } else {
    fan_key <- as.double(fa_id - 1L) * n_f + nb_id
    keep_fan <- fastmatch::fmatch(fan_key, fan_key) == seq_along(fan_key)
  }

  fa_u <- fa_id[keep_fan]
  f_u <- f_id[keep_fan]
  a_u <- a_id[keep_fan]
  nb_u <- nb_id[keep_fan]

  u_fa_pos <- which(!duplicated(fa_u))
  u_fa_keys <- fa_u[u_fa_pos]
  fa_grp <- fastmatch::fmatch(fa_u, u_fa_keys)
  n_groups <- length(u_fa_pos)
  base_counts <- tabulate(fa_grp, nbins = n_groups)

  if (is.null(covariance_edges) || nrow(covariance_edges) == 0L) {
    return(tidytable::tidytable(
      feature_id = u_f[f_u[u_fa_pos]],
      adduct = u_a[a_u[u_fa_pos]],
      adduct_support = base_counts
    ))
  }

  # Covariance pairs are unordered; map covariance endpoints to u_f integer IDs
  c1 <- fastmatch::fmatch(covariance_edges$feature_id, u_f)
  c2 <- fastmatch::fmatch(covariance_edges$feature_id_dest, u_f)
  cov_keep <- which(!is.na(c1) & !is.na(c2))
  if (length(cov_keep) == 0L) {
    return(tidytable::tidytable(
      feature_id = u_f[f_u[u_fa_pos]],
      adduct = u_a[a_u[u_fa_pos]],
      adduct_support = base_counts
    ))
  }

  c1_k <- c1[cov_keep]
  c2_k <- c2[cov_keep]
  lo_c <- pmin.int(c1_k, c2_k)
  hi_c <- pmax.int(c1_k, c2_k)
  lo_n <- pmin.int(f_u, nb_u)
  hi_n <- pmax.int(f_u, nb_u)

  if (as.double(n_f) * n_f < .Machine$integer.max) {
    cov_keys <- unique((lo_c - 1L) * n_f + hi_c)
    nb_keys <- (lo_n - 1L) * n_f + hi_n
  } else {
    cov_keys <- unique(as.double(lo_c - 1L) * n_f + hi_c)
    nb_keys <- as.double(lo_n - 1L) * n_f + hi_n
  }
  is_cov <- !is.na(fastmatch::fmatch(nb_keys, cov_keys))
  cov_bonus <- if (any(is_cov)) {
    tabulate(fa_grp[is_cov], nbins = n_groups)
  } else {
    0L
  }

  tidytable::tidytable(
    feature_id = u_f[f_u[u_fa_pos]],
    adduct = u_a[a_u[u_fa_pos]],
    adduct_support = as.integer(base_counts + cov_bonus)
  )
}

#' Apply optional support-based adduct consistency filtering.
#' @keywords internal
apply_adduct_consistency_filter <- function(
  df_add
) {
  # Fixed consistency policy for legacy helper callers.
  mode <- "conditional"
  support_minimum <- 2L
  degree_minimum <- 3L

  if (nrow(df_add) == 0L) {
    return(df_add)
  }

  u_f <- unique(c(df_add$feature_id, df_add$feature_id_dest))
  u_a <- unique(c(df_add$adduct, df_add$adduct_dest))
  n_f <- length(u_f)
  n_a <- length(u_a)

  f1 <- if (anyNA(u_f)) {
    match(df_add$feature_id, u_f)
  } else {
    fastmatch::fmatch(df_add$feature_id, u_f)
  }
  f2 <- if (anyNA(u_f)) {
    match(df_add$feature_id_dest, u_f)
  } else {
    fastmatch::fmatch(df_add$feature_id_dest, u_f)
  }
  a1 <- if (anyNA(u_a)) {
    match(df_add$adduct, u_a)
  } else {
    fastmatch::fmatch(df_add$adduct, u_a)
  }
  a2 <- if (anyNA(u_a)) {
    match(df_add$adduct_dest, u_a)
  } else {
    fastmatch::fmatch(df_add$adduct_dest, u_a)
  }

  # Pair hypotheses count per (feature_id, feature_id_dest)
  if (as.double(n_f) * n_f < .Machine$integer.max) {
    pair_k <- (f1 - 1L) * n_f + f2
  } else {
    pair_k <- as.double(f1 - 1L) * n_f + f2
  }
  u_pk <- unique(pair_k)
  pk_id <- fastmatch::fmatch(pair_k, u_pk)
  pair_hypotheses <- tabulate(pk_id, nbins = length(u_pk))[pk_id]

  # Feature degree (distinct undirected neighbors)
  nb_f <- c(f1, f2)
  nb_n <- c(f2, f1)
  if (as.double(n_f) * n_f < .Machine$integer.max) {
    nb_k <- (nb_f - 1L) * n_f + nb_n
    not_dup_nb <- !duplicated(nb_k)
  } else {
    nb_k <- as.double(nb_f - 1L) * n_f + nb_n
    not_dup_nb <- fastmatch::fmatch(nb_k, nb_k) == seq_along(nb_k)
  }
  deg_lut <- tabulate(nb_f[not_dup_nb], nbins = n_f)
  src_degree <- deg_lut[f1]
  dest_degree <- deg_lut[f2]

  # Support per (feature, adduct)
  nb_a <- c(a1, a2)
  fa_all <- (nb_f - 1L) * n_a + nb_a
  if (as.double(n_f) * n_a * n_f < .Machine$integer.max) {
    fan_k <- (fa_all - 1L) * n_f + nb_n
    not_dup_fan <- !duplicated(fan_k)
  } else {
    fan_k <- as.double(fa_all - 1L) * n_f + nb_n
    not_dup_fan <- fastmatch::fmatch(fan_k, fan_k) == seq_along(fan_k)
  }
  fa_u <- fa_all[not_dup_fan]
  u_fa <- unique(fa_u)
  fa_supp_lut <- tabulate(
    fastmatch::fmatch(fa_u, u_fa),
    nbins = length(u_fa)
  )
  src_support <- fa_supp_lut[fastmatch::fmatch((f1 - 1L) * n_a + a1, u_fa)]
  src_support[is.na(src_support)] <- 0L
  dest_support <- fa_supp_lut[fastmatch::fmatch((f2 - 1L) * n_a + a2, u_fa)]
  dest_support[is.na(dest_support)] <- 0L

  keep <- if (mode == "strict") {
    src_support >= support_minimum & dest_support >= support_minimum
  } else {
    need_check <- pair_hypotheses > 1L |
      src_degree >= degree_minimum |
      dest_degree >= degree_minimum
    !need_check |
      src_support >= support_minimum |
      dest_support >= support_minimum
  }

  idx_keep <- which(keep)
  tidytable::tidytable(
    feature_id = df_add$feature_id[idx_keep],
    adduct = df_add$adduct[idx_keep],
    adduct_dest = df_add$adduct_dest[idx_keep],
    feature_id_dest = df_add$feature_id_dest[idx_keep]
  ) |>
    tidytable::distinct()
}

#' Enforce graph-level adduct consistency on edge hypotheses
#' @keywords internal
enforce_graph_adduct_consistency <- function(df_add) {
  if (nrow(df_add) == 0L) {
    return(df_add)
  }
  support <- compute_feature_adduct_support(df_add)
  u_add <- unique(c(
    as.character(df_add$adduct),
    as.character(df_add$adduct_dest)
  ))
  state_map <- build_adduct_state_key_map(u_add)

  f1_chr <- as.character(df_add$feature_id)
  f2_chr <- as.character(df_add$feature_id_dest)
  a1_chr <- as.character(df_add$adduct)
  a2_chr <- as.character(df_add$adduct_dest)

  u_f <- unique(c(f1_chr, f2_chr))
  n_f <- length(u_f)
  n_a <- length(u_add)

  f1_id <- fastmatch::fmatch(f1_chr, u_f)
  f2_id <- fastmatch::fmatch(f2_chr, u_f)
  a1_id <- fastmatch::fmatch(a1_chr, u_add)
  a2_id <- fastmatch::fmatch(a2_chr, u_add)

  sf_id <- fastmatch::fmatch(as.character(support$feature_id), u_f)
  sa_id <- fastmatch::fmatch(as.character(support$adduct), u_add)
  s_key <- (sf_id - 1L) * n_a + sa_id
  src_support <- support$adduct_support[fastmatch::fmatch(
    (f1_id - 1L) * n_a + a1_id,
    s_key
  )]
  src_support[is.na(src_support)] <- 0L
  dest_support <- support$adduct_support[fastmatch::fmatch(
    (f2_id - 1L) * n_a + a2_id,
    s_key
  )]
  dest_support[is.na(dest_support)] <- 0L

  edge_score <- src_support + dest_support
  u_add_nchar <- nchar(u_add)
  edge_complexity <- u_add_nchar[a1_id] + u_add_nchar[a2_id]

  u_add_state <- tidytable::coalesce(
    state_map$state_key[fastmatch::fmatch(u_add, state_map$adduct)],
    u_add
  )
  u_states <- unique(u_add_state)
  u_state_nchar <- nchar(u_states)
  n_s <- length(u_states)

  s1_id <- fastmatch::fmatch(u_add_state[a1_id], u_states)
  s2_id <- fastmatch::fmatch(u_add_state[a2_id], u_states)

  g <- igraph::graph_from_data_frame(
    d = data.frame(
      feature_id = f1_chr,
      feature_id_dest = f2_chr,
      stringsAsFactors = FALSE
    ),
    directed = FALSE
  )
  comp_membership <- igraph::components(g)$membership

  # Compute node candidates, priors, and initial assignments globally ONCE
  # using integer node/state IDs instead of character joins/pastes.
  nd_f <- c(f1_id, f2_id)
  nd_s <- c(s1_id, s2_id)
  nd_sc <- c(edge_score, edge_score)
  fs_key <- (nd_f - 1L) * n_s + nd_s
  u_fs_pos <- which(!duplicated(fs_key))
  u_fs_keys <- fs_key[u_fs_pos]
  fs_grp <- fastmatch::fmatch(fs_key, u_fs_keys)

  np_f_id <- nd_f[u_fs_pos]
  np_s_id <- nd_s[u_fs_pos]
  np_prior <- as.numeric(rowsum.default(nd_sc, fs_grp, reorder = FALSE))

  # Sort by feature character rank (matching tidytable::arrange(feature, desc(prior), .key_len))
  f_rank <- rank(u_f, ties.method = "first")
  ord_init <- order(
    f_rank[np_f_id],
    -np_prior,
    u_state_nchar[np_s_id],
    method = "radix"
  )
  win_init <- ord_init[!duplicated(np_f_id[ord_init])]
  assign_state_id <- integer(n_f)
  assign_state_id[np_f_id[win_init]] <- np_s_id[win_init]

  # Only components containing at least one node with >1 candidate state_key
  # can ever change during iterative coordinate ascent.
  is_ambig_f <- tabulate(np_f_id, nbins = n_f) > 1L
  ambig_f_ids <- which(is_ambig_f)
  if (length(ambig_f_ids) > 0L) {
    node_comp_id <- as.integer(comp_membership[u_f])
    ambig_comps <- unique(node_comp_id[ambig_f_ids])
    edge_comp_id <- node_comp_id[f1_id]
    in_ambig_comp <- which(!is.na(fastmatch::fmatch(edge_comp_id, ambig_comps)))

    if (length(in_ambig_comp) > 0L) {
      comp_edge_groups <- split(in_ambig_comp, edge_comp_id[in_ambig_comp])
      in_ambig_np <- which(is_ambig_f[np_f_id])
      cand_states_split <- split(np_s_id[in_ambig_np], np_f_id[in_ambig_np])
      prior_term_split <- split(
        0.01 * np_prior[in_ambig_np],
        np_f_id[in_ambig_np]
      )
      cand_list <- vector("list", n_f)
      prior_list <- vector("list", n_f)
      ambig_node_keys <- as.integer(names(cand_states_split))
      cand_list[ambig_node_keys] <- cand_states_split
      prior_list[ambig_node_keys] <- prior_term_split

      for (ci in seq_along(comp_edge_groups)) {
        e_idx <- comp_edge_groups[[ci]]
        if (length(e_idx) == 0L) {
          next
        }
        e_f1 <- f1_id[e_idx]
        e_f2 <- f2_id[e_idx]
        e_s1 <- s1_id[e_idx]
        e_s2 <- s2_id[e_idx]
        e_sc <- edge_score[e_idx]

        comp_nodes <- unique(c(e_f1, e_f2))
        multi_nodes <- comp_nodes[is_ambig_f[comp_nodes]]
        if (length(multi_nodes) == 0L) {
          next
        }

        n_multi <- length(multi_nodes)
        s_sp <- split(seq_along(e_idx), e_f1)
        d_sp <- split(seq_along(e_idx), e_f2)
        src_split <- s_sp[fastmatch::fmatch(
          multi_nodes,
          as.integer(names(s_sp))
        )]
        dst_split <- d_sp[fastmatch::fmatch(
          multi_nodes,
          as.integer(names(d_sp))
        )]

        last_eval_step <- integer(n_f)
        last_change_step <- integer(n_f)
        step_counter <- 0L

        for (iter in seq_len(20L)) {
          changed <- FALSE
          for (mi in seq_len(n_multi)) {
            f <- multi_nodes[[mi]]
            s_i <- src_split[[mi]]
            d_i <- dst_split[[mi]]

            if (iter > 1L) {
              my_eval <- last_eval_step[[f]]
              nb_changed <- (length(s_i) > 0L &&
                any(last_change_step[e_f2[s_i]] >= my_eval)) ||
                (length(d_i) > 0L &&
                  any(last_change_step[e_f1[d_i]] >= my_eval))
              if (!nb_changed) {
                next
              }
            }

            step_counter <- step_counter + 1L
            last_eval_step[[f]] <- step_counter

            cands_f <- cand_list[[f]]
            cand_scores <- prior_list[[f]]

            if (length(s_i) > 0L) {
              active_s <- e_s2[s_i] == assign_state_id[e_f2[s_i]]
              if (any(active_s)) {
                s_i_act <- s_i[active_s]
                s1_act <- e_s1[s_i_act]
                sc_act <- e_sc[s_i_act]
                for (k in seq_along(cands_f)) {
                  cand_scores[[k]] <- cand_scores[[k]] +
                    sum(sc_act[s1_act == cands_f[[k]]])
                }
              }
            }

            if (length(d_i) > 0L) {
              active_d <- e_s1[d_i] == assign_state_id[e_f1[d_i]]
              if (any(active_d)) {
                d_i_act <- d_i[active_d]
                s2_act <- e_s2[d_i_act]
                sc_act <- e_sc[d_i_act]
                for (k in seq_along(cands_f)) {
                  cand_scores[[k]] <- cand_scores[[k]] +
                    sum(sc_act[s2_act == cands_f[[k]]])
                }
              }
            }

            best_idx <- which(cand_scores == max(cand_scores))
            if (length(best_idx) > 1L) {
              lens <- u_state_nchar[cands_f[best_idx]]
              best_idx <- best_idx[which.min(lens)]
            }
            best_cand <- cands_f[[best_idx[[1L]]]]
            if (assign_state_id[[f]] != best_cand) {
              assign_state_id[[f]] <- best_cand
              last_change_step[[f]] <- step_counter
              changed <- TRUE
            }
          }
          if (!changed) break
        }
      }
    }
  }

  # Deduplicate input rows on (f1_id, a1_id, a2_id, f2_id) since edge_score and
  # edge_complexity are deterministic functions of (f1_id, a1_id, a2_id, f2_id).
  pair_key <- as.double(f1_id - 1L) * n_f + f2_id
  u_pairs <- unique(pair_key)
  pair_id <- fastmatch::fmatch(pair_key, u_pairs)
  aa_id <- (a1_id - 1L) * n_a + a2_id
  n_aa <- n_a * n_a
  edge_key <- if (as.double(length(u_pairs)) * n_aa < .Machine$integer.max) {
    (pair_id - 1L) * n_aa + aa_id
  } else {
    as.double(pair_id - 1L) * n_aa + aa_id
  }
  keep_u_edge <- if (is.integer(edge_key)) {
    !duplicated(edge_key)
  } else {
    fastmatch::fmatch(edge_key, edge_key) == seq_along(edge_key)
  }

  u_idx <- which(keep_u_edge)
  n_input <- length(u_idx)

  f1_u <- f1_id[u_idx]
  f2_u <- f2_id[u_idx]
  a1_u <- a1_id[u_idx]
  a2_u <- a2_id[u_idx]
  p_u <- pair_id[u_idx]
  sc_u <- edge_score[u_idx]
  cx_u <- edge_complexity[u_idx]
  ks_u <- (s1_id[u_idx] == assign_state_id[f1_u]) &
    (s2_id[u_idx] == assign_state_id[f2_u])

  ord_u <- order(f_rank[f1_u], f_rank[f2_u], -sc_u, cx_u, method = "radix")

  ord_kept <- ord_u[ks_u[ord_u]]
  win_kept <- ord_kept[!duplicated(p_u[ord_kept])]
  n_kept <- length(win_kept)

  kept_all <- tidytable::tidytable(
    feature_id = df_add$feature_id[u_idx[win_kept]],
    adduct = df_add$adduct[u_idx[win_kept]],
    adduct_dest = df_add$adduct_dest[u_idx[win_kept]],
    feature_id_dest = df_add$feature_id_dest[u_idx[win_kept]]
  )

  n_dropped <- n_input - n_kept
  if (n_dropped > 0L) {
    is_kept_u <- logical(n_input)
    is_kept_u[win_kept] <- TRUE
    drop_u <- which(!is_kept_u)

    win_best <- ord_u[!duplicated(p_u[ord_u])]
    best_a1_by_p <- integer(length(u_pairs))
    best_a2_by_p <- integer(length(u_pairs))
    best_a1_by_p[p_u[win_best]] <- a1_u[win_best]
    best_a2_by_p[p_u[win_best]] <- a2_u[win_best]

    p_dr <- p_u[drop_u]
    is_node_conflict <- (a1_u[drop_u] == best_a1_by_p[p_dr]) &
      (a2_u[drop_u] == best_a2_by_p[p_dr])
    n_nc <- sum(is_node_conflict)
    n_pdc <- n_dropped - n_nc

    reasons <- character()
    counts <- integer()
    if (n_nc > 0L) {
      reasons <- c(reasons, "node_state_conflict")
      counts <- c(counts, n_nc)
    }
    if (n_pdc > 0L) {
      reasons <- c(reasons, "pair_dedup_or_conflict")
      counts <- c(counts, n_pdc)
    }
    ord_rc <- order(-counts)
    dropped_reason_counts <- tidytable::tidytable(
      drop_reason = reasons[ord_rc],
      n = counts[ord_rc]
    )
  } else {
    dropped_reason_counts <- tidytable::tidytable(
      drop_reason = character(),
      n = integer()
    )
  }

  attr(kept_all, "consistency_audit") <- list(
    n_input = n_input,
    n_kept = n_kept,
    n_dropped = n_dropped,
    dropped_reason_counts = dropped_reason_counts
  )
  kept_all
}

#' Build canonical adduct-state keys from adduct notation
#' @keywords internal
build_adduct_state_key_map <- function(adducts) {
  adducts <- unique(as.character(adducts))
  adducts <- adducts[!is.na(adducts)]
  if (length(adducts) == 0L) {
    return(tidytable::tidytable(adduct = character(), state_key = character()))
  }
  state_key <- vapply(
    X = adducts,
    FUN = adduct_to_state_key,
    FUN.VALUE = character(1)
  )
  tidytable::tidytable(adduct = adducts, state_key = state_key)
}

#' Convert adduct notation to a canonical state key
#' @keywords internal
adduct_to_state_key <- function(adduct) {
  parsed <- tryCatch(
    parse_adduct(adduct),
    error = function(.err) {
      invisible(.err)
      NULL
    }
  )
  if (!is.null(parsed) && !is_parse_failed(parsed)) {
    return(sprintf(
      "z:%d|m:%d|i:%d|d:%.6f",
      parsed[["n_charges"]],
      parsed[["n_mer"]],
      parsed[["n_iso"]],
      parsed[["los_add_clu"]]
    ))
  }
  mod_mass <- calculate_net_mod_mass_from_text(adduct)
  if (is.finite(mod_mass)) {
    return(paste0("fallback-d:", sprintf("%.6f", mod_mass)))
  }
  paste0("raw:", adduct)
}

#' Estimate net modification mass directly from adduct text
#' @keywords internal
calculate_net_mod_mass_from_text <- function(adduct) {
  adduct <- as.character(adduct)
  if (is.na(adduct) || !nzchar(adduct)) {
    return(NA_real_)
  }
  inner <- sub("^\\[(.*)\\][0-9]*[+-]+$", "\\1", adduct, perl = TRUE)
  if (!nzchar(inner) || identical(inner, adduct)) {
    return(NA_real_)
  }
  mods <- sub("^[0-9]*M[0-9]*", "", inner, perl = TRUE)
  if (!nzchar(mods)) {
    return(0)
  }
  tokens <- regmatches(mods, gregexpr("[+-][^+-]+", mods, perl = TRUE))[[1L]]
  if (length(tokens) == 0L) {
    return(NA_real_)
  }
  total <- 0
  for (tok in tokens) {
    sign <- if (startsWith(tok, "+")) 1 else -1
    body <- substr(tok, 2L, nchar(tok))
    coef_match <- regmatches(body, regexpr("^[0-9]+", body, perl = TRUE))
    coef <- if (length(coef_match) == 0L || coef_match[[1L]] == "") {
      1
    } else {
      as.numeric(coef_match[[1L]])
    }
    formula <- sub("^[0-9]+", "", body, perl = TRUE)
    if (!nzchar(formula)) {
      return(NA_real_)
    }
    mass <- suppressWarnings(calculate_formula_mass(formula))
    if (!is.finite(mass)) {
      return(NA_real_)
    }
    total <- total + (sign * coef * mass)
  }
  total
}

#' Join RT/mz couple windows with neutral losses using binary search instead of cartesian join.
#' Join couples with neutral losses using binary search
#' @keywords internal
join_couples_with_neutral_losses <- function(df_couples_diff, neutral_losses) {
  if (nrow(df_couples_diff) == 0L || nrow(neutral_losses) == 0L) {
    return(tidytable::tidytable(
      feature_id = character(),
      loss = character(),
      mass = numeric(),
      feature_id_dest = character()
    ))
  }
  nl_keep <- which(!is.na(neutral_losses$loss))
  if (length(nl_keep) == 0L) {
    return(tidytable::tidytable(
      feature_id = character(),
      loss = character(),
      mass = numeric(),
      feature_id_dest = character()
    ))
  }
  nl_loss <- neutral_losses$loss[nl_keep]
  nl_mass <- neutral_losses$mass[nl_keep]
  ord_nl <- order(nl_mass)
  loss_sorted <- nl_loss[ord_nl]
  mass_sorted <- nl_mass[ord_nl]
  n_nl <- length(mass_sorted)

  # compute all indices at once using vectorized operations
  lo_idx <- findInterval(
    df_couples_diff$delta_min,
    mass_sorted,
    left.open = TRUE
  ) +
    1L
  hi_idx <- findInterval(
    df_couples_diff$delta_max,
    mass_sorted,
    left.open = FALSE
  )
  lo_idx <- pmax.int(1L, lo_idx)
  hi_idx <- pmin.int(n_nl, hi_idx)

  # Identify rows with valid ranges (lo_idx <= hi_idx)
  valid_idx <- which(lo_idx <= hi_idx)
  if (length(valid_idx) == 0L) {
    return(tidytable::tidytable(
      feature_id = character(),
      loss = character(),
      mass = numeric(),
      feature_id_dest = character()
    ))
  }

  match_sizes <- hi_idx[valid_idx] - lo_idx[valid_idx] + 1L
  src_exp <- rep.int(valid_idx, match_sizes)
  nl_exp <- sequence(match_sizes, from = lo_idx[valid_idx])

  tidytable::tidytable(
    feature_id = df_couples_diff$feature_id[src_exp],
    loss = loss_sorted[nl_exp],
    mass = mass_sorted[nl_exp],
    feature_id_dest = df_couples_diff$feature_id_dest[src_exp]
  ) |>
    tidytable::distinct()
}

#' Join multi-adduct candidates with add/loss inferred masses using binary search instead of cartesian join.
#' @keywords internal
#' @include parse_adduct.R
#' @include calculate_mass_of_m.R
join_multi_with_addlossed <- function(df_multi, df_addlossed_rdy) {
  if (nrow(df_multi) == 0L || nrow(df_addlossed_rdy) == 0L) {
    return(tidytable::tidytable(
      feature_id = character(),
      rt = numeric(),
      mz = numeric(),
      adduct = character(),
      mass = numeric()
    ))
  }

  ord_al <- order(df_addlossed_rdy$rt)
  rt_obs <- df_addlossed_rdy$rt[ord_al]
  mass_obs <- df_addlossed_rdy$mass[ord_al]
  n_al <- length(rt_obs)

  lo_idx <- findInterval(df_multi$rt_min, rt_obs, left.open = TRUE) + 1L
  hi_idx <- findInterval(df_multi$rt_max, rt_obs, left.open = FALSE)
  lo_idx <- pmax.int(1L, lo_idx)
  hi_idx <- pmin.int(n_al, hi_idx)

  # Identify rows with valid RT ranges
  valid_idx <- which(lo_idx <= hi_idx)
  if (length(valid_idx) == 0L) {
    return(tidytable::tidytable(
      feature_id = character(),
      rt = numeric(),
      mz = numeric(),
      adduct = character(),
      mass = numeric()
    ))
  }

  match_sizes <- hi_idx[valid_idx] - lo_idx[valid_idx] + 1L
  mass_min_v <- df_multi$mass_min
  mass_max_v <- df_multi$mass_max
  total_pairs <- sum(as.double(match_sizes))

  if (total_pairs <= 5000000) {
    src_exp <- rep.int(valid_idx, match_sizes)
    al_exp <- sequence(match_sizes, from = lo_idx[valid_idx])
    m_obs <- mass_obs[al_exp]
    ok <- which(m_obs >= mass_min_v[src_exp] & m_obs <= mass_max_v[src_exp])
    if (length(ok) == 0L) {
      return(tidytable::tidytable(
        feature_id = character(),
        rt = numeric(),
        mz = numeric(),
        adduct = character(),
        mass = numeric()
      ))
    }
    src_hit <- src_exp[ok]
    mass_hit <- m_obs[ok]
  } else {
    n_val <- length(valid_idx)
    chunk_sz <- 4096L
    n_chunks <- ceiling(n_val / chunk_sz)
    src_chunks <- vector("list", n_chunks)
    mass_chunks <- vector("list", n_chunks)
    filled <- 0L
    for (ch in seq_len(n_chunks)) {
      rng <- ((ch - 1L) * chunk_sz + 1L):min(n_val, ch * chunk_sz)
      v_ch <- valid_idx[rng]
      ms_ch <- match_sizes[rng]
      s_exp <- rep.int(v_ch, ms_ch)
      a_exp <- sequence(ms_ch, from = lo_idx[v_ch])
      m_obs <- mass_obs[a_exp]
      ok <- which(m_obs >= mass_min_v[s_exp] & m_obs <= mass_max_v[s_exp])
      if (length(ok) > 0L) {
        filled <- filled + 1L
        src_chunks[[filled]] <- s_exp[ok]
        mass_chunks[[filled]] <- m_obs[ok]
      }
    }
    if (filled == 0L) {
      return(tidytable::tidytable(
        feature_id = character(),
        rt = numeric(),
        mz = numeric(),
        adduct = character(),
        mass = numeric()
      ))
    }
    src_hit <- unlist(src_chunks[seq_len(filled)], use.names = FALSE)
    mass_hit <- unlist(mass_chunks[seq_len(filled)], use.names = FALSE)
  }

  tidytable::tidytable(
    feature_id = df_multi$feature_id[src_hit],
    rt = df_multi$rt[src_hit],
    mz = df_multi$mz[src_hit],
    adduct = df_multi$adduct[src_hit],
    mass = mass_hit
  ) |>
    tidytable::distinct()
}
