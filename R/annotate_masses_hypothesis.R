#' Match feature pairs against pairwise adduct-mass differences
#' @keywords internal
match_pairs_to_adduct_diffs <- function(pairs, adduct_diffs) {
  if (nrow(pairs) == 0L || nrow(adduct_diffs) == 0L) {
    return(tidytable::tidytable(
      feature_id = character(),
      adduct = character(),
      adduct_dest = character(),
      feature_id_dest = character()
    ))
  }
  pairs_t <- tidytable::as_tidytable(pairs)[,
    .(feature_id, feature_id_dest, delta_min, delta_max)
  ]
  diffs_t <- tidytable::as_tidytable(adduct_diffs)[,
    .(Distance, Group1, Group2)
  ]
  diffs_t[
    pairs_t,
    on = .(Distance >= delta_min, Distance <= delta_max),
    nomatch = NA_real_,
    allow.cartesian = TRUE
  ][,
    .(
      feature_id,
      adduct = Group1,
      adduct_dest = Group2,
      feature_id_dest
    )
  ] |>
    unique() |>
    tidytable::as_tidytable() |>
    tidytable::filter(!is.na(adduct))
}

#' Match feature pairs against a single-mass diffs table (clusters or losses).
#'
#' Implementation note: `join_couples_with_neutral_losses()` is used as a
#' generic delta matcher. The output columns are renamed so that whatever
#' `diff_col` is (`loss` or `cluster`) appears as a column of that name.
#' @keywords internal
match_pairs_to_mass_diffs <- function(pairs, diffs, diff_col) {
  if (nrow(pairs) == 0L || nrow(diffs) == 0L) {
    return(
      tidytable::tidytable(
        feature_id = character(),
        mass = numeric(),
        feature_id_dest = character()
      ) |>
        tidytable::mutate(!!as.name(diff_col) := character())
    )
  }
  diffs_norm <- diffs |>
    tidytable::rename(loss = !!as.name(diff_col))
  out <- join_couples_with_neutral_losses(
    df_couples_diff = pairs,
    neutral_losses = diffs_norm
  )
  if (diff_col != "loss") {
    out <- out |> tidytable::rename(!!as.name(diff_col) := loss)
  }
  out
}

#' Fast per-group median using radix sort and midpoint indexing
#' @noRd
.fast_group_median <- function(vals, groups) {
  n <- length(vals)
  u_g <- unique(groups)
  g_id <- if (anyNA(u_g)) match(groups, u_g) else fastmatch::fmatch(groups, u_g)
  ord <- order(g_id, vals, method = "radix")
  g_sorted <- g_id[ord]
  v_sorted <- vals[ord]
  is_start <- if (n > 1L) c(TRUE, g_sorted[-1L] != g_sorted[-n]) else TRUE
  starts <- which(is_start)
  ends <- c(starts[-1L] - 1L, n)
  med_sorted <- 0.5 *
    (v_sorted[(starts + ends) %/% 2L] + v_sorted[(starts + ends + 1L) %/% 2L])
  med_lut <- numeric(length(u_g))
  med_lut[g_sorted[starts]] <- med_sorted
  list(
    groups = u_g,
    medians = med_lut,
    per_obs = med_lut[g_id]
  )
}

#' Solve M-consistency constraints in adduct edges
#'
#' For each adduct edge, the two endpoints must have the same neutral mass M.
#' This function:
#' 1. Builds a constraint graph where edges connect features that share adduct relationships
#' 2. For each connected component, solves for consistent M assignments
#' 3. Returns only edges where the implied M values are consistent
#' 4. Also returns the inferred M value for each feature in each component
#'
#' @details The consistency check accepts an edge if EITHER the ppm tolerance OR the
#' dalton tolerance is satisfied (whichever is less restrictive at that mass range).
#'
#' @keywords internal
solve_consistent_adduct_assignments <- function(
  adduct_edges,
  features_table,
  tolerance_ppm,
  tolerance_dalton,
  adduct_lookup = NULL,
  baseline_adduct = NULL
) {
  if (nrow(adduct_edges) == 0L || nrow(features_table) == 0L) {
    return(list(
      consistent_edges = adduct_edges[0L, ],
      feature_m_map = tidytable::tidytable(
        feature_id = character(),
        neutral_mass = numeric(),
        component_id = character()
      ),
      component_membership = tidytable::tidytable(
        feature_id = character(),
        component_id = character()
      )
    ))
  }

  keep_f <- !duplicated(features_table$feature_id)
  f_ids_min <- features_table$feature_id[keep_f]
  f_mzs_min <- as.numeric(features_table$mz[keep_f])
  m_src_idx <- fastmatch::fmatch(adduct_edges$feature_id, f_ids_min)
  m_dest_idx <- fastmatch::fmatch(adduct_edges$feature_id_dest, f_ids_min)
  mz_src_vec <- f_mzs_min[m_src_idx]
  mz_dest_vec <- f_mzs_min[m_dest_idx]
  valid_mz <- !is.na(mz_src_vec) & !is.na(mz_dest_vec)
  edges <- adduct_edges[which(valid_mz), , drop = FALSE]
  edges$mz_src <- mz_src_vec[valid_mz]
  edges$mz_dest <- mz_dest_vec[valid_mz]

  if (nrow(edges) == 0L) {
    return(list(
      consistent_edges = adduct_edges[0L, ],
      feature_m_map = tidytable::tidytable(
        feature_id = character(),
        neutral_mass = numeric(),
        component_id = character()
      ),
      component_membership = tidytable::tidytable(
        feature_id = character(),
        component_id = character()
      )
    ))
  }

  m_src <- calculate_neutral_mass_from_lookup(
    mzs = as.numeric(edges$mz_src),
    adducts = edges$adduct,
    adduct_lookup = adduct_lookup
  )
  m_dest <- calculate_neutral_mass_from_lookup(
    mzs = as.numeric(edges$mz_dest),
    adducts = edges$adduct_dest,
    adduct_lookup = adduct_lookup
  )
  m_diff <- abs(m_src - m_dest)
  ppm_consistent <- m_diff <=
    (tolerance_ppm * 1e-6 * pmax(m_src, m_dest, na.rm = TRUE))
  dalton_consistent <- if (is.null(tolerance_dalton)) {
    FALSE
  } else {
    (m_diff <= tolerance_dalton)
  }
  m_consistent <- !is.na(ppm_consistent | dalton_consistent) &
    (ppm_consistent | dalton_consistent)

  # Keep only consistent edges
  consistent <- edges[
    which(m_consistent),
    c("feature_id", "adduct", "feature_id_dest", "adduct_dest"),
    drop = FALSE
  ]

  # IMPORTANT: Enforce global M-consistency
  # This removes edges that cause conflicts when a feature has multiple edges
  # Example: F2 cannot be both M+Na (M≈100) and M-H2O (M≈200) simultaneously
  consistent <- enforce_global_m_consistency(
    consistent_edges = consistent,
    features_table = features_table,
    tolerance_ppm = tolerance_ppm,
    tolerance_dalton = tolerance_dalton,
    adduct_lookup = adduct_lookup
  )

  if (nrow(consistent) == 0L) {
    return(list(
      consistent_edges = consistent[0L, ],
      feature_m_map = tidytable::tidytable(
        feature_id = character(),
        neutral_mass = numeric(),
        component_id = character()
      ),
      component_membership = tidytable::tidytable(
        feature_id = character(),
        component_id = character()
      )
    ))
  }

  # Find connected components using igraph (O(V + E))
  g <- igraph::graph_from_data_frame(
    d = consistent[, c("feature_id", "feature_id_dest"), drop = FALSE],
    directed = FALSE
  )
  comp_info <- igraph::components(g)
  all_nodes <- names(comp_info$membership)
  comp_labels <- paste0("comp_", comp_info$membership)

  # Vectorized neutral mass computation across all consistent edges using fmatch
  src_ids <- as.character(consistent$feature_id)
  dest_ids <- as.character(consistent$feature_id_dest)
  src_mz <- f_mzs_min[fastmatch::fmatch(src_ids, f_ids_min)]
  dest_mz <- f_mzs_min[fastmatch::fmatch(dest_ids, f_ids_min)]

  all_f_ids <- c(src_ids, dest_ids)
  all_m_vals <- c(
    calculate_neutral_mass_from_lookup(
      mzs = src_mz,
      adducts = consistent$adduct,
      adduct_lookup = adduct_lookup
    ),
    calculate_neutral_mass_from_lookup(
      mzs = dest_mz,
      adducts = consistent$adduct_dest,
      adduct_lookup = adduct_lookup
    )
  )
  valid_m <- !is.na(all_m_vals)
  med_res <- .fast_group_median(all_m_vals[valid_m], all_f_ids[valid_m])
  m_values <- tidytable::tidytable(
    feature_id = med_res$groups,
    neutral_mass = med_res$medians
  )

  feature_m_map <- m_values |>
    tidytable::mutate(
      component_id = comp_labels[fastmatch::fmatch(
        as.character(feature_id),
        all_nodes
      )]
    )

  component_membership <- feature_m_map |>
    tidytable::distinct(feature_id, component_id)

  list(
    consistent_edges = consistent,
    feature_m_map = feature_m_map,
    component_membership = component_membership
  )
}

#' Enforce global M-consistency: all edges incident to a feature must agree on M
#'
#' This ensures that when multiple edges share a feature, they all imply the same
#' neutral mass M for that feature (within tolerance). If a feature has conflicting
#' M values from different edges, those conflicting edges are identified and removed,
#' which may split connected components into smaller consistent subcomponents.
#'
#' Algorithm:
#' 1. For each feature in each component, collect all implied M values from incident edges
#' 2. Check if all M values for that feature agree within tolerance
#' 3. If not, identify edges whose M conflicts with the consensus M
#' 4. Remove conflicting edges and rebuild components
#' 5. Repeat until global consistency is achieved
#'
#' @keywords internal
enforce_global_m_consistency <- function(
  consistent_edges,
  features_table,
  tolerance_ppm,
  tolerance_dalton,
  adduct_lookup = NULL
) {
  if (nrow(consistent_edges) == 0L) {
    return(consistent_edges)
  }

  keep_f <- !duplicated(features_table$feature_id)
  f_ids_min <- features_table$feature_id[keep_f]
  f_mzs_min <- as.numeric(features_table$mz[keep_f])
  m_src_idx <- fastmatch::fmatch(consistent_edges$feature_id, f_ids_min)
  m_dest_idx <- fastmatch::fmatch(consistent_edges$feature_id_dest, f_ids_min)
  mz_src_vec <- f_mzs_min[m_src_idx]
  mz_dest_vec <- f_mzs_min[m_dest_idx]
  valid_mz <- !is.na(mz_src_vec) & !is.na(mz_dest_vec)
  edge_with_m <- tidytable::as_tidytable(
    consistent_edges[which(valid_mz), , drop = FALSE]
  )
  edge_with_m$mz_src <- mz_src_vec[valid_mz]
  edge_with_m$mz_dest <- mz_dest_vec[valid_mz]

  edge_with_m$m_src <- calculate_neutral_mass_from_lookup(
    mzs = as.numeric(edge_with_m$mz_src),
    adducts = edge_with_m$adduct,
    adduct_lookup = adduct_lookup
  )
  edge_with_m$m_dest <- calculate_neutral_mass_from_lookup(
    mzs = as.numeric(edge_with_m$mz_dest),
    adducts = edge_with_m$adduct_dest,
    adduct_lookup = adduct_lookup
  )

  edge_with_m$edge_id <- seq_len(nrow(edge_with_m))

  # Iteratively filter until global consistency achieved
  prev_n_edges <- nrow(edge_with_m) + 1L
  iteration <- 0L
  max_iterations <- 10L

  while (
    nrow(edge_with_m) > 0L &&
      nrow(edge_with_m) < prev_n_edges &&
      iteration < max_iterations
  ) {
    iteration <- iteration + 1L
    prev_n_edges <- nrow(edge_with_m)

    feature_ids <- c(edge_with_m$feature_id, edge_with_m$feature_id_dest)
    nb_ids <- c(edge_with_m$feature_id_dest, edge_with_m$feature_id)
    m_values <- c(edge_with_m$m_src, edge_with_m$m_dest)
    edge_err <- abs(edge_with_m$m_src - edge_with_m$m_dest)
    edge_errs <- c(edge_err, edge_err)
    edge_ids <- c(edge_with_m$edge_id, edge_with_m$edge_id)

    valid <- !is.na(feature_ids) & !is.na(m_values)
    feature_ids <- feature_ids[valid]
    nb_ids <- nb_ids[valid]
    m_values <- m_values[valid]
    edge_errs <- edge_errs[valid]
    edge_ids <- edge_ids[valid]

    dalton_tol <- tolerance_dalton %||% 0.0025
    ppm_scale <- if (
      is.null(tolerance_ppm) || is.na(tolerance_ppm) || tolerance_ppm <= 0
    ) {
      0
    } else {
      tolerance_ppm * 1e-6
    }

    med_res <- .fast_group_median(m_values, feature_ids)
    feature_medians <- med_res$per_obs
    abs_diff <- abs(m_values - feature_medians)
    ppm_tol <- ppm_scale * pmax(abs(feature_medians), 1e-12)
    obs_consistent <- abs_diff <= ppm_tol | abs_diff <= dalton_tol

    if (!all(obs_consistent)) {
      u_f <- med_res$groups
      f_id <- fastmatch::fmatch(feature_ids, u_f)
      nb_id <- fastmatch::fmatch(nb_ids, u_f)
      n_deg <- tabulate(f_id, nbins = length(u_f))
      conf_f <- unique(f_id[!obs_consistent])
      in_conf <- which(f_id %in% conf_f)
      ord_c <- order(f_id[in_conf], m_values[in_conf])
      idx_s <- in_conf[ord_c]
      f_s <- f_id[idx_s]
      m_s <- m_values[idx_s]
      n_s <- length(idx_s)
      tol_s <- pmax(dalton_tol, ppm_scale * pmax(abs(m_s), 1e-12))
      new_f <- c(TRUE, f_s[-1L] != f_s[-n_s])
      new_cl <- new_f | c(FALSE, (m_s[-1L] - m_s[-n_s]) > tol_s[-1L])
      cl_s <- cumsum(new_cl)
      nb_deg <- ifelse(is.na(nb_id[idx_s]), 1L, n_deg[nb_id[idx_s]])
      acc_w <- 1 /
        (1 + tidytable::coalesce(edge_errs[idx_s], 0) / pmax(tol_s, 1e-12))
      w_s <- 1 + 0.1 * pmin(nb_deg, 10L) + 0.01 * acc_w
      cl_w <- as.numeric(rowsum.default(w_s, cl_s, reorder = FALSE))
      cl_f <- f_s[new_cl]
      ord_cl <- order(cl_f, -cl_w, seq_along(cl_w))
      best_cl_ids <- ord_cl[!duplicated(cl_f[ord_cl])]
      is_best_cl <- logical(length(cl_w))
      is_best_cl[best_cl_ids] <- TRUE
      keep_s <- is_best_cl[cl_s]
      m_best <- m_s[keep_s]
      f_best <- f_s[keep_s]
      n_b <- length(m_best)
      starts_b <- which(c(TRUE, f_best[-1L] != f_best[-n_b]))
      ends_b <- c(starts_b[-1L] - 1L, n_b)
      cnts_b <- ends_b - starts_b + 1L
      med_b <- 0.5 *
        (m_best[starts_b + (cnts_b - 1L) %/% 2L] +
          m_best[starts_b + cnts_b %/% 2L])
      f_med_lut <- rep(NA_real_, length(u_f))
      f_med_lut[f_best[starts_b]] <- med_b
      feature_medians[in_conf] <- f_med_lut[f_id[in_conf]]
      abs_diff <- abs(m_values - feature_medians)
      ppm_tol <- ppm_scale * pmax(abs(feature_medians), 1e-12)
      obs_consistent <- abs_diff <= ppm_tol | abs_diff <= dalton_tol
    }

    edges_to_remove <- unique(edge_ids[!obs_consistent])
    if (length(edges_to_remove) == 0L) {
      break
    }

    edge_with_m <- edge_with_m[
      is.na(fastmatch::fmatch(edge_with_m$edge_id, edges_to_remove)),
      ,
      drop = FALSE
    ]
  }

  # Return cleaned edges (drop calculation columns, keep original structure)
  edge_with_m |>
    tidytable::select(
      feature_id,
      adduct,
      feature_id_dest,
      adduct_dest
    ) |>
    tidytable::distinct()
}

#' Build the per-feature adduct hypotheses from the edge sets
#'
#' Hypotheses come from four sources, in priority order:
#'   - `pair`: an adduct edge directly named this feature with this adduct
#'   - `cluster`: a cluster edge implies a `+cluster` suffix on the dest node
#'   - `loss`: a neutral-loss edge implies a `-loss` suffix on the precursor
#'   - `preassigned`: an upstream tool assigned this feature this adduct
#'   - `baseline`: every feature is always tested under `[M+H]+` / `[M-H]-`
#'
#' IMPORTANT: This version ensures M-consistency by:
#' 1. First solving for consistent M assignments across adduct edges
#' 2. Only collecting hypotheses that correspond to inferred M values
#' 3. Expanding clusters/losses only on M-consistent base adducts
#'
#' @keywords internal
collect_node_adduct_hypotheses <- function(
  adduct_edges,
  cluster_edges,
  loss_edges,
  evidence_hypotheses,
  preassigned,
  baseline_adducts,
  features_table,
  adduct_lookup = NULL,
  tolerance_ppm = 5,
  tolerance_dalton = 0.0025,
  covariance_edges = NULL
) {
  if (length(baseline_adducts) == 0L || all(is.na(baseline_adducts))) {
    baseline_adducts <- "[M+H]+"
  }
  baseline_adducts <- unique(stats::na.omit(as.character(baseline_adducts)))
  # STEP 1: Solve for consistent M assignments across adduct edges
  consistency_result <- solve_consistent_adduct_assignments(
    adduct_edges = adduct_edges,
    features_table = features_table,
    tolerance_ppm = tolerance_ppm,
    tolerance_dalton = tolerance_dalton,
    adduct_lookup = adduct_lookup,
    baseline_adduct = baseline_adducts[[1L]]
  )
  consistent_edges <- consistency_result$consistent_edges
  feature_m_map <- consistency_result$feature_m_map
  component_membership <- consistency_result$component_membership

  # (a) pair-derived hypotheses - ONLY from consistent edges
  pair_a <- consistent_edges |>
    tidytable::distinct(feature_id, adduct)
  pair_b <- consistent_edges |>
    tidytable::distinct(feature_id_dest, adduct_dest) |>
    tidytable::rename(
      feature_id = feature_id_dest,
      adduct = adduct_dest
    )
  pair_hyps <- tidytable::bind_rows(pair_a, pair_b) |>
    tidytable::distinct() |>
    tidytable::mutate(
      source = "pair",
      is_preassigned = FALSE,
      candidate_adduct_origin = "supported"
    )

  # (b) preassigned (upstream tools) - ONLY if compatible with inferred M
  pre_propagated <- propagate_preassigned_over_adduct_edges(
    adduct_edges = consistent_edges,
    preassigned = preassigned
  )
  pre_hyps <- preassigned |>
    tidytable::distinct(feature_id, adduct) |>
    tidytable::mutate(
      source = "preassigned",
      is_preassigned = TRUE,
      candidate_adduct_origin = "supported"
    )
  pre_prop_hyps <- pre_propagated |>
    tidytable::distinct(feature_id, adduct) |>
    tidytable::mutate(
      source = "preassigned_propagated",
      is_preassigned = TRUE,
      candidate_adduct_origin = "supported"
    )

  # Filter preassigned to be compatible with inferred M values
  if (nrow(pre_hyps) > 0L && nrow(feature_m_map) > 0L) {
    pre_hyps <- pre_hyps |>
      tidytable::left_join(
        features_table |> tidytable::distinct(feature_id, mz),
        by = "feature_id"
      ) |>
      tidytable::left_join(
        feature_m_map |> tidytable::select(feature_id, neutral_mass),
        by = "feature_id"
      ) |>
      tidytable::mutate(
        implied_m = calculate_neutral_mass_from_lookup(
          mzs = as.numeric(mz),
          adducts = adduct,
          adduct_lookup = adduct_lookup
        ),
        compatible = is.na(neutral_mass) |
          abs(implied_m - neutral_mass) <=
            pmax(
              tolerance_ppm * 1e-6 * neutral_mass,
              tolerance_dalton %||% 0
            )
      ) |>
      tidytable::filter(compatible) |>
      tidytable::select(
        feature_id,
        adduct,
        source,
        is_preassigned,
        candidate_adduct_origin
      )
  }

  if (nrow(pre_prop_hyps) > 0L && nrow(feature_m_map) > 0L) {
    pre_prop_hyps <- pre_prop_hyps |>
      tidytable::left_join(
        features_table |> tidytable::distinct(feature_id, mz),
        by = "feature_id"
      ) |>
      tidytable::left_join(
        feature_m_map |> tidytable::select(feature_id, neutral_mass),
        by = "feature_id"
      ) |>
      tidytable::mutate(
        implied_m = calculate_neutral_mass_from_lookup(
          mzs = as.numeric(mz),
          adducts = adduct,
          adduct_lookup = adduct_lookup
        ),
        compatible = is.na(neutral_mass) |
          abs(implied_m - neutral_mass) <=
            pmax(
              tolerance_ppm * 1e-6 * neutral_mass,
              tolerance_dalton %||% 0
            )
      ) |>
      tidytable::filter(compatible) |>
      tidytable::select(
        feature_id,
        adduct,
        source,
        is_preassigned,
        candidate_adduct_origin
      )
  }

  # (d) evidence-derived adducts from typed-universe inference.
  if (missing(evidence_hypotheses) || is.null(evidence_hypotheses)) {
    evidence_hypotheses <- tidytable::tidytable(
      feature_id = character(),
      adduct = character(),
      source = character(),
      is_preassigned = logical(),
      adduct_support = integer()
    )
  }
  if (!"source" %in% colnames(evidence_hypotheses)) {
    evidence_hypotheses <- evidence_hypotheses |>
      tidytable::mutate(source = "evidence")
  }
  if (!"is_preassigned" %in% colnames(evidence_hypotheses)) {
    evidence_hypotheses <- evidence_hypotheses |>
      tidytable::mutate(is_preassigned = FALSE)
  }
  if (!"adduct_support" %in% colnames(evidence_hypotheses)) {
    evidence_hypotheses <- evidence_hypotheses |>
      tidytable::mutate(adduct_support = 0L)
  }
  if (!"candidate_adduct_origin" %in% colnames(evidence_hypotheses)) {
    evidence_hypotheses <- evidence_hypotheses |>
      tidytable::mutate(candidate_adduct_origin = "supported")
  }

  # (c) baseline - only for nodes still unsupported after evidence/pair/preassigned
  supported_seed <- tidytable::bind_rows(
    pair_hyps,
    evidence_hypotheses |>
      tidytable::distinct(
        feature_id,
        adduct,
        source,
        is_preassigned,
        candidate_adduct_origin
      ) |>
      tidytable::filter(candidate_adduct_origin == "supported"),
    pre_hyps,
    pre_prop_hyps
  ) |>
    tidytable::distinct()

  unsupported_features <- features_table |>
    tidytable::distinct(feature_id, mz) |>
    tidytable::anti_join(
      supported_seed |>
        tidytable::distinct(feature_id),
      by = "feature_id"
    )

  baseline_hyps <- unsupported_features |>
    tidytable::cross_join(
      tidytable::tidytable(adduct = baseline_adducts)
    ) |>
    tidytable::left_join(
      feature_m_map |> tidytable::select(feature_id, neutral_mass),
      by = "feature_id"
    ) |>
    tidytable::mutate(
      implied_m = calculate_neutral_mass_from_lookup(
        mzs = as.numeric(mz),
        adducts = adduct,
        adduct_lookup = adduct_lookup
      ),
      compatible = is.na(neutral_mass) |
        abs(implied_m - neutral_mass) <=
          pmax(tolerance_ppm * 1e-6 * neutral_mass, tolerance_dalton %||% 0),
      source = "baseline",
      is_preassigned = FALSE,
      candidate_adduct_origin = "baseline"
    ) |>
    tidytable::filter(compatible) |>
    tidytable::select(
      feature_id,
      adduct,
      source,
      is_preassigned,
      candidate_adduct_origin
    )

  base_hyps <- tidytable::bind_rows(
    supported_seed,
    baseline_hyps
  ) |>
    tidytable::distinct() |>
    harmonize_adducts(adducts_translations = adducts_translations)

  # Attach support count - compute from CONSISTENT edges only. Neighbors whose
  # intensities co-vary with the feature count as stronger support
  support <- compute_feature_adduct_support(
    consistent_edges,
    covariance_edges = covariance_edges
  )
  base_hyps <- base_hyps |>
    tidytable::left_join(support, by = c("feature_id", "adduct")) |>
    tidytable::mutate(
      is_preassigned = tidytable::if_else(
        is.na(is_preassigned),
        FALSE,
        is_preassigned
      ),
      adduct_support = tidytable::if_else(
        is.na(adduct_support),
        0L,
        as.integer(adduct_support)
      ),
      candidate_adduct_origin = tidytable::if_else(
        is.na(candidate_adduct_origin),
        "supported",
        candidate_adduct_origin
      )
    )

  cluster_hyps <- if (
    all(c("adduct", "adduct_dest") %in% colnames(cluster_edges))
  ) {
    tidytable::bind_rows(
      cluster_edges |>
        tidytable::transmute(
          feature_id,
          adduct,
          source = "cluster",
          is_preassigned = FALSE,
          candidate_adduct_origin = "supported"
        ),
      cluster_edges |>
        tidytable::transmute(
          feature_id = feature_id_dest,
          adduct = adduct_dest,
          source = "cluster",
          is_preassigned = FALSE,
          candidate_adduct_origin = "supported"
        )
    ) |>
      tidytable::distinct()
  } else {
    support_base <- base_hyps |>
      tidytable::filter(candidate_adduct_origin == "supported")
    tidytable::bind_rows(
      propagate_modifier_dest_to_src(
        base_hyps = support_base,
        edges = cluster_edges,
        mod_col = "cluster",
        mod_sign = "-",
        source_label = "cluster"
      ),
      propagate_modifier_src_to_dest(
        base_hyps = support_base,
        edges = cluster_edges,
        mod_col = "cluster",
        mod_sign = "+"
      )
    ) |>
      tidytable::mutate(candidate_adduct_origin = "supported")
  }

  loss_hyps <- if (all(c("adduct", "adduct_dest") %in% colnames(loss_edges))) {
    tidytable::bind_rows(
      loss_edges |>
        tidytable::transmute(
          feature_id,
          adduct,
          source = "loss",
          is_preassigned = FALSE,
          candidate_adduct_origin = "supported",
          loss_term = loss
        ),
      loss_edges |>
        tidytable::transmute(
          feature_id = feature_id_dest,
          adduct = adduct_dest,
          source = "loss",
          is_preassigned = FALSE,
          candidate_adduct_origin = "supported",
          loss_term = loss
        )
    ) |>
      tidytable::distinct()
  } else {
    support_base <- base_hyps |>
      tidytable::filter(candidate_adduct_origin == "supported")
    tidytable::bind_rows(
      propagate_modifier_dest_to_src(
        base_hyps = support_base,
        edges = loss_edges,
        mod_col = "loss",
        mod_sign = "-",
        strip_label = TRUE,
        source_label = "loss"
      ),
      propagate_modifier_src_to_dest(
        base_hyps = support_base,
        edges = loss_edges,
        mod_col = "loss",
        mod_sign = "+",
        strip_label = TRUE,
        source_label = "loss"
      )
    ) |>
      tidytable::mutate(candidate_adduct_origin = "supported")
  }

  all_hyps <- tidytable::bind_rows(
    base_hyps,
    cluster_hyps,
    loss_hyps
  ) |>
    tidytable::mutate(adduct = trimws(adduct)) |>
    tidytable::filter(!is.na(adduct) & nzchar(adduct)) |>
    tidytable::filter(is.na(fastmatch::fmatch(adduct, adducts_forbidden)))

  fea_meta <- features_table |>
    tidytable::distinct(feature_id, mz, rt)
  result <- all_hyps |>
    tidytable::left_join(fea_meta, by = "feature_id") |>
    dedupe_node_hypotheses()

  # Store component membership as attribute for annotation propagation
  attr(result, "component_membership") <- component_membership
  attr(result, "feature_m_map") <- feature_m_map

  result
}

#' Simplify adduct strings produced by cluster/loss expansion while preserving
#' the conceptual split between charge carriers and modifiers.
#'
#' Carrier terms (H/H2/H3/H4N and metal carriers) are kept as explicit tokens.
#' Non-carrier modifiers are aggregated atomically (Hill order) per sign.
#'
#' Example: \[M-H2+CH2O2+Na\]- remains carrier-distinct as \[M-H2+CH2O2+Na\]-.
#' Example: \[M-H2O+H\]+ remains \[M-H2O+H\]+ (and not \[M-HO\]+).
#'
#' Returns the original string unchanged if parsing fails.
#' @keywords internal
.simplify_adduct_after_modifier <- function(adduct_strings) {
  n_in <- length(adduct_strings)
  if (n_in == 0L) {
    return(character(0L))
  }
  valid_mask <- !is.na(adduct_strings) & nzchar(adduct_strings)
  if (!any(valid_mask)) {
    return(adduct_strings)
  }
  u_adducts <- unique(adduct_strings[valid_mask])

  carriers <- c(
    "H",
    "H2",
    "H3",
    "H4N",
    "Na",
    "K",
    "Li",
    "Mg",
    "Ca",
    "Fe",
    "Cu",
    "Zn",
    "Mn",
    "Al"
  )

  hill_formula <- function(counts) {
    counts <- Filter(function(x) x > 0L, counts)
    els <- names(counts)
    ord <- order(
      ifelse(els == "C", 0L, ifelse(els == "H", 1L, 2L)),
      els
    )
    paste0(
      vapply(
        els[ord],
        function(el) {
          n <- as.integer(counts[[el]])
          paste0(el, if (n > 1L) as.character(n) else "")
        },
        character(1L)
      ),
      collapse = ""
    )
  }

  u_simplified <- vapply(
    u_adducts,
    function(adduct) {
      m <- regexec("^\\[(.*)\\](\\d*[+-])$", adduct, perl = TRUE)
      g <- regmatches(adduct, m)[[1L]]
      if (length(g) == 0L) {
        return(adduct)
      }

      inner <- g[[2L]]
      suffix <- g[[3L]]

      mod_start <- regexpr("[+-]\\d*[A-Za-z]", inner, perl = TRUE)
      if (mod_start[[1L]] == -1L) {
        return(adduct)
      }

      core <- substr(inner, 1L, mod_start[[1L]] - 1L)
      mods <- substr(inner, mod_start[[1L]], nchar(inner))

      tok_re <- "([+-])(\\d*)([A-Za-z][A-Za-z0-9]*)"
      tokens <- regmatches(mods, gregexpr(tok_re, mods, perl = TRUE))[[1L]]

      remainder <- gsub(tok_re, "", mods, perl = TRUE)
      if (!identical(remainder, "")) {
        return(adduct)
      }

      has_pos_h4n <- any(grepl("^\\+[0-9]*H4N$", tokens, perl = TRUE))
      has_pos_h <- any(grepl("^\\+[0-9]*H$", tokens, perl = TRUE))

      gcd2 <- function(a, b) {
        while (b != 0L) {
          t <- b
          b <- a %% b
          a <- t
        }
        abs(a)
      }
      gcd_vec <- function(x) {
        Reduce(gcd2, as.integer(abs(x)))
      }

      tg_mat <- stri_match_first_regex(tokens, paste0("^", tok_re, "$"))
      signs <- ifelse(tg_mat[, 2L] == "+", 1L, -1L)
      mults <- ifelse(nzchar(tg_mat[, 3L]), as.integer(tg_mat[, 3L]), 1L)
      formulas <- tg_mat[, 4L]

      is_carrier <- formulas %in%
        carriers |
        ((has_pos_h4n || has_pos_h) & formulas == "H3N")

      carrier_net <- list()
      if (any(is_carrier)) {
        rs_c <- rowsum.default(
          signs[is_carrier] * mults[is_carrier],
          formulas[is_carrier],
          reorder = FALSE
        )
        carrier_net <- as.list(stats::setNames(
          as.integer(rs_c[, 1L]),
          rownames(rs_c)
        ))
      }

      mod_idx <- which(!is_carrier)
      neg_atoms <- list()
      pos_atoms <- list()
      if (length(mod_idx) > 0L) {
        mod_forms <- formulas[mod_idx]
        atoms_list <- lapply(mod_forms, parse_atomic_formula)
        if (any(lengths(atoms_list) == 0L)) {
          return(adduct)
        }
        gs <- vapply(atoms_list, gcd_vec, integer(1L))
        emp_atoms_list <- Map(
          function(a, g) lapply(a, function(v) as.integer(v) %/% g),
          atoms_list,
          gs
        )
        emp_fs <- vapply(emp_atoms_list, hill_formula, character(1L))
        rs_m <- rowsum.default(
          signs[mod_idx] * mults[mod_idx] * gs,
          emp_fs,
          reorder = FALSE
        )
        emp_names <- rownames(rs_m)
        emp_nets <- as.integer(rs_m[, 1L])
        emp_first <- match(emp_names, emp_fs)

        neg_e <- which(emp_nets < 0L)
        if (length(neg_e) > 0L) {
          neg_ea <- emp_atoms_list[emp_first[neg_e]]
          neg_els <- unlist(lapply(neg_ea, names), use.names = FALSE)
          neg_cnts <- unlist(neg_ea, use.names = FALSE) *
            rep.int(abs(emp_nets[neg_e]), lengths(neg_ea))
          rs_neg <- rowsum.default(neg_cnts, neg_els, reorder = FALSE)
          neg_atoms <- as.list(stats::setNames(
            as.integer(rs_neg[, 1L]),
            rownames(rs_neg)
          ))
        }

        pos_e <- which(emp_nets > 0L)
        if (length(pos_e) > 0L) {
          pos_ea <- emp_atoms_list[emp_first[pos_e]]
          pos_els <- unlist(lapply(pos_ea, names), use.names = FALSE)
          pos_cnts <- unlist(pos_ea, use.names = FALSE) *
            rep.int(emp_nets[pos_e], lengths(pos_ea))
          rs_pos <- rowsum.default(pos_cnts, pos_els, reorder = FALSE)
          pos_atoms <- as.list(stats::setNames(
            as.integer(rs_pos[, 1L]),
            rownames(rs_pos)
          ))
        }
      }

      h_keys <- intersect(names(carrier_net), c("H", "H2", "H3"))
      if (length(h_keys) > 0L) {
        h_vals <- unlist(carrier_net[h_keys], use.names = FALSE)
        if (any(h_vals > 0L) && any(h_vals < 0L)) {
          h_mults <- c(H = 1L, H2 = 2L, H3 = 3L)[h_keys]
          total_h <- sum(as.integer(h_vals) * as.integer(h_mults))
          carrier_net[h_keys] <- NULL
          if (total_h != 0L) {
            carrier_net[["H"]] <- total_h
          }
        }
      }

      new_mods <- character()

      # Negative atomic term (loss)
      if (length(neg_atoms) > 0L) {
        f <- hill_formula(neg_atoms)
        if (nzchar(f)) new_mods <- c(new_mods, paste0("-", f))
      }

      # Negative carrier terms
      neg_carriers <- Filter(function(x) x < 0L, carrier_net)
      if (length(neg_carriers) > 0L) {
        nms_neg <- sort(names(neg_carriers))
        ns_neg <- abs(unlist(neg_carriers[nms_neg], use.names = FALSE))
        new_mods <- c(
          new_mods,
          paste0("-", ifelse(ns_neg > 1L, as.character(ns_neg), ""), nms_neg)
        )
      }

      # Positive atomic term (addition)
      if (length(pos_atoms) > 0L) {
        f <- hill_formula(pos_atoms)
        if (nzchar(f)) new_mods <- c(new_mods, paste0("+", f))
      }

      # Positive carrier terms
      pos_carriers <- Filter(function(x) x > 0L, carrier_net)
      if (length(pos_carriers) > 0L) {
        nms_pos <- sort(names(pos_carriers))
        ns_pos <- unlist(pos_carriers[nms_pos], use.names = FALSE)
        new_mods <- c(
          new_mods,
          paste0("+", ifelse(ns_pos > 1L, as.character(ns_pos), ""), nms_pos)
        )
      }

      paste0("[", core, paste0(new_mods, collapse = ""), "]", suffix)
    },
    character(1L),
    USE.NAMES = FALSE
  )

  out <- adduct_strings
  out[valid_mask] <- u_simplified[fastmatch::fmatch(
    adduct_strings[valid_mask],
    u_adducts
  )]
  out
}

.apply_modifier_column_fast <- function(adducts, mods, mod_sign) {
  n <- length(adducts)
  if (n == 0L) {
    return(character(0L))
  }
  u_a <- unique(adducts)
  u_m <- unique(mods)
  a_id <- match(adducts, u_a)
  m_id <- match(mods, u_m)
  n_m <- max(1L, length(u_m))
  pair_code <- as.double(a_id - 1L) * n_m + m_id
  u_code <- unique(pair_code)
  first_pos <- fastmatch::fmatch(u_code, pair_code)

  u_a_sub <- adducts[first_pos]
  u_m_sub <- mods[first_pos]
  base_part <- sub("M(?![a-z]).*", "M", u_a_sub, perl = TRUE)
  char_part <- sub(".*M(?![a-z])", "", u_a_sub, perl = TRUE)
  u_simp <- .simplify_adduct_after_modifier(
    paste0(base_part, mod_sign, u_m_sub, char_part)
  )
  u_simp[fastmatch::fmatch(pair_code, u_code)]
}

#' Propagate a modifier from the SOURCE feature's hypothesis to the DESTINATION
#' feature, creating a new hypothesis for the destination.
#'
#' Unlike `expand_with_modifier` (which appends the modifier to the destination
#' feature's EXISTING hypothesis), this function takes the SOURCE feature's
#' hypothesis, appends the modifier, and assigns the result to the DESTINATION.
#'
#' This handles multi-hop cases such as:
#'   source \[M-H2+Na\]-  + cluster CH2O2  →  destination gets \[M+CO2+Na\]-
#'
#' @keywords internal
propagate_modifier_src_to_dest <- function(
  base_hyps,
  edges,
  mod_col,
  mod_sign,
  strip_label = FALSE,
  source_label = NULL
) {
  if (nrow(edges) == 0L || nrow(base_hyps) == 0L) {
    return(base_hyps[0L, ])
  }

  src_dest <- edges |>
    tidytable::distinct(
      feature_id,
      feature_id_dest,
      !!as.name(mod_col)
    )

  if (strip_label) {
    src_dest <- src_dest |>
      tidytable::mutate(
        !!as.name(mod_col) := gsub(" .*", "", !!as.name(mod_col))
      )
  }

  if (is.null(source_label)) {
    source_label <- if (mod_sign == "+") "cluster" else "loss"
  }

  # Join on the SOURCE feature, then rename feature_id_dest → feature_id
  joined <- base_hyps |>
    tidytable::inner_join(src_dest, by = "feature_id")

  # Recovering a precursor from a product over a loss edge (+loss) is only
  # valid when the source (product) adduct explicitly carries that loss.
  if (
    nrow(joined) > 0L &&
      identical(mod_col, "loss") &&
      identical(mod_sign, "+")
  ) {
    pair_key <- paste(joined$adduct, joined[[mod_col]], sep = "\r")
    u_idx <- which(!duplicated(pair_key))
    u_keep <- mapply(
      adduct_has_explicit_loss,
      joined$adduct[u_idx],
      joined[[mod_col]][u_idx],
      USE.NAMES = FALSE
    )
    keep_loss <- u_keep[fastmatch::fmatch(pair_key, pair_key[u_idx])]
    joined <- joined[which(keep_loss), , drop = FALSE]
  }
  if (nrow(joined) == 0L) {
    return(base_hyps[0L, ])
  }

  out <- joined |>
    tidytable::mutate(
      feature_id = feature_id_dest,
      adduct = .apply_modifier_column_fast(
        adduct,
        !!as.name(mod_col),
        mod_sign
      ),
      source = source_label,
      feature_id_dest = NULL
    )

  if (identical(mod_col, "loss")) {
    out <- out |>
      tidytable::mutate(loss_term = as.character(!!as.name(mod_col)))
  }

  out |>
    tidytable::select(-tidyselect::all_of(mod_col)) |>
    harmonize_adducts(adducts_translations = adducts_translations)
}

#' Propagate a modifier from the DESTINATION feature's hypothesis back to the
#' SOURCE feature.
#'
#' This is the natural complement of `propagate_modifier_src_to_dest()` for
#' edge types whose chemically informative endpoint sits in `feature_id_dest`,
#' such as neutral-loss edges where the precursor is the higher-m/z node.
#' @keywords internal
propagate_modifier_dest_to_src <- function(
  base_hyps,
  edges,
  mod_col,
  mod_sign,
  strip_label = FALSE,
  source_label = NULL
) {
  if (nrow(edges) == 0L || nrow(base_hyps) == 0L) {
    return(base_hyps[0L, ])
  }

  if (is.null(source_label)) {
    source_label <- if (mod_sign == "+") "cluster" else "loss"
  }

  dest_src <- edges |>
    tidytable::distinct(
      feature_id,
      feature_id_dest,
      !!as.name(mod_col)
    ) |>
    tidytable::rename(
      feature_id_src = feature_id,
      feature_id = feature_id_dest
    )

  if (strip_label) {
    dest_src <- dest_src |>
      tidytable::mutate(
        !!as.name(mod_col) := gsub(" .*", "", !!as.name(mod_col))
      )
  }

  joined <- base_hyps |>
    tidytable::inner_join(dest_src, by = "feature_id")

  # Recovering a lower-mz feature from a cluster-bearing destination over a
  # cluster edge (-cluster) is only valid when the destination adduct explicitly
  # carries that cluster.
  if (
    nrow(joined) > 0L &&
      identical(mod_col, "cluster") &&
      identical(mod_sign, "-")
  ) {
    pair_key <- paste(joined$adduct, joined[[mod_col]], sep = "\r")
    u_idx <- which(!duplicated(pair_key))
    u_keep <- mapply(
      adduct_has_explicit_cluster,
      joined$adduct[u_idx],
      joined[[mod_col]][u_idx],
      USE.NAMES = FALSE
    )
    keep_clu <- u_keep[fastmatch::fmatch(pair_key, pair_key[u_idx])]
    joined <- joined[which(keep_clu), , drop = FALSE]
  }
  if (nrow(joined) == 0L) {
    return(base_hyps[0L, ])
  }

  out <- joined |>
    tidytable::mutate(
      feature_id = feature_id_src,
      adduct = .apply_modifier_column_fast(
        adduct,
        !!as.name(mod_col),
        mod_sign
      ),
      source = source_label,
      feature_id_src = NULL
    )

  if (identical(mod_col, "loss")) {
    out <- out |>
      tidytable::mutate(loss_term = as.character(!!as.name(mod_col)))
  }

  out |>
    tidytable::select(-tidyselect::all_of(mod_col)) |>
    harmonize_adducts(adducts_translations = adducts_translations)
}

#' Add +mod or -mod suffix to existing adduct strings on selected features
#' @keywords internal
expand_with_modifier <- function(
  base_hyps,
  edges,
  edge_feature_col,
  mod_col,
  mod_sign,
  strip_label = FALSE
) {
  if (nrow(edges) == 0L || nrow(base_hyps) == 0L) {
    return(base_hyps[0L, ])
  }

  targets <- edges |>
    tidytable::distinct(
      feature_id = !!as.name(edge_feature_col),
      !!as.name(mod_col)
    )

  if (strip_label) {
    targets <- targets |>
      tidytable::mutate(
        !!as.name(mod_col) := gsub(" .*", "", !!as.name(mod_col))
      )
  }

  out <- base_hyps |>
    tidytable::inner_join(targets, by = "feature_id") |>
    tidytable::mutate(
      adduct = .apply_modifier_column_fast(
        adduct,
        !!as.name(mod_col),
        mod_sign
      ),
      source = if (mod_sign == "+") "cluster" else "loss"
    )

  if (identical(mod_col, "loss")) {
    out <- out |>
      tidytable::mutate(loss_term = as.character(!!as.name(mod_col)))
  }

  out |>
    tidytable::select(-tidyselect::all_of(mod_col)) |>
    harmonize_adducts(adducts_translations = adducts_translations)
}

#' Resolve duplicate hypotheses for the same (feature, adduct), keeping the
#' highest-priority source and the maximum support count.
#'
#' Source priority (lower = higher priority):
#'   pair < cluster/loss < preassigned < baseline < multi
#' @keywords internal
dedupe_node_hypotheses <- function(node_hypotheses) {
  n_rows <- nrow(node_hypotheses)
  if (n_rows == 0L) {
    return(node_hypotheses)
  }
  is_pre <- node_hypotheses$is_preassigned
  if (is.null(is_pre)) {
    is_pre <- logical(n_rows)
  } else {
    is_pre <- as.logical(is_pre)
    is_pre[is.na(is_pre)] <- FALSE
  }
  ranks <- c(
    pair = 1L,
    evidence = 1L,
    cluster = 2L,
    loss = 2L,
    preassigned = 3L,
    preassigned_propagated = 3L,
    baseline = 4L,
    multi = 5L
  )
  src_rank <- unname(ranks[as.character(node_hypotheses$source)])
  src_rank[is.na(src_rank)] <- 99L

  add_supp <- as.integer(node_hypotheses$adduct_support)
  add_supp[is.na(add_supp)] <- 0L

  orig <- if ("candidate_adduct_origin" %in% colnames(node_hypotheses)) {
    as.character(node_hypotheses$candidate_adduct_origin)
  } else {
    rep("supported", n_rows)
  }
  orig[is.na(orig)] <- "supported"
  orig_code <- rep(3L, n_rows)
  orig_code[orig == "supported_weak"] <- 2L
  orig_code[orig == "supported"] <- 1L

  f_col <- node_hypotheses$feature_id
  a_col <- node_hypotheses$adduct
  u_f <- unique(f_col)
  u_a <- unique(a_col)
  f_id <- match(f_col, u_f)
  a_id <- match(a_col, u_a)
  n_a <- length(u_a)
  pair_key <- as.double(f_id - 1L) * n_a + a_id

  ord <- order(src_rank, -add_supp, method = "radix")
  pair_key_ord <- pair_key[ord]
  is_dup <- duplicated(pair_key_ord)

  if (any(is_dup)) {
    kept_pos <- which(!is_dup)
    dup_pos <- which(is_dup)
    win_idx <- ord[kept_pos]
    dup_idx <- ord[dup_pos]
    kept_keys <- pair_key_ord[kept_pos]
    kept_pre <- is_pre[win_idx]
    kept_orig <- orig_code[win_idx]

    dup_pre <- dup_idx[is_pre[dup_idx]]
    if (length(dup_pre) > 0L) {
      m_pre <- fastmatch::fmatch(unique(pair_key[dup_pre]), kept_keys)
      kept_pre[m_pre[!is.na(m_pre)]] <- TRUE
    }
    dup_1 <- dup_idx[orig_code[dup_idx] == 1L]
    if (length(dup_1) > 0L) {
      m_1 <- fastmatch::fmatch(unique(pair_key[dup_1]), kept_keys)
      kept_orig[m_1[!is.na(m_1)]] <- 1L
    }
    dup_2 <- dup_idx[orig_code[dup_idx] == 2L]
    if (length(dup_2) > 0L) {
      m_2 <- fastmatch::fmatch(unique(pair_key[dup_2]), kept_keys)
      m_2 <- m_2[!is.na(m_2)]
      kept_orig[m_2] <- pmin.int(kept_orig[m_2], 2L)
    }
    add_supp <- add_supp[win_idx]
  } else {
    win_idx <- ord
    kept_pre <- is_pre[win_idx]
    kept_orig <- orig_code[win_idx]
    add_supp <- add_supp[win_idx]
  }

  cols <- lapply(as.list(node_hypotheses), `[`, win_idx)
  cols$is_preassigned <- kept_pre
  cols$adduct_support <- add_supp
  cols$candidate_adduct_origin <- c(
    "supported",
    "supported_weak",
    "baseline"
  )[kept_orig]
  tidytable::as_tidytable(cols)
}

#' Discover additional ion-species hypotheses/edges via typed-universe evidence
#'
#' This closes a key blind spot of pure delta-edge matching: species such as
#' multicharged ions (`[M+2H]2+`) or multimers can be supported by neutral-mass
#' agreement even when no explicit monocharged pair delta is available.
#' @keywords internal
discover_evidence_adduct_signal <- function(
  features_table,
  universe = NULL,
  adducts,
  candidate_adducts = NULL,
  clusters,
  neutral_losses,
  ms_mode,
  tolerance_ppm,
  tolerance_dalton = NULL,
  tolerance_rt,
  exact_masses
) {
  invisible(tolerance_dalton)
  invisible(clusters)
  invisible(neutral_losses)
  empty_hyp <- tidytable::tidytable(
    feature_id = character(),
    adduct = character(),
    source = character(),
    is_preassigned = logical(),
    adduct_support = integer()
  )
  empty_edges <- tidytable::tidytable(
    feature_id = character(),
    adduct = character(),
    feature_id_dest = character(),
    adduct_dest = character()
  )
  if (nrow(features_table) == 0L || length(adducts) == 0L) {
    return(list(hypotheses = empty_hyp, edges = empty_edges))
  }

  if (is.null(candidate_adducts)) {
    candidate_adducts <- adducts
  }
  candidate_adducts <- unique(as.character(candidate_adducts))
  candidate_adducts <- candidate_adducts[
    !is.na(candidate_adducts) & nzchar(candidate_adducts)
  ]
  if (length(candidate_adducts) == 0L) {
    return(list(hypotheses = empty_hyp, edges = empty_edges))
  }

  if (is.null(universe)) {
    universe <- build_adduct_universe(
      adducts_list = list(pos = candidate_adducts, neg = candidate_adducts),
      clusters_list = list(pos = character(), neg = character()),
      neutral_losses_list = character(),
      polarity = ms_mode
    )
  }
  if (nrow(universe) == 0L) {
    return(list(hypotheses = empty_hyp, edges = empty_edges))
  }

  feats_min <- features_table |>
    tidytable::distinct(feature_id, rt, mz, sample)
  hyps <- build_evidence_supported_hypotheses(
    df_fea_min = feats_min,
    universe = universe,
    tolerance_ppm = tolerance_ppm,
    tolerance_dalton = tolerance_dalton,
    tolerance_rt = tolerance_rt,
    ms_mode = ms_mode,
    exact_masses = exact_masses,
    max_hypotheses_per_feature = Inf
  )

  supported <- hyps |>
    tidytable::filter(candidate_adduct_origin == "supported")
  evidence_hyp <- supported |>
    tidytable::transmute(
      feature_id,
      adduct,
      source = "evidence",
      is_preassigned = FALSE,
      candidate_adduct_origin = "supported",
      adduct_support = as.integer(evidence_count)
    ) |>
    tidytable::distinct()

  evidence_edges <- build_evidence_edges(
    supported,
    tolerance_ppm = tolerance_ppm,
    tolerance_dalton = tolerance_dalton
  ) |>
    tidytable::distinct()
  list(hypotheses = evidence_hyp, edges = evidence_edges)
}

#' Keep modifier-bearing evidence hypotheses only when directly pairwise-backed
#'
#' Evidence is useful for recovering multicharged / multimer states that legacy
#' delta matching cannot see directly. In contrast, solvent/salt/loss-bearing
#' states are prone to overexpansion when inferred from evidence alone, so they
#' must also be supported by the direct pairwise mass-difference graph.
#' @keywords internal
filter_modifier_evidence_by_pairwise_support <- function(
  evidence_signal,
  universe,
  adduct_edges,
  cluster_edges,
  loss_edges,
  baseline_adduct,
  features_table
) {
  if (
    is.null(evidence_signal) ||
      !is.list(evidence_signal) ||
      nrow(evidence_signal$hypotheses) == 0L
  ) {
    return(evidence_signal)
  }
  invisible(universe)

  adduct_requires_pairwise_map <- tidytable::tidytable(
    adduct = unique(evidence_signal$hypotheses$adduct)
  ) |>
    tidytable::mutate(
      evidence_requires_pairwise = vapply(
        X = adduct,
        FUN = evidence_adduct_requires_pairwise_support,
        FUN.VALUE = logical(1L)
      )
    )

  hyps <- evidence_signal$hypotheses |>
    tidytable::left_join(adduct_requires_pairwise_map, by = "adduct") |>
    tidytable::mutate(
      evidence_requires_pairwise = tidytable::coalesce(
        evidence_requires_pairwise,
        FALSE
      )
    )
  if (!any(hyps$evidence_requires_pairwise)) {
    return(evidence_signal)
  }

  pairwise_supported <- collect_node_adduct_hypotheses(
    adduct_edges = adduct_edges,
    cluster_edges = cluster_edges,
    loss_edges = loss_edges,
    evidence_hypotheses = tidytable::tidytable(
      feature_id = character(),
      adduct = character(),
      source = character(),
      is_preassigned = logical(),
      adduct_support = integer()
    ),
    preassigned = tidytable::tidytable(
      feature_id = character(),
      adduct = character()
    ),
    baseline_adducts = baseline_adduct,
    features_table = features_table
  ) |>
    tidytable::filter(source %in% c("pair", "cluster", "loss")) |>
    tidytable::distinct(feature_id, adduct)

  state_map <- build_adduct_state_key_map(unique(c(
    hyps$adduct,
    pairwise_supported$adduct
  ))) |>
    tidytable::rename(adduct_state_key = state_key)

  pairwise_keys <- pairwise_supported |>
    tidytable::left_join(state_map, by = "adduct") |>
    tidytable::mutate(
      adduct_state_key = tidytable::coalesce(adduct_state_key, adduct)
    ) |>
    tidytable::distinct(feature_id, adduct_state_key)

  kept_hyps <- hyps |>
    tidytable::left_join(state_map, by = "adduct") |>
    tidytable::mutate(
      adduct_state_key = tidytable::coalesce(adduct_state_key, adduct)
    ) |>
    tidytable::left_join(
      pairwise_keys |>
        tidytable::mutate(pairwise_supported = TRUE),
      by = c("feature_id", "adduct_state_key")
    ) |>
    tidytable::mutate(
      pairwise_supported = tidytable::if_else(
        is.na(pairwise_supported),
        FALSE,
        pairwise_supported
      ),
      keep = !evidence_requires_pairwise | pairwise_supported
    )

  dropped_hyps <- kept_hyps |>
    tidytable::filter(!keep)
  if (nrow(dropped_hyps) > 0L) {
    log_info(
      paste0(
        "Pairwise-support filter removed %d modifier-bearing evidence ",
        "hypothesis row(s) lacking direct adduct/cluster/loss support."
      ),
      nrow(dropped_hyps)
    )
  }

  kept_hyps <- kept_hyps |>
    tidytable::filter(keep) |>
    tidytable::select(
      -evidence_requires_pairwise,
      -adduct_state_key,
      -pairwise_supported,
      -keep
    )

  if (nrow(evidence_signal$edges) == 0L) {
    return(list(hypotheses = kept_hyps, edges = evidence_signal$edges))
  }

  kept_state_keys <- kept_hyps |>
    tidytable::left_join(state_map, by = "adduct") |>
    tidytable::mutate(
      adduct_state_key = tidytable::coalesce(adduct_state_key, adduct)
    ) |>
    tidytable::distinct(feature_id, adduct_state_key)

  kept_edges <- evidence_signal$edges |>
    tidytable::left_join(
      state_map |>
        tidytable::rename(adduct_left_key = adduct_state_key),
      by = "adduct"
    ) |>
    tidytable::left_join(
      state_map |>
        tidytable::rename(
          adduct_dest = adduct,
          adduct_right_key = adduct_state_key
        ),
      by = "adduct_dest"
    ) |>
    tidytable::mutate(
      adduct_left_key = tidytable::coalesce(adduct_left_key, adduct),
      adduct_right_key = tidytable::coalesce(adduct_right_key, adduct_dest)
    ) |>
    tidytable::inner_join(
      kept_state_keys |>
        tidytable::rename(adduct_left_key = adduct_state_key),
      by = c("feature_id", "adduct_left_key")
    ) |>
    tidytable::inner_join(
      kept_state_keys |>
        tidytable::rename(
          feature_id_dest = feature_id,
          adduct_right_key = adduct_state_key
        ),
      by = c("feature_id_dest", "adduct_right_key")
    ) |>
    tidytable::select(feature_id, adduct, feature_id_dest, adduct_dest) |>
    tidytable::distinct()

  list(hypotheses = kept_hyps, edges = kept_edges)
}

#' Detect whether an evidence-derived adduct should require direct pairwise support
#'
#' Carrier-only states such as `[M+H]+`, `[M+Na]+`, `[M+2H]2+`, `[2M+H]+`, or
#' `[M+Fe-H2]+` remain eligible for evidence-only recovery. States containing
#' non-carrier modifiers (e.g. ACN/DMSO/NaCl/H2O losses) are considered
#' modifier-bearing and must also be backed by the pairwise mass-difference graph.
#' @keywords internal
evidence_adduct_requires_pairwise_support <- function(adduct) {
  if (is.null(adduct) || is.na(adduct) || !nzchar(adduct)) {
    return(FALSE)
  }
  adduct_norm <- tryCatch(
    normalize_adduct_string(adduct),
    error = function(.err) {
      invisible(.err)
      as.character(adduct)
    }
  )
  inner <- sub("^\\[(.*)\\][0-9]*[+-]+$", "\\1", adduct_norm, perl = TRUE)
  mods <- sub("^[0-9]*M[0-9]*", "", inner, perl = TRUE)
  if (!nzchar(mods)) {
    return(FALSE)
  }
  parsed <- parse_modification_components(mods)
  if (
    is.null(parsed) || !isTRUE(parsed$valid) || length(parsed$elements) == 0L
  ) {
    return(FALSE)
  }
  elems <- parsed$elements
  carrier_like <- elems %in%
    names(CARRIER_INTRINSIC_CHARGE) |
    grepl("^H[0-9]*$", elems)
  !all(carrier_like)
}

#' Generate multi-adduct hypotheses constrained by node-inferred neutral masses
#'
#' Expands `(feature, adduct)` only for multi adducts whose implied neutral mass
#' is within `tolerance_ppm` of at least one neutral mass already inferred for
#' that same feature from non-multi hypotheses.
#' @keywords internal
generate_multi_hypotheses_from_node_masses <- function(
  node_hypotheses,
  multi_adducts,
  adduct_lookup = NULL,
  tolerance_ppm,
  tolerance_dalton
) {
  empty_out <- tidytable::tidytable(
    feature_id = character(),
    adduct = character(),
    source = character(),
    is_preassigned = logical(),
    adduct_support = integer(),
    mz = numeric(),
    rt = numeric(),
    mass = numeric()
  )
  if (nrow(node_hypotheses) == 0L || nrow(multi_adducts) == 0L) {
    return(empty_out)
  }

  nh_src <- node_hypotheses$source
  nh_mass <- node_hypotheses$mass
  keep_bm <- which(
    (is.na(nh_src) | nh_src != "multi") &
      !is.na(nh_mass) &
      is.finite(nh_mass) &
      nh_mass > 0
  )
  if (length(keep_bm) == 0L) {
    return(empty_out)
  }
  bm_fids <- node_hypotheses$feature_id[keep_bm]
  bm_masses <- nh_mass[keep_bm]
  keep_u_bm <- !duplicated(paste(bm_fids, bm_masses, sep = "\r"))
  bm_fids <- bm_fids[keep_u_bm]
  bm_masses <- bm_masses[keep_u_bm]

  u_ma <- unique(multi_adducts$adduct)
  n_ma <- length(u_ma)

  nh_fids <- node_hypotheses$feature_id
  nh_mz <- node_hypotheses$mz
  nh_rt <- node_hypotheses$rt
  first_f <- !duplicated(nh_fids)
  fm_fids <- nh_fids[first_f]
  fm_mz <- nh_mz[first_f]
  fm_rt <- nh_rt[first_f]

  in_bm <- which(!is.na(fastmatch::fmatch(fm_fids, bm_fids)))
  fm_fids <- fm_fids[in_bm]
  fm_mz <- fm_mz[in_bm]
  fm_rt <- fm_rt[in_bm]
  fm_mz_num <- as.numeric(fm_mz)
  n_fm <- length(fm_fids)

  all_m <- calculate_neutral_mass_from_lookup(
    mzs = rep(fm_mz_num, each = n_ma),
    adducts = rep(u_ma, times = n_fm),
    adduct_lookup = adduct_lookup
  )
  bm_fm_id <- fastmatch::fmatch(bm_fids, fm_fids)
  ord_bm <- order(bm_fm_id, method = "radix")
  bm_fm_id <- bm_fm_id[ord_bm]
  bm_masses <- bm_masses[ord_bm]

  n_bm <- length(bm_fm_id)
  cand_idx <- rep((bm_fm_id - 1L) * n_ma, each = n_ma) +
    rep.int(seq_len(n_ma), n_bm)
  cand_m <- all_m[cand_idx]
  ref_m <- rep(bm_masses, each = n_ma)

  valid_m <- !is.na(cand_m) & is.finite(cand_m) & cand_m > 0
  diff <- abs(cand_m - ref_m)
  ppm <- diff * 1e6 / ref_m
  ppm_pass <- is.finite(ppm) & ppm <= tolerance_ppm
  dalton_pass <- if (is.null(tolerance_dalton)) {
    FALSE
  } else {
    !is.na(diff) & diff <= tolerance_dalton
  }
  pass <- which(valid_m & (ppm_pass | dalton_pass))
  if (length(pass) == 0L) {
    return(empty_out)
  }
  hit_cand_idx <- sort(unique(cand_idx[pass]))
  hit_fm <- (hit_cand_idx - 1L) %/% n_ma + 1L
  hit_ma <- (hit_cand_idx - 1L) %% n_ma + 1L

  tidytable::tidytable(
    feature_id = fm_fids[hit_fm],
    adduct = u_ma[hit_ma],
    source = "multi",
    adduct_support = 0L,
    mz = fm_mz[hit_fm],
    rt = fm_rt[hit_fm],
    mass = all_m[hit_cand_idx]
  ) |>
    tidytable::distinct()
}

#' Match candidate (feature, adduct, mass) rows against the library
#' @keywords internal
match_candidates_to_library <- function(
  candidates,
  library_em,
  tolerance_ppm,
  tolerance_dalton
) {
  invisible(c(tolerance_ppm, tolerance_dalton))
  empty_out <- tidytable::tidytable(
    feature_id = character(),
    rt = numeric(),
    mz = numeric(),
    adduct = character(),
    mass = numeric(),
    source = character(),
    is_preassigned = logical(),
    candidate_adduct_origin = character(),
    adduct_support = integer(),
    loss_term = character(),
    structure_exact_mass = numeric(),
    error_mz = numeric()
  )
  if (nrow(candidates) == 0L || nrow(library_em) == 0L) {
    return(empty_out)
  }

  # Ensure all required columns exist (single pass)
  has_ppm <- !is.null(tolerance_ppm) && is.finite(tolerance_ppm)
  has_dalton <- !is.null(tolerance_dalton) && is.finite(tolerance_dalton)
  has_loss_term <- "loss_term" %in% colnames(candidates)
  has_is_preassigned <- "is_preassigned" %in% colnames(candidates)
  has_origin <- "candidate_adduct_origin" %in% colnames(candidates)

  if (!has_is_preassigned) {
    candidates <- candidates |>
      tidytable::mutate(is_preassigned = FALSE)
  }
  if (!has_origin) {
    candidates <- candidates |>
      tidytable::mutate(candidate_adduct_origin = "supported")
  }
  if (!has_loss_term) {
    candidates <- candidates |>
      tidytable::mutate(loss_term = NA_character_)
  }

  cand_t <- tidytable::as_tidytable(candidates)[,
    .(
      src_feature_id = feature_id,
      src_rt = rt,
      src_mz = mz,
      src_adduct = adduct,
      src_mass = mass,
      src_mass_out = mass,
      src_source = source,
      src_is_preassigned = is_preassigned,
      src_origin = candidate_adduct_origin,
      src_support = adduct_support,
      src_loss_term = loss_term
    )
  ]

  em_t <- tidytable::as_tidytable(library_em)[,
    .(value_min, value_max, exact_mass)
  ]

  # Single efficient non-equi join with direct output formatting
  em_t[
    cand_t,
    on = .(value_min <= src_mass, value_max >= src_mass),
    nomatch = NA_real_,
    allow.cartesian = TRUE
  ][
    !is.na(exact_mass),
    .(
      feature_id = src_feature_id,
      rt = src_rt,
      mz = src_mz,
      adduct = src_adduct,
      mass = src_mass_out,
      source = src_source,
      is_preassigned = src_is_preassigned,
      candidate_adduct_origin = src_origin,
      adduct_support = src_support,
      loss_term = src_loss_term,
      structure_exact_mass = exact_mass,
      error_mz = exact_mass - src_mass_out
    )
  ] |>
    tidytable::as_tidytable() |>
    tidytable::mutate(
      .abs_error = abs(as.numeric(error_mz)),
      # Same denominator as the library window (structure exact mass)
      .ppm_error = .abs_error * 1e6 / as.numeric(structure_exact_mass),
      .ppm_ok = if (has_ppm) {
        is.finite(.ppm_error) & .ppm_error <= as.numeric(tolerance_ppm)
      } else {
        !has_dalton
      },
      .dalton_ok = if (has_dalton) {
        is.finite(.abs_error) & .abs_error <= as.numeric(tolerance_dalton)
      } else {
        !has_ppm
      }
    ) |>
    # A match within EITHER tolerance is accepted, i.e. the effective window
    # is max(ppm, Da), the same window used to pre-filter the library in
    # annotate_masses_features.R. With the defaults (10 ppm, 0.0025 Da) the
    # Da floor applies below m/z 250 and the ppm window above it.
    tidytable::filter(.ppm_ok | .dalton_ok) |>
    tidytable::select(-.abs_error, -.ppm_error, -.ppm_ok, -.dalton_ok) |>
    tidytable::distinct()
}

#' Network-consensus pruning of multi-candidate annotations.
#'
#' When a feature has several library hits, drop only those whose adduct has
#' zero adduct-graph support, *provided* at least one supported alternative
#' survives. Ties (both supported, both unsupported) are kept. Drops are
#' logged.
#'
#' Pre-assigned and baseline hypotheses are always kept regardless of support,
#' so a wrong pre-assignment cannot silently block annotation.
#' @keywords internal
prune_candidates_by_network_consensus <- function(
  matched,
  adduct_edges,
  baseline_adduct = NULL
) {
  n_in <- nrow(matched)
  if (n_in == 0L) {
    return(matched)
  }
  if (!"is_preassigned" %in% colnames(matched)) {
    matched$is_preassigned <- rep(FALSE, n_in)
  }
  state_map <- build_adduct_state_key_map(unique(c(
    matched$adduct,
    adduct_edges$adduct,
    adduct_edges$adduct_dest
  )))
  sm_add <- as.character(state_map$adduct)
  sm_key <- as.character(state_map$state_key)
  sm_key[is.na(sm_key)] <- sm_add[is.na(sm_key)]

  supp_raw <- compute_feature_adduct_support(adduct_edges)
  net_support <- integer(n_in)
  if (nrow(supp_raw) > 0L) {
    s_m <- fastmatch::fmatch(supp_raw$adduct, sm_add)
    supp_raw$adduct_state_key <- ifelse(
      is.na(s_m),
      as.character(supp_raw$adduct),
      sm_key[s_m]
    )
    support <- supp_raw |>
      tidytable::summarize(
        net_support = as.integer(max(adduct_support, na.rm = TRUE)),
        .by = c(feature_id, adduct_state_key)
      )
    in_supp_f <- !is.na(fastmatch::fmatch(
      matched$feature_id,
      support$feature_id
    ))
    if (any(in_supp_f)) {
      idx_sf <- which(in_supp_f)
      m_add_sf <- fastmatch::fmatch(matched$adduct[idx_sf], sm_add)
      row_sk_sf <- ifelse(
        is.na(m_add_sf),
        as.character(matched$adduct[idx_sf]),
        sm_key[m_add_sf]
      )
      supp_keys <- paste(
        support$feature_id,
        support$adduct_state_key,
        sep = "\r"
      )
      m_supp <- fastmatch::fmatch(
        paste(matched$feature_id[idx_sf], row_sk_sf, sep = "\r"),
        supp_keys
      )
      valid_s <- !is.na(m_supp)
      net_support[idx_sf[valid_s]] <- as.integer(
        support$net_support[m_supp[valid_s]]
      )
    }
  }

  f_col <- matched$feature_id
  u_f <- unique(f_col)
  n_f <- length(u_f)
  f_id <- match(f_col, u_f)

  # Feature has max_support == 0L iff no row for that feature has net_support > 0L
  feat_has_pos_supp <- logical(n_f)
  feat_has_pos_supp[f_id[net_support > 0L]] <- TRUE
  max_support_zero <- !feat_has_pos_supp[f_id]

  # Feature has >1 distinct adduct iff some row has adduct != first adduct of feature
  add_col <- matched$adduct
  u_a <- unique(add_col)
  a_id <- match(add_col, u_a)
  first_idx <- which(!duplicated(f_id))
  first_a_per_f <- integer(n_f)
  first_a_per_f[f_id[first_idx]] <- a_id[first_idx]
  feat_multi_cand <- logical(n_f)
  feat_multi_cand[f_id[a_id != first_a_per_f[f_id]]] <- TRUE
  multi_candidates <- feat_multi_cand[f_id]

  # Flag features that have at least one non-"multi" library hit
  src_col <- as.character(matched$source)
  feat_has_non_multi <- logical(n_f)
  feat_has_non_multi[f_id[!is.na(src_col) & src_col != "multi"]] <- TRUE
  has_non_multi <- feat_has_non_multi[f_id]

  is_pre <- as.logical(matched$is_preassigned)
  is_pre[is.na(is_pre)] <- FALSE

  baseline_match <- if (is.null(baseline_adduct)) {
    logical(n_in)
  } else {
    !is.na(add_col) & add_col == baseline_adduct
  }
  feat_has_baseline_match <- logical(n_f)
  feat_has_baseline_match[f_id[baseline_match]] <- TRUE
  has_baseline_match <- feat_has_baseline_match[f_id]
  is_non_primary_baseline <- !baseline_match &
    !is.na(src_col) &
    src_col == "baseline" &
    has_baseline_match

  is_safe <- baseline_match |
    is_pre |
    (!is.na(src_col) &
      src_col %in% c("preassigned", "baseline") &
      !is_non_primary_baseline) |
    (net_support > 0L) |
    (max_support_zero &
      !is_non_primary_baseline &
      !(!is.na(src_col) & src_col == "multi" & has_non_multi))

  drop <- !is_safe & multi_candidates

  if (any(drop)) {
    drop_idx <- which(drop)
    n_a <- max(1L, length(u_a))
    pair_code <- as.double(f_id[drop_idx] - 1L) * n_a + a_id[drop_idx]
    n_unique <- sum(!duplicated(pair_code))
    log_info(
      paste0(
        "Network-consensus pruning dropped %d (feature, adduct) ",
        "candidate(s) with zero adduct-graph support when a supported ",
        "alternative existed."
      ),
      n_unique
    )
    keep_idx <- which(!drop)
    return(tidytable::as_tidytable(lapply(as.list(matched), `[`, keep_idx)))
  }

  tidytable::as_tidytable(matched)
}

#' Propagate annotations across M-consistent cliques
#'
#' When a feature in an M-consistent clique receives a library match, propagate
#' that annotation to all other features in the same clique. This ensures that
#' if one feature is confidently annotated to a structure, the same structure is
#' suggested for all linked features with the same neutral mass.
#'
#' @include logs_utils.R
#' @include adduct_universe.R
#' @include adduct_universe_formula.R
#' @include adduct_universe_matching.R
#' @include mass_evidence.R
#' @include harmonize_adducts.R
#' @include parse_adduct.R
#' @include annotate_masses_consistency.R
#' @include annotate_masses_features.R
#' @include mass_evidence_hypothesis.R
#' @param annotations Annotation table with structure_exact_mass, feature_id, etc.
#' @param node_hypotheses Node hypotheses table (should have component_membership
#'   and feature_m_map as attributes)
#'
#' @return Updated annotation table with propagated rows
#'
#' @keywords internal
propagate_annotations_across_m_cliques <- function(
  annotations,
  node_hypotheses,
  tolerance_ppm = 10,
  tolerance_dalton = 0.0025
) {
  if (nrow(annotations) == 0L) {
    return(annotations)
  }

  component_membership <- attr(node_hypotheses, "component_membership")
  feature_m_map <- attr(node_hypotheses, "feature_m_map")

  if (is.null(component_membership) || nrow(component_membership) == 0L) {
    return(annotations)
  }

  # Only components with >= 2 distinct features can propagate across features
  comp_mem_u <- component_membership |>
    tidytable::distinct(feature_id, component_id) |>
    tidytable::filter(!is.na(component_id))
  dup_comps <- unique(comp_mem_u$component_id[duplicated(
    comp_mem_u$component_id
  )])
  if (length(dup_comps) == 0L) {
    return(annotations)
  }
  comp_mem_multi <- comp_mem_u[
    !is.na(fastmatch::fmatch(comp_mem_u$component_id, dup_comps)),
  ]
  multi_features <- unique(comp_mem_multi$feature_id)

  # Pre-filter annotations to multi-feature components with matched structures
  # before selecting or deduplicating (avoids copying 9M rows)
  idx_match <- which(
    !is.na(annotations$structure_exact_mass) &
      !is.na(fastmatch::fmatch(annotations$feature_id, multi_features))
  )
  if (length(idx_match) == 0L) {
    return(annotations)
  }

  cov_audit <- attr(annotations, "coverage_audit")

  matched <- tidytable::tidytable(
    feature_id = annotations$feature_id[idx_match],
    mz = annotations$mz[idx_match],
    rt = annotations$rt[idx_match],
    adduct = annotations$adduct[idx_match],
    mass = annotations$mass[idx_match],
    structure_exact_mass = annotations$structure_exact_mass[idx_match]
  ) |>
    tidytable::distinct()

  matched_with_component <- matched |>
    tidytable::inner_join(
      comp_mem_multi |>
        tidytable::rename(source_component = component_id),
      by = "feature_id"
    )

  source_comps <- unique(matched_with_component$source_component)
  component_features <- comp_mem_multi[
    !is.na(fastmatch::fmatch(comp_mem_multi$component_id, source_comps)),
  ]

  # Create source->target mapping via equijoin on component_id (not cross_join)
  propagation_map <- matched_with_component |>
    tidytable::select(
      source_feature = feature_id,
      source_component,
      adduct
    ) |>
    tidytable::distinct() |>
    tidytable::inner_join(
      component_features |>
        tidytable::rename(
          target_feature = feature_id,
          source_component = component_id
        ),
      by = "source_component"
    ) |>
    tidytable::filter(source_feature != target_feature) |>
    tidytable::select(source_feature, target_feature, source_component, adduct)

  # Get target metadata and M values in single operations
  target_meta <- node_hypotheses |>
    tidytable::distinct(feature_id, mz, rt) |>
    tidytable::rename(
      target_feature = feature_id,
      target_mz = mz,
      target_rt = rt
    )

  target_m_map <- feature_m_map |>
    tidytable::rename(target_feature = feature_id) |>
    tidytable::select(
      target_feature,
      source_component = component_id,
      neutral_mass
    )

  # Get target adduct hypotheses for compatibility check
  target_hyp <- node_hypotheses |>
    tidytable::select(feature_id, adduct, mass) |>
    tidytable::rename(target_feature = feature_id, target_mass = mass)

  # Combine all data in single join operation
  propagation_combined <- propagation_map |>
    tidytable::left_join(
      matched |>
        tidytable::select(
          source_feature = feature_id,
          adduct,
          mz,
          rt,
          mass,
          structure_exact_mass
        ),
      by = c("source_feature", "adduct")
    ) |>
    tidytable::left_join(target_meta, by = "target_feature") |>
    tidytable::left_join(
      target_m_map,
      by = c("target_feature", "source_component")
    ) |>
    tidytable::left_join(
      target_hyp,
      by = c("target_feature", "adduct")
    ) |>
    tidytable::mutate(
      target_mass = suppressWarnings(as.numeric(target_mass)),
      mass = suppressWarnings(as.numeric(mass)),
      structure_exact_mass = suppressWarnings(as.numeric(structure_exact_mass))
    ) |>
    tidytable::filter(
      !is.na(target_mass) & !is.na(mass) & !is.na(structure_exact_mass)
    )

  if (nrow(propagation_combined) == 0L) {
    return(annotations)
  }

  # Mass compatibility check
  propagation_combined <- propagation_combined |>
    tidytable::mutate(
      mass_diff = abs(target_mass - mass),
      ppm_window = tolerance_ppm * 1e-6 * mass,
      dalton_ok = if (is.null(tolerance_dalton)) {
        FALSE
      } else {
        mass_diff <= tolerance_dalton
      },
      mass_compatible = (mass_diff <= ppm_window) | dalton_ok
    ) |>
    tidytable::filter(mass_compatible) |>
    tidytable::select(
      -mass_diff,
      -ppm_window,
      -dalton_ok,
      -mass_compatible
    )

  if (nrow(propagation_combined) == 0L) {
    return(annotations)
  }

  propagated_annotations <- propagation_combined |>
    tidytable::transmute(
      feature_id = target_feature,
      rt = target_rt,
      mz = target_mz,
      adduct = adduct,
      mass = target_mass,
      structure_exact_mass = structure_exact_mass,
      error_mz = structure_exact_mass - target_mass,
      source = "propagated_clique_supported",
      is_preassigned = FALSE,
      candidate_adduct_origin = "supported",
      adduct_support = 0L
    ) |>
    tidytable::distinct()

  if ("source" %in% colnames(annotations)) {
    existing_prop_idx <- which(
      !is.na(annotations$source) &
        annotations$source == "propagated_clique_supported"
    )
    if (length(existing_prop_idx) > 0L) {
      propagated_annotations <- propagated_annotations |>
        tidytable::anti_join(
          annotations[existing_prop_idx, ],
          by = intersect(
            colnames(propagated_annotations),
            colnames(annotations)
          )
        )
    }
  }
  if (nrow(propagated_annotations) == 0L) {
    return(annotations)
  }

  result <- tidytable::bind_rows(annotations, propagated_annotations)
  if (!is.null(cov_audit)) {
    attr(result, "coverage_audit") <- cov_audit
  }

  result
}
