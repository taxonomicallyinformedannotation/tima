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
  if (all(c("mass", "adduct", "adduct_dest") %in% colnames(diffs))) {
    pairs_t <- tidytable::as_tidytable(pairs)[,
      .(feature_id, feature_id_dest, delta_min, delta_max)
    ]
    diffs_t <- tidytable::as_tidytable(diffs)
    out <- diffs_t[
      pairs_t,
      on = .(mass >= delta_min, mass <= delta_max),
      nomatch = NA_real_,
      allow.cartesian = TRUE
    ][,
      .(
        feature_id,
        mass,
        feature_id_dest,
        adduct,
        adduct_dest,
        modifier = get(diff_col)
      )
    ] |>
      tidytable::as_tidytable() |>
      tidytable::filter(!is.na(modifier)) |>
      tidytable::rename(!!as.name(diff_col) := modifier) |>
      tidytable::distinct()
    return(out)
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
  adduct_lookup = NULL
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

  # Add mz values to edges
  features_min <- features_table |>
    tidytable::distinct(feature_id, mz)
  edges <- adduct_edges |>
    tidytable::left_join(
      features_min,
      by = c("feature_id")
    ) |>
    tidytable::rename(mz_src = mz) |>
    tidytable::left_join(
      features_min |>
        tidytable::rename(feature_id_dest = feature_id, mz_dest = mz),
      by = c("feature_id_dest")
    ) |>
    tidytable::filter(!is.na(mz_src) & !is.na(mz_dest))

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

  # Calculate implied M for each edge endpoint
  edges <- edges |>
    tidytable::mutate(
      m_src = calculate_neutral_mass_from_lookup(
        mzs = as.numeric(mz_src),
        adducts = adduct,
        adduct_lookup = adduct_lookup
      ),
      m_dest = calculate_neutral_mass_from_lookup(
        mzs = as.numeric(mz_dest),
        adducts = adduct_dest,
        adduct_lookup = adduct_lookup
      ),
      m_diff = abs(m_src - m_dest),
      ppm_consistent = m_diff <=
        (tolerance_ppm * 1e-6 * pmax(m_src, m_dest, na.rm = TRUE)),
      dalton_consistent = if (is.null(tolerance_dalton)) {
        FALSE
      } else {
        (m_diff <= tolerance_dalton)
      },
      m_consistent = ppm_consistent | dalton_consistent
    )

  # Keep only consistent edges
  consistent <- edges |>
    tidytable::filter(m_consistent) |>
    tidytable::select(feature_id, adduct, feature_id_dest, adduct_dest)

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

  # Build undirected graph for connected components
  undirected <- tidytable::bind_rows(
    consistent |>
      tidytable::transmute(src = feature_id, dest = feature_id_dest),
    consistent |>
      tidytable::transmute(src = feature_id_dest, dest = feature_id)
  ) |>
    tidytable::distinct()

  # Find connected components using BFS
  all_nodes <- unique(c(undirected$src, undirected$dest))
  if (length(all_nodes) == 0L) {
    return(list(
      consistent_edges = consistent[0L, ],
      feature_m_map = tidytable::tidytable(
        feature_id = character(),
        neutral_mass = numeric(),
        component_id = character()
      )
    ))
  }

  neighbors <- split(undirected$dest, undirected$src)
  visited <- stats::setNames(rep(FALSE, length(all_nodes)), all_nodes)
  components <- list()

  for (node in all_nodes) {
    if (isTRUE(visited[[node]])) {
      next
    }
    queue <- c(node)
    visited[[node]] <- TRUE
    comp_nodes <- character()
    while (length(queue) > 0L) {
      current <- queue[[1L]]
      queue <- queue[-1L]
      comp_nodes <- c(comp_nodes, current)
      next_nodes <- neighbors[[current]]
      if (!is.null(next_nodes)) {
        for (nxt in next_nodes) {
          if (!isTRUE(visited[[nxt]])) {
            visited[[nxt]] <- TRUE
            queue <- c(queue, nxt)
          }
        }
      }
    }
    # comp_nodes is already unique by construction (visited check ensures no duplicates)
    components[[length(components) + 1L]] <- comp_nodes
  }

  # For each component, compute consistent M assignments
  all_m_maps <- list()
  for (ci in seq_along(components)) {
    comp_nodes <- components[[ci]]
    comp_id <- paste0("comp_", ci)

    # Get all edges in this component
    comp_edges <- consistent |>
      tidytable::filter(
        feature_id %in% comp_nodes & feature_id_dest %in% comp_nodes
      )

    # Add mz info for M calculation
    comp_edges <- comp_edges |>
      tidytable::left_join(
        features_min,
        by = c("feature_id")
      ) |>
      tidytable::rename(mz_src = mz) |>
      tidytable::left_join(
        features_min |>
          tidytable::rename(feature_id_dest = feature_id, mz_dest = mz),
        by = c("feature_id_dest")
      )

    # Calculate M for each feature from its edges
    m_values <- tidytable::bind_rows(
      comp_edges |>
        tidytable::transmute(
          feature_id,
          neutral_mass = calculate_neutral_mass_from_lookup(
            mzs = as.numeric(mz_src),
            adducts = adduct,
            adduct_lookup = adduct_lookup
          )
        ),
      comp_edges |>
        tidytable::transmute(
          feature_id = feature_id_dest,
          neutral_mass = calculate_neutral_mass_from_lookup(
            mzs = as.numeric(mz_dest),
            adducts = adduct_dest,
            adduct_lookup = adduct_lookup
          )
        )
    ) |>
      tidytable::filter(!is.na(neutral_mass)) |>
      tidytable::group_by(feature_id) |>
      tidytable::mutate(
        m_med = stats::median(neutral_mass, na.rm = TRUE),
        m_mean = mean(neutral_mass, na.rm = TRUE),
        m_sd = stats::sd(neutral_mass, na.rm = TRUE),
        m_sd = tidytable::if_else(is.na(m_sd), 0, m_sd)
      ) |>
      tidytable::distinct(feature_id, .keep_all = TRUE) |>
      tidytable::ungroup() |>
      tidytable::select(feature_id, neutral_mass = m_med)

    # For nodes with no edges, use baseline M (e.g., [M+H]+)
    missing_nodes <- setdiff(comp_nodes, m_values$feature_id)
    if (length(missing_nodes) > 0L) {
      baseline_m <- features_table |>
        tidytable::filter(feature_id %in% missing_nodes) |>
        tidytable::transmute(
          feature_id,
          neutral_mass = calculate_neutral_mass_from_lookup(
            mzs = as.numeric(mz),
            adducts = "[M+H]+",
            adduct_lookup = adduct_lookup
          )
        )
      m_values <- tidytable::bind_rows(m_values, baseline_m)
    }

    m_values <- m_values |>
      tidytable::mutate(component_id = comp_id)
    all_m_maps[[ci]] <- m_values
  }

  feature_m_map <- tidytable::bind_rows(all_m_maps)

  # Build component_membership map (feature_id -> component_id)
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

  # Build full edge table with M calculations
  features_min <- features_table |>
    tidytable::distinct(feature_id, mz)

  edge_with_m <- consistent_edges |>
    tidytable::left_join(
      features_min,
      by = c("feature_id")
    ) |>
    tidytable::rename(mz_src = mz) |>
    tidytable::left_join(
      features_min |>
        tidytable::rename(feature_id_dest = feature_id, mz_dest = mz),
      by = c("feature_id_dest")
    ) |>
    tidytable::filter(!is.na(mz_src) & !is.na(mz_dest))

  # Add edge IDs
  edge_with_m <- edge_with_m |>
    tidytable::mutate(
      m_src = calculate_neutral_mass_from_lookup(
        mzs = as.numeric(mz_src),
        adducts = adduct,
        adduct_lookup = adduct_lookup
      ),
      m_dest = calculate_neutral_mass_from_lookup(
        mzs = as.numeric(mz_dest),
        adducts = adduct_dest,
        adduct_lookup = adduct_lookup
      )
    ) |>
    tidytable::as_tidytable()

  # Add edge_id column using row numbers
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

    # Collect all implied M values for each feature
    m_values_src <- edge_with_m |>
      tidytable::select(feature_id, m_value = m_src, edge_id)
    m_values_dest <- edge_with_m |>
      tidytable::select(feature_id = feature_id_dest, m_value = m_dest, edge_id)
    all_m_values <- tidytable::bind_rows(m_values_src, m_values_dest) |>
      tidytable::filter(!is.na(m_value))

    if (nrow(all_m_values) == 0L) {
      break
    }

    # For each feature, find consensus M and check consistency
    feature_consistency <- all_m_values |>
      tidytable::group_by(feature_id) |>
      tidytable::mutate(
        m_median = stats::median(m_value, na.rm = TRUE),
        m_iqr = stats::IQR(m_value, na.rm = TRUE),
        m_sd = stats::sd(m_value, na.rm = TRUE),
        m_sd = tidytable::if_else(is.na(m_sd), 0, m_sd),
        # Check if this edge's M is within tolerance of consensus
        ppm_tol = tolerance_ppm * 1e-6 * m_median,
        dalton_tol = tolerance_dalton %||% 0.01,
        ppm_consistent = abs(m_value - m_median) <= ppm_tol,
        dalton_consistent = abs(m_value - m_median) <= dalton_tol,
        is_consistent = ppm_consistent | dalton_consistent
      ) |>
      tidytable::ungroup()

    # Identify edges to remove (those whose M conflicts with consensus for either endpoint)
    edges_to_remove <- feature_consistency |>
      tidytable::filter(!is_consistent) |>
      tidytable::pull(edge_id) |>
      unique()

    if (length(edges_to_remove) == 0L) {
      # All features have consistent M values
      break
    }

    # Remove conflicting edges and recalculate
    edge_with_m <- edge_with_m |>
      tidytable::filter(!(edge_id %in% edges_to_remove))
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
  tolerance_dalton = 0.005
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
    adduct_lookup = adduct_lookup
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

  # Attach support count - compute from CONSISTENT edges only
  support <- compute_feature_adduct_support(consistent_edges)
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
      expand_with_modifier(
        base_hyps = support_base,
        edges = cluster_edges,
        edge_feature_col = "feature_id_dest",
        mod_col = "cluster",
        mod_sign = "+"
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
    tidytable::filter(!(adduct %in% adducts_forbidden))

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
  vapply(
    adduct_strings,
    function(adduct) {
      if (is.na(adduct) || !nzchar(adduct)) {
        return(adduct)
      }

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
      if (length(tokens) == 0L) {
        return(adduct)
      }

      remainder <- gsub(tok_re, "", mods, perl = TRUE)
      if (!identical(remainder, "")) {
        return(adduct)
      }

      # Keep charge-carrier tokens separate from modifier chemistry.
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
      has_pos_h4n <- any(grepl("^\\+[0-9]*H4N$", tokens, perl = TRUE))

      carrier_net <- list()
      modifier_atomic_net <- list()

      for (tok in tokens) {
        tg <- regmatches(tok, regexec(tok_re, tok, perl = TRUE))[[1L]]
        sign <- if (tg[[2L]] == "+") 1L else -1L
        mult <- if (nzchar(tg[[3L]])) as.integer(tg[[3L]]) else 1L
        formula <- tg[[4L]]

        # Keep NH3 explicit in presence of NH4 so harmonize_adducts can reduce
        # (-H3N,+H4N) pairs to +H instead of hiding NH3 in atomic collapse.
        if (formula %in% carriers || (has_pos_h4n && formula == "H3N")) {
          prev <- carrier_net[[formula]] %||% 0L
          carrier_net[[formula]] <- prev + sign * mult
        } else {
          atoms <- parse_atomic_formula(formula)
          if (is.null(atoms) || length(atoms) == 0L) {
            return(adduct)
          }
          for (el in names(atoms)) {
            prev <- modifier_atomic_net[[el]] %||% 0L
            modifier_atomic_net[[el]] <- prev +
              sign * mult * as.integer(atoms[[el]])
          }
        }
      }

      # Hill formula builder
      hill_formula <- function(counts) {
        counts <- Filter(function(x) x > 0L, counts)
        if (length(counts) == 0L) {
          return("")
        }
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

      new_mods <- character()

      # Negative atomic term (loss)
      neg_atoms <- Filter(function(x) x < 0L, modifier_atomic_net)
      if (length(neg_atoms) > 0L) {
        f <- hill_formula(lapply(neg_atoms, abs))
        if (nzchar(f)) new_mods <- c(new_mods, paste0("-", f))
      }

      # Negative carrier terms
      neg_carriers <- Filter(function(x) x < 0L, carrier_net)
      for (nm in sort(names(neg_carriers))) {
        n <- abs(neg_carriers[[nm]])
        new_mods <- c(new_mods, paste0("-", if (n > 1L) n else "", nm))
      }

      # Positive atomic term (addition)
      pos_atoms <- Filter(function(x) x > 0L, modifier_atomic_net)
      if (length(pos_atoms) > 0L) {
        f <- hill_formula(pos_atoms)
        if (nzchar(f)) new_mods <- c(new_mods, paste0("+", f))
      }

      # Positive carrier terms
      pos_carriers <- Filter(function(x) x > 0L, carrier_net)
      for (nm in sort(names(pos_carriers))) {
        n <- pos_carriers[[nm]]
        new_mods <- c(new_mods, paste0("+", if (n > 1L) n else "", nm))
      }

      paste0("[", core, paste0(new_mods, collapse = ""), "]", suffix)
    },
    character(1L),
    USE.NAMES = FALSE
  )
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
  out <- base_hyps |>
    tidytable::inner_join(src_dest, by = "feature_id") |>
    tidytable::mutate(
      feature_id = feature_id_dest,
      .base_part = sub("M(?![a-z]).*", "M", adduct, perl = TRUE),
      .char_part = sub(".*M(?![a-z])", "", adduct, perl = TRUE),
      adduct = .simplify_adduct_after_modifier(
        paste0(.base_part, mod_sign, !!as.name(mod_col), .char_part)
      ),
      source = source_label,
      .base_part = NULL,
      .char_part = NULL,
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

  out <- base_hyps |>
    tidytable::inner_join(dest_src, by = "feature_id") |>
    tidytable::mutate(
      feature_id = feature_id_src,
      .base_part = sub("M(?![a-z]).*", "M", adduct, perl = TRUE),
      .char_part = sub(".*M(?![a-z])", "", adduct, perl = TRUE),
      adduct = .simplify_adduct_after_modifier(
        paste0(.base_part, mod_sign, !!as.name(mod_col), .char_part)
      ),
      source = source_label,
      .base_part = NULL,
      .char_part = NULL,
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
      # Extract base adduct (M part) and suffix (post-M part)
      .base_part = sub("M(?![a-z]).*", "M", adduct, perl = TRUE),
      .char_part = sub(".*M(?![a-z])", "", adduct, perl = TRUE),
      adduct = .simplify_adduct_after_modifier(
        paste0(.base_part, mod_sign, !!as.name(mod_col), .char_part)
      ),
      source = if (mod_sign == "+") "cluster" else "loss",
      .base_part = NULL,
      .char_part = NULL
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
  if (nrow(node_hypotheses) == 0L) {
    return(node_hypotheses)
  }
  if (!"candidate_adduct_origin" %in% colnames(node_hypotheses)) {
    node_hypotheses <- node_hypotheses |>
      tidytable::mutate(candidate_adduct_origin = "supported")
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
  node_hypotheses |>
    tidytable::mutate(
      is_preassigned = tidytable::if_else(
        is.na(is_preassigned),
        FALSE,
        is_preassigned
      ),
      source_rank = unname(ranks[source]),
      source_rank = tidytable::if_else(
        is.na(source_rank),
        99L,
        source_rank
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
    ) |>
    tidytable::arrange(source_rank, tidytable::desc(adduct_support)) |>
    tidytable::group_by(feature_id, adduct) |>
    tidytable::mutate(
      is_preassigned = any(is_preassigned),
      candidate_adduct_origin = tidytable::case_when(
        any(candidate_adduct_origin == "supported") ~ "supported",
        any(candidate_adduct_origin == "supported_weak") ~ "supported_weak",
        TRUE ~ "baseline"
      )
    ) |>
    tidytable::ungroup() |>
    tidytable::distinct(feature_id, adduct, .keep_all = TRUE) |>
    tidytable::select(-source_rank)
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
  clusters,
  neutral_losses,
  ms_mode,
  tolerance_ppm,
  tolerance_dalton = NULL,
  tolerance_rt,
  exact_masses
) {
  invisible(tolerance_dalton)
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

  if (is.null(universe)) {
    universe <- build_adduct_universe(
      adducts_list = list(pos = adducts, neg = adducts),
      clusters_list = clusters,
      neutral_losses_list = neutral_losses,
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
    tolerance_rt = tolerance_rt,
    ms_mode = ms_mode,
    exact_masses = exact_masses,
    max_hypotheses_per_feature = Inf
  )
  if (nrow(hyps) == 0L) {
    return(list(hypotheses = empty_hyp, edges = empty_edges))
  }

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
    tolerance_ppm = tolerance_ppm
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

  base_mass_by_feature <- node_hypotheses |>
    tidytable::filter(source != "multi") |>
    tidytable::filter(!is.na(mass) & is.finite(mass) & mass > 0) |>
    tidytable::distinct(feature_id, neutral_mass_ref = mass)
  if (nrow(base_mass_by_feature) == 0L) {
    return(empty_out)
  }

  feature_meta <- node_hypotheses |>
    tidytable::distinct(feature_id, mz, rt)

  multi_candidates <- feature_meta |>
    tidytable::cross_join(multi_adducts |> tidytable::distinct(adduct)) |>
    tidytable::mutate(
      source = "multi",
      adduct_support = 0L,
      mass = calculate_neutral_mass_from_lookup(
        mzs = as.numeric(mz),
        adducts = adduct,
        adduct_lookup = adduct_lookup
      )
    ) |>
    tidytable::filter(!is.na(mass) & is.finite(mass) & mass > 0)
  if (nrow(multi_candidates) == 0L) {
    return(empty_out)
  }

  kept <- multi_candidates |>
    tidytable::inner_join(base_mass_by_feature, by = "feature_id") |>
    tidytable::mutate(
      ppm = abs(mass - neutral_mass_ref) * 1E6 / neutral_mass_ref,
      dalton_diff = abs(mass - neutral_mass_ref),
      ppm_pass = is.finite(ppm) & ppm <= tolerance_ppm,
      dalton_pass = if (is.null(tolerance_dalton)) {
        FALSE
      } else {
        (dalton_diff <= tolerance_dalton)
      },
      passes_tolerance = ppm_pass | dalton_pass
    ) |>
    tidytable::filter(passes_tolerance) |>
    tidytable::select(
      feature_id,
      adduct,
      source,
      adduct_support,
      mz,
      rt,
      mass
    ) |>
    tidytable::distinct()

  if (nrow(kept) == 0L) {
    return(empty_out)
  }
  kept
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
      mass = src_mass,
      source = src_source,
      is_preassigned = src_is_preassigned,
      candidate_adduct_origin = src_origin,
      adduct_support = src_support,
      loss_term = src_loss_term,
      structure_exact_mass = exact_mass,
      error_mz = exact_mass - src_mass
    )
  ] |>
    tidytable::as_tidytable() |>
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
  if (nrow(matched) == 0L) {
    return(matched)
  }
  if (!"is_preassigned" %in% colnames(matched)) {
    matched <- matched |>
      tidytable::mutate(is_preassigned = FALSE)
  }
  state_map <- build_adduct_state_key_map(unique(c(
    matched$adduct,
    adduct_edges$adduct,
    adduct_edges$adduct_dest
  )))

  safe_max_support <- function(x) {
    if (length(x) == 0L || all(is.na(x))) {
      return(0L)
    }
    as.integer(max(x, na.rm = TRUE))
  }

  support <- compute_feature_adduct_support(adduct_edges) |>
    tidytable::left_join(
      state_map |>
        tidytable::rename(adduct_state_key = state_key),
      by = "adduct"
    ) |>
    tidytable::mutate(
      adduct_state_key = tidytable::if_else(
        is.na(adduct_state_key),
        adduct,
        adduct_state_key
      )
    ) |>
    tidytable::summarize(
      adduct_support = safe_max_support(adduct_support),
      .by = c(feature_id, adduct_state_key)
    ) |>
    tidytable::rename(net_support = adduct_support)

  scored <- matched |>
    tidytable::left_join(
      state_map |>
        tidytable::rename(adduct_state_key = state_key),
      by = "adduct"
    ) |>
    tidytable::mutate(
      adduct_state_key = tidytable::if_else(
        is.na(adduct_state_key),
        adduct,
        adduct_state_key
      )
    ) |>
    tidytable::left_join(support, by = c("feature_id", "adduct_state_key")) |>
    tidytable::mutate(
      is_preassigned = tidytable::if_else(
        is.na(is_preassigned),
        FALSE,
        is_preassigned
      ),
      net_support = tidytable::if_else(
        is.na(net_support),
        0L,
        as.integer(net_support)
      )
    )

  per_feature_max <- scored |>
    tidytable::summarize(
      max_support = max(net_support, na.rm = TRUE),
      n_candidates = tidytable::n_distinct(adduct),
      .by = feature_id
    )

  # Flag features that have at least one non-"multi" library hit.
  # "multi" candidates (exotic multi-charge / dimer adducts assigned via cross-
  # join with NO pairwise evidence) are dropped whenever a more parsimonious
  # alternative (pair, cluster, loss, preassigned, baseline) already matched the
  # library for the same feature. This prevents [2M+Fe]2+, [M+Ca]2+, etc. from
  # crowding out sensible annotations for isolated features.
  per_feature_sources <- scored |>
    tidytable::summarize(
      has_non_multi = any(source != "multi"),
      .by = feature_id
    )

  scored <- scored |>
    tidytable::left_join(per_feature_max, by = "feature_id") |>
    tidytable::left_join(per_feature_sources, by = "feature_id") |>
    tidytable::mutate(
      has_non_multi = tidytable::if_else(
        is.na(has_non_multi),
        FALSE,
        has_non_multi
      ),
      # A candidate is "safe" (never dropped) when:
      #   - it is the baseline adduct,
      #   - it was pre-assigned upstream,
      #   - it has network edge support,
      #   - or it is the sole type of candidate for this feature *and* it is
      #     not a speculative "multi" hypothesis that has a better alternative.
      is_safe = (if (is.null(baseline_adduct)) {
        FALSE
      } else {
        adduct == baseline_adduct
      }) |
        is_preassigned |
        source %in% c("preassigned", "baseline") |
        net_support > 0L |
        (max_support == 0L & !(source == "multi" & has_non_multi)),
      drop = !is_safe & n_candidates > 1L
    )

  dropped <- scored |> tidytable::filter(drop)
  if (nrow(dropped) > 0L) {
    n_unique <- nrow(dropped |> tidytable::distinct(feature_id, adduct))
    log_info(
      paste0(
        "Network-consensus pruning dropped %d (feature, adduct) ",
        "candidate(s) with zero adduct-graph support when a supported ",
        "alternative existed."
      ),
      n_unique
    )
  }

  scored |>
    tidytable::filter(!drop) |>
    tidytable::select(
      -drop,
      -is_safe,
      -has_non_multi,
      -max_support,
      -n_candidates,
      -net_support,
      -adduct_state_key
    )
}

#' Propagate annotations across M-consistent cliques
#'
#' When a feature in an M-consistent clique receives a library match, propagate
#' that annotation to all other features in the same clique. This ensures that
#' if one feature is confidently annotated to a structure, the same structure is
#' suggested for all linked features with the same neutral mass.
#'
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
  tolerance_dalton = 0.005
) {
  # Early exits
  if (nrow(annotations) == 0L) {
    return(annotations)
  }

  component_membership <- attr(node_hypotheses, "component_membership")
  feature_m_map <- attr(node_hypotheses, "feature_m_map")

  if (is.null(component_membership) || nrow(component_membership) == 0L) {
    return(annotations)
  }

  # Get matched annotations with structures
  matched <- annotations |>
    tidytable::filter(!is.na(structure_exact_mass)) |>
    tidytable::select(
      feature_id,
      mz,
      rt,
      adduct,
      mass,
      structure_exact_mass
    )

  if (nrow(matched) == 0L) {
    return(annotations)
  }

  # Matched annotations with their components
  matched_with_component <- matched |>
    tidytable::left_join(
      component_membership |>
        tidytable::rename(source_component = component_id),
      by = "feature_id"
    ) |>
    tidytable::filter(!is.na(source_component))

  if (nrow(matched_with_component) == 0L) {
    return(annotations)
  }

  # Get all features in same components (targets for propagation)
  component_features <- component_membership |>
    tidytable::left_join(
      matched_with_component |>
        tidytable::select(source_component) |>
        tidytable::distinct() |>
        tidytable::rename(component_id = source_component),
      by = c("component_id")
    ) |>
    tidytable::filter(!is.na(component_id))

  if (nrow(component_features) == 0L) {
    return(annotations)
  }

  # Create source->target mapping with single cross_join
  propagation_map <- matched_with_component |>
    tidytable::select(feature_id, source_component, adduct) |>
    tidytable::rename(source_feature = feature_id) |>
    tidytable::cross_join(
      component_features |>
        tidytable::rename(target_feature = feature_id)
    ) |>
    tidytable::filter(
      source_component == component_id &
        source_feature != target_feature
    ) |>
    tidytable::select(source_feature, target_feature, source_component, adduct)

  if (nrow(propagation_map) == 0L) {
    return(annotations)
  }

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
        tidytable::select(feature_id, mz, rt, mass, structure_exact_mass) |>
        tidytable::rename(source_feature = feature_id),
      by = "source_feature"
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

  # Create propagated annotation rows
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

  # Combine with original annotations
  result <- tidytable::bind_rows(annotations, propagated_annotations) |>
    tidytable::distinct()

  result
}
