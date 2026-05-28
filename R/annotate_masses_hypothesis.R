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

#' Build the per-feature adduct hypotheses from the edge sets
#'
#' Hypotheses come from four sources, in priority order:
#'   - `pair`: an adduct edge directly named this feature with this adduct
#'   - `cluster`: a cluster edge implies a `+cluster` suffix on the dest node
#'   - `loss`: a neutral-loss edge implies a `-loss` suffix on the precursor
#'   - `preassigned`: an upstream tool assigned this feature this adduct
#'   - `baseline`: every feature is always tested under `[M+H]+` / `[M-H]-`
#'
#' @keywords internal
collect_node_adduct_hypotheses <- function(
  adduct_edges,
  cluster_edges,
  loss_edges,
  evidence_hypotheses,
  preassigned,
  baseline_adduct,
  features_table
) {
  # (a) pair-derived hypotheses (both endpoints)
  pair_a <- adduct_edges |>
    tidytable::distinct(feature_id, adduct)
  pair_b <- adduct_edges |>
    tidytable::distinct(feature_id_dest, adduct_dest) |>
    tidytable::rename(
      feature_id = feature_id_dest,
      adduct = adduct_dest
    )
  pair_hyps <- tidytable::bind_rows(pair_a, pair_b) |>
    tidytable::distinct() |>
    tidytable::mutate(source = "pair", is_preassigned = FALSE)

  # (b) preassigned (upstream tools)
  pre_propagated <- propagate_preassigned_over_adduct_edges(
    adduct_edges = adduct_edges,
    preassigned = preassigned
  )
  pre_hyps <- preassigned |>
    tidytable::distinct(feature_id, adduct) |>
    tidytable::mutate(source = "preassigned", is_preassigned = TRUE)
  pre_prop_hyps <- pre_propagated |>
    tidytable::distinct(feature_id, adduct) |>
    tidytable::mutate(source = "preassigned_propagated", is_preassigned = TRUE)

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

  # (c) baseline - always tested for every feature
  baseline_hyps <- features_table |>
    tidytable::distinct(feature_id) |>
    tidytable::mutate(
      adduct = baseline_adduct,
      source = "baseline",
      is_preassigned = FALSE
    )

  base_hyps <- tidytable::bind_rows(
    pair_hyps,
    evidence_hypotheses |>
      tidytable::distinct(feature_id, adduct, source, is_preassigned),
    pre_hyps,
    pre_prop_hyps,
    baseline_hyps
  ) |>
    tidytable::distinct() |>
    harmonize_adducts(adducts_translations = adducts_translations)

  # Attach support count
  support <- compute_feature_adduct_support(adduct_edges)
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
      )
    )

  # (d) cluster expansion - suffix +cluster onto the DEST node's hypotheses.
  cluster_hyps <- expand_with_modifier(
    base_hyps = base_hyps,
    edges = cluster_edges,
    edge_feature_col = "feature_id_dest",
    mod_col = "cluster",
    mod_sign = "+"
  )

  # (e) loss expansion - suffix -loss onto the PRECURSOR's hypotheses.
  loss_hyps <- expand_with_modifier(
    base_hyps = base_hyps,
    edges = loss_edges,
    edge_feature_col = "feature_id_dest",
    mod_col = "loss",
    mod_sign = "-",
    strip_label = TRUE
  )

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
  all_hyps |>
    tidytable::left_join(fea_meta, by = "feature_id") |>
    dedupe_node_hypotheses()
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
      adduct = paste0(
        gsub("M(?![a-z]).*", "M", adduct, perl = TRUE),
        mod_sign,
        !!as.name(mod_col),
        gsub(".*M(?![a-z])", "", adduct, perl = TRUE)
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
  if (nrow(node_hypotheses) == 0L) {
    return(node_hypotheses)
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
      )
    ) |>
    tidytable::arrange(source_rank, tidytable::desc(adduct_support)) |>
    tidytable::group_by(feature_id, adduct) |>
    tidytable::mutate(is_preassigned = any(is_preassigned)) |>
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
  adducts,
  clusters,
  neutral_losses,
  ms_mode,
  tolerance_ppm,
  tolerance_rt,
  exact_masses
) {
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

  universe <- build_adduct_universe(
    adducts_list = list(pos = adducts, neg = adducts),
    clusters_list = list(pos = clusters, neg = clusters),
    neutral_losses_list = neutral_losses,
    polarity = ms_mode
  )
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
    max_hypotheses_per_feature = nrow(universe)
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
      adduct_support = as.integer(evidence_count)
    ) |>
    tidytable::distinct()

  evidence_edges <- build_evidence_edges(supported) |>
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

  hyps <- evidence_signal$hypotheses |>
    tidytable::mutate(
      evidence_requires_pairwise = vapply(
        X = adduct,
        FUN = evidence_adduct_requires_pairwise_support,
        FUN.VALUE = logical(1L)
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
    baseline_adduct = baseline_adduct,
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
    dropped_bd <- dropped_hyps |>
      tidytable::distinct(feature_id, adduct) |>
      tidytable::count(adduct, name = "N") |>
      tidytable::arrange(tidytable::desc(N))
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
  tolerance_ppm
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
      mass = calculate_mass_of_m_batch(adducts = adduct, mzs = as.numeric(mz))
    ) |>
    tidytable::filter(!is.na(mass) & is.finite(mass) & mass > 0)
  if (nrow(multi_candidates) == 0L) {
    return(empty_out)
  }

  kept <- multi_candidates |>
    tidytable::inner_join(base_mass_by_feature, by = "feature_id") |>
    tidytable::mutate(
      ppm = abs(mass - neutral_mass_ref) * 1E6 / neutral_mass_ref
    ) |>
    tidytable::filter(is.finite(ppm) & ppm <= tolerance_ppm) |>
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
match_candidates_to_library <- function(candidates, library_em, tolerance_ppm) {
  invisible(tolerance_ppm)
  empty_out <- tidytable::tidytable(
    feature_id = character(),
    rt = numeric(),
    mz = numeric(),
    adduct = character(),
    mass = numeric(),
    source = character(),
    adduct_support = integer(),
    loss_term = character(),
    structure_exact_mass = numeric(),
    error_mz = numeric()
  )
  if (nrow(candidates) == 0L || nrow(library_em) == 0L) {
    return(empty_out)
  }
  has_loss_term <- "loss_term" %in% colnames(candidates)
  has_is_preassigned <- "is_preassigned" %in% colnames(candidates)
  if (!has_is_preassigned) {
    candidates <- candidates |>
      tidytable::mutate(is_preassigned = FALSE)
  }
  if (has_loss_term) {
    cand_t <- tidytable::as_tidytable(candidates)[,
      .(
        src_feature_id = feature_id,
        src_rt = rt,
        src_mz = mz,
        src_adduct = adduct,
        src_mass = mass,
        src_source = source,
        src_is_preassigned = is_preassigned,
        src_support = adduct_support,
        src_loss_term = loss_term
      )
    ]
  } else {
    cand_t <- tidytable::as_tidytable(candidates)[,
      .(
        src_feature_id = feature_id,
        src_rt = rt,
        src_mz = mz,
        src_adduct = adduct,
        src_mass = mass,
        src_source = source,
        src_is_preassigned = is_preassigned,
        src_support = adduct_support,
        src_loss_term = NA_character_
      )
    ]
  }
  em_t <- tidytable::as_tidytable(library_em)
  # Non-equi join: after the join, the i-side `src_mass` is no longer in
  # scope as a column name; data.table exposes it via the join columns
  # `value_min` / `value_max` (both equal to the matched src_mass for the
  # surviving rows). We pick `value_min` to recover the original mass.
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
      mass = value_min,
      source = src_source,
      is_preassigned = src_is_preassigned,
      adduct_support = src_support,
      loss_term = src_loss_term,
      structure_exact_mass = exact_mass,
      error_mz = exact_mass - value_min
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
      is_safe = (if (is.null(baseline_adduct)) FALSE else adduct == baseline_adduct) |
        is_preassigned |
        source %in% c("preassigned", "baseline") |
        net_support > 0L |
        (max_support == 0L & !(source == "multi" & has_non_multi)),
      drop = !is_safe & n_candidates > 1L
    )

  dropped <- scored |> tidytable::filter(drop)
  if (nrow(dropped) > 0L) {
    by_adduct <- dropped |>
      tidytable::distinct(feature_id, adduct) |>
      tidytable::count(adduct, name = "N") |>
      tidytable::arrange(tidytable::desc(N))
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
