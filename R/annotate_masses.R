#' @title Annotate masses
#'
#' @description Mass-based MS1 annotation. The pipeline is a sequence of
#'     clearly-bounded steps; each step is documented inline. In short:
#'
#'     1. **Pairs in RT windows.** For every feature, find all other features
#'        in the same RT tolerance window (per sample) and compute the m/z
#'        delta. The pair is always oriented `(lower_mz, higher_mz)` so that
#'        `delta = mz_higher - mz_lower >= 0`.
#'
#'     2. **Adduct edges.** Match each pair's `delta` against the table of
#'        precomputed pairwise differences between known mode-specific
#'        adducts. A match labels the edge `adduct_low _ adduct_high` and
#'        tentatively assigns the corresponding adduct to each endpoint.
#'
#'     3. **Cluster edges.** Match `delta` against cluster masses (e.g. ACN,
#'        MeOH, Na). A cluster adds mass to the *higher* m/z peak, so the
#'        cluster suffix `+<cluster>` is attached to the **dest** node's
#'        adduct hypotheses.
#'
#'     4. **Neutral-loss edges.** Match `delta` against neutral-loss masses
#'        (e.g. H2O, CO2). For an NL pair, the **higher** m/z peak is the
#'        precursor and the **lower** m/z peak is the product. The loss
#'        suffix `-<loss>` is attached to the precursor's adduct hypotheses
#'        (so the same neutral M is inferred from both peaks).
#'
#'     5. **Node hypotheses.** Gather, per feature, **all** plausible adduct
#'        labels: (a) what we inferred from adduct/cluster/loss edges, (b)
#'        any adduct supplied upstream by the preprocessing tool, and
#'        (c) the universal baseline `[M+H]+` / `[M-H]-`. Hypotheses are
#'        never dropped at this stage.
#'
#'     6. **Library match.** For every `(feature, candidate_adduct)` pair,
#'        compute the implied neutral mass M and look it up in the library
#'        within the ppm tolerance.
#'
#'     7. **Network-consensus pruning.** If a feature ends up with several
#'        library hits, drop only the candidates whose adduct has *zero*
#'        support in the adduct edge graph **and** whose drop still leaves a
#'        supported alternative. Ties are kept and drops are logged.
#'
#'     8. **Keep unmatched adducts.** Adduct hypotheses are exported even
#'        when no library structure matches, so downstream tools still see
#'        the adduct annotation.
#'
#' @include adducts_utils.R
#' @include adduct_universe.R
#' @include calculate_mass_of_m.R
#' @include decorate_masses.R
#' @include dists_utils.R
#' @include get_params.R
#' @include harmonize_adducts.R
#' @include mass_evidence.R
#' @include round_reals.R
#' @include safe_fread.R
#' @include validations_utils.R
#'
#' @param features Table containing your previous annotation to complement
#' @param output_annotations Output for mass based structural annotations
#' @param output_edges Output for mass based edges
#' @param name_source Name of the source features column
#' @param name_target Name of the target features column
#' @param library Library containing the keys
#' @param str_stereo File containing structures stereo
#' @param str_met File containing structures metadata
#' @param str_tax_cla File containing Classyfire taxonomy
#' @param str_tax_npc File containing NPClassifier taxonomy
#' @param adducts_list List of adducts to be used
#' @param clusters_list List of clusters to be used
#' @param neutral_losses_list List of neutral losses to be used
#' @param ms_mode Ionization mode. Must be 'pos' or 'neg'
#' @param tolerance_ppm Tolerance to perform annotation. Should be <= 20 ppm
#' @param tolerance_dalton Absolute mass tolerance in Daltons for annotation
#' @param tolerance_rt Tolerance to group adducts. Should be <= 0.05 minutes
#' @param adduct_consistency Consistency mode for adduct edge filtering: one of
#'   `off`, `conditional`, `strict`
#' @param adduct_min_support Minimum number of independent supporting neighbors
#'   for an adduct assignment in consistency-filtered regions
#' @param adduct_consistency_min_degree In `conditional` mode, minimum local
#'   graph degree at which support filtering is activated
#'
#' @return Named character of paths to the annotations and edges files.
#'
#' @family annotation
#'
#' @export
#'
#' @examples
#' \dontrun{
#' annotate_masses()
#' }
annotate_masses <- function(
  features = get_params(step = "annotate_masses")$files$features$prepared,
  output_annotations = get_params(
    step = "annotate_masses"
  )$files$annotations$prepared$structural$ms1,
  output_edges = get_params(
    step = "annotate_masses"
  )$files$networks$spectral$edges$raw$ms1,
  name_source = get_params(step = "annotate_masses")$names$source,
  name_target = get_params(step = "annotate_masses")$names$target,
  library = get_params(
    step = "annotate_masses"
  )$files$libraries$sop$merged$keys,
  str_stereo = get_params(
    step = "annotate_masses"
  )$files$libraries$sop$merged$structures$stereo,
  str_met = get_params(
    step = "annotate_masses"
  )$files$libraries$sop$merged$structures$metadata,
  str_tax_cla = get_params(
    step = "annotate_masses"
  )$files$libraries$sop$merged$structures$taxonomies$cla,
  str_tax_npc = get_params(
    step = "annotate_masses"
  )$files$libraries$sop$merged$structures$taxonomies$npc,
  adducts_list = get_params(step = "annotate_masses")$ms$adducts,
  clusters_list = get_params(step = "annotate_masses")$ms$clusters,
  neutral_losses_list = get_params(
    step = "annotate_masses"
  )$ms$neutral_losses,
  ms_mode = get_params(step = "annotate_masses")$ms$polarity,
  tolerance_ppm = get_params(
    step = "annotate_masses"
  )$ms$tolerances$mass$ppm$ms1,
  tolerance_dalton = get_params(
    step = "annotate_masses"
  )$ms$tolerances$mass$dalton$ms1,
  tolerance_rt = get_params(
    step = "annotate_masses"
  )$ms$tolerances$rt$adducts,
  adduct_consistency = get_params(
    step = "annotate_masses"
  )$ms$adducts$consistency$type,
  adduct_min_support = get_params(
    step = "annotate_masses"
  )$ms$adducts$consistency$min_support,
  adduct_consistency_min_degree = get_params(
    step = "annotate_masses"
  )$ms$adducts$consistency$min_degree
) {
  ctx <- log_operation(
    "annotate_masses",
    ms_mode = ms_mode,
    tolerance_ppm = tolerance_ppm,
    tolerance_dalton = tolerance_dalton,
    tolerance_rt = tolerance_rt
  )
  log_info("Starting mass-based annotation")

  # ---- Step 0: input validation ----------------------------------------
  sanitize_all_inputs(features_file = features)
  validate_ms_mode(ms_mode)
  validate_tolerances(
    tolerance_ppm = tolerance_ppm,
    tolerance_rt = tolerance_rt,
    max_ppm = 20,
    max_rt = 0.05,
    context = "mass annotation"
  )
  validate_adduct_list(adducts_list, ms_mode, "adducts_list")
  validate_adduct_list(clusters_list, ms_mode, "clusters_list")
  validate_file_existence(list(
    features = features,
    library = library,
    str_stereo = str_stereo,
    str_met = str_met,
    str_tax_cla = str_tax_cla,
    str_tax_npc = str_tax_npc
  ))

  cfg <- resolve_adduct_consistency_config(
    adduct_consistency = adduct_consistency,
    adduct_min_support = adduct_min_support,
    adduct_consistency_min_degree = adduct_consistency_min_degree
  )
  coverage_mode <- "best_supported_conflict_free"
  baseline_adduct <- switch(ms_mode, "pos" = "[M+H]+", "neg" = "[M-H]-")
  baseline_adducts <- switch(
    ms_mode,
    "pos" = c("[M+H]+", "[M+Na]+", "[M+H4N]+"),
    "neg" = c("[M-H]-", "[M+COOH-H]-")
  )

  # ---- Step 1: load features -------------------------------------------
  features_table <- safe_fread(
    file = features,
    file_type = "features table",
    required_cols = "feature_id",
    na.strings = c("", "NA"),
    colClasses = "character"
  )

  if (nrow(features_table) == 0L) {
    log_info("Empty features table provided - no annotations to perform")
    return(write_empty_annotate_masses_outputs(
      output_annotations = output_annotations,
      output_edges = output_edges
    ))
  }
  log_info("Processing %d features for annotation", nrow(features_table))

  loaded_inputs <- load_annotate_masses_runtime_inputs(
    features_table = features_table,
    tolerance_rt = tolerance_rt,
    baseline_adduct = baseline_adduct,
    library = library,
    str_stereo = str_stereo,
    str_met = str_met,
    str_tax_cla = str_tax_cla,
    str_tax_npc = str_tax_npc,
    tolerance_ppm = tolerance_ppm
  )
  features_table <- loaded_inputs$features_table
  already_assigned <- loaded_inputs$already_assigned
  lib <- loaded_inputs$lib

  # ---- Step 3: feature pairs inside RT windows -------------------------
  pairs <- build_feature_pairs_within_rt(
    df_rt_tol = features_table,
    df_fea_min = features_table,
    tolerance_ppm = tolerance_ppm
  )
  log_top_pair_deltas(pairs)

  ion_tables <- build_annotate_masses_ion_tables(
    adducts_list = adducts_list,
    clusters_list = clusters_list,
    ms_mode = ms_mode
  )
  adducts <- ion_tables$adducts
  clusters <- ion_tables$clusters
  monocharged_adducts <- ion_tables$monocharged_adducts
  multi_adducts <- ion_tables$multi_adducts

  # ---- Step 4: classify each pair --------------------------------------
  edge_sets <- discover_annotate_masses_edge_sets(
    pairs = pairs,
    features_table = features_table,
    monocharged_adducts = monocharged_adducts,
    adducts = adducts,
    clusters = clusters,
    neutral_losses_list = neutral_losses_list,
    ms_mode = ms_mode,
    tolerance_ppm = tolerance_ppm,
    tolerance_rt = tolerance_rt,
    tolerance_dalton = tolerance_dalton,
    cfg = cfg,
    exact_masses = lib$em_windows$exact_mass
  )
  adduct_edges <- edge_sets$adduct_edges
  adduct_edges_combined <- edge_sets$adduct_edges_combined
  cluster_edges <- edge_sets$cluster_edges
  loss_edges <- edge_sets$loss_edges
  evidence_signal <- edge_sets$evidence_signal

  # ---- Step 5: per-feature node-level adduct hypotheses ----------------
  node_hypotheses <- build_annotate_masses_candidate_hypotheses(
    adduct_edges = adduct_edges_combined,
    cluster_edges = cluster_edges,
    loss_edges = loss_edges,
    evidence_hypotheses = evidence_signal$hypotheses,
    preassigned = already_assigned,
    baseline_adducts = baseline_adducts,
    features_table = features_table,
    multi_adducts = multi_adducts,
    tolerance_ppm = tolerance_ppm,
    tolerance_dalton = tolerance_dalton
  )

  # Recover weak but M-coherent modifier states for unresolved nodes in
  # supported components without relaxing strict primary graph semantics.
  node_hypotheses <- append_component_weak_hypotheses(
    node_hypotheses = node_hypotheses,
    features_table = features_table,
    neutral_losses_list = neutral_losses_list,
    clusters = clusters,
    baseline_adducts = baseline_adducts,
    tolerance_ppm = tolerance_ppm,
    tolerance_dalton = tolerance_dalton
  )

  # ---- Step 7: library match by neutral mass ---------------------------
  annotations <- build_annotate_masses_annotations(
    node_hypotheses = node_hypotheses,
    library_em = lib$em_windows,
    structure_table = lib$structures,
    tolerance_ppm = tolerance_ppm,
    tolerance_dalton = tolerance_dalton,
    adduct_edges = adduct_edges_combined,
    baseline_adduct = baseline_adduct,
    coverage_mode = coverage_mode
  )

  # ---- Step 8: derive primary/secondary per-feature ion species -----------
  annotations <- derive_primary_secondary_annotations(
    annotations = annotations,
    baseline_adducts = baseline_adducts
  )

  # ---- Step 9: propagate annotations across M-cliques ---------------------
  annotations <- propagate_annotations_across_m_cliques(
    annotations = annotations,
    node_hypotheses = node_hypotheses,
    tolerance_ppm = tolerance_ppm,
    tolerance_dalton = tolerance_dalton
  )

  supported_graph <- retain_supported_single_m_edges(
    adduct_edges = adduct_edges_combined,
    cluster_edges = cluster_edges,
    loss_edges = loss_edges,
    annotations = annotations,
    tolerance_ppm = tolerance_ppm,
    tolerance_dalton = tolerance_dalton
  )
  adduct_edges_combined <- supported_graph$adduct_edges
  cluster_edges <- supported_graph$cluster_edges
  loss_edges <- supported_graph$loss_edges
  annotations <- supported_graph$annotations

  coverage_audit <- attr(annotations, "coverage_audit")
  if (is.list(coverage_audit)) {
    log_info(
      paste0(
        "Coverage audit: kept %d/%d annotation rows across %d/%d features; ",
        "%d annotation rows were pruned from %d feature(s)."
      ),
      coverage_audit$n_kept,
      coverage_audit$n_input,
      coverage_audit$n_kept_features,
      coverage_audit$n_input_features,
      coverage_audit$n_dropped,
      coverage_audit$n_dropped_features
    )
  }

  decorate_masses(annotations)
  log_adduct_breakdown(annotations)

  # ---- Step 11: edges file ---------------------------------------------
  edges_out <- build_output_edges(
    adduct_edges = adduct_edges_combined,
    loss_edges = loss_edges,
    cluster_edges = cluster_edges,
    features_table = features_table,
    name_source = name_source,
    name_target = name_target
  )

  # ---- Step 12: write outputs ------------------------------------------
  export_params(
    parameters = get_params(step = "annotate_masses"),
    step = "annotate_masses"
  )
  export_output(x = edges_out, file = output_edges[[1L]])

  coverage_report <- build_annotate_masses_coverage_report(
    annotations = annotations,
    baseline_adduct = baseline_adduct
  )
  coverage_file <- derive_annotate_masses_coverage_path(output_annotations[[
    1L
  ]])

  export_output(
    x = annotations |>
      # Rename 'adduct' to the canonical output column name expected by
      # downstream tools (select_annotations_columns, weight_annotations, etc.)
      tidytable::rename(
        candidate_adduct = adduct,
        candidate_annotation_level = annotation_level,
        candidate_evidence_tier = evidence_tier
      ) |>
      select_annotations_columns(
        str_stereo = str_stereo,
        str_met = str_met,
        str_tax_cla = str_tax_cla,
        str_tax_npc = str_tax_npc
      ),
    file = output_annotations[[1L]]
  )
  export_output(x = coverage_report, file = coverage_file)
  log_info("Coverage summary written to: %s", coverage_file)

  log_complete(
    ctx,
    n_annotations = nrow(annotations),
    n_edges = nrow(edges_out)
  )

  c(
    "annotations" = output_annotations[[1L]],
    "edges" = output_edges[[1L]]
  )
}

#' Enforce one primary ion species per feature while keeping non-conflicting
#' secondary alternatives.
#' @keywords internal
derive_primary_secondary_annotations <- function(
  annotations,
  baseline_adducts
) {
  if (nrow(annotations) == 0L) {
    return(annotations)
  }
  out <- annotations

  # Ensure all required columns exist efficiently
  if (!"source" %in% colnames(out)) {
    out$source <- NA_character_
  }
  if (!"adduct_support" %in% colnames(out)) {
    out$adduct_support <- 0L
  }
  if (!"error_mz" %in% colnames(out)) {
    out$error_mz <- NA_real_
  }
  if (!"candidate_adduct_origin" %in% colnames(out)) {
    out <- out |>
      tidytable::mutate(
        candidate_adduct_origin = tidytable::if_else(
          source == "baseline" | adduct %in% baseline_adducts,
          "baseline",
          "supported"
        )
      )
  }

  # Single comprehensive mutation pass with all ranking logic
  out <- out |>
    tidytable::mutate(
      evidence_tier = tidytable::case_when(
        candidate_adduct_origin == "supported" ~ "supported_strong",
        candidate_adduct_origin == "supported_weak" ~ "supported_weak",
        TRUE ~ "baseline"
      ),
      has_structure = !is.na(structure_exact_mass),
      support_rank = tidytable::case_when(
        evidence_tier == "supported_strong" ~ 1L,
        evidence_tier == "supported_weak" ~ 2L,
        TRUE ~ 3L
      ),
      source_rank = tidytable::case_when(
        source == "pair" ~ 1L,
        source == "evidence" ~ 2L,
        source == "loss" ~ 3L,
        source == "cluster" ~ 4L,
        source == "preassigned" ~ 5L,
        source == "preassigned_propagated" ~ 6L,
        source == "multi" ~ 7L,
        source == "baseline" ~ 8L,
        source == "weak_component" ~ 9L,
        source == "weak_component_loss" ~ 10L,
        source == "weak_component_cluster" ~ 11L,
        TRUE ~ 12L
      ),
      abs_error = abs(as.numeric(error_mz))
    )

  # Single aggregation pass for all statistics per feature-adduct pair
  picked <- out |>
    tidytable::summarize(
      any_structure = any(has_structure, na.rm = TRUE),
      max_support = max(adduct_support, na.rm = TRUE),
      best_support_rank = min(support_rank, na.rm = TRUE),
      best_source_rank = min(source_rank, na.rm = TRUE),
      min_abs_error = suppressWarnings(min(abs_error, na.rm = TRUE)),
      .by = c(feature_id, adduct)
    ) |>
    tidytable::mutate(
      min_abs_error = tidytable::if_else(
        !is.finite(min_abs_error),
        Inf,
        min_abs_error
      )
    ) |>
    tidytable::arrange(
      feature_id,
      tidytable::desc(any_structure),
      best_support_rank,
      tidytable::desc(max_support),
      best_source_rank,
      min_abs_error,
      adduct
    ) |>
    tidytable::distinct(feature_id, .keep_all = TRUE) |>
    tidytable::select(feature_id, primary_adduct = adduct)

  # Final annotation level assignment with cleanup
  out |>
    tidytable::left_join(picked, by = "feature_id") |>
    tidytable::mutate(
      annotation_level = tidytable::if_else(
        adduct == primary_adduct,
        "primary",
        "secondary"
      )
    ) |>
    tidytable::select(
      -has_structure,
      -support_rank,
      -source_rank,
      -abs_error,
      -primary_adduct
    )
}

#' Recover weak (component-M coherent) hypotheses for unresolved nodes
#' @keywords internal
append_component_weak_hypotheses <- function(
  node_hypotheses,
  features_table,
  neutral_losses_list,
  clusters,
  baseline_adducts,
  tolerance_ppm,
  tolerance_dalton
) {
  if (nrow(node_hypotheses) == 0L) {
    return(node_hypotheses)
  }
  component_membership <- attr(node_hypotheses, "component_membership")
  feature_m_map <- attr(node_hypotheses, "feature_m_map")
  if (is.null(component_membership) || nrow(component_membership) == 0L) {
    return(node_hypotheses)
  }
  if (is.null(feature_m_map) || nrow(feature_m_map) == 0L) {
    return(node_hypotheses)
  }

  baseline_adducts <- unique(stats::na.omit(as.character(baseline_adducts)))
  if (length(baseline_adducts) == 0L) {
    return(node_hypotheses)
  }

  component_m <- feature_m_map |>
    tidytable::filter(
      !is.na(neutral_mass) & is.finite(neutral_mass) & neutral_mass > 0
    ) |>
    tidytable::summarize(
      component_mass = stats::median(neutral_mass),
      .by = component_id
    )
  if (nrow(component_m) == 0L) {
    return(node_hypotheses)
  }

  supported_features <- node_hypotheses |>
    tidytable::mutate(
      candidate_adduct_origin = tidytable::coalesce(
        candidate_adduct_origin,
        "supported"
      )
    ) |>
    tidytable::filter(candidate_adduct_origin == "supported") |>
    tidytable::distinct(feature_id)

  unresolved <- component_membership |>
    tidytable::anti_join(supported_features, by = "feature_id") |>
    tidytable::left_join(component_m, by = "component_id") |>
    tidytable::left_join(
      features_table |>
        tidytable::distinct(feature_id, mz, rt),
      by = "feature_id"
    ) |>
    tidytable::filter(!is.na(component_mass) & !is.na(mz) & is.finite(mz))
  if (nrow(unresolved) == 0L) {
    return(node_hypotheses)
  }

  build_modified_adduct <- function(adduct, modifier, sign) {
    paste0(
      gsub("M(?![a-z]).*", "M", adduct, perl = TRUE),
      sign,
      modifier,
      gsub(".*M(?![a-z])", "", adduct, perl = TRUE)
    )
  }

  base_candidates <- unresolved |>
    tidytable::cross_join(tidytable::tidytable(adduct = baseline_adducts)) |>
    tidytable::mutate(source = "weak_component")

  loss_terms <- unique(gsub(" .*", "", as.character(neutral_losses_list)))
  loss_terms <- loss_terms[!is.na(loss_terms) & nzchar(loss_terms)]
  loss_candidates <- if (length(loss_terms) > 0L) {
    base_candidates |>
      tidytable::cross_join(tidytable::tidytable(modifier = loss_terms)) |>
      tidytable::mutate(
        adduct = mapply(
          FUN = build_modified_adduct,
          adduct = adduct,
          modifier = modifier,
          MoreArgs = list(sign = "-"),
          USE.NAMES = FALSE
        ),
        source = "weak_component_loss"
      ) |>
      tidytable::select(-modifier)
  } else {
    base_candidates[0L, ]
  }

  cluster_terms <- unique(as.character(clusters))
  cluster_terms <- cluster_terms[!is.na(cluster_terms) & nzchar(cluster_terms)]
  cluster_candidates <- if (length(cluster_terms) > 0L) {
    base_candidates |>
      tidytable::cross_join(tidytable::tidytable(modifier = cluster_terms)) |>
      tidytable::mutate(
        adduct = mapply(
          FUN = build_modified_adduct,
          adduct = adduct,
          modifier = modifier,
          MoreArgs = list(sign = "+"),
          USE.NAMES = FALSE
        ),
        source = "weak_component_cluster"
      ) |>
      tidytable::select(-modifier)
  } else {
    base_candidates[0L, ]
  }

  weak_all <- tidytable::bind_rows(
    base_candidates,
    loss_candidates,
    cluster_candidates
  ) |>
    harmonize_adducts(adducts_translations = adducts_translations) |>
    tidytable::mutate(
      mz_expected = mapply(
        FUN = function(m, ad) {
          calculate_mz_from_mass(
            neutral_mass = as.numeric(m),
            adduct_string = ad
          )
        },
        m = component_mass,
        ad = adduct,
        USE.NAMES = FALSE
      ),
      mz_diff = abs(as.numeric(mz) - as.numeric(mz_expected)),
      ppm_diff = mz_diff * 1e6 / pmax(as.numeric(mz), as.numeric(mz_expected)),
      ppm_ok = is.finite(ppm_diff) & ppm_diff <= tolerance_ppm,
      dalton_ok = if (is.null(tolerance_dalton)) {
        FALSE
      } else {
        mz_diff <= tolerance_dalton
      }
    ) |>
    tidytable::filter(ppm_ok | dalton_ok) |>
    tidytable::transmute(
      feature_id,
      adduct,
      source,
      is_preassigned = FALSE,
      adduct_support = 0L,
      candidate_adduct_origin = "supported_weak",
      mz = as.numeric(mz),
      rt = as.numeric(rt),
      mass = as.numeric(component_mass)
    ) |>
    tidytable::distinct()

  if (nrow(weak_all) == 0L) {
    return(node_hypotheses)
  }

  out <- tidytable::bind_rows(node_hypotheses, weak_all) |>
    dedupe_node_hypotheses()
  attr(out, "component_membership") <- component_membership
  attr(out, "feature_m_map") <- feature_m_map
  out
}

#' Keep only supported edges that are coherent with a single neutral M
#' @keywords internal
retain_supported_single_m_edges <- function(
  adduct_edges,
  cluster_edges,
  loss_edges,
  annotations,
  tolerance_ppm,
  tolerance_dalton
) {
  invisible(tolerance_dalton)
  if (nrow(annotations) == 0L) {
    return(list(
      adduct_edges = adduct_edges[0L, ],
      cluster_edges = cluster_edges[0L, ],
      loss_edges = loss_edges[0L, ],
      annotations = annotations
    ))
  }

  selected <- annotations |>
    tidytable::distinct(
      feature_id,
      adduct,
      annotation_level,
      evidence_tier,
      source,
      candidate_adduct_origin,
      mass,
      mz
    ) |>
    tidytable::mutate(
      annotation_level = tidytable::coalesce(annotation_level, "primary"),
      evidence_tier = tidytable::coalesce(
        evidence_tier,
        "supported_strong"
      ),
      is_supported = annotation_level == "primary" &
        evidence_tier == "supported_strong"
    )

  supported_nodes <- selected |>
    tidytable::filter(is_supported)
  if (nrow(supported_nodes) == 0L) {
    return(list(
      adduct_edges = adduct_edges[0L, ],
      cluster_edges = cluster_edges[0L, ],
      loss_edges = loss_edges[0L, ],
      annotations = annotations
    ))
  }

  adduct_kept <- adduct_edges |>
    tidytable::inner_join(
      supported_nodes |>
        tidytable::select(feature_id, adduct, mass_src = mass),
      by = c("feature_id", "adduct")
    ) |>
    tidytable::inner_join(
      supported_nodes |>
        tidytable::select(
          feature_id_dest = feature_id,
          adduct_dest = adduct,
          mass_dest = mass
        ),
      by = c("feature_id_dest", "adduct_dest")
    ) |>
    tidytable::mutate(
      mass_diff = abs(mass_src - mass_dest),
      ppm_ok = mass_diff <= (tolerance_ppm * 1e-6 * pmax(mass_src, mass_dest)),
      dalton_ok = if (is.null(tolerance_dalton)) {
        FALSE
      } else {
        mass_diff <= tolerance_dalton
      }
    ) |>
    tidytable::filter(ppm_ok | dalton_ok) |>
    tidytable::select(feature_id, adduct, feature_id_dest, adduct_dest) |>
    tidytable::distinct()

  if (nrow(adduct_kept) == 0L) {
    return(list(
      adduct_edges = adduct_kept,
      cluster_edges = cluster_edges[0L, ],
      loss_edges = loss_edges[0L, ],
      annotations = annotations
    ))
  }

  undirected <- tidytable::bind_rows(
    adduct_kept |>
      tidytable::transmute(src = feature_id, dest = feature_id_dest),
    adduct_kept |>
      tidytable::transmute(src = feature_id_dest, dest = feature_id)
  ) |>
    tidytable::distinct()
  all_nodes <- unique(c(undirected$src, undirected$dest))
  neighbors <- split(undirected$dest, undirected$src)
  visited <- stats::setNames(rep(FALSE, length(all_nodes)), all_nodes)
  comp_members_list <- list()
  comp_i <- 0L
  for (node in all_nodes) {
    if (isTRUE(visited[[node]])) {
      next
    }
    comp_i <- comp_i + 1L
    queue <- c(node)
    visited[[node]] <- TRUE
    comp_nodes <- character()
    while (length(queue) > 0L) {
      current <- queue[[1L]]
      queue <- queue[-1L]
      comp_nodes <- c(comp_nodes, current)
      nxt <- neighbors[[current]]
      if (is.null(nxt)) {
        next
      }
      for (nn in nxt) {
        if (!isTRUE(visited[[nn]])) {
          visited[[nn]] <- TRUE
          queue <- c(queue, nn)
        }
      }
    }
    comp_members_list[[length(comp_members_list) + 1L]] <- tidytable::tidytable(
      feature_id = unique(comp_nodes),
      component_id = paste0("M_", comp_i)
    )
  }
  comp_members <- tidytable::bind_rows(comp_members_list)

  cluster_kept <- cluster_edges |>
    tidytable::inner_join(comp_members, by = "feature_id") |>
    tidytable::inner_join(
      comp_members |>
        tidytable::rename(
          feature_id_dest = feature_id,
          component_id_dest = component_id
        ),
      by = "feature_id_dest"
    ) |>
    tidytable::filter(component_id == component_id_dest) |>
    tidytable::select(feature_id, cluster, mass, feature_id_dest) |>
    tidytable::distinct()

  loss_kept <- loss_edges |>
    tidytable::inner_join(comp_members, by = "feature_id") |>
    tidytable::inner_join(
      comp_members |>
        tidytable::rename(
          feature_id_dest = feature_id,
          component_id_dest = component_id
        ),
      by = "feature_id_dest"
    ) |>
    tidytable::filter(component_id == component_id_dest) |>
    tidytable::select(feature_id, loss, mass, feature_id_dest) |>
    tidytable::distinct()

  annotations <- annotations |>
    tidytable::left_join(comp_members, by = "feature_id") |>
    tidytable::mutate(
      component_id = tidytable::if_else(
        is.na(component_id),
        paste0("M_singleton_", feature_id),
        component_id
      )
    )

  list(
    adduct_edges = adduct_kept,
    cluster_edges = cluster_kept,
    loss_edges = loss_kept,
    annotations = annotations
  )
}

#' Load prepared features and structural library for annotate_masses
#' @keywords internal
load_annotate_masses_runtime_inputs <- function(
  features_table,
  tolerance_rt,
  baseline_adduct,
  library,
  str_stereo,
  str_met,
  str_tax_cla,
  str_tax_npc,
  tolerance_ppm
) {
  features_table <- prepare_features_table(features_table, tolerance_rt)
  already_assigned <- extract_preassigned_adducts(features_table)
  log_info(
    paste0(
      "Pre-assigned adducts kept as hypotheses alongside the %s baseline: %d"
    ),
    baseline_adduct,
    nrow(already_assigned)
  )
  lib <- load_structural_library(
    library = library,
    str_stereo = str_stereo,
    str_met = str_met,
    str_tax_cla = str_tax_cla,
    str_tax_npc = str_tax_npc,
    tolerance_ppm = tolerance_ppm
  )
  list(
    features_table = features_table,
    already_assigned = already_assigned,
    lib = lib
  )
}

#' Resolve adduct tables used by annotate_masses ion discovery
#' @keywords internal
build_annotate_masses_ion_tables <- function(
  adducts_list,
  clusters_list,
  ms_mode
) {
  adducts <- adducts_list[[ms_mode]]
  clusters <- clusters_list[[ms_mode]]
  adducts_table <- adducts |>
    tidytable::tidytable() |>
    tidytable::rename(adduct = 1) |>
    harmonize_adducts(adducts_translations = adducts_translations)
  monocharged_adducts <- adducts_table |>
    tidytable::filter(grepl("[M", adduct, fixed = TRUE)) |>
    tidytable::filter(grepl("](\\+|\\-)", adduct)) |>
    tidytable::mutate(
      adduct_mass = -1 * calculate_mass_of_m_batch(adducts = adduct, mzs = 0)
    )
  if (nrow(monocharged_adducts) == 0L) {
    log_error(
      "No valid monocharged adducts for mode '%s'. Aborting.",
      ms_mode
    )
    cli::cli_abort(
      c(
        "no valid monocharged adducts or clusters found",
        "x" = paste0("mode: ", ms_mode),
        "i" = "check adduct and cluster configuration"
      ),
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }
  multi_adducts <- adducts_table |>
    tidytable::filter(!adduct %in% monocharged_adducts$adduct) |>
    tidytable::distinct(adduct)
  list(
    adducts = adducts,
    clusters = clusters,
    adducts_table = adducts_table,
    monocharged_adducts = monocharged_adducts,
    multi_adducts = multi_adducts
  )
}

#' Discover all edge classes used by annotate_masses
#' @keywords internal
discover_annotate_masses_edge_sets <- function(
  pairs,
  features_table,
  monocharged_adducts,
  adducts,
  clusters,
  neutral_losses_list,
  ms_mode,
  tolerance_ppm,
  tolerance_rt,
  tolerance_dalton,
  cfg,
  exact_masses
) {
  invisible(tolerance_dalton)
  adduct_diffs <- build_adduct_pair_differences(
    add_clu_table = monocharged_adducts,
    tolerance_ppm = tolerance_ppm,
    max_mz = max(features_table$mz, na.rm = TRUE)
  )
  adduct_edges <- match_pairs_to_adduct_diffs(pairs, adduct_diffs)
  adduct_edges <- apply_adduct_consistency_filter(
    df_add = adduct_edges,
    adduct_consistency = cfg$adduct_consistency,
    adduct_min_support = cfg$adduct_min_support,
    adduct_consistency_min_degree = cfg$adduct_consistency_min_degree
  )
  if (cfg$adduct_consistency == "strict" && nrow(adduct_edges) > 0L) {
    adduct_edges <- enforce_graph_adduct_consistency(df_add = adduct_edges)
    log_consistency_audit(attr(adduct_edges, "consistency_audit"))
  } else if (cfg$adduct_consistency == "conditional") {
    log_debug(
      paste0(
        "Adduct consistency mode is 'conditional': applying local support ",
        "filter only (graph-level single-state enforcement is skipped)."
      )
    )
  }

  clusters_table <- clusters |>
    tidytable::tidytable() |>
    tidytable::rename(cluster = 1) |>
    tidytable::mutate_rowwise(
      mass = suppressWarnings(MetaboCoreUtils::calculateMass(cluster))
    ) |>
    tidytable::filter(!is.na(mass) & is.finite(mass) & mass > 0)
  cluster_edges <- match_pairs_to_mass_diffs(
    pairs = pairs,
    diffs = clusters_table,
    diff_col = "cluster"
  )

  neutral_losses_table <- neutral_losses_list |>
    tidytable::tidytable() |>
    tidytable::rename(loss = 1) |>
    tidytable::mutate_rowwise(
      mass = suppressWarnings(
        MetaboCoreUtils::calculateMass(gsub(" .*", "", loss))
      )
    ) |>
    tidytable::filter(!is.na(mass) & is.finite(mass) & mass > 0)
  loss_edges <- match_pairs_to_mass_diffs(
    pairs = pairs,
    diffs = neutral_losses_table,
    diff_col = "loss"
  )

  evidence_signal <- discover_evidence_adduct_signal(
    features_table = features_table,
    adducts = adducts,
    clusters = clusters,
    neutral_losses = neutral_losses_list,
    ms_mode = ms_mode,
    tolerance_ppm = tolerance_ppm,
    tolerance_dalton = tolerance_dalton,
    tolerance_rt = tolerance_rt,
    exact_masses = exact_masses
  ) |>
    filter_modifier_evidence_by_pairwise_support(
      universe = build_adduct_universe(
        adducts_list = list(pos = adducts, neg = adducts),
        clusters_list = list(pos = clusters, neg = clusters),
        neutral_losses_list = neutral_losses_list,
        polarity = ms_mode
      ),
      adduct_edges = adduct_edges,
      cluster_edges = cluster_edges,
      loss_edges = loss_edges,
      baseline_adduct = switch(ms_mode, "pos" = "[M+H]+", "neg" = "[M-H]-"),
      features_table = features_table
    )

  adduct_edges_combined <- tidytable::bind_rows(
    adduct_edges,
    evidence_signal$edges
  ) |>
    tidytable::distinct()
  if (nrow(evidence_signal$edges) > 0L) {
    log_info(
      "Evidence-based discovery added %d adduct edge(s)",
      nrow(evidence_signal$edges)
    )
  }

  list(
    adduct_edges = adduct_edges,
    adduct_edges_combined = adduct_edges_combined,
    cluster_edges = cluster_edges,
    loss_edges = loss_edges,
    evidence_signal = evidence_signal
  )
}

#' Build and expand node-level candidate hypotheses for annotate_masses
#' @keywords internal
build_annotate_masses_candidate_hypotheses <- function(
  adduct_edges,
  cluster_edges,
  loss_edges,
  evidence_hypotheses,
  preassigned,
  baseline_adducts,
  features_table,
  multi_adducts,
  tolerance_ppm,
  tolerance_dalton
) {
  node_hypotheses <- collect_node_adduct_hypotheses(
    adduct_edges = adduct_edges,
    cluster_edges = cluster_edges,
    loss_edges = loss_edges,
    evidence_hypotheses = evidence_hypotheses,
    preassigned = preassigned,
    baseline_adducts = baseline_adducts,
    features_table = features_table,
    tolerance_ppm = tolerance_ppm,
    tolerance_dalton = tolerance_dalton
  ) |>
    tidytable::mutate(
      mass = calculate_mass_of_m_batch(
        adducts = adduct,
        mzs = as.numeric(mz)
      )
    )

  bad_mass <- node_hypotheses |>
    tidytable::filter(is.na(mass) | !is.finite(mass) | mass <= 0)
  if (nrow(bad_mass) > 0L) {
    log_warn(
      paste0(
        "%d candidate adduct hypotheses produced an invalid neutral mass ",
        "(NA/non-finite/<=0) and will be dropped. Consider extending ",
        "adducts_translations if you see unexpected adduct strings."
      ),
      nrow(bad_mass)
    )
  }
  node_hypotheses <- node_hypotheses |>
    tidytable::filter(!is.na(mass) & is.finite(mass) & mass > 0)

  if (nrow(multi_adducts) > 0L && nrow(node_hypotheses) > 0L) {
    multi_seed <- node_hypotheses |>
      tidytable::filter(candidate_adduct_origin == "supported")
    multi_hypotheses <- generate_multi_hypotheses_from_node_masses(
      node_hypotheses = multi_seed,
      multi_adducts = multi_adducts,
      tolerance_ppm = tolerance_ppm,
      tolerance_dalton = tolerance_dalton
    )
    if (nrow(multi_hypotheses) > 0L) {
      multi_hypotheses <- multi_hypotheses |>
        tidytable::mutate(candidate_adduct_origin = "supported")
      node_hypotheses <- tidytable::bind_rows(
        node_hypotheses,
        multi_hypotheses
      ) |>
        dedupe_node_hypotheses()
      log_info(
        "Constrained multi-adduct expansion kept %d hypothesis row(s)",
        nrow(multi_hypotheses)
      )
    }
  }

  node_hypotheses
}

#' Match, enrich, and prune annotate_masses annotations
#' @keywords internal
build_annotate_masses_annotations <- function(
  node_hypotheses,
  library_em,
  structure_table,
  tolerance_ppm,
  tolerance_dalton,
  adduct_edges,
  baseline_adduct,
  coverage_mode = "best_supported_conflict_free"
) {
  matched <- match_candidates_to_library(
    candidates = node_hypotheses,
    library_em = library_em,
    tolerance_ppm = tolerance_ppm,
    tolerance_dalton = tolerance_dalton
  ) |>
    prune_candidates_by_network_consensus(
      adduct_edges = adduct_edges,
      baseline_adduct = baseline_adduct
    )

  unmatched <- node_hypotheses |>
    tidytable::anti_join(
      matched |> tidytable::distinct(feature_id, adduct),
      by = c("feature_id", "adduct")
    ) |>
    tidytable::mutate(
      structure_exact_mass = NA_real_,
      error_mz = NA_real_
    )

  annotations <- tidytable::bind_rows(matched, unmatched) |>
    enrich_with_structure_metadata(structure_table = structure_table)

  # Structural matches are kept only when atoms removed in the adduct (e.g.
  # -H2O, -C6H10O5) are actually available in candidate molecular formula.
  # Incompatible rows are demoted to unmatched adduct hypotheses.
  # annotations <- enforce_loss_formula_compatibility(annotations)

  annotations <- enforce_annotation_edge_adduct_agreement(
    annotations = annotations,
    adduct_edges = adduct_edges,
    baseline_adduct = baseline_adduct
  )

  enforce_non_conflicting_annotation_states(
    annotations = annotations,
    adduct_edges = adduct_edges,
    baseline_adduct = baseline_adduct,
    coverage_mode = coverage_mode
  )
}

#' Enforce globally non-conflicting annotation states where graph evidence exists
#'
#' Strategy:
#'   1) Build a graph-consistent subset of adduct edges (single state per node)
#'      via [enforce_graph_adduct_consistency()].
#'   2) For features represented in that constrained graph, keep only annotation
#'      rows whose adduct state key matches the assigned graph-consistent state.
#'   3) For features with no constraining edge evidence, either keep broad rows
#'      (`broad_conflict_free`) or keep only best-supported rows
#'      (`best_supported_conflict_free`).
#' @keywords internal
enforce_non_conflicting_annotation_states <- function(
  annotations,
  adduct_edges,
  baseline_adduct = NULL,
  coverage_mode = "best_supported_conflict_free"
) {
  if (nrow(annotations) == 0L || nrow(adduct_edges) == 0L) {
    return(annotations)
  }
  required_cols <- c("feature_id", "adduct")
  if (!all(required_cols %in% colnames(annotations))) {
    return(annotations)
  }

  consistent_edges <- enforce_graph_adduct_consistency(adduct_edges)
  if (nrow(consistent_edges) == 0L) {
    return(annotations)
  }

  assigned_states <- tidytable::bind_rows(
    consistent_edges |>
      tidytable::transmute(feature_id, adduct),
    consistent_edges |>
      tidytable::transmute(feature_id = feature_id_dest, adduct = adduct_dest)
  ) |>
    tidytable::distinct()
  if (nrow(assigned_states) == 0L) {
    return(annotations)
  }

  state_map <- build_adduct_state_key_map(unique(c(
    annotations$adduct,
    assigned_states$adduct
  ))) |>
    tidytable::rename(adduct_state_key = state_key)

  assigned_states <- assigned_states |>
    tidytable::left_join(state_map, by = "adduct") |>
    tidytable::mutate(
      assigned_state_key = tidytable::coalesce(adduct_state_key, adduct)
    ) |>
    tidytable::select(feature_id, assigned_state_key) |>
    tidytable::distinct()

  out <- annotations |>
    tidytable::left_join(state_map, by = "adduct") |>
    tidytable::mutate(
      adduct_state_key = tidytable::coalesce(adduct_state_key, adduct)
    ) |>
    tidytable::left_join(assigned_states, by = "feature_id") |>
    tidytable::mutate(
      keep = is.na(assigned_state_key) |
        (adduct_state_key == assigned_state_key)
    )

  coverage_mode <- resolve_annotate_masses_coverage_mode(coverage_mode)
  if (coverage_mode == "best_supported_conflict_free") {
    if (!"source" %in% colnames(out)) {
      out$source <- NA_character_
    }
    if (!"candidate_structure_error_mz" %in% colnames(out)) {
      out$candidate_structure_error_mz <- NA_real_
    }
    if (!"adduct_support" %in% colnames(out)) {
      out$adduct_support <- 0L
    }
    baseline_on <- !is.null(baseline_adduct) &&
      length(baseline_adduct) >= 1L &&
      !is.na(baseline_adduct[[1L]]) &&
      nzchar(baseline_adduct[[1L]])
    baseline_label <- if (baseline_on) baseline_adduct[[1L]] else NA_character_

    out <- out |>
      tidytable::mutate(
        support_class_rank = tidytable::case_when(
          !is.na(candidate_structure_error_mz) ~ 1L,
          source %in% c("pair", "preassigned", "preassigned_propagated") ~ 2L,
          source == "multi" ~ 3L,
          source %in% c("cluster", "loss") ~ 4L,
          source == "evidence" ~ 5L,
          baseline_on & !is.na(adduct) & adduct == baseline_label ~ 6L,
          TRUE ~ 7L
        ),
        support_strength = tidytable::if_else(
          is.na(adduct_support),
          0L,
          as.integer(adduct_support)
        )
      ) |>
      tidytable::group_by(feature_id) |>
      tidytable::mutate(
        feature_is_unconstrained = all(is.na(assigned_state_key)),
        best_rank = ifelse(
          any(keep),
          min(support_class_rank[keep], na.rm = TRUE),
          min(support_class_rank, na.rm = TRUE)
        ),
        best_support = ifelse(
          any(keep & support_class_rank == best_rank),
          max(
            support_strength[keep & support_class_rank == best_rank],
            na.rm = TRUE
          ),
          max(support_strength[support_class_rank == best_rank], na.rm = TRUE)
        ),
        keep = keep &
          (!feature_is_unconstrained |
            (support_class_rank == best_rank &
              support_strength == best_support))
      ) |>
      tidytable::ungroup()
  }

  dropped <- out |>
    tidytable::filter(!keep)
  if (nrow(dropped) > 0L) {
    dropped_n <- dropped |>
      tidytable::distinct(feature_id, adduct) |>
      nrow()
    dropped_features <- dropped |>
      tidytable::distinct(feature_id) |>
      nrow()
    kept_features <- out |>
      tidytable::filter(keep) |>
      tidytable::distinct(feature_id) |>
      nrow()
    dropped_all_features <- max(
      0L,
      nrow(tidytable::distinct(annotations, feature_id)) - kept_features
    )
    log_info(
      paste0(
        "Conflict-resolution filter removed %d annotation row(s) with states ",
        "incompatible with graph-consistent evidence."
      ),
      dropped_n
    )
    log_info(
      "Conflict-resolution pruning touched %d feature(s) and removed all annotations from %d feature(s).",
      dropped_features,
      dropped_all_features
    )
  }

  out <- out |>
    tidytable::filter(keep) |>
    tidytable::select(
      -adduct_state_key,
      -assigned_state_key,
      -keep,
      -tidyselect::any_of(c(
        "support_class_rank",
        "support_strength",
        "feature_is_unconstrained",
        "best_rank",
        "best_support"
      ))
    )

  attr(out, "coverage_audit") <- list(
    n_input = nrow(annotations),
    n_kept = nrow(out),
    n_dropped = nrow(dropped),
    n_input_features = nrow(annotations |> tidytable::distinct(feature_id)),
    n_kept_features = nrow(out |> tidytable::distinct(feature_id)),
    n_dropped_features = if (nrow(dropped) > 0L) {
      nrow(dropped |> tidytable::distinct(feature_id))
    } else {
      0L
    }
  )
  out
}
