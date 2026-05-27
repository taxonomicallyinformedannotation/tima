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
    baseline_adduct = baseline_adduct,
    features_table = features_table,
    multi_adducts = multi_adducts,
    tolerance_ppm = tolerance_ppm
  )

  # ---- Step 7: library match by neutral mass ---------------------------
  annotations <- build_annotate_masses_annotations(
    node_hypotheses = node_hypotheses,
    library_em = lib$em_windows,
    structure_table = lib$structures,
    tolerance_ppm = tolerance_ppm,
    adduct_edges = adduct_edges_combined,
    baseline_adduct = baseline_adduct,
    coverage_mode = coverage_mode
  )

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
      tidytable::rename(candidate_adduct = adduct) |>
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
  cfg,
  exact_masses
) {
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
  baseline_adduct,
  features_table,
  multi_adducts,
  tolerance_ppm
) {
  node_hypotheses <- collect_node_adduct_hypotheses(
    adduct_edges = adduct_edges,
    cluster_edges = cluster_edges,
    loss_edges = loss_edges,
    evidence_hypotheses = evidence_hypotheses,
    preassigned = preassigned,
    baseline_adduct = baseline_adduct,
    features_table = features_table
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
    multi_hypotheses <- generate_multi_hypotheses_from_node_masses(
      node_hypotheses = node_hypotheses,
      multi_adducts = multi_adducts,
      tolerance_ppm = tolerance_ppm
    )
    if (nrow(multi_hypotheses) > 0L) {
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
  adduct_edges,
  baseline_adduct,
  coverage_mode = "best_supported_conflict_free"
) {
  matched <- match_candidates_to_library(
    candidates = node_hypotheses,
    library_em = library_em,
    tolerance_ppm = tolerance_ppm
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

# ============================================================
#                       STEP HELPERS
# ============================================================

#' Resolve annotate_masses coverage mode
#' @keywords internal
resolve_annotate_masses_coverage_mode <- function(coverage_mode) {
  mode <- coverage_mode
  if (is.null(mode) || (length(mode) == 1L && is.na(mode))) {
    mode <- "best_supported_conflict_free"
  }
  mode <- tolower(as.character(mode[[1L]]))
  allowed <- c("best_supported_conflict_free", "broad_conflict_free")
  if (!mode %in% allowed) {
    log_warn(
      "Invalid coverage_mode='%s', falling back to 'best_supported_conflict_free'",
      mode
    )
    mode <- "best_supported_conflict_free"
  }
  mode
}

#' Resolve `adduct_consistency*` config defaults
#' @keywords internal
resolve_adduct_consistency_config <- function(
  adduct_consistency,
  adduct_min_support,
  adduct_consistency_min_degree
) {
  ac <- adduct_consistency
  if (is.null(ac) || (length(ac) == 1L && is.na(ac))) {
    ac <- "conditional"
  }
  ac <- tolower(as.character(ac[[1L]]))
  if (!ac %in% c("off", "conditional", "strict")) {
    log_warn(
      "Invalid adduct_consistency='%s', falling back to 'conditional'",
      ac
    )
    ac <- "conditional"
  }

  ms <- adduct_min_support
  if (is.null(ms) || (length(ms) == 1L && is.na(ms))) {
    ms <- 2L
  }
  ms <- suppressWarnings(as.integer(ms[[1L]]))
  if (is.na(ms) || !is.finite(ms) || ms < 1L) {
    ms <- 2L
  }

  md <- adduct_consistency_min_degree
  if (is.null(md) || (length(md) == 1L && is.na(md))) {
    md <- 3L
  }
  md <- suppressWarnings(as.integer(md[[1L]]))
  if (is.na(md) || !is.finite(md) || md < 1L) {
    md <- 3L
  }

  list(
    adduct_consistency = ac,
    adduct_min_support = ms,
    adduct_consistency_min_degree = md
  )
}

#' Write the empty-output sentinel files
#' @keywords internal
write_empty_annotate_masses_outputs <- function(
  output_annotations,
  output_edges
) {
  ann_cols <- c(
    "feature_id",
    "candidate_structure_error_mz",
    "candidate_structure_name",
    "candidate_structure_inchikey_connectivity_layer",
    "candidate_structure_smiles_no_stereo",
    "candidate_structure_molecular_formula",
    "candidate_structure_exact_mass",
    "candidate_structure_xlogp",
    "candidate_structure_tag",
    "candidate_library",
    "candidate_structure_tax_npc_01pat",
    "candidate_structure_tax_npc_02sup",
    "candidate_structure_tax_npc_03cla",
    "candidate_structure_tax_cla_chemontid",
    "candidate_structure_tax_cla_01kin",
    "candidate_structure_tax_cla_02sup",
    "candidate_structure_tax_cla_03cla",
    "candidate_structure_tax_cla_04dirpar",
    "candidate_adduct"
  )
  empty_annotations <- as.data.frame(
    matrix(ncol = length(ann_cols), nrow = 0L)
  )
  colnames(empty_annotations) <- ann_cols

  edge_cols <- c("CLUSTERID1", "CLUSTERID2", "label")
  empty_edges <- as.data.frame(matrix(ncol = length(edge_cols), nrow = 0L))
  colnames(empty_edges) <- edge_cols

  empty_coverage <- build_annotate_masses_coverage_report(
    annotations = tidytable::tidytable(),
    baseline_adduct = NA_character_
  )

  export_output(x = empty_annotations, file = output_annotations)
  export_output(x = empty_edges, file = output_edges)
  export_output(
    x = empty_coverage,
    file = derive_annotate_masses_coverage_path(output_annotations[[1L]])
  )
  c(
    "annotations" = output_annotations[[1L]],
    "edges" = output_edges[[1L]]
  )
}

#' Derive the annotate_masses coverage-report file path
#' @keywords internal
derive_annotate_masses_coverage_path <- function(output_annotations) {
  output_annotations <- as.character(output_annotations)[1L]
  if (is.na(output_annotations) || !nzchar(output_annotations)) {
    return("annotate_masses_coverage.tsv")
  }
  if (grepl("\\.tsv(\\.gz)?$", output_annotations)) {
    return(sub("\\.tsv(\\.gz)?$", "_coverage.tsv\\1", output_annotations))
  }
  paste0(output_annotations, "_coverage.tsv")
}

#' Build a feature-level coverage report for annotate_masses
#' @keywords internal
build_annotate_masses_coverage_report <- function(
  annotations,
  baseline_adduct
) {
  if (nrow(annotations) == 0L) {
    return(tidytable::tidytable(
      coverage_scope = c("best", "any"),
      coverage_class = c("all", "all"),
      coverage_tier = c(0L, 0L),
      N_features = c(0L, 0L),
      N_annotations = c(0L, 0L),
      Pct_features = c("0.00%", "0.00%"),
      Pct_annotations = c("0.00%", "0.00%")
    ))
  }

  support_ranked <- tidytable::as_tidytable(
    as.data.frame(annotations, stringsAsFactors = FALSE)
  )
  if (!"source" %in% names(support_ranked)) {
    support_ranked$source <- NA_character_
  }
  if (!"adduct" %in% names(support_ranked)) {
    support_ranked$adduct <- NA_character_
  }
  if (!"candidate_structure_error_mz" %in% names(support_ranked)) {
    support_ranked$candidate_structure_error_mz <- NA_real_
  }
  if (!"adduct_support" %in% names(support_ranked)) {
    support_ranked$adduct_support <- 0L
  }

  baseline_active <- !is.null(baseline_adduct) &&
    length(baseline_adduct) >= 1L &&
    !is.na(baseline_adduct[[1L]]) &&
    nzchar(baseline_adduct[[1L]])
  baseline_label <- if (baseline_active) {
    baseline_adduct[[1L]]
  } else {
    NA_character_
  }

  support_ranked <- support_ranked |>
    tidytable::mutate(
      has_structure_match = !is.na(candidate_structure_error_mz),
      has_pairwise_support = source %in%
        c("pair", "preassigned", "preassigned_propagated"),
      has_modifier_pairwise_support = source %in% c("cluster", "loss"),
      has_multicharge_evidence = source %in% c("multi"),
      has_evidence_support = source %in% c("evidence"),
      has_baseline_fallback = baseline_active &
        !is.na(adduct) &
        adduct == baseline_label,
      coverage_class = tidytable::case_when(
        has_structure_match ~ "structure_matched",
        has_pairwise_support ~ "pairwise_supported",
        has_multicharge_evidence ~ "evidence_multicharge_supported",
        has_modifier_pairwise_support ~ "modifier_pairwise_supported",
        has_evidence_support ~ "evidence_supported",
        has_baseline_fallback ~ "baseline_fallback",
        TRUE ~ "adduct_only"
      ),
      coverage_tier = tidytable::case_when(
        coverage_class == "structure_matched" ~ 1L,
        coverage_class == "pairwise_supported" ~ 2L,
        coverage_class == "evidence_multicharge_supported" ~ 3L,
        coverage_class == "modifier_pairwise_supported" ~ 4L,
        coverage_class == "evidence_supported" ~ 5L,
        coverage_class == "baseline_fallback" ~ 6L,
        TRUE ~ 7L
      )
    )

  feature_annotation_counts <- support_ranked |>
    tidytable::summarize(N_annotations = .N, .by = feature_id)

  best_feature_class <- support_ranked |>
    tidytable::arrange(
      feature_id,
      coverage_tier,
      tidytable::desc(has_structure_match),
      tidytable::desc(has_pairwise_support),
      tidytable::desc(has_multicharge_evidence),
      tidytable::desc(has_modifier_pairwise_support),
      tidytable::desc(has_evidence_support),
      tidytable::desc(has_baseline_fallback),
      tidytable::desc(adduct_support),
      adduct
    )
  best_feature_class <- best_feature_class[, .SD[1L], by = feature_id]

  best_summary <- best_feature_class |>
    tidytable::left_join(feature_annotation_counts, by = "feature_id") |>
    tidytable::summarize(
      N_features = .N,
      N_annotations = sum(N_annotations, na.rm = TRUE),
      .by = c(coverage_class, coverage_tier)
    )

  any_summary <- support_ranked |>
    tidytable::summarize(
      N_features = tidytable::n_distinct(feature_id),
      N_annotations = .N,
      .by = c(coverage_class, coverage_tier)
    )

  all_features <- tidytable::n_distinct(support_ranked$feature_id)
  all_annotations <- nrow(support_ranked)

  all_rows <- tidytable::tidytable(
    coverage_scope = c("best", "any"),
    coverage_class = c("all", "all"),
    coverage_tier = c(0L, 0L),
    N_features = c(all_features, all_features),
    N_annotations = c(all_annotations, all_annotations)
  )

  coverage_report <- tidytable::bind_rows(
    all_rows,
    tidytable::as_tidytable(best_summary) |>
      tidytable::mutate(coverage_scope = "best"),
    tidytable::as_tidytable(any_summary) |>
      tidytable::mutate(coverage_scope = "any")
  )

  coverage_report$Pct_features <- sprintf(
    "%.2f%%",
    if (all_features == 0L) {
      rep(0, nrow(coverage_report))
    } else {
      100 * coverage_report$N_features / all_features
    }
  )
  coverage_report$Pct_annotations <- sprintf(
    "%.2f%%",
    if (all_annotations == 0L) {
      rep(0, nrow(coverage_report))
    } else {
      100 * coverage_report$N_annotations / all_annotations
    }
  )

  coverage_report <- coverage_report |>
    tidytable::select(
      coverage_scope,
      coverage_class,
      coverage_tier,
      N_features,
      N_annotations,
      Pct_features,
      Pct_annotations
    ) |>
    tidytable::arrange(coverage_scope, coverage_tier, coverage_class)

  coverage_report
}

#' Coerce, harmonize, and prepare the features table for downstream steps
#' @keywords internal
prepare_features_table <- function(features_table, tolerance_rt) {
  if (!"adduct" %in% colnames(features_table)) {
    features_table$adduct <- NA_character_
  }
  if ("candidate_adduct" %in% colnames(features_table)) {
    features_table$adduct <- ifelse(
      is.na(features_table$adduct) | !nzchar(features_table$adduct),
      features_table$candidate_adduct,
      features_table$adduct
    )
  }
  features_table <- features_table |>
    harmonize_adducts(adducts_translations = adducts_translations)

  if (!"sample" %in% colnames(features_table)) {
    log_debug("No 'sample' column; using 'all'")
    features_table$sample <- "all"
  }
  if (!"rt" %in% colnames(features_table)) {
    log_warn("No 'rt' column; using sequential numbering as RT proxy")
    features_table$rt <- seq_len(nrow(features_table))
  }

  features_table |>
    tidytable::mutate(
      mz = as.numeric(mz),
      rt = as.numeric(rt)
    ) |>
    tidytable::distinct(feature_id, sample, .keep_all = TRUE) |>
    tidytable::mutate(
      rt_min = rt - tolerance_rt,
      rt_max = rt + tolerance_rt
    )
}

#' Extract any non-NA pre-assigned adducts as explicit hypotheses
#' @keywords internal
extract_preassigned_adducts <- function(features_table) {
  if (
    !"adduct" %in% colnames(features_table) &&
      !"candidate_adduct" %in% colnames(features_table)
  ) {
    return(tidytable::tidytable(feature_id = character(), adduct = character()))
  }
  if (!"adduct" %in% colnames(features_table)) {
    features_table$adduct <- NA_character_
  }
  if (!"candidate_adduct" %in% colnames(features_table)) {
    features_table$candidate_adduct <- NA_character_
  }
  pre_raw <- features_table |>
    tidytable::mutate(
      adduct = tidytable::if_else(
        is.na(adduct) | !nzchar(adduct),
        candidate_adduct,
        adduct
      )
    ) |>
    tidytable::distinct(feature_id, adduct)

  adduct_chr <- as.character(pre_raw$adduct)
  adduct_chr[is.na(adduct_chr)] <- ""
  parts <- strsplit(adduct_chr, "[|/]", perl = TRUE)
  lens <- lengths(parts)
  if (sum(lens) == 0L) {
    return(tidytable::tidytable(feature_id = character(), adduct = character()))
  }

  expanded <- tidytable::tidytable(
    feature_id = rep(pre_raw$feature_id, lens),
    adduct = trimws(unlist(parts, use.names = FALSE))
  )

  expanded |>
    tidytable::filter(!is.na(adduct) & nzchar(adduct)) |>
    tidytable::distinct(feature_id, adduct) |>
    harmonize_adducts(adducts_translations = adducts_translations)
}

#' Propagate pre-assigned adduct labels through adduct edges in both directions
#'
#' If an edge hypothesis says `(feature_id, adduct) -> (feature_id_dest,
#' adduct_dest)`, and one side is pre-assigned upstream, the opposite side gets
#' the corresponding propagated pre-assignment candidate.
#' @keywords internal
propagate_preassigned_over_adduct_edges <- function(adduct_edges, preassigned) {
  if (nrow(adduct_edges) == 0L || nrow(preassigned) == 0L) {
    return(tidytable::tidytable(feature_id = character(), adduct = character()))
  }
  pre <- preassigned |>
    tidytable::distinct(feature_id, adduct) |>
    tidytable::filter(!is.na(adduct) & nzchar(adduct))
  if (nrow(pre) == 0L) {
    return(tidytable::tidytable(feature_id = character(), adduct = character()))
  }

  forward <- adduct_edges |>
    tidytable::inner_join(pre, by = c("feature_id", "adduct")) |>
    tidytable::transmute(feature_id = feature_id_dest, adduct = adduct_dest)

  reverse <- adduct_edges |>
    tidytable::inner_join(
      pre,
      by = c("feature_id_dest" = "feature_id", "adduct_dest" = "adduct")
    ) |>
    tidytable::transmute(feature_id, adduct)

  tidytable::bind_rows(forward, reverse) |>
    tidytable::distinct() |>
    harmonize_adducts(adducts_translations = adducts_translations)
}

#' Load the structural library, join supplementary tables, and build the
#' ppm-window table for neutral-mass lookups
#' @keywords internal
load_structural_library <- function(
  library,
  str_stereo,
  str_met,
  str_tax_cla,
  str_tax_npc,
  tolerance_ppm
) {
  library_table <- safe_fread(
    file = library,
    file_type = "structure library",
    na.strings = c("", "NA"),
    colClasses = "character"
  )
  supp_files <- list(str_stereo, str_met, str_tax_cla, str_tax_npc)
  supp_names <- c(
    "stereochemistry",
    "metadata",
    "ClassyFire taxonomy",
    "NPClassifier taxonomy"
  )
  supp_tables <- purrr::map2(
    .x = supp_files,
    .y = supp_names,
    .f = ~ safe_fread(
      file = .x,
      file_type = .y,
      na.strings = c("", "NA"),
      colClasses = "character"
    )
  )
  joined <- purrr::reduce(
    .x = c(list(library_table), supp_tables),
    .f = tidytable::left_join
  )

  structures <- joined |>
    tidytable::filter(!is.na(structure_exact_mass)) |>
    tidytable::mutate(
      structure_exact_mass = as.numeric(structure_exact_mass)
    )
  if (!"structure_inchikey_connectivity_layer" %in% colnames(structures)) {
    structures <- structures |>
      tidytable::mutate(
        structure_inchikey_connectivity_layer = stringi::stri_sub(
          str = structure_inchikey,
          from = 1L,
          to = 14L
        )
      )
  }
  structures <- round_reals(structures)

  em_windows <- structures |>
    tidytable::distinct(exact_mass = structure_exact_mass) |>
    tidytable::filter(!is.na(exact_mass) & exact_mass > 0) |>
    tidytable::mutate(
      value_min = exact_mass - (1E-6 * tolerance_ppm * exact_mass),
      value_max = exact_mass + (1E-6 * tolerance_ppm * exact_mass)
    )

  list(structures = structures, em_windows = em_windows)
}

#' Log the top observed pair-delta bins for QC
#' @keywords internal
log_top_pair_deltas <- function(pairs) {
  if (nrow(pairs) == 0L) {
    return(invisible(NULL))
  }
  bins <- pairs[, .N, by = .(bin = cut(delta, breaks = 10000L))] |>
    tidytable::arrange(tidytable::desc(N)) |>
    tidytable::slice_head(n = 10L)
  bins <- add_percentage_column(bins, count_col = "N", out_col = "Pct")
  log_info(
    "Here are the top 10 observed m/z differences inside the RT windows:"
  )
  log_info(
    "\n%s",
    paste(
      utils::capture.output(print.data.frame(bins, row.names = FALSE)),
      collapse = "\n"
    )
  )
  invisible(NULL)
}

#' Log the per-adduct annotation breakdown table
#'
#' Produces the classic "Breakdown of the annotated adduct species" table
#' (N_features / N_annotations / Pct) that was present in the original
#' monolithic \code{annotate_masses()} implementation.
#'
#' @param annotations Data frame returned by \code{enrich_with_structure_metadata()}.
#'   Expected to have at minimum \code{feature_id} and \code{adduct} columns.
#'   When a \code{source} column is present the "baseline / enforced" fallback
#'   rows are reported separately.
#' @keywords internal
log_adduct_breakdown <- function(annotations) {
  if (nrow(annotations) == 0L || !"adduct" %in% colnames(annotations)) {
    return(invisible(NULL))
  }

  # ---- library-matched adduct breakdown (main breakdown) ------------------
  # Only count rows that have an actual library hit (error_mz not NA).
  # Unmatched hypotheses retained for downstream use are excluded here to
  # avoid inflating counts with speculative cross-join candidates.
  has_error_col <- "candidate_structure_error_mz" %in% colnames(annotations)
  matched_ann <- if (has_error_col) {
    annotations |> tidytable::filter(!is.na(candidate_structure_error_mz))
  } else {
    annotations |> tidytable::filter(!is.na(adduct))
  }

  if (nrow(matched_ann) > 0L) {
    adduct_bd <- matched_ann |>
      tidytable::filter(!is.na(adduct)) |>
      tidytable::summarise(
        N_features = tidytable::n_distinct(feature_id),
        N_annotations = .N,
        .by = adduct
      ) |>
      tidytable::arrange(
        tidytable::desc(N_features),
        tidytable::desc(N_annotations)
      )
    adduct_bd <- add_percentage_column(
      adduct_bd,
      count_col = "N_features",
      out_col = "Pct_features"
    )
    adduct_bd <- add_percentage_column(
      adduct_bd,
      count_col = "N_annotations",
      out_col = "Pct_annotations"
    )
    log_info("Breakdown of the annotated adduct species (library-matched):")
    log_info(
      "\n%s",
      paste(
        utils::capture.output(
          print.data.frame(x = adduct_bd, row.names = FALSE)
        ),
        collapse = "\n"
      )
    )
  }

  # ---- retained-but-unmatched adduct hypotheses --------------------------
  # These are adducts kept in the output even though no library structure
  # matched; they still carry adduct-network evidence and are useful for
  # downstream tools.  Log a compact summary grouped by source.
  if (has_error_col && "source" %in% colnames(annotations)) {
    unmatched_ann <- annotations |>
      tidytable::filter(
        !is.na(adduct) & is.na(candidate_structure_error_mz)
      )
    if (nrow(unmatched_ann) > 0L) {
      unmatched_bd <- unmatched_ann |>
        tidytable::summarise(
          N_features = tidytable::n_distinct(feature_id),
          N_adduct_types = tidytable::n_distinct(adduct),
          .by = source
        ) |>
        tidytable::arrange(tidytable::desc(N_features))
      log_info(
        "Adduct hypotheses retained without library match (by source):"
      )
      log_info(
        "\n%s",
        paste(
          utils::capture.output(
            print.data.frame(x = unmatched_bd, row.names = FALSE)
          ),
          collapse = "\n"
        )
      )
    }
  }

  invisible(NULL)
}

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
      is_safe = (!is.null(baseline_adduct) & adduct == baseline_adduct) |
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
  struct_unique <- structure_table |>
    tidytable::distinct(tidyselect::any_of(struct_cols)) |>
    tidytable::distinct(
      tidyselect::any_of(c(
        "structure_inchikey_connectivity_layer",
        "structure_molecular_formula",
        "structure_exact_mass"
      )),
      .keep_all = TRUE
    ) |>
    tidytable::mutate(tidytable::across(
      .cols = tidyselect::where(is.numeric),
      .fns = as.character
    ))

  enriched <- combined |>
    tidytable::mutate(
      structure_exact_mass = as.character(structure_exact_mass)
    )
  join_keys <- intersect(colnames(enriched), colnames(struct_unique))
  if (length(join_keys) > 0L) {
    enriched <- enriched |>
      tidytable::left_join(struct_unique, by = join_keys)
  }

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
  if (any(tax_cols %in% colnames(structure_table))) {
    taxonomy_table <- structure_table |>
      tidytable::distinct(tidyselect::any_of(tax_cols))
    tax_keys <- intersect(colnames(enriched), colnames(taxonomy_table))
    if (length(tax_keys) > 0L) {
      enriched <- enriched |>
        tidytable::left_join(taxonomy_table, by = tax_keys)
    }
  }

  # canonical_adduct <- vapply(
  #   X = enriched$adduct,
  #   FUN = function(a) {
  #     if (is.na(a)) NA_character_ else canonicalize_adduct_notation(a)
  #   },
  #   FUN.VALUE = character(1L),
  #   USE.NAMES = FALSE
  # )

  enriched |>
    tidytable::mutate(
      # candidate_adduct = canonical_adduct,
      candidate_library = "TIMA MS1",
      candidate_structure_error_mz = as.numeric(error_mz)
    ) |>
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
    ) |>
    tidytable::distinct()
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
  req <- integer()
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
  for (el in names(parsed)) {
    req[el] <- (if (is.na(req[el])) 0L else req[el]) +
      as.integer(parsed[[el]] * coef)
  }
  req
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
  req <- integer()
  for (tok in toks) {
    if (startsWith(tok, "-")) {
      tok_req <- loss_term_atom_requirements(substring(tok, 2L))
      for (el in names(tok_req)) {
        req[el] <- (if (is.na(req[el])) 0L else req[el]) + tok_req[[el]]
      }
    }
  }
  req
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
  for (el in names(loss_requirements)) {
    if (!el %in% names(parsed) || parsed[[el]] < loss_requirements[[el]]) {
      return(FALSE)
    }
  }
  TRUE
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
  idx_check <- which(
    !is.na(annotations$candidate_structure_error_mz) &
      !is.na(annotations$adduct) &
      ((has_source & annotations$source == "loss") |
        (has_loss_term & !is.na(annotations$loss_term)))
  )
  if (length(idx_check) == 0L) {
    return(annotations)
  }

  keep_struct <- vapply(
    X = idx_check,
    FUN = function(i) {
      req <- if (has_loss_term && !is.na(annotations$loss_term[[i]])) {
        loss_term_atom_requirements(annotations$loss_term[[i]])
      } else {
        adduct_loss_atom_requirements(annotations$adduct[[i]])
      }
      fs <- formula_satisfies_loss_requirements(
        formula = annotations$candidate_structure_molecular_formula[[i]],
        loss_requirements = req
      )
      if (is.na(fs)) TRUE else fs
    },
    FUN.VALUE = logical(1L)
  )
  idx_drop <- idx_check[!keep_struct]
  if (length(idx_drop) == 0L) {
    return(annotations)
  }

  out <- tidytable::as_tidytable(annotations)
  struct_cols <- unique(c(
    grep("^candidate_structure_", colnames(out), value = TRUE),
    intersect(
      colnames(out),
      c("candidate_library", "structure_exact_mass", "error_mz")
    )
  ))
  for (cc in struct_cols) {
    out[idx_drop, (cc) := NA]
  }
  out <- out |> tidytable::distinct()

  dropped <- annotations[idx_drop, , drop = FALSE]
  dropped_bd <- dropped |>
    tidytable::distinct(feature_id, adduct) |>
    tidytable::count(adduct, name = "N") |>
    tidytable::arrange(tidytable::desc(N))
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
  if (nrow(annotations) == 0L) {
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
  if (!"adduct" %in% colnames(adduct_key_map)) {
    adduct_key_map$adduct <- character(nrow(adduct_key_map))
  }
  if (!"state_key" %in% colnames(adduct_key_map)) {
    adduct_key_map$state_key <- adduct_key_map$adduct
  }
  adduct_key_map <- adduct_key_map |>
    tidytable::mutate(adduct_state_key = state_key) |>
    tidytable::select(adduct, adduct_state_key)
  key_lookup <- stats::setNames(
    object = adduct_key_map$adduct_state_key,
    nm = adduct_key_map$adduct
  )

  edge_supported <- if (nrow(adduct_edges) > 0L) {
    tidytable::bind_rows(
      adduct_edges |>
        tidytable::transmute(feature_id, adduct),
      adduct_edges |>
        tidytable::transmute(feature_id = feature_id_dest, adduct = adduct_dest)
    ) |>
      tidytable::mutate(adduct_state_key = unname(key_lookup[adduct])) |>
      tidytable::filter(!is.na(adduct_state_key)) |>
      tidytable::distinct(feature_id, adduct_state_key)
  } else {
    tidytable::tidytable(
      feature_id = character(),
      adduct_state_key = character()
    )
  }

  has_source <- "source" %in% colnames(annotations)
  has_is_preassigned <- "is_preassigned" %in% colnames(annotations)
  has_origin <- "candidate_adduct_origin" %in% colnames(annotations)
  preassigned_safe <- if (has_is_preassigned) {
    annotations$is_preassigned
  } else {
    rep(FALSE, nrow(annotations))
  }
  origin_safe <- if (has_origin) {
    annotations$candidate_adduct_origin %in% c("enforced", "preassigned")
  } else {
    rep(FALSE, nrow(annotations))
  }
  baseline_safe <- if (!is.null(baseline_adduct)) {
    annotations$adduct == baseline_adduct
  } else {
    rep(FALSE, nrow(annotations))
  }

  out <- annotations |>
    tidytable::mutate(adduct_state_key = unname(key_lookup[adduct])) |>
    tidytable::left_join(
      edge_supported |>
        tidytable::mutate(edge_supported = TRUE),
      by = c("feature_id", "adduct_state_key")
    ) |>
    tidytable::mutate(
      edge_supported = tidytable::if_else(
        is.na(edge_supported),
        FALSE,
        edge_supported
      )
    ) |>
    tidytable::group_by(feature_id) |>
    tidytable::mutate(feature_has_edge_support = any(edge_supported)) |>
    tidytable::ungroup() |>
    tidytable::mutate(
      keep = edge_supported |
        (!feature_has_edge_support & !(has_source & source == "multi")) |
        (has_source & source %in% c("loss", "cluster")) |
        preassigned_safe |
        origin_safe |
        baseline_safe |
        (has_source &
          source %in%
            c(
              "baseline",
              "preassigned",
              "preassigned_propagated"
            ))
    )

  dropped <- out |> tidytable::filter(!keep)
  if (nrow(dropped) > 0L) {
    if ("adduct" %in% colnames(dropped)) {
      dropped_n <- nrow(dropped |> tidytable::distinct(feature_id, adduct))
      dropped_bd <- dropped |>
        tidytable::distinct(feature_id, adduct) |>
        tidytable::count(adduct, name = "N") |>
        tidytable::arrange(tidytable::desc(N))
    } else {
      dropped_n <- nrow(dropped |> tidytable::distinct(feature_id))
      dropped_bd <- dropped |>
        tidytable::count(name = "N")
    }
    log_info(
      paste0(
        "Annotation/edge adduct agreement removed %d unsupported ",
        "(feature, adduct) assignment(s)."
      ),
      dropped_n
    )
  }

  out |>
    tidytable::filter(keep) |>
    tidytable::select(
      -tidyselect::any_of(c(
        "adduct_state_key",
        "edge_supported",
        "feature_has_edge_support",
        "keep"
      ))
    )
}

#' Build the output edges table from the three edge sets
#' @keywords internal
build_output_edges <- function(
  adduct_edges,
  loss_edges,
  cluster_edges,
  features_table,
  name_source,
  name_target
) {
  parts <- list()
  if (nrow(adduct_edges) > 0L) {
    parts[[length(parts) + 1L]] <- adduct_edges |>
      tidytable::mutate(label = paste0(adduct, " _ ", adduct_dest)) |>
      tidytable::select(
        !!as.name(name_source) := feature_id,
        !!as.name(name_target) := feature_id_dest,
        label
      ) |>
      tidytable::distinct()
  }
  if (nrow(loss_edges) > 0L) {
    parts[[length(parts) + 1L]] <- loss_edges |>
      tidytable::mutate(label = paste0(loss, " loss")) |>
      tidytable::select(
        # NL convention: precursor (higher mz, feature_id_dest) -> product
        !!as.name(name_source) := feature_id_dest,
        !!as.name(name_target) := feature_id,
        label
      ) |>
      tidytable::distinct()
  }
  if (nrow(cluster_edges) > 0L) {
    parts[[length(parts) + 1L]] <- cluster_edges |>
      tidytable::mutate(label = paste0("+", cluster, " cluster")) |>
      tidytable::select(
        # cluster: lower mz -> higher mz (which carries the cluster)
        !!as.name(name_source) := feature_id,
        !!as.name(name_target) := feature_id_dest,
        label
      ) |>
      tidytable::distinct()
  }
  edges <- if (length(parts) > 0L) {
    tidytable::bind_rows(parts)
  } else {
    tidytable::tidytable(
      !!as.name(name_source) := character(),
      !!as.name(name_target) := character(),
      label = character()
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
#               because tests import these by name)
# ============================================================

#' Build feature pairs inside RT windows.
#'
#' For every pair `(src, dest)` of features in the same RT tolerance window
#' (and same sample), with `mz_dest >= mz_src`, returns `delta = mz_dest -
#' mz_src` together with `delta_min/delta_max` widened by the ppm tolerance.
#' @keywords internal
build_feature_pairs_within_rt <- function(
  df_rt_tol,
  df_fea_min,
  tolerance_ppm
) {
  src <- tidytable::as_tidytable(df_rt_tol)[,
    .(
      src_feature_id = feature_id,
      src_rt = rt,
      src_mz = mz,
      src_adduct = adduct,
      sample,
      rt_min,
      rt_max
    )
  ]
  dest <- tidytable::as_tidytable(df_fea_min)[,
    .(
      feature_id_dest = feature_id,
      rt_dest = rt,
      mz_dest = mz,
      adduct_dest = adduct,
      sample
    )
  ]
  matches <- dest[
    src,
    on = .(sample, rt_dest >= rt_min, rt_dest <= rt_max),
    nomatch = 0L,
    allow.cartesian = TRUE
  ][
    src_feature_id != feature_id_dest & mz_dest >= src_mz,
    .(
      feature_id = src_feature_id,
      rt = src_rt,
      mz = src_mz,
      adduct = src_adduct,
      feature_id_dest,
      mz_dest,
      adduct_dest
    )
  ]
  if (nrow(matches) == 0L) {
    return(tidytable::as_tidytable(matches)[,
      `:=`(delta = numeric(), delta_min = numeric(), delta_max = numeric())
    ])
  }
  matches <- unique(matches)
  matches[, delta := mz_dest - mz]
  matches[,
    `:=`(
      delta_min = delta - (1E-6 * tolerance_ppm * (mz + mz_dest) / 2),
      delta_max = delta + (1E-6 * tolerance_ppm * (mz + mz_dest) / 2)
    )
  ]
  tidytable::as_tidytable(matches) |>
    tidytable::select(-adduct, -adduct_dest)
}

#' Build all ordered adduct-pair differences for single-charge adducts.
#' @keywords internal
build_adduct_pair_differences <- function(
  add_clu_table,
  tolerance_ppm,
  max_mz
) {
  add_src <- add_clu_table |>
    tidytable::distinct(adduct, adduct_mass) |>
    tidytable::rename(Group1 = adduct, mass1 = adduct_mass) |>
    tidytable::mutate(join = "x")
  add_dest <- add_clu_table |>
    tidytable::distinct(adduct, adduct_mass) |>
    tidytable::rename(Group2 = adduct, mass2 = adduct_mass) |>
    tidytable::mutate(join = "x")
  add_src |>
    tidytable::left_join(y = add_dest, by = "join") |>
    tidytable::filter(Group1 != Group2) |>
    tidytable::filter(mass1 < mass2) |>
    tidytable::mutate(Distance = mass2 - mass1) |>
    tidytable::distinct(Group1, Group2, Distance) |>
    tidytable::filter(Distance >= tolerance_ppm * 1E-6 * max_mz) |>
    tidytable::select(Distance, Group1, Group2)
}

#' Count, for each (feature, adduct), how many distinct neighbours in the
#' adduct graph independently support that label.
#' @keywords internal
compute_feature_adduct_support <- function(df_add) {
  if (nrow(df_add) == 0L) {
    return(tidytable::tidytable(
      feature_id = character(),
      adduct = character(),
      adduct_support = integer()
    ))
  }
  tidytable::bind_rows(
    df_add |>
      tidytable::transmute(
        feature_id,
        adduct,
        neighbor = feature_id_dest
      ),
    df_add |>
      tidytable::transmute(
        feature_id = feature_id_dest,
        adduct = adduct_dest,
        neighbor = feature_id
      )
  ) |>
    tidytable::distinct(feature_id, adduct, neighbor) |>
    tidytable::count(feature_id, adduct, name = "adduct_support") |>
    tidytable::mutate(adduct_support = as.integer(adduct_support))
}

#' Apply optional support-based adduct consistency filtering.
#' @keywords internal
apply_adduct_consistency_filter <- function(
  df_add,
  adduct_consistency = "conditional",
  adduct_min_support = 2L,
  adduct_consistency_min_degree = 3L
) {
  if (nrow(df_add) == 0L || adduct_consistency == "off") {
    return(df_add)
  }
  pair_hyp <- df_add |>
    tidytable::count(feature_id, feature_id_dest, name = "pair_hypotheses")
  feature_degree <- tidytable::bind_rows(
    df_add |>
      tidytable::transmute(feature_id, neighbor = feature_id_dest),
    df_add |>
      tidytable::transmute(feature_id = feature_id_dest, neighbor = feature_id)
  ) |>
    tidytable::distinct(feature_id, neighbor) |>
    tidytable::count(feature_id, name = "feature_degree")
  support <- compute_feature_adduct_support(df_add)
  scored <- df_add |>
    tidytable::left_join(
      pair_hyp,
      by = c("feature_id", "feature_id_dest")
    ) |>
    tidytable::left_join(
      support |> tidytable::rename(src_support = adduct_support),
      by = c("feature_id", "adduct")
    ) |>
    tidytable::left_join(
      support |>
        tidytable::rename(
          feature_id_dest = feature_id,
          adduct_dest = adduct,
          dest_support = adduct_support
        ),
      by = c("feature_id_dest", "adduct_dest")
    ) |>
    tidytable::left_join(
      feature_degree |> tidytable::rename(src_degree = feature_degree),
      by = "feature_id"
    ) |>
    tidytable::left_join(
      feature_degree |>
        tidytable::rename(
          feature_id_dest = feature_id,
          dest_degree = feature_degree
        ),
      by = "feature_id_dest"
    ) |>
    tidytable::mutate(
      pair_hypotheses = tidytable::if_else(
        is.na(pair_hypotheses),
        1L,
        as.integer(pair_hypotheses)
      ),
      src_support = tidytable::if_else(
        is.na(src_support),
        0L,
        as.integer(src_support)
      ),
      dest_support = tidytable::if_else(
        is.na(dest_support),
        0L,
        as.integer(dest_support)
      ),
      src_degree = tidytable::if_else(
        is.na(src_degree),
        0L,
        as.integer(src_degree)
      ),
      dest_degree = tidytable::if_else(
        is.na(dest_degree),
        0L,
        as.integer(dest_degree)
      )
    )
  if (adduct_consistency == "strict") {
    return(
      scored |>
        tidytable::filter(src_support >= adduct_min_support) |>
        tidytable::filter(dest_support >= adduct_min_support) |>
        tidytable::select(feature_id, adduct, adduct_dest, feature_id_dest) |>
        tidytable::distinct()
    )
  }
  scored |>
    tidytable::mutate(
      need_check = pair_hypotheses > 1L |
        src_degree >= adduct_consistency_min_degree |
        dest_degree >= adduct_consistency_min_degree
    ) |>
    tidytable::filter(
      !need_check |
        src_support >= adduct_min_support |
        dest_support >= adduct_min_support
    ) |>
    tidytable::select(feature_id, adduct, adduct_dest, feature_id_dest) |>
    tidytable::distinct()
}

#' Enforce graph-level adduct consistency on edge hypotheses
#' @keywords internal
enforce_graph_adduct_consistency <- function(df_add) {
  if (nrow(df_add) == 0L) {
    return(df_add)
  }
  support <- compute_feature_adduct_support(df_add)
  state_map <- build_adduct_state_key_map(unique(c(
    df_add$adduct,
    df_add$adduct_dest
  )))
  scored <- df_add |>
    tidytable::left_join(
      support |> tidytable::rename(src_support = adduct_support),
      by = c("feature_id", "adduct")
    ) |>
    tidytable::left_join(
      support |>
        tidytable::rename(
          feature_id_dest = feature_id,
          adduct_dest = adduct,
          dest_support = adduct_support
        ),
      by = c("feature_id_dest", "adduct_dest")
    ) |>
    tidytable::mutate(
      src_support = tidytable::if_else(
        is.na(src_support),
        0L,
        as.integer(src_support)
      ),
      dest_support = tidytable::if_else(
        is.na(dest_support),
        0L,
        as.integer(dest_support)
      ),
      edge_score = src_support + dest_support,
      edge_complexity = nchar(adduct) + nchar(adduct_dest)
    ) |>
    tidytable::left_join(
      state_map |> tidytable::rename(adduct_state_key = state_key),
      by = "adduct"
    ) |>
    tidytable::left_join(
      state_map |>
        tidytable::rename(
          adduct_dest = adduct,
          adduct_dest_state_key = state_key
        ),
      by = "adduct_dest"
    ) |>
    tidytable::mutate(
      adduct_state_key = tidytable::coalesce(adduct_state_key, adduct),
      adduct_dest_state_key = tidytable::coalesce(
        adduct_dest_state_key,
        adduct_dest
      )
    )

  undirected <- tidytable::bind_rows(
    scored |>
      tidytable::transmute(src = feature_id, dest = feature_id_dest),
    scored |>
      tidytable::transmute(src = feature_id_dest, dest = feature_id)
  ) |>
    tidytable::distinct()

  nodes <- unique(c(undirected$src, undirected$dest))
  neighbors <- split(undirected$dest, undirected$src)
  visited <- stats::setNames(rep(FALSE, length(nodes)), nodes)
  components <- list()
  for (node in nodes) {
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
      if (is.null(next_nodes)) {
        next
      }
      for (nxt in next_nodes) {
        if (!isTRUE(visited[[nxt]])) {
          visited[[nxt]] <- TRUE
          queue <- c(queue, nxt)
        }
      }
    }
    components[[length(components) + 1L]] <- unique(comp_nodes)
  }

  component_results <- vector("list", length(components))
  for (ci in seq_along(components)) {
    comp_nodes <- components[[ci]]
    sub <- scored |>
      tidytable::filter(
        feature_id %in% comp_nodes & feature_id_dest %in% comp_nodes
      )
    if (nrow(sub) == 0L) {
      next
    }
    node_candidates <- tidytable::bind_rows(
      sub |>
        tidytable::transmute(
          feature = feature_id,
          state_key = adduct_state_key
        ),
      sub |>
        tidytable::transmute(
          feature = feature_id_dest,
          state_key = adduct_dest_state_key
        )
    ) |>
      tidytable::distinct()
    node_priors <- tidytable::bind_rows(
      sub |>
        tidytable::transmute(
          feature = feature_id,
          state_key = adduct_state_key,
          prior = edge_score
        ),
      sub |>
        tidytable::transmute(
          feature = feature_id_dest,
          state_key = adduct_dest_state_key,
          prior = edge_score
        )
    ) |>
      tidytable::summarize(prior = sum(prior), .by = c(feature, state_key))
    assignments <- node_priors |>
      tidytable::arrange(feature, tidytable::desc(prior), nchar(state_key)) |>
      tidytable::distinct(feature, .keep_all = TRUE) |>
      tidytable::select(feature, state_key)
    assign_map <- stats::setNames(assignments$state_key, assignments$feature)
    max_iter <- 20L
    for (iter in seq_len(max_iter)) {
      changed <- FALSE
      for (f in names(assign_map)) {
        candidates_f <- node_candidates |>
          tidytable::filter(feature == f) |>
          tidytable::pull(state_key)
        if (length(candidates_f) <= 1L) {
          next
        }
        cand_scores <- rep(0, length(candidates_f))
        for (k in seq_along(candidates_f)) {
          cand <- candidates_f[[k]]
          contrib_src <- sub |>
            tidytable::filter(feature_id == f, adduct_state_key == cand)
          if (nrow(contrib_src) > 0L) {
            dest_match <- unname(assign_map[contrib_src$feature_id_dest])
            cand_scores[[k]] <- cand_scores[[k]] +
              sum(
                contrib_src$edge_score[
                  contrib_src$adduct_dest_state_key == dest_match
                ]
              )
          }
          contrib_dest <- sub |>
            tidytable::filter(
              feature_id_dest == f,
              adduct_dest_state_key == cand
            )
          if (nrow(contrib_dest) > 0L) {
            src_match <- unname(assign_map[contrib_dest$feature_id])
            cand_scores[[k]] <- cand_scores[[k]] +
              sum(
                contrib_dest$edge_score[
                  contrib_dest$adduct_state_key == src_match
                ]
              )
          }
          prior_val <- node_priors |>
            tidytable::filter(feature == f, state_key == cand) |>
            tidytable::pull(prior)
          if (length(prior_val) > 0L) {
            cand_scores[[k]] <- cand_scores[[k]] + (0.01 * prior_val[[1L]])
          }
        }
        best_idx <- which(cand_scores == max(cand_scores))
        if (length(best_idx) > 1L) {
          lens <- nchar(candidates_f[best_idx])
          best_idx <- best_idx[which.min(lens)]
        }
        best_cand <- candidates_f[[best_idx[[1L]]]]
        if (!identical(assign_map[[f]], best_cand)) {
          assign_map[[f]] <- best_cand
          changed <- TRUE
        }
      }
      if (!changed) break
    }
    kept <- sub |>
      tidytable::filter(
        adduct_state_key == unname(assign_map[feature_id]) &
          adduct_dest_state_key == unname(assign_map[feature_id_dest])
      ) |>
      tidytable::arrange(
        feature_id,
        feature_id_dest,
        tidytable::desc(edge_score),
        edge_complexity
      ) |>
      tidytable::distinct(feature_id, feature_id_dest, .keep_all = TRUE)
    component_results[[ci]] <- kept
  }
  kept_all <- tidytable::bind_rows(component_results) |>
    tidytable::select(feature_id, adduct, adduct_dest, feature_id_dest) |>
    tidytable::distinct()

  scored_min <- scored |>
    tidytable::select(
      feature_id,
      adduct,
      adduct_dest,
      feature_id_dest,
      edge_score,
      edge_complexity
    ) |>
    tidytable::distinct()
  dropped <- scored_min |>
    tidytable::anti_join(
      kept_all,
      by = c("feature_id", "adduct", "adduct_dest", "feature_id_dest")
    )
  if (nrow(dropped) > 0L) {
    pair_best <- scored_min |>
      tidytable::arrange(
        feature_id,
        feature_id_dest,
        tidytable::desc(edge_score),
        edge_complexity
      ) |>
      tidytable::distinct(feature_id, feature_id_dest, .keep_all = TRUE) |>
      tidytable::select(
        feature_id,
        feature_id_dest,
        best_adduct = adduct,
        best_adduct_dest = adduct_dest
      )
    dropped <- dropped |>
      tidytable::left_join(
        pair_best,
        by = c("feature_id", "feature_id_dest")
      ) |>
      tidytable::mutate(
        drop_reason = tidytable::if_else(
          adduct == best_adduct & adduct_dest == best_adduct_dest,
          "node_state_conflict",
          "pair_dedup_or_conflict"
        )
      )
  }
  dropped_reason_counts <- if (nrow(dropped) > 0L) {
    dropped |>
      tidytable::count(drop_reason, name = "n") |>
      tidytable::arrange(tidytable::desc(n))
  } else {
    tidytable::tidytable(drop_reason = character(), n = integer())
  }
  attr(kept_all, "consistency_audit") <- list(
    n_input = nrow(scored_min),
    n_kept = nrow(kept_all),
    n_dropped = nrow(dropped),
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
    mass <- suppressWarnings(MetaboCoreUtils::calculateMass(formula))
    if (!is.finite(mass)) {
      return(NA_real_)
    }
    total <- total + (sign * coef * mass)
  }
  total
}

#' Join RT/mz couple windows with neutral losses
#' @keywords internal
join_couples_with_neutral_losses <- function(df_couples_diff, neutral_losses) {
  cd_src <- tidytable::as_tidytable(df_couples_diff)[,
    .(
      src_feature_id = feature_id,
      src_feature_id_dest = feature_id_dest,
      delta_min,
      delta_max
    )
  ]
  nl_win <- tidytable::as_tidytable(neutral_losses)[
    !is.na(loss),
    .(loss, loss_mass = mass, loss_mass_keep = mass)
  ]
  nl_win[
    cd_src,
    on = .(loss_mass >= delta_min, loss_mass <= delta_max),
    nomatch = 0L,
    allow.cartesian = TRUE
  ][,
    .(
      feature_id = src_feature_id,
      loss,
      mass = loss_mass_keep,
      feature_id_dest = src_feature_id_dest
    )
  ] |>
    unique() |>
    tidytable::as_tidytable()
}

#' Join multi-adduct candidates with add/loss inferred masses (kept for
#' backward compatibility with downstream callers and tests).
#' @keywords internal
join_multi_with_addlossed <- function(df_multi, df_addlossed_rdy) {
  multi_src <- tidytable::as_tidytable(df_multi)[,
    .(
      feature_id,
      adduct,
      rt,
      mz,
      rt_min,
      rt_max,
      mass_min,
      mass_max
    )
  ]
  addloss_win <- tidytable::as_tidytable(df_addlossed_rdy)[,
    .(
      rt_obs = rt,
      mass_obs = mass,
      rt_obs_keep = rt,
      mass_obs_keep = mass
    )
  ]
  addloss_win[
    multi_src,
    on = .(
      rt_obs >= rt_min,
      rt_obs <= rt_max,
      mass_obs >= mass_min,
      mass_obs <= mass_max
    ),
    nomatch = 0L,
    allow.cartesian = TRUE
  ][,
    .(
      feature_id,
      rt,
      mz,
      adduct,
      mass = mass_obs_keep
    )
  ] |>
    tidytable::as_tidytable()
}
