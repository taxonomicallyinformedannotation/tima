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
#'     3. **Cluster / solvent edges.** Match `delta` against modifier masses
#'        (e.g. salts, solvents, ACN, MeOH, Na). A modifier adds mass to the
#'        *higher* m/z peak, so the modifier suffix `+<modifier>` is attached
#'        to the **dest** node's adduct hypotheses.
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
#' @param str_nam Optional file containing structures names
#' @param str_tax_cla File containing Classyfire taxonomy
#' @param str_tax_npc File containing NPClassifier taxonomy
#' @param adducts_list List of adducts to be used
#' @param clusters_list List of clusters to be used
#' @param solvents_list List of solvents to be used
#' @param neutral_losses_list List of neutral losses to be used
#' @param ms_mode Ionization mode. Must be 'pos' or 'neg'
#' @param tolerance_ppm Tolerance to perform annotation. Should be <= 20 ppm
#' @param tolerance_dalton Absolute mass tolerance in Daltons for annotation
#' @param tolerance_rt Tolerance to group adducts. Should be <= 0.05 minutes
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
  str_nam = NULL,
  str_tax_cla = get_params(
    step = "annotate_masses"
  )$files$libraries$sop$merged$structures$taxonomies$cla,
  str_tax_npc = get_params(
    step = "annotate_masses"
  )$files$libraries$sop$merged$structures$taxonomies$npc,
  adducts_list = get_params(step = "annotate_masses")$ms$adducts,
  clusters_list = get_params(step = "annotate_masses")$ms$clusters,
  solvents_list = get_params(step = "annotate_masses")$ms$solvents,
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
  )$ms$tolerances$rt$adducts
) {
  ctx <- log_operation(
    "annotate_masses",
    ms_mode = ms_mode,
    tolerance_ppm = tolerance_ppm,
    tolerance_dalton = tolerance_dalton,
    tolerance_rt = tolerance_rt
  )
  log_info("Starting mass-based annotation (streaming network-first path)")

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
  validate_list_or_vector(clusters_list, param_name = "clusters_list")
  validate_list_or_vector(solvents_list, param_name = "solvents_list")
  files_to_check <- list(
    features = features,
    library = library,
    str_stereo = str_stereo,
    str_met = str_met,
    str_tax_cla = str_tax_cla,
    str_tax_npc = str_tax_npc
  )
  if (!is.null(str_nam) && length(str_nam) > 0L && nzchar(str_nam[[1L]])) {
    files_to_check$str_nam <- str_nam
  }
  validate_file_existence(files_to_check)

  baseline_adduct <- switch(ms_mode, "pos" = "[M+H]+", "neg" = "[M-H]-")

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
    str_nam = str_nam,
    str_tax_cla = str_tax_cla,
    str_tax_npc = str_tax_npc,
    tolerance_ppm = tolerance_ppm,
    tolerance_dalton = tolerance_dalton
  )
  features_table <- loaded_inputs$features_table
  already_assigned <- loaded_inputs$already_assigned
  lib <- loaded_inputs$lib

  pairs <- build_feature_pairs_within_rt(
    df_rt_tol = features_table,
    df_fea_min = features_table,
    tolerance_ppm = tolerance_ppm
  )
  log_info("Built %d RT-window pair(s)", nrow(pairs))

  ion_tables <- build_annotate_masses_ion_tables(
    adducts_list = adducts_list,
    clusters_list = clusters_list,
    solvents_list = solvents_list,
    neutral_losses_list = neutral_losses_list,
    ms_mode = ms_mode
  )
  adducts <- ion_tables$adducts
  clusters <- ion_tables$clusters
  multi_adducts <- ion_tables$multi_adducts
  baseline_adducts <- ion_tables$baseline_adducts
  log_debug(
    paste0(
      "Adduct delta tables ready: %d adduct deltas, %d modifier deltas, ",
      "%d neutral-loss deltas"
    ),
    nrow(ion_tables$adduct_diffs),
    nrow(ion_tables$cluster_transitions),
    nrow(ion_tables$loss_transitions)
  )

  # ---- Step 4: classify each pair --------------------------------------
  start_time_edges <- Sys.time()
  edge_sets <- discover_annotate_masses_edge_sets(
    pairs = pairs,
    features_table = features_table,
    transition_tables = list(
      adduct_diffs = ion_tables$adduct_diffs,
      cluster_transitions = ion_tables$cluster_transitions,
      loss_transitions = ion_tables$loss_transitions
    ),
    adducts = adducts,
    evidence_seed_adducts = ion_tables$evidence_seed_adducts,
    clusters = clusters,
    neutral_losses_list = neutral_losses_list,
    ms_mode = ms_mode,
    tolerance_ppm = tolerance_ppm,
    tolerance_rt = tolerance_rt,
    tolerance_dalton = tolerance_dalton,
    exact_masses = lib$em_windows$exact_mass
  )
  elapsed_edges <- difftime(Sys.time(), start_time_edges, units = "secs")
  adduct_edges <- edge_sets$adduct_edges
  adduct_edges_combined <- edge_sets$adduct_edges_combined
  cluster_edges <- edge_sets$cluster_edges
  loss_edges <- edge_sets$loss_edges
  evidence_signal <- edge_sets$evidence_signal
  lookup_adducts <- unique(stats::na.omit(c(
    baseline_adducts,
    multi_adducts$adduct,
    already_assigned$adduct,
    adduct_edges_combined$adduct,
    adduct_edges_combined$adduct_dest,
    evidence_signal$hypotheses$adduct,
    evidence_signal$edges$adduct,
    evidence_signal$edges$adduct_dest
  )))
  adduct_lookup <- build_adduct_lookup_from_strings(lookup_adducts)

  log_info(
    "Edge classification complete in %.2f seconds: %d adduct edges, %d cluster edges, %d loss edges",
    as.numeric(elapsed_edges),
    nrow(adduct_edges),
    nrow(cluster_edges),
    nrow(loss_edges)
  )

  node_hypotheses <- build_annotate_masses_candidate_hypotheses(
    adduct_edges = adduct_edges_combined,
    cluster_edges = cluster_edges,
    loss_edges = loss_edges,
    evidence_hypotheses = evidence_signal$hypotheses,
    preassigned = already_assigned,
    baseline_adducts = baseline_adducts,
    features_table = features_table,
    multi_adducts = multi_adducts,
    adduct_lookup = adduct_lookup,
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
    adduct_lookup = adduct_lookup,
    tolerance_ppm = tolerance_ppm,
    tolerance_dalton = tolerance_dalton
  )

  modifier_resolution <- resolve_competing_cluster_loss_edges_by_hypotheses(
    cluster_edges = cluster_edges,
    loss_edges = loss_edges,
    node_hypotheses = node_hypotheses,
    ms_mode = ms_mode
  )
  cluster_edges <- modifier_resolution$cluster_edges
  loss_edges <- modifier_resolution$loss_edges

  if (
    modifier_resolution$n_cluster_dropped > 0L ||
      modifier_resolution$n_loss_dropped > 0L
  ) {
    start_time_refine <- Sys.time()
    node_hypotheses <- build_annotate_masses_candidate_hypotheses(
      adduct_edges = adduct_edges_combined,
      cluster_edges = cluster_edges,
      loss_edges = loss_edges,
      evidence_hypotheses = evidence_signal$hypotheses,
      preassigned = already_assigned,
      baseline_adducts = baseline_adducts,
      features_table = features_table,
      multi_adducts = multi_adducts,
      adduct_lookup = adduct_lookup,
      tolerance_ppm = tolerance_ppm,
      tolerance_dalton = tolerance_dalton
    )

    node_hypotheses <- append_component_weak_hypotheses(
      node_hypotheses = node_hypotheses,
      features_table = features_table,
      neutral_losses_list = neutral_losses_list,
      clusters = clusters,
      baseline_adducts = baseline_adducts,
      adduct_lookup = adduct_lookup,
      tolerance_ppm = tolerance_ppm,
      tolerance_dalton = tolerance_dalton
    )

    log_info(
      paste0(
        "Refined node hypotheses after resolving %d ambiguous cluster edge(s) ",
        "and %d ambiguous loss edge(s) in %.2f seconds"
      ),
      modifier_resolution$n_cluster_dropped,
      modifier_resolution$n_loss_dropped,
      as.numeric(difftime(Sys.time(), start_time_refine, units = "secs"))
    )
  }

  start_time_lib <- Sys.time()
  annotations <- build_annotate_masses_annotations(
    node_hypotheses = node_hypotheses,
    library_em = lib$em_windows,
    structure_table = lib$structures,
    tolerance_ppm = tolerance_ppm,
    tolerance_dalton = tolerance_dalton,
    adduct_edges = adduct_edges_combined,
    baseline_adduct = baseline_adduct
  )
  elapsed_lib <- difftime(Sys.time(), start_time_lib, units = "secs")
  log_info(
    "Library matching complete in %.2f seconds: %d annotations",
    as.numeric(elapsed_lib),
    nrow(annotations)
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
  start_time_export <- Sys.time()
  edges_out <- build_output_edges(
    adduct_edges = adduct_edges_combined,
    loss_edges = loss_edges,
    cluster_edges = cluster_edges,
    features_table = features_table,
    name_source = name_source,
    name_target = name_target
  )
  log_debug("Built edges output with %d edges", nrow(edges_out))

  export_params(
    parameters = get_params(step = "annotate_masses"),
    step = "annotate_masses"
  )
  export_output(x = edges_out, file = output_edges[[1L]])
  log_file_op("Exported edges", output_edges[[1L]], n_rows = nrow(edges_out))

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
  log_file_op(
    "Exported annotations",
    output_annotations[[1L]],
    n_rows = nrow(annotations)
  )

  export_output(x = coverage_report, file = coverage_file)
  log_file_op("Exported coverage report", coverage_file)
  elapsed_export <- difftime(Sys.time(), start_time_export, units = "secs")
  log_info("All outputs exported in %.2f seconds", as.numeric(elapsed_export))

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
  adduct_lookup,
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

  base_candidates <- unresolved |>
    tidytable::cross_join(tidytable::tidytable(adduct = baseline_adducts)) |>
    tidytable::mutate(source = "weak_component")

  loss_terms <- unique(gsub(" .*", "", as.character(neutral_losses_list)))
  loss_terms <- loss_terms[!is.na(loss_terms) & nzchar(loss_terms)]
  loss_candidates <- if (length(loss_terms) > 0L) {
    base_candidates |>
      tidytable::cross_join(tidytable::tidytable(modifier = loss_terms)) |>
      tidytable::mutate(
        .base_part = sub("M(?![a-z]).*", "M", adduct, perl = TRUE),
        .char_part = sub(".*M(?![a-z])", "", adduct, perl = TRUE),
        adduct = paste0(.base_part, "-", modifier, .char_part),
        source = "weak_component_loss"
      ) |>
      tidytable::select(-modifier, -.base_part, -.char_part)
  } else {
    base_candidates[0L, ]
  }

  cluster_terms <- unique(as.character(clusters))
  cluster_terms <- cluster_terms[!is.na(cluster_terms) & nzchar(cluster_terms)]
  cluster_candidates <- if (length(cluster_terms) > 0L) {
    base_candidates |>
      tidytable::cross_join(tidytable::tidytable(modifier = cluster_terms)) |>
      tidytable::mutate(
        .base_part = sub("M(?![a-z]).*", "M", adduct, perl = TRUE),
        .char_part = sub(".*M(?![a-z])", "", adduct, perl = TRUE),
        adduct = paste0(.base_part, "+", modifier, .char_part),
        source = "weak_component_cluster"
      ) |>
      tidytable::select(-modifier, -.base_part, -.char_part)
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
      mz_expected = calculate_mz_from_lookup(
        neutral_mass = as.numeric(component_mass),
        adducts = adduct,
        adduct_lookup = adduct_lookup
      ),
      # Cache numeric conversions to avoid repeated conversions
      .mz_num = as.numeric(mz),
      .mz_exp_num = as.numeric(mz_expected),
      mz_diff = abs(.mz_num - .mz_exp_num),
      ppm_diff = mz_diff * 1e6 / pmax(.mz_num, .mz_exp_num),
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

  # Keep modifier edges aligned with currently assigned primary supported adducts
  # whenever both endpoints have such assignments; otherwise keep the edge.
  modifier_filtered <- filter_modifier_edges_by_assigned_adducts(
    cluster_edges = cluster_kept,
    loss_edges = loss_kept,
    assigned_nodes = supported_nodes
  )
  cluster_kept <- modifier_filtered$cluster_edges
  loss_kept <- modifier_filtered$loss_edges

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

#' Keep only modifier edges coherent with assigned primary supported adducts
#'
#' An edge is kept if:
#' - one or both endpoints lack assigned nodes (insufficient evidence to reject), or
#' - at least one assigned adduct combination supports the modifier relation.
#' @keywords internal
filter_modifier_edges_by_assigned_adducts <- function(
  cluster_edges,
  loss_edges,
  assigned_nodes
) {
  assigned_nodes <- assigned_nodes |>
    tidytable::distinct(feature_id, adduct)

  state_map <- build_adduct_state_key_map(unique(c(
    assigned_nodes$adduct,
    cluster_edges$adduct,
    cluster_edges$adduct_dest,
    loss_edges$adduct,
    loss_edges$adduct_dest
  )))
  key_lookup <- stats::setNames(state_map$state_key, state_map$adduct)

  filter_state_labeled_edges <- function(edges) {
    if (
      nrow(edges) == 0L ||
        nrow(assigned_nodes) == 0L ||
        !all(c("adduct", "adduct_dest") %in% colnames(edges))
    ) {
      return(edges)
    }

    assigned_keys <- assigned_nodes |>
      tidytable::mutate(adduct_state_key = unname(key_lookup[adduct])) |>
      tidytable::filter(!is.na(adduct_state_key)) |>
      tidytable::distinct(feature_id, adduct_state_key)

    edges |>
      tidytable::mutate(
        adduct_state_key = unname(key_lookup[adduct]),
        adduct_dest_state_key = unname(key_lookup[adduct_dest])
      ) |>
      tidytable::left_join(
        assigned_keys |>
          tidytable::rename(src_assigned_key = adduct_state_key),
        by = c("feature_id")
      ) |>
      tidytable::left_join(
        assigned_keys |>
          tidytable::rename(
            feature_id_dest = feature_id,
            dest_assigned_key = adduct_state_key
          ),
        by = c("feature_id_dest")
      ) |>
      tidytable::filter(
        is.na(src_assigned_key) |
          is.na(dest_assigned_key) |
          (adduct_state_key == src_assigned_key &
            adduct_dest_state_key == dest_assigned_key)
      ) |>
      tidytable::select(
        -tidyselect::any_of(c(
          "adduct_state_key",
          "adduct_dest_state_key",
          "src_assigned_key",
          "dest_assigned_key"
        ))
      )
  }

  cluster_labeled <- filter_state_labeled_edges(cluster_edges)
  loss_labeled <- filter_state_labeled_edges(loss_edges)
  if (
    all(c("adduct", "adduct_dest") %in% colnames(cluster_edges)) ||
      all(c("adduct", "adduct_dest") %in% colnames(loss_edges))
  ) {
    return(list(cluster_edges = cluster_labeled, loss_edges = loss_labeled))
  }

  edge_supported_any_combo <- function(
    src_adducts,
    dest_adducts,
    modifier,
    relation
  ) {
    src_adducts <- unique(stats::na.omit(as.character(src_adducts)))
    dest_adducts <- unique(stats::na.omit(as.character(dest_adducts)))
    if (length(src_adducts) == 0L || length(dest_adducts) == 0L) {
      return(TRUE)
    }
    modifier <- normalize_modifier_formula_term(modifier)
    if (is.na(modifier) || !nzchar(modifier)) {
      return(TRUE)
    }

    if (identical(relation, "cluster")) {
      expected_dest <- unique(apply_modifier_to_adducts(
        src_adducts,
        modifier,
        "+"
      ))
      state_map <- build_adduct_state_key_map(unique(c(
        expected_dest,
        dest_adducts
      )))
      key_lookup <- stats::setNames(state_map$state_key, state_map$adduct)
      return(any(
        unname(key_lookup[expected_dest]) %in% unname(key_lookup[dest_adducts])
      ))
    }

    if (identical(relation, "loss")) {
      expected_product <- unique(apply_modifier_to_adducts(
        dest_adducts,
        modifier,
        "-"
      ))
      state_map <- build_adduct_state_key_map(unique(c(
        expected_product,
        src_adducts
      )))
      key_lookup <- stats::setNames(state_map$state_key, state_map$adduct)
      return(any(
        unname(key_lookup[expected_product]) %in%
          unname(key_lookup[src_adducts])
      ))
    }

    TRUE
  }

  filter_edges <- function(edges, relation, modifier_col) {
    if (nrow(edges) == 0L || nrow(assigned_nodes) == 0L) {
      return(edges)
    }

    src_map <- split(assigned_nodes$adduct, assigned_nodes$feature_id)
    keep_flag <- vapply(
      seq_len(nrow(edges)),
      function(i) {
        src_id <- edges$feature_id[[i]]
        dest_id <- edges$feature_id_dest[[i]]
        src_adducts <- src_map[[src_id]]
        dest_adducts <- src_map[[dest_id]]
        if (is.null(src_adducts) || is.null(dest_adducts)) {
          return(TRUE)
        }
        modifier <- edges[[modifier_col]][[i]]
        edge_supported_any_combo(
          src_adducts = src_adducts,
          dest_adducts = dest_adducts,
          modifier = modifier,
          relation = relation
        )
      },
      logical(1L)
    )

    edges[keep_flag, , drop = FALSE]
  }

  cluster_kept <- filter_edges(
    cluster_edges,
    relation = "cluster",
    modifier_col = "cluster"
  )
  loss_kept <- filter_edges(
    loss_edges,
    relation = "loss",
    modifier_col = "loss"
  )

  list(cluster_edges = cluster_kept, loss_edges = loss_kept)
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
  str_nam,
  str_tax_cla,
  str_tax_npc,
  tolerance_ppm,
  tolerance_dalton
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
    str_nam = str_nam,
    str_tax_cla = str_tax_cla,
    str_tax_npc = str_tax_npc,
    tolerance_ppm = tolerance_ppm,
    tolerance_dalton = tolerance_dalton
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
  solvents_list,
  neutral_losses_list,
  ms_mode
) {
  adducts <- resolve_annotate_masses_mode_terms(adducts_list, ms_mode)
  adducts <- harmonize_adduct_vector(adducts)
  clusters <- resolve_annotate_masses_mode_terms(clusters_list, ms_mode)
  solvents <- resolve_annotate_masses_mode_terms(solvents_list, ms_mode)
  clusters <- normalize_modifier_terms(clusters)
  solvents <- normalize_modifier_terms(solvents)
  cluster_terms <- unique(c(clusters, solvents))
  neutral_losses_mode <- resolve_annotate_masses_mode_terms(
    neutral_losses_list,
    ms_mode
  )

  single_adducts <- select_single_charge_monomer_adducts(adducts, ms_mode)
  single_adduct_table <- build_single_legacy_adduct_mass_table(single_adducts)
  parsed_adducts <- build_parsed_legacy_adduct_table(adducts)
  multi_adducts <- parsed_adducts |>
    tidytable::filter(n_iso == 0L) |>
    tidytable::filter(n_mer != 1L | abs(z) != 1L) |>
    tidytable::distinct(adduct)
  adduct_diffs <- build_single_adduct_pairwise_deltas(single_adduct_table)
  cluster_transitions <- build_modifier_mass_table(
    modifiers = cluster_terms,
    out_col = "cluster",
    strip_label = FALSE
  )
  loss_transitions <- build_modifier_mass_table(
    modifiers = normalize_modifier_terms(neutral_losses_mode),
    out_col = "loss",
    strip_label = TRUE
  )
  baseline_adducts <- switch(ms_mode, pos = "[M+H]+", neg = "[M-H]-")

  list(
    adducts = adducts,
    clusters = cluster_terms,
    baseline_adducts = baseline_adducts,
    multi_adducts = multi_adducts,
    evidence_seed_adducts = unique(c(adducts, baseline_adducts)),
    adduct_diffs = adduct_diffs,
    cluster_transitions = cluster_transitions,
    loss_transitions = loss_transitions
  )
}

#' Harmonize a plain adduct vector to canonical notation
#' @keywords internal
harmonize_adduct_vector <- function(adducts) {
  adducts <- unique(as.character(adducts))
  adducts <- adducts[!is.na(adducts) & nzchar(adducts)]
  if (length(adducts) == 0L) {
    return(character())
  }
  tidytable::tidytable(adduct = adducts) |>
    harmonize_adducts(adducts_translations = adducts_translations) |>
    tidytable::pull(adduct) |>
    unique()
}

#' Resolve a mode-specific config entry to a flat character vector
#' @keywords internal
resolve_annotate_masses_mode_terms <- function(x, ms_mode) {
  if (is.list(x) && !is.null(x[[ms_mode]])) {
    x <- x[[ms_mode]]
  }
  x <- unique(as.character(x))
  x[!is.na(x) & nzchar(x)]
}

#' Parse configured adduct strings into a minimal lookup-ready table
#' @keywords internal
build_parsed_legacy_adduct_table <- function(adducts) {
  adducts <- harmonize_adduct_vector(adducts)
  if (length(adducts) == 0L) {
    return(tidytable::tidytable(
      adduct = character(),
      n_mer = integer(),
      n_iso = integer(),
      z = integer(),
      adduct_mass = numeric()
    ))
  }
  rows <- lapply(adducts, .parse_single_legacy_adduct_row)
  rows <- rows[!vapply(rows, is.null, logical(1L))]
  if (length(rows) == 0L) {
    return(tidytable::tidytable(
      adduct = character(),
      n_mer = integer(),
      n_iso = integer(),
      z = integer(),
      adduct_mass = numeric()
    ))
  }
  tidytable::as_tidytable(do.call(
    rbind,
    lapply(rows, function(r) {
      data.frame(
        adduct = r$adduct,
        n_mer = as.integer(r$n_mer),
        n_iso = as.integer(r$n_iso),
        z = as.integer(r$z),
        adduct_mass = as.numeric(r$adduct_mass),
        stringsAsFactors = FALSE
      )
    })
  )) |>
    tidytable::distinct(adduct, .keep_all = TRUE)
}

#' Build an adduct lookup directly from attributed adduct strings
#' @keywords internal
build_adduct_lookup_from_strings <- function(adducts) {
  parsed <- build_parsed_legacy_adduct_table(adducts)
  if (nrow(parsed) == 0L) {
    return(tidytable::tidytable(
      adduct = character(),
      n_mer = integer(),
      z = integer(),
      n_iso = integer(),
      adduct_mass = numeric(),
      adduct_mass_per_monomer = numeric(),
      mz_slope = numeric(),
      mz_offset = numeric(),
      carrier_key = character(),
      cluster_key = character(),
      loss_key = character()
    ))
  }
  isotope_shift <- get0(
    x = "EVIDENCE_ISOTOPE_SHIFT_DA",
    ifnotfound = ISOTOPE_MASS_SHIFT_DALTONS,
    inherits = TRUE
  )
  parsed |>
    tidytable::mutate(
      adduct_mass_per_monomer = 0,
      mz_slope = n_mer / abs(z),
      mz_offset = adduct_mass / abs(z) + n_iso * isotope_shift,
      carrier_key = "",
      cluster_key = "",
      loss_key = ""
    ) |>
    tidytable::select(
      adduct,
      n_mer,
      z,
      n_iso,
      adduct_mass,
      adduct_mass_per_monomer,
      mz_slope,
      mz_offset,
      carrier_key,
      cluster_key,
      loss_key
    )
}

#' Keep only single-charge adduct strings used for delta matching
#' @keywords internal
select_single_charge_monomer_adducts <- function(adducts, ms_mode) {
  adducts <- unique(as.character(adducts))
  adducts <- adducts[!is.na(adducts) & nzchar(adducts)]
  if (length(adducts) == 0L) {
    return(character())
  }
  charge_tag <- if (identical(ms_mode, "neg")) "]-" else "]+"
  adducts[
    grepl(pattern = "[M", x = adducts, fixed = TRUE) &
      grepl(pattern = charge_tag, x = adducts, fixed = TRUE)
  ]
}

#' Parse legacy adduct rows and keep valid single-charge monomers
#' @keywords internal
build_single_legacy_adduct_mass_table <- function(adducts) {
  adducts <- harmonize_adduct_vector(adducts)
  if (length(adducts) == 0L) {
    return(tidytable::tidytable(adduct = character(), adduct_mass = numeric()))
  }
  rows <- lapply(adducts, .parse_single_legacy_adduct_row)
  rows <- rows[!vapply(rows, is.null, logical(1L))]
  if (length(rows) == 0L) {
    return(tidytable::tidytable(adduct = character(), adduct_mass = numeric()))
  }
  tab <- tidytable::as_tidytable(do.call(
    rbind,
    lapply(rows, function(r) {
      data.frame(
        adduct = r$adduct,
        adduct_mass = as.numeric(r$adduct_mass),
        n_mer = as.integer(r$n_mer),
        z = as.integer(r$z),
        stringsAsFactors = FALSE
      )
    })
  ))
  tab |>
    tidytable::filter(n_mer == 1L, abs(z) == 1L) |>
    tidytable::select(adduct, adduct_mass) |>
    tidytable::distinct()
}

#' Build pairwise positive deltas between parsed adduct masses
#' @keywords internal
build_single_adduct_pairwise_deltas <- function(single_adduct_table) {
  if (nrow(single_adduct_table) == 0L) {
    return(tidytable::tidytable(
      Distance = numeric(),
      Group1 = character(),
      Group2 = character()
    ))
  }
  src <- single_adduct_table |>
    tidytable::rename(Group1 = adduct, mass1 = adduct_mass)
  dest <- single_adduct_table |>
    tidytable::rename(Group2 = adduct, mass2 = adduct_mass)
  src |>
    tidytable::cross_join(dest) |>
    tidytable::mutate(Distance = mass2 - mass1) |>
    tidytable::filter(Group1 != Group2, Distance > 0) |>
    tidytable::distinct(Distance, Group1, Group2)
}

#' Compute direct mass-diff table from cluster/solvent/loss formulas
#' @keywords internal
build_modifier_mass_table <- function(modifiers, out_col, strip_label = FALSE) {
  modifiers <- unique(as.character(modifiers))
  modifiers <- modifiers[!is.na(modifiers) & nzchar(modifiers)]
  if (length(modifiers) == 0L) {
    out <- tidytable::tidytable(mass = numeric())
    out[[out_col]] <- character()
    return(
      out |>
        tidytable::select(!!as.name(out_col), mass)
    )
  }
  formula <- trimws(sub(" .*", "", modifiers))
  labels <- if (isTRUE(strip_label)) formula else modifiers
  mass <- vapply(
    X = formula,
    FUN = function(f) {
      suppressWarnings(tryCatch(
        MetaboCoreUtils::calculateMass(f),
        error = function(.err) {
          invisible(.err)
          NA_real_
        }
      ))
    },
    FUN.VALUE = numeric(1L)
  )
  out <- tidytable::tidytable(mass = as.numeric(mass))
  out[[out_col]] <- as.character(labels)
  out |>
    tidytable::filter(!is.na(mass) & is.finite(mass) & mass > 0) |>
    tidytable::select(!!as.name(out_col), mass) |>
    tidytable::distinct()
}

#' Normalize modifier terms to a flat character vector
#' @keywords internal
normalize_modifier_terms <- function(x) {
  if (is.null(x)) {
    return(character())
  }
  if (is.list(x)) {
    x <- unlist(x, recursive = TRUE, use.names = FALSE)
  }
  x <- unique(as.character(x))
  x <- x[!is.na(x) & nzchar(x)]
  x
}

#' Return TRUE when an adduct string explicitly carries a given neutral loss term
#' @keywords internal
adduct_has_explicit_loss <- function(adduct, loss_formula) {
  adduct <- as.character(adduct)
  loss_formula <- trimws(sub(" .*", "", as.character(loss_formula)))
  if (
    is.na(adduct) ||
      !nzchar(adduct) ||
      is.na(loss_formula) ||
      !nzchar(loss_formula)
  ) {
    return(FALSE)
  }
  inner <- sub("^\\[(.*)\\][0-9]*[+-]+$", "\\1", adduct, perl = TRUE)
  if (!nzchar(inner) || identical(inner, adduct)) {
    return(FALSE)
  }
  mods <- sub("^[0-9]*M[0-9]*", "", inner, perl = TRUE)
  if (!nzchar(mods)) {
    return(FALSE)
  }
  tokens <- regmatches(mods, gregexpr("[+-][^+-]+", mods, perl = TRUE))[[1L]]
  if (length(tokens) == 0L) {
    return(FALSE)
  }
  loss_norm <- normalize_modifier_formula_term(loss_formula)
  loss_parsed <- tryCatch(
    parse_atomic_formula(loss_norm),
    error = function(.err) {
      invisible(.err)
      NULL
    }
  )
  any(vapply(
    X = tokens,
    FUN = function(tok) {
      if (!startsWith(tok, "-")) {
        return(FALSE)
      }
      body <- substr(tok, 2L, nchar(tok))
      formula <- sub("^[0-9]+", "", body, perl = TRUE)
      formula_norm <- normalize_modifier_formula_term(formula)
      if (identical(formula_norm, loss_norm)) {
        return(TRUE)
      }
      token_parsed <- tryCatch(
        parse_atomic_formula(formula_norm),
        error = function(.err) {
          invisible(.err)
          NULL
        }
      )
      if (
        is.null(loss_parsed) ||
          length(loss_parsed) == 0L ||
          is.null(token_parsed) ||
          length(token_parsed) == 0L
      ) {
        return(FALSE)
      }
      all_names <- sort(unique(c(names(loss_parsed), names(token_parsed))))
      loss_full <- stats::setNames(numeric(length(all_names)), all_names)
      token_full <- stats::setNames(numeric(length(all_names)), all_names)
      loss_full[names(loss_parsed)] <- as.numeric(loss_parsed)
      token_full[names(token_parsed)] <- as.numeric(token_parsed)
      if (
        any(loss_full <= 0) ||
          any(token_full < 0) ||
          any(token_full == 0 & loss_full > 0)
      ) {
        return(FALSE)
      }
      ratios <- token_full[loss_full > 0] / loss_full[loss_full > 0]
      length(ratios) > 0L &&
        all(is.finite(ratios)) &&
        all(abs(ratios - round(ratios)) < 1e-8) &&
        length(unique(round(ratios))) == 1L &&
        unique(round(ratios)) >= 1L
    },
    FUN.VALUE = logical(1L)
  ))
}

#' Discover all edge classes used by annotate_masses
#' @keywords internal
discover_annotate_masses_edge_sets <- function(
  pairs,
  features_table,
  transition_tables,
  adducts,
  evidence_seed_adducts,
  clusters,
  neutral_losses_list,
  ms_mode,
  tolerance_ppm,
  tolerance_rt,
  tolerance_dalton,
  exact_masses
) {
  invisible(tolerance_dalton)
  adduct_diffs <- transition_tables$adduct_diffs
  adduct_edges <- match_pairs_to_adduct_diffs(pairs, adduct_diffs)
  adduct_edges <- apply_adduct_consistency_filter(df_add = adduct_edges)

  cluster_edges <- match_pairs_to_mass_diffs(
    pairs = pairs,
    diffs = transition_tables$cluster_transitions,
    diff_col = "cluster"
  )

  loss_edges <- match_pairs_to_mass_diffs(
    pairs = pairs,
    diffs = transition_tables$loss_transitions,
    diff_col = "loss"
  )

  evidence_candidate_adducts <- unique(stats::na.omit(c(
    evidence_seed_adducts,
    adduct_edges$adduct,
    adduct_edges$adduct_dest
  )))

  evidence_signal <- discover_evidence_adduct_signal(
    features_table = features_table,
    adducts = adducts,
    candidate_adducts = evidence_candidate_adducts,
    clusters = clusters,
    neutral_losses = neutral_losses_list,
    ms_mode = ms_mode,
    tolerance_ppm = tolerance_ppm,
    tolerance_dalton = tolerance_dalton,
    tolerance_rt = tolerance_rt,
    exact_masses = exact_masses
  ) |>
    filter_modifier_evidence_by_pairwise_support(
      universe = NULL,
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

#' Normalize a cluster/loss term to a canonical atomic formula key
#' @keywords internal
normalize_modifier_formula_term <- function(term) {
  if (is.null(term) || is.na(term) || !nzchar(term)) {
    return(NA_character_)
  }
  term <- trimws(sub(" .*", "", as.character(term)))
  if (!nzchar(term)) {
    return(NA_character_)
  }
  parsed <- tryCatch(
    parse_atomic_formula(term),
    error = function(.err) {
      invisible(.err)
      NULL
    }
  )
  if (is.null(parsed) || length(parsed) == 0L) {
    return(term)
  }
  els <- names(parsed)
  ord <- order(ifelse(els == "C", 0L, ifelse(els == "H", 1L, 2L)), els)
  paste0(
    vapply(
      els[ord],
      function(el) {
        n <- as.integer(parsed[[el]])
        paste0(el, if (n > 1L) as.character(n) else "")
      },
      character(1L)
    ),
    collapse = ""
  )
}

#' Apply a modifier formula to adduct strings and simplify notation
#' @keywords internal
apply_modifier_to_adducts <- function(adducts, modifier, sign) {
  if (length(adducts) == 0L) {
    return(character())
  }
  base_part <- sub("M(?![a-z]).*", "M", adducts, perl = TRUE)
  char_part <- sub(".*M(?![a-z])", "", adducts, perl = TRUE)
  simplified <- .simplify_adduct_after_modifier(
    paste0(base_part, sign, modifier, char_part)
  )
  vapply(
    X = simplified,
    FUN = canonicalize_adduct_notation,
    FUN.VALUE = character(1L),
    USE.NAMES = FALSE
  )
}

#' Score a node-level hypothesis row for modifier-interpretation support
#' @keywords internal
score_modifier_hypothesis_row <- function(
  source,
  candidate_adduct_origin,
  adduct_support
) {
  source <- tidytable::coalesce(as.character(source), "")
  candidate_adduct_origin <- tidytable::coalesce(
    as.character(candidate_adduct_origin),
    "baseline"
  )
  adduct_support <- ifelse(
    is.na(adduct_support),
    0L,
    as.integer(adduct_support)
  )

  origin_weight <- tidytable::case_when(
    candidate_adduct_origin == "supported" ~ 100L,
    candidate_adduct_origin == "supported_weak" ~ 25L,
    TRUE ~ 0L
  )
  source_weight <- tidytable::case_when(
    source %in% c("pair", "evidence") ~ 40L,
    source %in% c("preassigned", "preassigned_propagated") ~ 35L,
    source %in% c("cluster", "loss") ~ 25L,
    source %in% c("weak_component_cluster", "weak_component_loss") ~ 10L,
    source == "weak_component" ~ 5L,
    source == "baseline" ~ 0L,
    TRUE ~ 0L
  )

  as.integer(origin_weight + source_weight + pmax(adduct_support, 0L))
}

#' Score how well node hypotheses support one modifier interpretation
#' @keywords internal
score_modifier_relation_from_hypotheses <- function(
  src_hypotheses,
  dest_hypotheses,
  modifier,
  relation
) {
  if (nrow(src_hypotheses) == 0L || nrow(dest_hypotheses) == 0L) {
    return(0L)
  }
  modifier <- normalize_modifier_formula_term(modifier)
  if (is.na(modifier) || !nzchar(modifier)) {
    return(0L)
  }

  src_hypotheses <- src_hypotheses |>
    tidytable::mutate(
      .row_weight = score_modifier_hypothesis_row(
        source = source,
        candidate_adduct_origin = candidate_adduct_origin,
        adduct_support = adduct_support
      )
    )
  dest_hypotheses <- dest_hypotheses |>
    tidytable::mutate(
      .row_weight = score_modifier_hypothesis_row(
        source = source,
        candidate_adduct_origin = candidate_adduct_origin,
        adduct_support = adduct_support
      )
    )

  if (identical(relation, "cluster")) {
    expected <- apply_modifier_to_adducts(src_hypotheses$adduct, modifier, "+")
    state_map <- build_adduct_state_key_map(unique(c(
      expected,
      dest_hypotheses$adduct
    )))
    key_lookup <- stats::setNames(state_map$state_key, state_map$adduct)

    src_keys <- unname(key_lookup[expected])
    dest_keys <- unname(key_lookup[dest_hypotheses$adduct])
  } else if (identical(relation, "loss")) {
    expected <- apply_modifier_to_adducts(dest_hypotheses$adduct, modifier, "-")
    state_map <- build_adduct_state_key_map(unique(c(
      expected,
      src_hypotheses$adduct
    )))
    key_lookup <- stats::setNames(state_map$state_key, state_map$adduct)

    src_keys <- unname(key_lookup[src_hypotheses$adduct])
    dest_keys <- unname(key_lookup[expected])
  } else {
    return(0L)
  }

  src_scored <- tidytable::tidytable(
    state_key = src_keys,
    row_weight = src_hypotheses$.row_weight
  ) |>
    tidytable::filter(!is.na(state_key)) |>
    tidytable::summarize(src_score = max(row_weight), .by = state_key)

  dest_scored <- tidytable::tidytable(
    state_key = dest_keys,
    row_weight = dest_hypotheses$.row_weight
  ) |>
    tidytable::filter(!is.na(state_key)) |>
    tidytable::summarize(dest_score = max(row_weight), .by = state_key)

  matched <- src_scored |>
    tidytable::inner_join(dest_scored, by = "state_key")
  if (nrow(matched) == 0L) {
    return(0L)
  }

  as.integer(sum(matched$src_score + matched$dest_score, na.rm = TRUE))
}

#' Resolve competing cluster/loss interpretations using node hypotheses
#'
#' For pairs that match both a cluster and a neutral-loss explanation with the
#' same underlying modifier formula, score both interpretations directly from the
#' current node hypotheses. Neighboring same-modifier edges are used only as a
#' fallback tie-breaker when hypothesis scores are equal or absent.
#' @keywords internal
resolve_competing_cluster_loss_edges_by_hypotheses <- function(
  cluster_edges,
  loss_edges,
  node_hypotheses,
  ms_mode
) {
  if (
    nrow(cluster_edges) == 0L ||
      nrow(loss_edges) == 0L ||
      nrow(node_hypotheses) == 0L
  ) {
    return(list(
      cluster_edges = cluster_edges,
      loss_edges = loss_edges,
      n_cluster_dropped = 0L,
      n_loss_dropped = 0L
    ))
  }

  if (
    all(c("adduct", "adduct_dest") %in% colnames(cluster_edges)) &&
      all(c("adduct", "adduct_dest") %in% colnames(loss_edges))
  ) {
    hyp_scores <- node_hypotheses |>
      tidytable::mutate(
        candidate_adduct_origin = tidytable::coalesce(
          candidate_adduct_origin,
          "baseline"
        ),
        source = tidytable::coalesce(source, "baseline"),
        adduct_support = tidytable::if_else(
          is.na(adduct_support),
          0L,
          as.integer(adduct_support)
        ),
        .row_weight = score_modifier_hypothesis_row(
          source = source,
          candidate_adduct_origin = candidate_adduct_origin,
          adduct_support = adduct_support
        )
      ) |>
      tidytable::summarize(
        .row_weight = max(.row_weight),
        .by = c(feature_id, adduct)
      )

    score_edges <- function(edges, relation_col) {
      if (nrow(edges) == 0L) {
        return(edges |> tidytable::mutate(.edge_score = integer()))
      }
      edges |>
        tidytable::left_join(
          hyp_scores |>
            tidytable::rename(src_score = .row_weight),
          by = c("feature_id", "adduct")
        ) |>
        tidytable::left_join(
          hyp_scores |>
            tidytable::rename(
              feature_id_dest = feature_id,
              adduct_dest = adduct,
              dest_score = .row_weight
            ),
          by = c("feature_id_dest", "adduct_dest")
        ) |>
        tidytable::mutate(
          .modifier_key = vapply(
            X = .data[[relation_col]],
            FUN = normalize_modifier_formula_term,
            FUN.VALUE = character(1L)
          ),
          src_score = tidytable::coalesce(src_score, 0L),
          dest_score = tidytable::coalesce(dest_score, 0L),
          .edge_score = as.integer(src_score + dest_score)
        )
    }

    cluster_scored <- score_edges(cluster_edges, "cluster")
    loss_scored <- score_edges(loss_edges, "loss")

    decisions <- cluster_scored |>
      tidytable::select(
        feature_id,
        feature_id_dest,
        .modifier_key,
        cluster_score = .edge_score
      ) |>
      tidytable::left_join(
        loss_scored |>
          tidytable::select(
            feature_id,
            feature_id_dest,
            .modifier_key,
            loss_score = .edge_score
          ),
        by = c("feature_id", "feature_id_dest", ".modifier_key")
      ) |>
      tidytable::mutate(
        cluster_score = tidytable::coalesce(cluster_score, 0L),
        loss_score = tidytable::coalesce(loss_score, 0L),
        keep_cluster = cluster_score >= loss_score,
        keep_loss = loss_score >= cluster_score
      ) |>
      tidytable::distinct(
        feature_id,
        feature_id_dest,
        .modifier_key,
        .keep_all = TRUE
      )

    cluster_edges_kept <- cluster_scored |>
      tidytable::left_join(
        decisions |>
          tidytable::select(
            feature_id,
            feature_id_dest,
            .modifier_key,
            keep_cluster
          ),
        by = c("feature_id", "feature_id_dest", ".modifier_key")
      ) |>
      tidytable::filter(tidytable::coalesce(keep_cluster, TRUE)) |>
      tidytable::select(
        -tidyselect::any_of(c(
          "src_score",
          "dest_score",
          ".modifier_key",
          ".edge_score",
          "keep_cluster"
        ))
      )

    loss_edges_kept <- loss_scored |>
      tidytable::left_join(
        decisions |>
          tidytable::select(
            feature_id,
            feature_id_dest,
            .modifier_key,
            keep_loss
          ),
        by = c("feature_id", "feature_id_dest", ".modifier_key")
      ) |>
      tidytable::filter(tidytable::coalesce(keep_loss, TRUE)) |>
      tidytable::select(
        -tidyselect::any_of(c(
          "src_score",
          "dest_score",
          ".modifier_key",
          ".edge_score",
          "keep_loss"
        ))
      )

    return(list(
      cluster_edges = cluster_edges_kept,
      loss_edges = loss_edges_kept,
      n_cluster_dropped = nrow(cluster_edges) - nrow(cluster_edges_kept),
      n_loss_dropped = nrow(loss_edges) - nrow(loss_edges_kept)
    ))
  }

  hyp_min <- node_hypotheses |>
    tidytable::mutate(
      candidate_adduct_origin = tidytable::coalesce(
        candidate_adduct_origin,
        "baseline"
      ),
      source = tidytable::coalesce(source, "baseline"),
      adduct_support = tidytable::if_else(
        is.na(adduct_support),
        0L,
        as.integer(adduct_support)
      )
    ) |>
    tidytable::distinct(
      feature_id,
      adduct,
      source,
      candidate_adduct_origin,
      adduct_support
    )

  cluster_cmp <- cluster_edges |>
    tidytable::mutate(
      .modifier_key = vapply(
        X = cluster,
        FUN = normalize_modifier_formula_term,
        FUN.VALUE = character(1L)
      )
    )
  loss_cmp <- loss_edges |>
    tidytable::mutate(
      .modifier_key = vapply(
        X = loss,
        FUN = normalize_modifier_formula_term,
        FUN.VALUE = character(1L)
      )
    )

  ambiguous_pairs <- cluster_cmp |>
    tidytable::select(feature_id, feature_id_dest, .modifier_key) |>
    tidytable::distinct() |>
    tidytable::inner_join(
      loss_cmp |>
        tidytable::select(feature_id, feature_id_dest, .modifier_key) |>
        tidytable::distinct(),
      by = c("feature_id", "feature_id_dest", ".modifier_key")
    )

  if (nrow(ambiguous_pairs) == 0L) {
    return(list(
      cluster_edges = cluster_edges,
      loss_edges = loss_edges,
      n_cluster_dropped = 0L,
      n_loss_dropped = 0L
    ))
  }

  hyp_by_feature <- split(hyp_min, hyp_min$feature_id)
  decisions <- lapply(seq_len(nrow(ambiguous_pairs)), function(i) {
    pair_i <- ambiguous_pairs[i, ]
    src_id <- pair_i$feature_id[[1L]]
    dest_id <- pair_i$feature_id_dest[[1L]]
    modifier_key <- pair_i$.modifier_key[[1L]]
    src_hypotheses <- hyp_by_feature[[src_id]]
    dest_hypotheses <- hyp_by_feature[[dest_id]]
    if (is.null(src_hypotheses)) {
      src_hypotheses <- hyp_min[0L, ]
    }
    if (is.null(dest_hypotheses)) {
      dest_hypotheses <- hyp_min[0L, ]
    }

    cluster_score <- score_modifier_relation_from_hypotheses(
      src_hypotheses = src_hypotheses,
      dest_hypotheses = dest_hypotheses,
      modifier = modifier_key,
      relation = "cluster"
    )
    loss_score <- score_modifier_relation_from_hypotheses(
      src_hypotheses = src_hypotheses,
      dest_hypotheses = dest_hypotheses,
      modifier = modifier_key,
      relation = "loss"
    )

    cluster_neighbor_score <- sum(
      cluster_cmp$.modifier_key == modifier_key &
        cluster_cmp$feature_id == src_id &
        cluster_cmp$feature_id_dest != dest_id,
      na.rm = TRUE
    ) +
      sum(
        cluster_cmp$.modifier_key == modifier_key &
          cluster_cmp$feature_id_dest == dest_id &
          cluster_cmp$feature_id != src_id,
        na.rm = TRUE
      )

    loss_neighbor_score <- sum(
      loss_cmp$.modifier_key == modifier_key &
        loss_cmp$feature_id == src_id &
        loss_cmp$feature_id_dest != dest_id,
      na.rm = TRUE
    ) +
      sum(
        loss_cmp$.modifier_key == modifier_key &
          loss_cmp$feature_id_dest == dest_id &
          loss_cmp$feature_id != src_id,
        na.rm = TRUE
      )

    if (cluster_score > loss_score) {
      keep_cluster <- TRUE
      keep_loss <- FALSE
    } else if (loss_score > cluster_score) {
      keep_cluster <- FALSE
      keep_loss <- TRUE
    } else if (cluster_neighbor_score > loss_neighbor_score) {
      keep_cluster <- TRUE
      keep_loss <- FALSE
    } else if (loss_neighbor_score > cluster_neighbor_score) {
      keep_cluster <- FALSE
      keep_loss <- TRUE
    } else {
      keep_cluster <- TRUE
      keep_loss <- TRUE
    }

    tidytable::tidytable(
      feature_id = src_id,
      feature_id_dest = dest_id,
      .modifier_key = modifier_key,
      cluster_score = cluster_score,
      loss_score = loss_score,
      keep_cluster = keep_cluster,
      keep_loss = keep_loss
    )
  })
  decisions <- tidytable::bind_rows(decisions)

  n_cluster_only <- sum(
    decisions$keep_cluster & !decisions$keep_loss,
    na.rm = TRUE
  )
  n_loss_only <- sum(
    !decisions$keep_cluster & decisions$keep_loss,
    na.rm = TRUE
  )
  n_unresolved <- sum(
    decisions$keep_cluster & decisions$keep_loss,
    na.rm = TRUE
  )

  log_info(
    paste0(
      "Modifier ambiguity resolution: %d pair(s)%s; ",
      "cluster=%d, loss=%d, unresolved=%d"
    ),
    nrow(ambiguous_pairs),
    if (identical(ms_mode, "pos")) " in positive mode" else "",
    n_cluster_only,
    n_loss_only,
    n_unresolved
  )

  cluster_edges_kept <- cluster_cmp |>
    tidytable::left_join(
      decisions,
      by = c("feature_id", "feature_id_dest", ".modifier_key")
    ) |>
    tidytable::mutate(keep_cluster = tidytable::coalesce(keep_cluster, TRUE)) |>
    tidytable::filter(keep_cluster) |>
    tidytable::select(-.modifier_key, -keep_cluster, -keep_loss)

  loss_edges_kept <- loss_cmp |>
    tidytable::left_join(
      decisions,
      by = c("feature_id", "feature_id_dest", ".modifier_key")
    ) |>
    tidytable::mutate(keep_loss = tidytable::coalesce(keep_loss, TRUE)) |>
    tidytable::filter(keep_loss) |>
    tidytable::select(-.modifier_key, -keep_cluster, -keep_loss)

  n_cluster_dropped <- nrow(cluster_edges) - nrow(cluster_edges_kept)
  n_loss_dropped <- nrow(loss_edges) - nrow(loss_edges_kept)
  if (n_cluster_dropped > 0L || n_loss_dropped > 0L) {
    log_info(
      paste0(
        "Resolved %d ambiguous cluster edge(s) and %d ambiguous loss edge(s) ",
        "using hypothesis-level modifier scoring%s."
      ),
      n_cluster_dropped,
      n_loss_dropped,
      if (identical(ms_mode, "pos")) " in positive mode" else ""
    )
  }

  list(
    cluster_edges = cluster_edges_kept,
    loss_edges = loss_edges_kept,
    n_cluster_dropped = n_cluster_dropped,
    n_loss_dropped = n_loss_dropped
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
  adduct_lookup,
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
    adduct_lookup = adduct_lookup,
    tolerance_ppm = tolerance_ppm,
    tolerance_dalton = tolerance_dalton
  )

  # Save attributes before chaining (tidytable operations drop custom attrs)
  feature_m_map <- attr(node_hypotheses, "feature_m_map")
  component_membership <- attr(node_hypotheses, "component_membership")

  node_hypotheses <- node_hypotheses |>
    tidytable::mutate(
      mass = calculate_neutral_mass_from_lookup(
        mzs = as.numeric(mz),
        adducts = adduct,
        adduct_lookup = adduct_lookup
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

  # Enforce M-consistency: for features with a consensus M from adduct-edge
  # evidence, discard any hypothesis whose implied neutral mass does not agree
  # with that consensus within the configured tolerance. Cluster and loss
  # expansion can produce adducts that map to a different M than the one the
  # network has determined; removing these prevents inflated candidate counts.
  if (!is.null(feature_m_map) && nrow(feature_m_map) > 0L) {
    .tol_ppm <- suppressWarnings(as.numeric(tolerance_ppm))
    .tol_da <- suppressWarnings(as.numeric(
      tolerance_dalton %||% 0
    ))

    if (!is.finite(.tol_ppm) || !is.finite(.tol_da)) {
      log_warn(
        paste0(
          "Invalid M-consistency tolerances (ppm=%s, dalton=%s). ",
          "Skipping consensus-mass filtering for this batch."
        ),
        as.character(tolerance_ppm),
        as.character(tolerance_dalton)
      )
    } else {
      node_hypotheses <- node_hypotheses |>
        tidytable::left_join(
          feature_m_map |>
            tidytable::select(feature_id, .consensus_m = neutral_mass),
          by = "feature_id"
        ) |>
        tidytable::mutate(
          .consensus_m = suppressWarnings(as.numeric(.consensus_m))
        ) |>
        tidytable::filter(
          is.na(.consensus_m) |
            abs(mass - .consensus_m) <=
              pmax(.tol_ppm * 1e-6 * .consensus_m, .tol_da)
        ) |>
        tidytable::select(-tidyselect::any_of(".consensus_m"))
    }
  }

  # Restore custom attributes lost through tidytable operations
  attr(node_hypotheses, "feature_m_map") <- feature_m_map
  attr(node_hypotheses, "component_membership") <- component_membership

  if (nrow(multi_adducts) > 0L && nrow(node_hypotheses) > 0L) {
    multi_seed <- node_hypotheses |>
      tidytable::filter(candidate_adduct_origin == "supported")
    multi_hypotheses <- generate_multi_hypotheses_from_node_masses(
      node_hypotheses = multi_seed,
      multi_adducts = multi_adducts,
      adduct_lookup = adduct_lookup,
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
      # Restore attributes that bind_rows/dedupe may have dropped
      attr(node_hypotheses, "feature_m_map") <- feature_m_map
      attr(node_hypotheses, "component_membership") <- component_membership
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
  annotations <- enforce_loss_formula_compatibility(annotations)

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
        best_rank = {
          any_keep <- any(keep, na.rm = TRUE)
          if (isTRUE(any_keep)) {
            min(support_class_rank[keep], na.rm = TRUE)
          } else {
            min(support_class_rank, na.rm = TRUE)
          }
        },
        best_support = {
          keep_at_best <- keep & support_class_rank == best_rank
          if (isTRUE(any(keep_at_best, na.rm = TRUE))) {
            max(support_strength[keep_at_best], na.rm = TRUE)
          } else {
            max(support_strength[support_class_rank == best_rank], na.rm = TRUE)
          }
        },
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
    n_input_features = annotations |>
      tidytable::distinct(feature_id) |>
      nrow(),
    n_kept_features = out |>
      tidytable::distinct(feature_id) |>
      nrow(),
    n_dropped_features = if (nrow(dropped) > 0L) {
      dropped |>
        tidytable::distinct(feature_id) |>
        nrow()
    } else {
      0L
    }
  )
  out
}
