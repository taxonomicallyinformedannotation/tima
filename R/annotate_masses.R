#' @title Annotate masses
#'
#' @description Mass-based MS1 annotation:
#'
#'     1. **Pairs in RT windows.** Group co-eluting features within
#'        `tolerance_rt` and orient every pair from the higher `m/z` peak
#'        (`feature_source`) to the lower `m/z` peak (`feature_target`) so mass
#'        differences (`delta_mz >= 0`) have a fixed direction.
#'
#'     2. **Adduct, cluster, and neutral-loss edges.** Match `delta_mz` against
#'        mode-specific adduct differences, solvent/salt clusters, and in-source
#'        neutral losses. Because pairs are oriented from heavier to lighter
#'        ions, a `-18.01 Da` difference below an `[M+H]+` parent attributes
#'        `[M-H2O+H]+` to the lighter product ion, whereas solvent clusters
#'        (`[M+H2O+H]+`) are only tested when enabled in `clusters_list`.
#'
#'     3. **Intensity co-variance edges.** Pairs Detected together across `>= 3`
#'        samples are tested for positive Pearson correlation (BH-adjusted
#'        `p < correlation_p_threshold`), reinforcing adduct support.
#'
#'     4. **Node hypotheses & library matching.** Combine graph-supported
#'        adducts, upstream pre-assigned adducts, and the baseline ion
#'        (`[M+H]+` or `[M-H]-`), then match implied neutral masses `M` against
#'        the structure library within `max(tolerance_ppm, tolerance_dalton)`.
#'
#'     5. **Formula & network consistency.** Drop neutral-loss structure matches
#'        whose molecular formula lacks the lost atoms (e.g. `[M-H3N+H]+` on a
#'        nitrogen-free structure), prune unsupported adduct hypotheses when a
#'        graph-supported alternative exists, and keep unmatched adduct
#'        hypotheses for downstream inspection.
#'
#' @include adducts_utils.R
#' @include adduct_universe.R
#' @include annotate_masses_covariance.R
#' @include calculate_mass_of_m.R
#' @include decorate_masses.R
#' @include dists_utils.R
#' @include get_params.R
#' @include harmonize_adducts.R
#' @include mass_evidence.R
#' @include round_reals.R
#' @include safe_fread.R
#' @include validations_utils.R
#' @include logs_utils.R
#' @include adduct_universe_formula.R
#' @include adduct_universe_matching.R
#' @include annotate_masses_consistency.R
#' @include annotate_masses_coverage.R
#' @include annotate_masses_enforcement.R
#' @include annotate_masses_features.R
#' @include annotate_masses_hypothesis.R
#' @include select_annotations_columns.R
#' @include export_params.R
#' @include export_output.R
#' @include sanitize_data.R
#' @include validations_params.R
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
#' @param tolerance_ppm Relative mass tolerance (ppm) to perform annotation.
#'   Should be <= 20 ppm. A match within either `tolerance_ppm` or
#'   `tolerance_dalton` is accepted, so the effective window is the larger of
#'   the two.
#' @param tolerance_dalton Absolute mass tolerance (Da) to perform annotation.
#'   Acts as a floor for low masses: the default (0.0025 Da) equals 10 ppm at
#'   m/z 250, so it only widens the window below that.
#' @param tolerance_rt Tolerance to group adducts. Should be <= 0.05 minutes
#' @param correlation_p_threshold False discovery rate threshold for the
#'   Pearson correlation of feature intensities across samples (needs >= 3
#'   samples where both features are detected). P-values of all pairs tested
#'   are Benjamini-Hochberg adjusted; positively correlated pairs with adjusted
#'   p < threshold are exported as covariance edges, and an adduct neighbor
#'   that co-varies counts twice in `adduct_support`. Pairs that fail the test
#'   are not rejected.
#'
#' @return Named character of paths to the annotations and edges files.
#'
#' @family annotation
#' @concept EDAM:operation_3629 Deisotoping
#' @concept EDAM:operation_3803 Natural product identification
#' @concept EDAM:operation_3766 Weighted correlation network analysis
#' @concept EDAM:data_2536 Mass spectrometry data
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
  )$ms$tolerances$rt$adducts,
  correlation_p_threshold = get_params(
    step = "annotate_masses"
  )$ms$tolerances$correlation$p_value
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
  rm(loaded_inputs)

  pairs <- build_feature_pairs_within_rt(
    df_rt_tol = features_table,
    df_fea_min = features_table,
    tolerance_ppm = tolerance_ppm,
    tolerance_dalton = tolerance_dalton
  )
  log_info("Built %d RT-window pair(s)", nrow(pairs))
  .log_top_pair_deltas(pairs)

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
  if (nrow(already_assigned) > 0L && "adduct" %in% colnames(already_assigned)) {
    already_assigned$adduct <- align_carrier_multiplicity_adducts(
      already_assigned$adduct,
      c(adducts, baseline_adducts)
    )
  }
  log_debug(
    paste0(
      "Adduct delta tables ready: %d adduct deltas, %d modifier deltas, ",
      "%d neutral-loss deltas"
    ),
    nrow(ion_tables$adduct_diffs),
    nrow(ion_tables$cluster_transitions),
    nrow(ion_tables$loss_transitions)
  )

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
    exact_masses = lib$em_windows$exact_mass,
    correlation_p_threshold = correlation_p_threshold
  )
  elapsed_edges <- difftime(Sys.time(), start_time_edges, units = "secs")
  adduct_edges <- edge_sets$adduct_edges
  adduct_edges_combined <- edge_sets$adduct_edges_combined
  cluster_edges <- edge_sets$cluster_edges
  loss_edges <- edge_sets$loss_edges
  evidence_signal <- edge_sets$evidence_signal
  covariance_edges <- edge_sets$covariance_edges

  # Free memory from large intermediate objects (keep clusters and
  # neutral_losses_list until after append_component_weak_hypotheses()).
  rm(
    edge_sets,
    ion_tables,
    pairs,
    adducts,
    adduct_edges,
    solvents_list,
    adducts_list
  )

  # Covariance edges are exported with the edge table (see
  # build_output_edges()) and strengthen the adduct support of co-varying
  # neighbors in the hypotheses below; they never remove an edge

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

  # Count pure adduct edges vs covariance edges
  n_adduct_only <- nrow(
    adduct_edges_combined |>
      tidytable::filter(!is.na(adduct) & !is.na(adduct_dest))
  )
  n_covariance_only <- nrow(covariance_edges)

  log_info(
    "Edge classification complete in %.2f seconds: %d adduct edges, %d covariance edges, %d cluster edges, %d loss edges",
    as.numeric(elapsed_edges),
    n_adduct_only,
    n_covariance_only,
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
    tolerance_dalton = tolerance_dalton,
    covariance_edges = covariance_edges
  )
  rm(lookup_adducts)

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
  n_cluster_dropped <- modifier_resolution$n_cluster_dropped
  n_loss_dropped <- modifier_resolution$n_loss_dropped
  rm(modifier_resolution)

  if (
    n_cluster_dropped > 0L ||
      n_loss_dropped > 0L
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
      n_cluster_dropped,
      n_loss_dropped,
      as.numeric(difftime(Sys.time(), start_time_refine, units = "secs"))
    )
  }

  rm(multi_adducts, clusters, neutral_losses_list)

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
  coverage_audit <- attr(annotations, "coverage_audit")
  elapsed_lib <- difftime(Sys.time(), start_time_lib, units = "secs")
  log_info(
    "Library matching complete in %.2f seconds: %d annotations",
    as.numeric(elapsed_lib),
    nrow(annotations)
  )
  rm(lib, adduct_lookup, already_assigned)

  # Intensity co-variance edges are now integrated into the network
  # and validated through consensus, not a hard pre-filter

  annotations <- derive_primary_secondary_annotations(
    annotations = annotations,
    baseline_adducts = baseline_adducts
  )

  annotations <- propagate_annotations_across_m_cliques(
    annotations = annotations,
    node_hypotheses = node_hypotheses,
    tolerance_ppm = tolerance_ppm,
    tolerance_dalton = tolerance_dalton
  )
  rm(node_hypotheses)

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

  # Free memory from graph structures
  rm(supported_graph, evidence_signal)

  if (!is.list(coverage_audit)) {
    coverage_audit <- attr(annotations, "coverage_audit")
  } else {
    attr(annotations, "coverage_audit") <- coverage_audit
  }
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

  start_time_export <- Sys.time()
  edges_out <- build_output_edges(
    adduct_edges = adduct_edges_combined,
    loss_edges = loss_edges,
    cluster_edges = cluster_edges,
    covariance_edges = covariance_edges,
    features_table = features_table,
    name_source = name_source,
    name_target = name_target
  )
  log_debug("Built edges output with %d edges", nrow(edges_out))

  export_params(
    parameters = get_params(step = "annotate_masses"),
    step = "annotate_masses"
  )
  nrows_edges_out <- nrow(edges_out)
  export_output(x = edges_out, file = output_edges[[1L]])
  log_file_op("Exported edges", output_edges[[1L]], n_rows = nrows_edges_out)
  rm(
    edges_out,
    adduct_edges_combined,
    cluster_edges,
    covariance_edges,
    loss_edges,
    features_table
  )

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
  nrows_annotations <- nrow(annotations)
  rm(annotations, coverage_report)
  elapsed_export <- difftime(Sys.time(), start_time_export, units = "secs")
  log_info("All outputs exported in %.2f seconds", as.numeric(elapsed_export))

  log_complete(
    ctx,
    n_annotations = nrows_annotations,
    n_edges = nrows_edges_out
  )

  c(
    "annotations" = output_annotations[[1L]],
    "edges" = output_edges[[1L]]
  )
}

.log_top_pair_deltas <- function(pairs) {
  if (nrow(pairs) == 0L) {
    return(invisible(NULL))
  }
  x <- pairs$delta
  x <- x[is.finite(x)]
  if (length(x) == 0L) {
    return(invisible(NULL))
  }
  nb <- 100000L
  rx <- range(x)
  dx <- rx[2L] - rx[1L]
  if (dx == 0) {
    dx <- abs(rx[1L])
    breaks <- seq.int(
      rx[1L] - if (dx == 0) 0.001 else dx / 1000,
      rx[2L] + if (dx == 0) 0.001 else dx / 1000,
      length.out = nb + 1L
    )
  } else {
    breaks <- seq.int(rx[1L], rx[2L], length.out = nb + 1L)
    breaks[c(1L, nb + 1L)] <- c(rx[1L] - dx * 0.001, rx[2L] + dx * 0.001)
  }
  codes <- .bincode(x, breaks = breaks, right = TRUE, include.lowest = FALSE)
  counts <- tabulate(codes, nbins = nb)
  nz <- which(counts > 0L)
  cand <- if (length(nz) > 16L) {
    thresh <- sort(counts[nz], partial = length(nz) - 15L)[length(nz) - 15L]
    which(counts >= thresh)
  } else {
    nz
  }
  first_pos <- match(cand, codes)
  ord <- cand[order(-counts[cand], first_pos)]
  top_idx <- ord[seq_len(min(16L, length(ord)))]

  step <- (breaks[nb] - breaks[2L]) / (nb - 2L)
  p10 <- 10^(floor(log10(max(step, abs(rx[1L])))):ceiling(log10(max(
    step,
    abs(rx[2L])
  ))))
  idx_lo <- as.integer(ceiling((p10 - rx[1L]) / step)) + 1L
  idx_hi <- as.integer(floor((p10 - rx[1L]) / step))
  probe_lo <- unique(c(
    1L,
    2L,
    nb - 1L,
    nb,
    as.integer(seq.int(1L, nb, length.out = 64L)),
    idx_lo[idx_lo >= 1L & idx_lo <= nb],
    idx_hi[idx_hi >= 1L & idx_hi <= nb],
    top_idx
  ))
  for (dig in 3L:12L) {
    ch_lo_p <- formatC(0 + breaks[probe_lo], digits = dig, width = 1L)
    ch_hi_p <- formatC(0 + breaks[probe_lo + 1L], digits = dig, width = 1L)
    if (all(ch_lo_p != ch_hi_p)) {
      break
    }
  }
  nt <- length(top_idx)
  ch_top <- formatC(
    0 + breaks[c(top_idx, top_idx + 1L)],
    digits = dig,
    width = 1L
  )
  top_labels <- paste0(
    "(",
    ch_top[seq_len(nt)],
    ",",
    ch_top[nt + seq_len(nt)],
    "]"
  )
  bins <- tidytable::tidytable(
    bin = top_labels,
    N = counts[top_idx]
  )
  bins <- add_percentage_column(bins, count_col = "N", out_col = "Pct")
  log_info(
    "Here are the top 16 observed m/z differences inside the RT windows:"
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

#' Enforce one primary ion species per feature while keeping non-conflicting
#' secondary alternatives.
#' @keywords internal
derive_primary_secondary_annotations <- function(
  annotations,
  baseline_adducts
) {
  n <- nrow(annotations)
  if (n == 0L) {
    return(annotations)
  }
  cov_audit <- attr(annotations, "coverage_audit")
  out <- tidytable::as_tidytable(annotations)

  if (!"source" %in% colnames(out)) {
    data.table::set(out, j = "source", value = rep(NA_character_, n))
  }
  if (!"adduct_support" %in% colnames(out)) {
    data.table::set(out, j = "adduct_support", value = rep(0L, n))
  }
  if (!"error_mz" %in% colnames(out)) {
    data.table::set(out, j = "error_mz", value = rep(NA_real_, n))
  }
  if (!"candidate_adduct_origin" %in% colnames(out)) {
    orig_v <- ifelse(
      (!is.na(out$source) & out$source == "baseline") |
        !is.na(fastmatch::fmatch(out$adduct, baseline_adducts)),
      "baseline",
      "supported"
    )
    data.table::set(out, j = "candidate_adduct_origin", value = orig_v)
  }

  orig_col <- out$candidate_adduct_origin
  ev_tier <- rep("baseline", n)
  ev_tier[!is.na(orig_col) & orig_col == "supported_weak"] <- "supported_weak"
  ev_tier[!is.na(orig_col) & orig_col == "supported"] <- "supported_strong"
  data.table::set(out, j = "evidence_tier", value = ev_tier)

  supp_rank <- rep(3L, n)
  supp_rank[ev_tier == "supported_weak"] <- 2L
  supp_rank[ev_tier == "supported_strong"] <- 1L

  src_ranks <- c(
    pair = 1L,
    evidence = 2L,
    loss = 3L,
    cluster = 4L,
    preassigned = 5L,
    preassigned_propagated = 6L,
    multi = 7L,
    baseline = 8L,
    weak_component = 9L,
    weak_component_loss = 10L,
    weak_component_cluster = 11L
  )
  src_rank <- unname(src_ranks[as.character(out$source)])
  src_rank[is.na(src_rank)] <- 12L

  has_str <- !is.na(out$structure_exact_mass)
  supp_v <- as.integer(out$adduct_support)
  abs_err <- abs(as.numeric(out$error_mz))
  abs_err_inf <- ifelse(is.finite(abs_err), abs_err, Inf)

  u_f <- unique(out$feature_id)
  u_a <- sort(unique(out$adduct), na.last = TRUE)
  f_id <- match(out$feature_id, u_f)
  a_id <- match(out$adduct, u_a)

  same_prev <- if (n > 1L) {
    c(
      FALSE,
      f_id[-1L] == f_id[-n] &
        a_id[-1L] == a_id[-n] &
        has_str[-1L] == has_str[-n] &
        supp_rank[-1L] == supp_rank[-n] &
        (is.na(supp_v[-1L]) == is.na(supp_v[-n]) &
          (is.na(supp_v[-1L]) | supp_v[-1L] == supp_v[-n])) &
        src_rank[-1L] == src_rank[-n] &
        abs_err_inf[-1L] == abs_err_inf[-n]
    )
  } else {
    FALSE
  }
  keep_sub <- which(!same_prev)
  f_s <- f_id[keep_sub]
  a_s <- a_id[keep_sub]
  hs_s <- has_str[keep_sub]
  sr_s <- supp_rank[keep_sub]
  sp_s <- supp_v[keep_sub]
  sp_s[is.na(sp_s)] <- -999999L
  src_s <- src_rank[keep_sub]
  err_s <- abs_err_inf[keep_sub]

  n_a <- length(u_a)
  fa_key <- as.double(f_s - 1L) * n_a + a_s

  if (anyDuplicated(fa_key) > 0L) {
    u_fa <- unique(fa_key)
    fa_id <- fastmatch::fmatch(fa_key, u_fa)
    n_fa <- length(u_fa)
    first_pos <- which(!duplicated(fa_id))
    f_fa <- f_s[first_pos]
    a_fa <- a_s[first_pos]

    any_str_fa <- logical(n_fa)
    any_str_fa[fa_id[hs_s]] <- TRUE

    best_sr_fa <- rep(3L, n_fa)
    best_sr_fa[fa_id[sr_s == 2L]] <- 2L
    best_sr_fa[fa_id[sr_s == 1L]] <- 1L

    max_sp_fa <- sp_s[first_pos]
    best_src_fa <- src_s[first_pos]
    min_err_fa <- err_s[first_pos]

    is_multi_fa <- logical(n_fa)
    is_multi_fa[fa_id[duplicated(fa_id)]] <- TRUE
    m_idx <- which(is_multi_fa[fa_id])
    if (length(m_idx) > 0L) {
      ord_sp <- m_idx[order(-sp_s[m_idx], method = "radix")]
      win_sp <- ord_sp[!duplicated(fa_id[ord_sp])]
      max_sp_fa[fa_id[win_sp]] <- sp_s[win_sp]

      ord_src <- m_idx[order(src_s[m_idx], method = "radix")]
      win_src <- ord_src[!duplicated(fa_id[ord_src])]
      best_src_fa[fa_id[win_src]] <- src_s[win_src]

      ord_err <- m_idx[order(err_s[m_idx], method = "radix")]
      win_err <- ord_err[!duplicated(fa_id[ord_err])]
      min_err_fa[fa_id[win_err]] <- err_s[win_err]
    }
  } else {
    f_fa <- f_s
    a_fa <- a_s
    any_str_fa <- hs_s
    best_sr_fa <- sr_s
    max_sp_fa <- sp_s
    best_src_fa <- src_s
    min_err_fa <- err_s
  }

  ord_fa <- order(
    f_fa,
    !any_str_fa,
    best_sr_fa,
    -max_sp_fa,
    best_src_fa,
    min_err_fa,
    a_fa,
    method = "radix"
  )
  win_fa <- ord_fa[!duplicated(f_fa[ord_fa])]
  prim_a_lut <- rep(NA_integer_, length(u_f))
  prim_a_lut[f_fa[win_fa]] <- a_fa[win_fa]
  prim_adduct_vec <- u_a[prim_a_lut[f_id]]

  ann_level <- ifelse(
    !is.na(prim_adduct_vec) &
      !is.na(out$adduct) &
      out$adduct == prim_adduct_vec,
    "primary",
    "secondary"
  )
  data.table::set(out, j = "annotation_level", value = ann_level)
  if (!is.null(cov_audit)) {
    attr(out, "coverage_audit") <- cov_audit
  }
  out
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

  fm_nm <- feature_m_map$neutral_mass
  valid_fm <- which(!is.na(fm_nm) & is.finite(fm_nm) & fm_nm > 0)
  if (length(valid_fm) == 0L) {
    return(node_hypotheses)
  }
  med_c <- .fast_group_median(
    fm_nm[valid_fm],
    feature_m_map$component_id[valid_fm]
  )

  nh_orig <- tidytable::coalesce(
    node_hypotheses$candidate_adduct_origin,
    "supported"
  )
  supp_fids <- unique(node_hypotheses$feature_id[nh_orig == "supported"])

  cm_fids <- component_membership$feature_id
  keep_unres <- is.na(fastmatch::fmatch(cm_fids, supp_fids))
  if (!any(keep_unres)) {
    return(node_hypotheses)
  }
  idx_unres <- which(keep_unres)
  u_fids <- cm_fids[idx_unres]
  u_cids <- component_membership$component_id[idx_unres]
  u_cmass <- med_c$medians[fastmatch::fmatch(u_cids, med_c$groups)]

  keep_ft <- !duplicated(features_table$feature_id)
  ft_fids <- features_table$feature_id[keep_ft]
  ft_mz <- features_table$mz[keep_ft]
  ft_rt <- features_table$rt[keep_ft]
  m_ft <- fastmatch::fmatch(u_fids, ft_fids)
  u_mz <- as.numeric(ft_mz[m_ft])
  u_rt <- as.numeric(ft_rt[m_ft])

  valid_u <- which(!is.na(u_cmass) & !is.na(u_mz) & is.finite(u_mz))
  if (length(valid_u) == 0L) {
    return(node_hypotheses)
  }
  u_fids <- u_fids[valid_u]
  u_cmass <- as.numeric(u_cmass[valid_u])
  u_mz <- u_mz[valid_u]
  u_rt <- u_rt[valid_u]
  n_u <- length(u_fids)

  base_parts <- sub("M(?![a-z]).*", "M", baseline_adducts, perl = TRUE)
  char_parts <- sub(".*M(?![a-z])", "", baseline_adducts, perl = TRUE)
  tpl_adduct <- baseline_adducts
  tpl_source <- rep.int("weak_component", length(baseline_adducts))

  loss_terms <- unique(gsub(" .*", "", as.character(neutral_losses_list)))
  loss_terms <- loss_terms[!is.na(loss_terms) & nzchar(loss_terms)]
  if (length(loss_terms) > 0L) {
    n_b <- length(baseline_adducts)
    n_l <- length(loss_terms)
    l_ads <- paste0(
      rep(base_parts, each = n_l),
      "-",
      rep(loss_terms, times = n_b),
      rep(char_parts, each = n_l)
    )
    tpl_adduct <- c(tpl_adduct, l_ads)
    tpl_source <- c(tpl_source, rep.int("weak_component_loss", length(l_ads)))
  }

  cluster_terms <- unique(as.character(clusters))
  cluster_terms <- cluster_terms[!is.na(cluster_terms) & nzchar(cluster_terms)]
  if (length(cluster_terms) > 0L) {
    n_b <- length(baseline_adducts)
    n_c <- length(cluster_terms)
    c_ads <- paste0(
      rep(base_parts, each = n_c),
      "+",
      rep(cluster_terms, times = n_b),
      rep(char_parts, each = n_c)
    )
    tpl_adduct <- c(tpl_adduct, c_ads)
    tpl_source <- c(
      tpl_source,
      rep.int("weak_component_cluster", length(c_ads))
    )
  }

  tpl_df <- harmonize_adducts(
    tidytable::tidytable(adduct = tpl_adduct, source = tpl_source),
    adducts_translations = adducts_translations
  )
  keep_tpl <- !duplicated(paste(tpl_df$adduct, tpl_df$source, sep = "\r"))
  tpl_adduct <- tpl_df$adduct[keep_tpl]
  tpl_source <- tpl_df$source[keep_tpl]
  n_tpl <- length(tpl_adduct)

  u_tpl_ads <- unique(tpl_adduct)
  n_uta <- length(u_tpl_ads)
  all_cmass_rep <- rep(u_cmass, times = n_uta)
  all_ads_rep <- rep(u_tpl_ads, each = n_u)
  all_mz_exp <- as.numeric(calculate_mz_from_lookup(
    neutral_mass = all_cmass_rep,
    adducts = all_ads_rep,
    adduct_lookup = adduct_lookup
  ))
  mz_exp_mat <- matrix(all_mz_exp, nrow = n_u, ncol = n_uta)

  has_dalton <- !is.null(tolerance_dalton)
  hit_u_list <- vector("list", n_tpl)
  hit_t_list <- vector("list", n_tpl)
  for (t_i in seq_len(n_tpl)) {
    col_i <- match(tpl_adduct[[t_i]], u_tpl_ads)
    mz_exp <- mz_exp_mat[, col_i]
    mz_diff <- abs(u_mz - mz_exp)
    ppm_diff <- mz_diff * 1e6 / pmax(u_mz, mz_exp)
    ok <- (is.finite(ppm_diff) & ppm_diff <= tolerance_ppm) |
      (if (has_dalton) !is.na(mz_diff) & mz_diff <= tolerance_dalton else FALSE)
    w_ok <- which(ok)
    if (length(w_ok) > 0L) {
      hit_u_list[[t_i]] <- w_ok
      hit_t_list[[t_i]] <- rep.int(t_i, length(w_ok))
    }
  }

  hit_u <- unlist(hit_u_list, use.names = FALSE)
  if (length(hit_u) == 0L) {
    return(node_hypotheses)
  }
  hit_t <- unlist(hit_t_list, use.names = FALSE)

  weak_all <- tidytable::tidytable(
    feature_id = u_fids[hit_u],
    adduct = tpl_adduct[hit_t],
    source = tpl_source[hit_t],
    is_preassigned = FALSE,
    adduct_support = 0L,
    candidate_adduct_origin = "supported_weak",
    mz = u_mz[hit_u],
    rt = u_rt[hit_u],
    mass = u_cmass[hit_u]
  ) |>
    tidytable::distinct()

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
  invisible(c(tolerance_ppm, tolerance_dalton))
  if (
    nrow(adduct_edges) == 0L &&
      nrow(cluster_edges) == 0L &&
      nrow(loss_edges) == 0L
  ) {
    return(list(
      adduct_edges = adduct_edges[0L, ],
      cluster_edges = cluster_edges[0L, ],
      loss_edges = loss_edges[0L, ],
      annotations = annotations
    ))
  }
  adduct_kept <- enforce_graph_adduct_consistency(adduct_edges) |>
    tidytable::distinct(feature_id, adduct, feature_id_dest, adduct_dest)

  assigned_nodes <- tidytable::bind_rows(
    adduct_kept |>
      tidytable::transmute(feature_id, adduct),
    adduct_kept |>
      tidytable::transmute(feature_id = feature_id_dest, adduct = adduct_dest)
  ) |>
    tidytable::distinct()

  if (nrow(adduct_kept) == 0L) {
    modifier_filtered <- filter_modifier_edges_by_assigned_adducts(
      cluster_edges = cluster_edges,
      loss_edges = loss_edges,
      assigned_nodes = assigned_nodes
    )
    return(list(
      adduct_edges = adduct_kept,
      cluster_edges = modifier_filtered$cluster_edges,
      loss_edges = modifier_filtered$loss_edges,
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

  # Use igraph to compute connected components (faster and vectorized)
  graph_df <- data.frame(
    from = undirected$src,
    to = undirected$dest,
    stringsAsFactors = FALSE
  )
  g <- igraph::graph_from_data_frame(graph_df, directed = FALSE)
  comps <- igraph::components(g, mode = "weak")
  membership <- comps$membership
  names(membership) <- igraph::V(g)$name
  comp_ids <- paste0("M_", as.integer(membership))
  comp_members <- tidytable::as_tidytable(data.frame(
    feature_id = names(membership),
    component_id = comp_ids,
    stringsAsFactors = FALSE
  ))

  # Keep directly matched modifier edges; avoid over-pruning by adduct-component
  # membership and enforce coherence with assigned adduct states below.
  cluster_kept <- cluster_edges |>
    tidytable::distinct()

  loss_kept <- loss_edges |>
    tidytable::distinct()

  # Keep modifier edges aligned with currently assigned primary supported adducts
  # whenever both endpoints have such assignments; otherwise keep the edge.
  modifier_filtered <- filter_modifier_edges_by_assigned_adducts(
    cluster_edges = cluster_kept,
    loss_edges = loss_kept,
    assigned_nodes = assigned_nodes
  )
  cluster_kept <- modifier_filtered$cluster_edges
  loss_kept <- modifier_filtered$loss_edges

  if (nrow(annotations) > 0L) {
    cov_audit <- attr(annotations, "coverage_audit")
    annotations <- tidytable::as_tidytable(annotations)
    u_f <- unique(annotations$feature_id)
    m_comp <- fastmatch::fmatch(u_f, comp_members$feature_id)
    u_comp_id <- comp_members$component_id[m_comp]
    na_comp <- is.na(u_comp_id)
    if (any(na_comp)) {
      u_comp_id[na_comp] <- paste0("M_singleton_", u_f[na_comp])
    }
    f_idx <- match(annotations$feature_id, u_f)
    data.table::set(
      annotations,
      j = "component_id",
      value = u_comp_id[f_idx]
    )
    if (!is.null(cov_audit)) {
      attr(annotations, "coverage_audit") <- cov_audit
    }
  }

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
  if (nrow(assigned_nodes) > 0L) {
    keep_an <- !duplicated(paste(
      assigned_nodes$feature_id,
      assigned_nodes$adduct,
      sep = "\r"
    ))
    assigned_nodes <- assigned_nodes[which(keep_an), , drop = FALSE]
  }

  state_map <- build_adduct_state_key_map(unique(c(
    assigned_nodes$adduct,
    cluster_edges$adduct,
    cluster_edges$adduct_dest,
    loss_edges$adduct,
    loss_edges$adduct_dest
  )))
  map_adducts <- state_map$adduct
  map_keys <- state_map$state_key

  filter_state_labeled_edges <- function(edges) {
    if (
      nrow(edges) == 0L ||
        nrow(assigned_nodes) == 0L ||
        !all(c("adduct", "adduct_dest") %in% colnames(edges))
    ) {
      return(edges)
    }

    an_keys <- map_keys[fastmatch::fmatch(assigned_nodes$adduct, map_adducts)]
    valid_an <- which(!is.na(an_keys))
    an_fids <- assigned_nodes$feature_id[valid_an]
    an_keys <- an_keys[valid_an]
    keep_ak <- !duplicated(paste(an_fids, an_keys, sep = "\r"))
    an_fids <- an_fids[keep_ak]
    an_keys <- an_keys[keep_ak]

    if (anyDuplicated(an_fids) == 0L) {
      e_src_key <- map_keys[fastmatch::fmatch(edges$adduct, map_adducts)]
      e_dest_key <- map_keys[fastmatch::fmatch(edges$adduct_dest, map_adducts)]
      src_assigned <- an_keys[fastmatch::fmatch(edges$feature_id, an_fids)]
      dest_assigned <- an_keys[fastmatch::fmatch(
        edges$feature_id_dest,
        an_fids
      )]
      keep <- is.na(src_assigned) |
        is.na(dest_assigned) |
        (!is.na(e_src_key) &
          !is.na(e_dest_key) &
          e_src_key == src_assigned &
          e_dest_key == dest_assigned)
      return(edges[which(keep), , drop = FALSE])
    }

    src_keys <- tidytable::tidytable(
      feature_id = an_fids,
      src_assigned_key = an_keys
    )
    dest_keys <- tidytable::tidytable(
      feature_id_dest = an_fids,
      dest_assigned_key = an_keys
    )

    edges |>
      tidytable::mutate(
        adduct_state_key = map_keys[fastmatch::fmatch(adduct, map_adducts)],
        adduct_dest_state_key = map_keys[fastmatch::fmatch(
          adduct_dest,
          map_adducts
        )]
      ) |>
      tidytable::left_join(src_keys, by = c("feature_id")) |>
      tidytable::left_join(dest_keys, by = c("feature_id_dest")) |>
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
    modifier <- normalize_modifier_formula_term(modifier)

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
    any(
      unname(key_lookup[expected_product]) %in%
        unname(key_lookup[src_adducts])
    )
  }

  assigned_by_fid <- if (nrow(assigned_nodes) > 0L) {
    split(assigned_nodes$adduct, assigned_nodes$feature_id)
  } else {
    list()
  }
  an_names <- names(assigned_by_fid)

  filter_edges <- function(edges, relation, modifier_col) {
    if (nrow(edges) == 0L || nrow(assigned_nodes) == 0L) {
      return(edges)
    }

    m_src <- fastmatch::fmatch(edges$feature_id, an_names)
    m_dest <- fastmatch::fmatch(edges$feature_id_dest, an_names)
    mods <- edges[[modifier_col]]
    n_e <- nrow(edges)
    keep_flag <- rep.int(TRUE, n_e)
    chk_idx <- which(!is.na(m_src) & !is.na(m_dest))
    if (length(chk_idx) > 0L) {
      for (i in chk_idx) {
        s_add <- assigned_by_fid[[m_src[[i]]]]
        d_add <- assigned_by_fid[[m_dest[[i]]]]
        if (length(s_add) > 0L && length(d_add) > 0L) {
          keep_flag[[i]] <- edge_supported_any_combo(
            src_adducts = s_add,
            dest_adducts = d_add,
            modifier = mods[[i]],
            relation = relation
          )
        }
      }
    }

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
  adducts <- unique(harmonize_adduct_vector(adducts))
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
  baseline_adducts <- align_carrier_multiplicity_adducts(
    switch(
      ms_mode,
      "pos" = c(
        "[M+H2]2+",
        "[M+H]+",
        "[M+H4N]+",
        "[M+Na]+",
        "[M+K]+",
        "[2M+H]+"
      ),
      "neg" = c(
        "[M-H2]2-",
        "[M-H]-",
        "[M-H+CH2O2]-",
        "[M-H+CH5NO2]-",
        "[M-H+CHNaO2]-",
        "[M-H+CHKO2]-",
        "[2M-H]-"
      )
    ),
    adducts
  )

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

#' Harmonize a plain adduct vector to canonical notation.
#' Length-preserving: returns one harmonized string per input element.
#' @keywords internal
harmonize_adduct_vector <- function(adducts, colname = "adduct") {
  adducts <- as.character(adducts)
  if (length(adducts) == 0L) {
    return(character())
  }
  tmp <- tidytable::tidytable(tmp__ = adducts)
  names(tmp)[[1L]] <- colname
  harmonize_adducts(
    tmp,
    adducts_colname = colname,
    adducts_translations = adducts_translations
  )[[colname]]
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
  adducts <- unique(harmonize_adduct_vector(adducts))
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
  # Combine via C-level rbindlist (via tidytable::bind_rows) —
  # avoids per-row data.frame + repeated rbind copies
  do.call(
    tidytable::bind_rows,
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
  ) |>
    tidytable::as_tidytable() |>
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
      mz_offset = (adduct_mass + n_iso * isotope_shift) / abs(z),
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
  adducts <- unique(harmonize_adduct_vector(adducts))
  if (length(adducts) == 0L) {
    return(tidytable::tidytable(adduct = character(), adduct_mass = numeric()))
  }
  rows <- lapply(adducts, .parse_single_legacy_adduct_row)
  rows <- rows[!vapply(rows, is.null, logical(1L))]
  if (length(rows) == 0L) {
    return(tidytable::tidytable(adduct = character(), adduct_mass = numeric()))
  }
  # Combine via C-level rbindlist (via tidytable::bind_rows) —
  # avoids per-row data.frame + repeated rbind copies
  tab <- do.call(
    tidytable::bind_rows,
    lapply(rows, function(r) {
      data.frame(
        adduct = r$adduct,
        adduct_mass = as.numeric(r$adduct_mass),
        n_mer = as.integer(r$n_mer),
        z = as.integer(r$z),
        stringsAsFactors = FALSE
      )
    })
  )
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
        calculate_formula_mass(f),
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
  loss_parsed <- parse_atomic_formula(loss_norm)
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
      token_parsed <- parse_atomic_formula(formula_norm)
      if (
        is.null(loss_parsed) ||
          length(loss_parsed) == 0L ||
          is.null(token_parsed) ||
          !setequal(names(loss_parsed), names(token_parsed))
      ) {
        return(FALSE)
      }
      ratios <- as.numeric(token_parsed[names(loss_parsed)]) /
        as.numeric(loss_parsed)
      all(abs(ratios - round(ratios)) < 1e-8) &&
        length(unique(round(ratios))) == 1L &&
        unique(round(ratios)) >= 1L
    },
    FUN.VALUE = logical(1L)
  ))
}

#' Return TRUE when an adduct string explicitly carries a given cluster term
#' @keywords internal
adduct_has_explicit_cluster <- function(adduct, cluster_formula) {
  adduct <- as.character(adduct)
  cluster_formula <- trimws(sub(" .*", "", as.character(cluster_formula)))
  if (
    is.na(adduct) ||
      !nzchar(adduct) ||
      is.na(cluster_formula) ||
      !nzchar(cluster_formula)
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
  clu_norm <- normalize_modifier_formula_term(cluster_formula)
  clu_parsed <- parse_atomic_formula(clu_norm)
  any(vapply(
    X = tokens,
    FUN = function(tok) {
      if (!startsWith(tok, "+")) {
        return(FALSE)
      }
      body <- substr(tok, 2L, nchar(tok))
      formula <- sub("^[0-9]+", "", body, perl = TRUE)
      formula_norm <- normalize_modifier_formula_term(formula)
      if (identical(formula_norm, clu_norm)) {
        return(TRUE)
      }
      token_parsed <- parse_atomic_formula(formula_norm)
      if (
        is.null(clu_parsed) ||
          length(clu_parsed) == 0L ||
          is.null(token_parsed) ||
          !setequal(names(clu_parsed), names(token_parsed))
      ) {
        return(FALSE)
      }
      ratios <- as.numeric(token_parsed[names(clu_parsed)]) /
        as.numeric(clu_parsed)
      all(abs(ratios - round(ratios)) < 1e-8) &&
        length(unique(round(ratios))) == 1L &&
        unique(round(ratios)) >= 1L
    },
    FUN.VALUE = logical(1L)
  ))
}

#' Align equivalent multicharged carrier notations (e.g. `[M+2H]2+` vs `[M+H2]2+`)
#' to whichever convention is present in `reference_adducts`.
#' @keywords internal
align_carrier_multiplicity_adducts <- function(adducts, reference_adducts) {
  if (length(adducts) == 0L) {
    return(adducts)
  }
  pairs <- list(
    c("[M+2H]2+", "[M+H2]2+"),
    c("[M+3H]3+", "[M+H3]3+"),
    c("[M-2H]2-", "[M-H2]2-"),
    c("[M-3H]3-", "[M-H3]3-"),
    c("[M+2Na]2+", "[M+Na2]2+"),
    c("[M+3Na]3+", "[M+Na3]3+"),
    c("[M+2K]2+", "[M+K2]2+"),
    c("[M+3K]3+", "[M+K3]3+"),
    c("[M+2Cu]2+", "[M+Cu2]2+"),
    c("[M+2H4N]2+", "[M+H8N2]2+")
  )
  ref_set <- unique(as.character(reference_adducts))
  out <- as.character(adducts)
  for (p in pairs) {
    p_pref <- p[[1L]]
    p_sub <- p[[2L]]
    has_pref <- p_pref %in% ref_set
    has_sub <- p_sub %in% ref_set
    if (has_pref && !has_sub) {
      out[!is.na(out) & out == p_sub] <- p_pref
    } else {
      out[!is.na(out) & out == p_pref] <- p_sub
    }
  }
  out
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
  exact_masses,
  correlation_p_threshold = 0.05
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

  # Compute intensity co-variance edges within RT windows
  covariance_edges <- compute_intensity_covariance_edges(
    features_table = features_table,
    tolerance_rt = tolerance_rt,
    correlation_p_threshold = correlation_p_threshold
  )

  # Drop intensity_all now that covariance is done (no longer needed for annotation)
  if ("intensity_all" %in% names(features_table)) {
    features_table$intensity_all <- NULL
  }

  # NOTE: Covariance edges are NOT added to adduct_edges_combined because they lack
  # adduct/adduct_dest columns. They're returned separately and added to output at the final stage.

  list(
    adduct_edges = adduct_edges,
    adduct_edges_combined = adduct_edges_combined,
    cluster_edges = cluster_edges,
    loss_edges = loss_edges,
    evidence_signal = evidence_signal,
    covariance_edges = covariance_edges
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

#' Normalize modifier formula terms with memoization for repeated values
#'
#' @description Fast vectorized normalization of modifier formula terms.
#'     Caches the normalization of unique values and maps all inputs through
#'     the cache using direct vector indexing, achieving 10-40x speedup on
#'     realistic data where modifiers repeat frequently (e.g., H2O, NH3, Na, K
#'     appearing 100+ times).
#'
#' @param terms Character vector of modifier formula terms to normalize
#'
#' @return Character vector of normalized terms
#'
#' @details This is a performance-critical optimization for:
#'     - score_edges() in .annotate_single_feature (1000s of edges per feature)
#'     - score_constraints() in consistency checking (cascading edge normalization)
#'     Caching exploits the fact that real mass-spec edge lists have few unique
#'     modifier types repeated many times. Speedup scales with repetition ratio.
#'
#'     Uses vectorized match() for efficient mapping instead of loops.
#'
#' @keywords internal
normalize_modifier_formula_terms <- function(terms) {
  # Fast path: empty input
  if (length(terms) == 0L) {
    return(character())
  }

  # Separate NA positions for correct NA propagation
  na_mask <- is.na(terms)

  unique_terms <- unique(terms[!na_mask])

  # Normalize each unique term exactly once (cached)
  normalized_unique <- vapply(
    X = unique_terms,
    FUN = normalize_modifier_formula_term,
    FUN.VALUE = character(1L),
    USE.NAMES = FALSE
  )

  # Map all input terms through cached lookup via vectorized match()
  # match() returns indices of where each term appears in unique_terms,
  # then subscript into normalized_unique to get results
  indices <- match(terms[!na_mask], unique_terms)

  # Pre-allocate result and fill in one assignment
  result <- character(length(terms))
  result[!na_mask] <- normalized_unique[indices]
  result[na_mask] <- NA_character_

  result
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
  canonicalize_adducts_vectorized(simplified)
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

  if (identical(relation, "cluster")) {
    keep_dest <- vapply(
      dest_hypotheses$adduct,
      adduct_has_explicit_cluster,
      logical(1L),
      cluster_formula = modifier,
      USE.NAMES = FALSE
    )
    dest_hypotheses <- dest_hypotheses[which(keep_dest), , drop = FALSE]
    if (nrow(dest_hypotheses) == 0L) {
      return(0L)
    }
    expected <- apply_modifier_to_adducts(src_hypotheses$adduct, modifier, "+")
    state_map <- build_adduct_state_key_map(unique(c(
      expected,
      dest_hypotheses$adduct
    )))
    key_lookup <- stats::setNames(state_map$state_key, state_map$adduct)

    src_keys <- unname(key_lookup[expected])
    dest_keys <- unname(key_lookup[dest_hypotheses$adduct])
  } else {
    keep_src <- vapply(
      src_hypotheses$adduct,
      adduct_has_explicit_loss,
      logical(1L),
      loss_formula = modifier,
      USE.NAMES = FALSE
    )
    src_hypotheses <- src_hypotheses[which(keep_src), , drop = FALSE]
    if (nrow(src_hypotheses) == 0L) {
      return(0L)
    }
    expected <- apply_modifier_to_adducts(dest_hypotheses$adduct, modifier, "-")
    state_map <- build_adduct_state_key_map(unique(c(
      expected,
      src_hypotheses$adduct
    )))
    key_lookup <- stats::setNames(state_map$state_key, state_map$adduct)

    src_keys <- unname(key_lookup[src_hypotheses$adduct])
    dest_keys <- unname(key_lookup[expected])
  }

  valid_s <- which(!is.na(src_keys))
  valid_d <- which(!is.na(dest_keys))
  sk_s <- src_keys[valid_s]
  sk_d <- dest_keys[valid_d]
  common_keys <- intersect(sk_s, sk_d)
  if (length(common_keys) == 0L) {
    return(0L)
  }
  w_s <- src_hypotheses$.row_weight[valid_s]
  w_d <- dest_hypotheses$.row_weight[valid_d]
  ord_s <- order(-w_s)
  sk_s_u <- sk_s[ord_s]
  keep_s <- !duplicated(sk_s_u)
  max_w_s <- w_s[ord_s[keep_s]][fastmatch::fmatch(common_keys, sk_s_u[keep_s])]

  ord_d <- order(-w_d)
  sk_d_u <- sk_d[ord_d]
  keep_d <- !duplicated(sk_d_u)
  max_w_d <- w_d[ord_d[keep_d]][fastmatch::fmatch(common_keys, sk_d_u[keep_d])]

  as.integer(sum(as.integer(max_w_s) + as.integer(max_w_d)))
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
    nh_orig <- tidytable::coalesce(
      node_hypotheses$candidate_adduct_origin,
      "baseline"
    )
    nh_src <- tidytable::coalesce(node_hypotheses$source, "baseline")
    nh_sup <- ifelse(
      is.na(node_hypotheses$adduct_support),
      0L,
      as.integer(node_hypotheses$adduct_support)
    )
    nh_rw <- score_modifier_hypothesis_row(
      source = nh_src,
      candidate_adduct_origin = nh_orig,
      adduct_support = nh_sup
    )
    u_f <- unique(c(
      node_hypotheses$feature_id,
      cluster_edges$feature_id,
      cluster_edges$feature_id_dest,
      loss_edges$feature_id,
      loss_edges$feature_id_dest
    ))
    u_a <- unique(c(
      node_hypotheses$adduct,
      cluster_edges$adduct,
      cluster_edges$adduct_dest,
      loss_edges$adduct,
      loss_edges$adduct_dest
    ))
    n_a <- length(u_a)
    nh_f_id <- fastmatch::fmatch(node_hypotheses$feature_id, u_f)
    nh_a_id <- fastmatch::fmatch(node_hypotheses$adduct, u_a)
    nh_key <- as.double(nh_f_id - 1L) * n_a + nh_a_id
    ord_nh <- order(-nh_rw, na.last = TRUE, method = "radix")
    nh_key_ord <- nh_key[ord_nh]
    keep_nh <- !duplicated(nh_key_ord)
    u_nh_key <- nh_key_ord[keep_nh]
    u_nh_rw <- nh_rw[ord_nh[keep_nh]]

    score_edge_vec <- function(f_src_id, f_dest_id, a_src_id, a_dest_id) {
      s_k <- as.double(f_src_id - 1L) * n_a + a_src_id
      d_k <- as.double(f_dest_id - 1L) * n_a + a_dest_id
      s_sc <- u_nh_rw[fastmatch::fmatch(s_k, u_nh_key)]
      d_sc <- u_nh_rw[fastmatch::fmatch(d_k, u_nh_key)]
      s_sc[is.na(s_sc)] <- 0L
      d_sc[is.na(d_sc)] <- 0L
      as.integer(s_sc + d_sc)
    }

    c_fs <- fastmatch::fmatch(cluster_edges$feature_id, u_f)
    c_fd <- fastmatch::fmatch(cluster_edges$feature_id_dest, u_f)
    c_as <- fastmatch::fmatch(cluster_edges$adduct, u_a)
    c_ad <- fastmatch::fmatch(cluster_edges$adduct_dest, u_a)

    l_fs <- fastmatch::fmatch(loss_edges$feature_id, u_f)
    l_fd <- fastmatch::fmatch(loss_edges$feature_id_dest, u_f)
    l_as <- fastmatch::fmatch(loss_edges$adduct, u_a)
    l_ad <- fastmatch::fmatch(loss_edges$adduct_dest, u_a)

    c_edge_score <- score_edge_vec(c_fs, c_fd, c_as, c_ad)
    l_edge_score <- score_edge_vec(l_fs, l_fd, l_as, l_ad)
    c_mod_key <- normalize_modifier_formula_terms(cluster_edges$cluster)
    l_mod_key <- normalize_modifier_formula_terms(loss_edges$loss)

    u_m <- unique(c(c_mod_key, l_mod_key))
    n_m <- length(u_m)
    n_f <- length(u_f)
    c_m_id <- fastmatch::fmatch(c_mod_key, u_m)
    l_m_id <- fastmatch::fmatch(l_mod_key, u_m)

    c_key <- (as.double(c_fs - 1L) * n_f + (c_fd - 1L)) * n_m + c_m_id
    l_key <- (as.double(l_fs - 1L) * n_f + (l_fd - 1L)) * n_m + l_m_id

    first_c <- !duplicated(c_key)
    u_c_key <- c_key[first_c]
    u_c_sc <- c_edge_score[first_c]

    first_l <- !duplicated(l_key)
    u_l_key <- l_key[first_l]
    u_l_sc <- l_edge_score[first_l]

    m_cl <- fastmatch::fmatch(u_c_key, u_l_key)
    l_sc_for_c <- ifelse(is.na(m_cl), 0L, u_l_sc[m_cl])
    u_keep_c <- u_c_sc >= l_sc_for_c
    u_keep_l <- l_sc_for_c >= u_c_sc

    m_c_all <- fastmatch::fmatch(c_key, u_c_key)
    keep_c_rows <- u_keep_c[m_c_all]
    keep_c_rows[is.na(keep_c_rows)] <- TRUE

    m_l_all <- fastmatch::fmatch(l_key, u_c_key)
    keep_l_rows <- u_keep_l[m_l_all]
    keep_l_rows[is.na(keep_l_rows)] <- TRUE

    cluster_edges_kept <- cluster_edges[which(keep_c_rows), , drop = FALSE]
    loss_edges_kept <- loss_edges[which(keep_l_rows), , drop = FALSE]

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
      .modifier_key = normalize_modifier_formula_terms(cluster)
    )
  loss_cmp <- loss_edges |>
    tidytable::mutate(
      .modifier_key = normalize_modifier_formula_terms(loss)
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

  ambig_feats <- unique(c(
    ambiguous_pairs$feature_id,
    ambiguous_pairs$feature_id_dest
  ))
  hyp_sub <- hyp_min[
    which(!is.na(fastmatch::fmatch(hyp_min$feature_id, ambig_feats))),
    ,
    drop = FALSE
  ]
  hyp_sub$.row_weight <- score_modifier_hypothesis_row(
    source = hyp_sub$source,
    candidate_adduct_origin = hyp_sub$candidate_adduct_origin,
    adduct_support = hyp_sub$adduct_support
  )
  hyp_by_feature <- split(hyp_sub, hyp_sub$feature_id)
  n_ambig <- nrow(ambiguous_pairs)
  amb_src <- ambiguous_pairs$feature_id
  amb_dest <- ambiguous_pairs$feature_id_dest
  amb_mod <- ambiguous_pairs$.modifier_key
  c_scores <- integer(n_ambig)
  l_scores <- integer(n_ambig)
  keep_c_vec <- logical(n_ambig)
  keep_l_vec <- logical(n_ambig)

  for (i in seq_len(n_ambig)) {
    src_id <- amb_src[[i]]
    dest_id <- amb_dest[[i]]
    modifier_key <- amb_mod[[i]]
    src_hypotheses <- hyp_by_feature[[src_id]] %||% hyp_sub[0L, ]
    dest_hypotheses <- hyp_by_feature[[dest_id]] %||% hyp_sub[0L, ]

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
    c_scores[[i]] <- cluster_score
    l_scores[[i]] <- loss_score

    if (cluster_score > loss_score) {
      keep_c_vec[[i]] <- TRUE
      keep_l_vec[[i]] <- FALSE
    } else if (loss_score > cluster_score) {
      keep_c_vec[[i]] <- FALSE
      keep_l_vec[[i]] <- TRUE
    } else {
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

      if (cluster_neighbor_score > loss_neighbor_score) {
        keep_c_vec[[i]] <- TRUE
        keep_l_vec[[i]] <- FALSE
      } else if (loss_neighbor_score > cluster_neighbor_score) {
        keep_c_vec[[i]] <- FALSE
        keep_l_vec[[i]] <- TRUE
      } else {
        keep_c_vec[[i]] <- TRUE
        keep_l_vec[[i]] <- TRUE
      }
    }
  }
  decisions <- tidytable::tidytable(
    feature_id = amb_src,
    feature_id_dest = amb_dest,
    .modifier_key = amb_mod,
    cluster_score = c_scores,
    loss_score = l_scores,
    keep_cluster = keep_c_vec,
    keep_loss = keep_l_vec
  )

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
  tolerance_dalton,
  covariance_edges = NULL
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
    tolerance_dalton = tolerance_dalton,
    covariance_edges = covariance_edges
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

  unmatched <- if (nrow(matched) == 0L) {
    unm <- tidytable::as_tidytable(node_hypotheses)
    unm$structure_exact_mass <- rep(NA_real_, nrow(unm))
    unm$error_mz <- rep(NA_real_, nrow(unm))
    unm
  } else {
    u_f <- unique(c(node_hypotheses$feature_id, matched$feature_id))
    u_a <- unique(c(node_hypotheses$adduct, matched$adduct))
    n_a <- max(1L, length(u_a))
    f_h <- match(node_hypotheses$feature_id, u_f)
    a_h <- match(node_hypotheses$adduct, u_a)
    f_m <- match(matched$feature_id, u_f)
    a_m <- match(matched$adduct, u_a)
    key_h <- as.double(f_h - 1L) * n_a + a_h
    key_m <- unique(as.double(f_m - 1L) * n_a + a_m)
    unm_idx <- which(is.na(fastmatch::fmatch(key_h, key_m)))
    unm <- tidytable::as_tidytable(
      lapply(as.list(node_hypotheses), `[`, unm_idx)
    )
    unm$structure_exact_mass <- rep(NA_real_, length(unm_idx))
    unm$error_mz <- rep(NA_real_, length(unm_idx))
    unm
  }

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
  n_in <- nrow(annotations)
  if (n_in == 0L || nrow(adduct_edges) == 0L) {
    return(annotations)
  }
  required_cols <- c("feature_id", "adduct")
  if (!all(required_cols %in% colnames(annotations))) {
    return(annotations)
  }

  consistent_edges <- enforce_graph_adduct_consistency(adduct_edges)

  assigned_states <- tidytable::bind_rows(
    consistent_edges |>
      tidytable::transmute(feature_id, adduct),
    consistent_edges |>
      tidytable::transmute(feature_id = feature_id_dest, adduct = adduct_dest)
  ) |>
    tidytable::distinct()

  u_add_all <- unique(c(annotations$adduct, assigned_states$adduct))
  state_map <- build_adduct_state_key_map(u_add_all)
  sm_adduct <- as.character(state_map$adduct)
  sm_key <- as.character(state_map$state_key)
  sm_key[is.na(sm_key)] <- sm_adduct[is.na(sm_key)]

  ass_m <- fastmatch::fmatch(assigned_states$adduct, sm_adduct)
  ass_key <- ifelse(is.na(ass_m), assigned_states$adduct, sm_key[ass_m])
  assigned_states <- tidytable::tidytable(
    feature_id = assigned_states$feature_id,
    assigned_state_key = ass_key
  ) |>
    tidytable::distinct()

  row_m_add <- fastmatch::fmatch(annotations$adduct, sm_adduct)
  row_state_key <- ifelse(
    is.na(row_m_add),
    as.character(annotations$adduct),
    sm_key[row_m_add]
  )

  m_feat <- fastmatch::fmatch(
    annotations$feature_id,
    assigned_states$feature_id
  )
  ass_row_key <- assigned_states$assigned_state_key[m_feat]
  keep <- is.na(ass_row_key) |
    (!is.na(row_state_key) & row_state_key == ass_row_key)
  feat_is_unconstrained <- is.na(ass_row_key)

  f_col <- annotations$feature_id
  u_f <- unique(f_col)
  n_u_f <- length(u_f)
  f_id <- match(f_col, u_f)

  coverage_mode <- resolve_annotate_masses_coverage_mode(coverage_mode)
  if (coverage_mode == "best_supported_conflict_free") {
    src_col <- if ("source" %in% colnames(annotations)) {
      as.character(annotations$source)
    } else {
      rep(NA_character_, n_in)
    }
    has_err <- if ("candidate_structure_error_mz" %in% colnames(annotations)) {
      !is.na(annotations$candidate_structure_error_mz)
    } else {
      logical(n_in)
    }
    supp_strength <- if ("adduct_support" %in% colnames(annotations)) {
      s_tmp <- as.integer(annotations$adduct_support)
      s_tmp[is.na(s_tmp)] <- 0L
      s_tmp
    } else {
      integer(n_in)
    }

    baseline_on <- !is.null(baseline_adduct) &&
      length(baseline_adduct) >= 1L &&
      !is.na(baseline_adduct[[1L]]) &&
      nzchar(baseline_adduct[[1L]])
    baseline_label <- if (baseline_on) baseline_adduct[[1L]] else NA_character_

    supp_class_rank <- rep(7L, n_in)
    if (baseline_on) {
      supp_class_rank[
        !is.na(annotations$adduct) & annotations$adduct == baseline_label
      ] <- 6L
    }
    supp_class_rank[!is.na(src_col) & src_col == "evidence"] <- 5L
    supp_class_rank[!is.na(src_col) & src_col %in% c("cluster", "loss")] <- 4L
    supp_class_rank[!is.na(src_col) & src_col == "multi"] <- 3L
    supp_class_rank[
      !is.na(src_col) &
        src_col %in% c("pair", "preassigned", "preassigned_propagated")
    ] <- 2L
    supp_class_rank[!has_err] <- supp_class_rank[!has_err] + 10L

    # Unconstrained features have keep == TRUE for all their rows; pick best
    # (min supp_class_rank, max supp_strength) per unconstrained feature.
    unc_idx <- which(feat_is_unconstrained)
    if (length(unc_idx) > 0L) {
      f_unc <- f_id[unc_idx]
      r_unc <- supp_class_rank[unc_idx]
      s_unc <- supp_strength[unc_idx]
      max_s <- max(s_unc, 0L) + 2L
      score_unc <- r_unc * max_s - s_unc

      # Collapse consecutive identical (f_unc, score_unc)
      n_u <- length(unc_idx)
      diff_prev <- if (n_u > 1L) {
        c(TRUE, f_unc[-1L] != f_unc[-n_u] | score_unc[-1L] != score_unc[-n_u])
      } else {
        TRUE
      }
      f_sub <- f_unc[diff_prev]
      sc_sub <- score_unc[diff_prev]

      ord_sc <- order(sc_sub, method = "radix")
      win_sc <- ord_sc[!duplicated(f_sub[ord_sc])]
      best_sc_lut <- integer(n_u_f)
      best_sc_lut[f_sub[win_sc]] <- sc_sub[win_sc]

      keep[unc_idx] <- keep[unc_idx] & (score_unc == best_sc_lut[f_unc])
    }
  }

  n_dropped_rows <- sum(!keep)
  n_kept_features <- sum(tabulate(f_id[keep], nbins = n_u_f) > 0L)
  n_dropped_features <- if (n_dropped_rows > 0L) {
    sum(tabulate(f_id[!keep], nbins = n_u_f) > 0L)
  } else {
    0L
  }

  if (n_dropped_rows > 0L) {
    drop_idx <- which(!keep)
    add_drop <- annotations$adduct[drop_idx]
    u_add_drop <- unique(add_drop)
    add_drop_id <- match(add_drop, u_add_drop)
    n_u_add_drop <- length(u_add_drop)
    pair_key <- as.double(f_id[drop_idx] - 1L) * n_u_add_drop + add_drop_id
    dropped_n <- length(unique(pair_key))
    dropped_all_features <- max(0L, n_u_f - n_kept_features)
    log_info(
      paste0(
        "Conflict-resolution filter removed %d annotation row(s) with states ",
        "incompatible with graph-consistent evidence."
      ),
      dropped_n
    )
    log_info(
      "Conflict-resolution pruning touched %d feature(s) and removed all annotations from %d feature(s).",
      n_dropped_features,
      dropped_all_features
    )
  }

  out <- if (n_dropped_rows > 0L) {
    keep_idx <- which(keep)
    tidytable::as_tidytable(lapply(as.list(annotations), `[`, keep_idx))
  } else {
    tidytable::as_tidytable(annotations)
  }

  attr(out, "coverage_audit") <- list(
    n_input = n_in,
    n_kept = nrow(out),
    n_dropped = n_dropped_rows,
    n_input_features = n_u_f,
    n_kept_features = n_kept_features,
    n_dropped_features = n_dropped_features
  )
  out
}
