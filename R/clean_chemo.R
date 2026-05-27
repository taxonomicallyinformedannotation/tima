#' @title Clean Chemical Annotations
#'
#' @description Cleans and filters chemically weighted annotation results
#'     through
#' a multi-tier pipeline. Applies MS1 score thresholds, percentile filtering,
#' ranking, and optional high-confidence filtering. Returns three-tier output:
#' full (comprehensive), filtered (top candidates), and mini (one row per
#'     feature).
#'
#' @include add_xrefs_to_annotations.R
#' @include calculate_mass_of_m.R
#' @include constants.R
#' @include filter_high_confidence_only.R
#' @include parse_adduct.R
#' @include summarize_results.R
#'
#' @param annot_table_wei_chemo Data frame with chemically weighted annotations.
#' Required columns: feature_id,
#'     candidate_structure_inchikey_connectivity_layer,
#' score_weighted_chemo, score_biological, score_chemical,
#'     candidate_score_pseudo_initial
#' @param components_table Data frame with molecular network component
#'     assignments.
#'     Required columns: feature_id, component_id
#' @param features_table Data frame with feature metadata (RT, m/z, etc.).
#'     Required columns: feature_id
#' @param structure_organism_pairs_table Data frame linking structures to
#'     organisms.
#'     Required columns: structure_inchikey_connectivity_layer
#' @param candidates_final Integer, number of top candidates to retain per
#'     feature (>= 1)
#' @param best_percentile Numeric (0-1), percentile threshold for score
#'     filtering.
#' Candidates with scores >= percentile * max_score are kept. Default: 0.9 (90th
#'     percentile)
#' @param minimal_ms1_bio Numeric (0-1), minimum biological score for MS1-only
#'     annotations
#' @param minimal_ms1_chemo Numeric (0-1), minimum chemical score for MS1-only
#'     annotations
#' @param minimal_ms1_condition Character, logical operator for MS1 filtering:
#'     "OR" or "AND".
#'     "OR" = keep if bio >= threshold OR chem >= threshold.
#'     "AND" = keep if bio >= threshold AND chem >= threshold
#' @param compounds_names Logical, include compound names in output (may
#'     increase size)
#' @param high_confidence Logical, apply strict high-confidence filters
#' @param remove_ties Logical, remove tied scores (keep only highest-ranked)
#' @param summarize Logical, collapse results to one row per feature
#' @param max_per_score Integer, max candidates to keep per feature per score.
#'   If more exist, they are randomly sampled and a note is added. Default 7.
#' @param score_chemical_cla_kingdom Numeric (0-1), score for ClassyFire kingdom
#'     level
#' @param score_chemical_cla_superclass Numeric (0-1), score for ClassyFire
#'     superclass level
#' @param score_chemical_cla_class Numeric (0-1), score for ClassyFire class
#'     level
#' @param score_chemical_cla_parent Numeric (0-1), score for ClassyFire direct
#'     parent level
#' @param score_chemical_npc_pathway Numeric (0-1), score for NPClassifier
#'     pathway level
#' @param score_chemical_npc_superclass Numeric (0-1), score for NPClassifier
#'     superclass level
#' @param score_chemical_npc_class Numeric (0-1), score for NPClassifier class
#'     level
#' @param xrefs_table Optional data frame with columns inchikey/prefix/id from
#'   get_compounds_xrefs(), used to add candidate_structure_id_* columns before
#'   summarization.
#'
#' @return Named list with three data frames:
#'   \describe{
#'     \item{full}{All annotations (optionally high-confidence filtered)}
#'     \item{filtered}{Top candidates meeting percentile + rank thresholds}
#'     \item{mini}{One row per feature with best compound/taxonomy}
#'   }
#'
#' @seealso \code{\link{weight_chemo}},
#'     \code{\link{filter_high_confidence_only}},
#'     \code{\link{summarize_results}}
#'
#' @examples
#' \dontrun{
#' results <- clean_chemo(
#'   annot_table_wei_chemo = annotations,
#'   features_table = features,
#'   components_table = components,
#'   structure_organism_pairs_table = sop_table,
#'   candidates_final = 10,
#'   best_percentile = 0.9,
#'   minimal_ms1_bio = 0.5,
#'   minimal_ms1_chemo = 0.5,
#'   minimal_ms1_condition = "OR",
#'   compounds_names = TRUE,
#'   high_confidence = FALSE,
#'   remove_ties = FALSE,
#'   summarize = FALSE
#' )
#' }
clean_chemo <- function(
  annot_table_wei_chemo,
  components_table,
  features_table,
  structure_organism_pairs_table,
  candidates_final,
  best_percentile,
  minimal_ms1_bio,
  minimal_ms1_chemo,
  minimal_ms1_condition,
  compounds_names,
  high_confidence,
  remove_ties,
  summarize,
  # Explicit taxonomy weights (0-1)
  score_chemical_cla_kingdom = 0.2,
  score_chemical_cla_superclass = 0.4,
  score_chemical_cla_class = 0.6,
  score_chemical_cla_parent = 0.8,
  score_chemical_npc_pathway = 0.25,
  score_chemical_npc_superclass = 0.5,
  score_chemical_npc_class = 0.75,
  max_per_score = 7L,
  xrefs_table = NULL
) {
  # Initialize logging context
  ctx <- log_operation(
    "clean_chemo",
    n_annotations = nrow(annot_table_wei_chemo),
    candidates_final = candidates_final,
    high_confidence = high_confidence
  )

  # Input Validation ----

  validate_clean_chemo_inputs(
    annot_table_wei_chemo = annot_table_wei_chemo,
    candidates_final = candidates_final,
    best_percentile = best_percentile,
    minimal_ms1_bio = minimal_ms1_bio,
    minimal_ms1_chemo = minimal_ms1_chemo,
    minimal_ms1_condition = minimal_ms1_condition,
    compounds_names = compounds_names,
    high_confidence = high_confidence,
    remove_ties = remove_ties,
    summarize = summarize,
    max_per_score = max_per_score,
    score_chemical_cla_kingdom = score_chemical_cla_kingdom,
    score_chemical_cla_superclass = score_chemical_cla_superclass,
    score_chemical_cla_class = score_chemical_cla_class,
    score_chemical_cla_parent = score_chemical_cla_parent,
    score_chemical_npc_pathway = score_chemical_npc_pathway,
    score_chemical_npc_superclass = score_chemical_npc_superclass,
    score_chemical_npc_class = score_chemical_npc_class
  )

  # Early exit for empty input
  if (nrow(annot_table_wei_chemo) == 0L) {
    log_complete(ctx, n_final = 0, note = "Empty annotation table")
    return(annot_table_wei_chemo)
  }

  # Validate features and components schema minimally
  purrr::walk(
    .x = list(features_table, components_table),
    .f = .validate_features_dataframe
  )
  if (!is.data.frame(structure_organism_pairs_table)) {
    tima_abort(
      problem = "structure_organism_pairs_table must be a data frame",
      class = c("tima_validation_error", "tima_error")
    )
  }

  # Ensure Score Columns are Numeric ----

  annot_table_wei_chemo <- coerce_score_columns(annot_table_wei_chemo)

  # Ensure feature-level `rt` and `mz` are present on every candidate row:
  # `sample_candidates_per_group` uses them for the cross-feature M + RT
  # anchor. If upstream scoring dropped them, left-join from features_table.
  .ft_cols <- intersect(c("feature_id", "rt", "mz"), names(features_table))
  .missing_feature_cols <- setdiff(
    c("rt", "mz"),
    names(annot_table_wei_chemo)
  )
  if (
    length(.missing_feature_cols) > 0L && all(c("feature_id") %in% .ft_cols)
  ) {
    annot_table_wei_chemo <- annot_table_wei_chemo |>
      tidytable::left_join(
        y = features_table |>
          tidytable::select(tidyselect::any_of(.ft_cols)) |>
          tidytable::distinct(feature_id, .keep_all = TRUE)
      )
  }

  # Precompute feature-level consensus metadata once to avoid repeated
  # full-table coercion inside summarize_results for each output tier.
  feature_consensus_table <- .build_feature_consensus_table(
    annot_table_wei_chemo = annot_table_wei_chemo,
    model = columns_model()
  )

  # Core Filtering Pipeline ----
  log_metadata(
    ctx,
    phase = "filtering",
    best_percentile = best_percentile,
    minimal_ms1_bio = minimal_ms1_bio,
    minimal_ms1_chemo = minimal_ms1_chemo,
    max_per_score = max_per_score
  )

  candidate_tables <- prepare_ranked_candidates(
    annot_table_wei_chemo = annot_table_wei_chemo,
    minimal_ms1_bio = minimal_ms1_bio,
    minimal_ms1_chemo = minimal_ms1_chemo,
    minimal_ms1_condition = minimal_ms1_condition,
    best_percentile = best_percentile,
    max_per_score = max_per_score
  )

  df_ranked <- candidate_tables$df_ranked
  df_percentile <- candidate_tables$df_percentile
  results_candidates <- candidate_tables$results_candidates
  annotation_notes_lookup <- candidate_tables$annotation_notes_lookup
  organism_lookup <- .build_organism_lookup(
    structure_organism_pairs_table = structure_organism_pairs_table,
    df = df_ranked
  )

  if (candidate_tables$n_sampled_features > 0L) {
    log_info(
      "Sampling candidates for %d features with more than %d candidates per score",
      candidate_tables$n_sampled_features,
      max_per_score
    )
  }

  # Three-Tier Output Generation ----

  # Apply High-Confidence Filter for Filtered and Full Tiers ----

  # Apply filter_high_confidence_only for filtered tier
  df_filtered <- df_percentile |>
    filter_high_confidence_only(context = "filtered") |>
    tidytable::filter(rank_final <= candidates_final)

  if (!is.null(xrefs_table) && nrow(xrefs_table) > 0L) {
    df_filtered <- .add_xrefs_to_df(df_filtered, xrefs_table)
  }

  results_filtered <- df_filtered |>
    summarize_results(
      features_table = features_table,
      components_table = components_table,
      structure_organism_pairs_table = structure_organism_pairs_table,
      annot_table_wei_chemo = annot_table_wei_chemo,
      remove_ties = remove_ties,
      summarize = summarize,
      annotation_notes_lookup = annotation_notes_lookup,
      feature_consensus_table = feature_consensus_table,
      organism_lookup = organism_lookup
    ) |>
    tidytable::left_join(y = results_candidates) |>
    tidytable::mutate(
      # Set candidates_best to NA when no inchikey
      candidates_best = tidytable::if_else(
        is.na(candidate_structure_inchikey_connectivity_layer),
        NA_integer_,
        candidates_best
      )
    )

  # Tier 1: MINI - Extract from df_percentile BEFORE high-confidence filter
  # This ensures we get predicted taxonomy for features without inchikey

  df_classes_mini <- build_mini_taxonomy_table(
    df_percentile = df_percentile,
    score_chemical_cla_kingdom = score_chemical_cla_kingdom,
    score_chemical_cla_superclass = score_chemical_cla_superclass,
    score_chemical_cla_class = score_chemical_cla_class,
    score_chemical_cla_parent = score_chemical_cla_parent,
    score_chemical_npc_pathway = score_chemical_npc_pathway,
    score_chemical_npc_superclass = score_chemical_npc_superclass,
    score_chemical_npc_class = score_chemical_npc_class
  )

  # Combine mini results and adjust candidates_best based on inchikey presence
  results_mini <- build_mini_results_table(
    features_table = features_table,
    df_classes_mini = df_classes_mini,
    results_filtered = results_filtered,
    df_filtered = df_filtered,
    xrefs_table = xrefs_table
  )

  rm(df_classes_mini, df_filtered, df_percentile)
  invisible(gc(verbose = FALSE))

  # Tier 3: FULL - Optionally apply high-confidence filter
  df_full <- if (high_confidence) {
    df_ranked |>
      filter_high_confidence_only(context = "full")
  } else {
    df_ranked
  }

  if (!is.null(xrefs_table) && nrow(xrefs_table) > 0L) {
    df_full <- .add_xrefs_to_df(df_full, xrefs_table)
  }

  results_full <- df_full |>
    summarize_results(
      features_table = features_table,
      components_table = components_table,
      structure_organism_pairs_table = structure_organism_pairs_table,
      annot_table_wei_chemo = annot_table_wei_chemo,
      remove_ties = remove_ties,
      summarize = summarize,
      annotation_notes_lookup = annotation_notes_lookup,
      feature_consensus_table = feature_consensus_table,
      organism_lookup = organism_lookup
    ) |>
    tidytable::left_join(y = results_candidates) |>
    tidytable::mutate(
      # Set candidates_best to NA when no inchikey
      candidates_best = tidytable::if_else(
        is.na(candidate_structure_inchikey_connectivity_layer),
        NA_integer_,
        candidates_best
      )
    )

  rm(df_full, df_ranked)
  invisible(gc(verbose = FALSE))

  # Optionally Remove Compound Names (After All Processing) ----

  # Build results list
  results_list <- list(
    full = results_full,
    filtered = results_filtered,
    mini = results_mini
  )

  # Remove compound names from outputs if requested
  results_list <- remove_compound_names(results_list, compounds_names)

  log_complete(
    ctx,
    n_final_full = nrow(results_full),
    n_final_filtered = nrow(results_filtered),
    n_final_mini = nrow(results_mini),
    n_features = tidytable::n_distinct(results_filtered$feature_id)
  )

  # Clean up intermediate objects
  rm(
    annot_table_wei_chemo,
    results_candidates,
    features_table,
    components_table,
    structure_organism_pairs_table,
    candidate_tables,
    feature_consensus_table,
    organism_lookup
  )

  results_list
}
