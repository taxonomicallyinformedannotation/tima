#' @title Clean bio
#'
#' @description This function cleans and filters biologically weighted
#'     annotation results by calculating chemical consistency scores across
#'     network neighbors. Only features with at least 2 neighbors are evaluated.
#'
#' @include validators.R
#'
#' @param annot_table_wei_bio Data frame containing biologically weighted annotations
#' @param edges_table Data frame containing network edges between features
#' @param minimal_consistency Numeric minimum consistency score (0-1) required
#'     to retain a classification at each taxonomic level
#'
#' @return Data frame containing filtered biologically weighted annotations
#'     with consistency scores
#'
#' @seealso weight_bio
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Clean biological annotations by network consistency
#' cleaned <- clean_bio(
#'   annot_table_wei_bio = bio_weighted_annotations,
#'   edges_table = network_edges,
#'   minimal_consistency = 0.5
#' )
#' }
clean_bio <- function(
  annot_table_wei_bio,
  edges_table,
  minimal_consistency
) {
  # Input Validation (using centralized validators) ----

  validate_dataframe(
    annot_table_wei_bio,
    param_name = "annot_table_wei_bio",
    allow_empty = TRUE
  )

  validate_dataframe(
    edges_table,
    param_name = "edges_table",
    allow_empty = TRUE
  )

  validate_numeric_range(
    minimal_consistency,
    min_value = 0,
    max_value = 1,
    param_name = "minimal_consistency"
  )

  # Early exit for empty inputs
  if (nrow(annot_table_wei_bio) == 0L) {
    logger::log_warn("Empty annotation table provided")
    return(annot_table_wei_bio)
  }

  if (nrow(edges_table) == 0L) {
    logger::log_warn("Empty edges table provided, cannot calculate consistency")
    return(.add_default_prediction_columns(annot_table_wei_bio))
  }

  # Extract Distinct Structure-Taxonomy Pairs ----

  # Extract unique structure-taxonomy combinations
  annotations_distinct <- .extract_distinct_taxonomy_pairs(annot_table_wei_bio)

  # Filter Edges for Consistency Calculation ----

  edges_filtered <- .filter_edges_for_consistency(edges_table)

  logger::log_debug(
    "Found {nrow(edges_filtered)} valid edges for consistency calculation"
  )

  # Early exit if no valid edges - add required columns with defaults
  if (nrow(edges_filtered) == 0L) {
    logger::log_warn(
      "No features with >=2 neighbors found, skipping consistency calculation"
    )
    logger::log_debug(
      "Adding default feature_pred_tax columns (all features marked as 'empty')"
    )
    return(.add_default_prediction_columns(annot_table_wei_bio))
  }

  # Join Edges with Annotations ----

  # Join edges with annotations
  df3 <- tidytable::right_join(
    x = edges_filtered,
    y = annotations_distinct |>
      tidytable::distinct(
        feature_id,
        candidate_structure_tax_cla_01kin,
        candidate_structure_tax_npc_01pat,
        candidate_structure_tax_cla_02sup,
        candidate_structure_tax_npc_02sup,
        candidate_structure_tax_cla_03cla,
        candidate_structure_tax_npc_03cla,
        candidate_structure_tax_cla_04dirpar,
        score_weighted_bio
      ),
    by = stats::setNames(object = "feature_id", nm = "feature_target")
  ) |>
    tidytable::filter(!is.na(feature_source))

  # logger::log_trace("Calculating consistency scores across network edges")

  # Calculate Consistency Scores ----

  # Calculate consistency scores for each taxonomic level
  consistency_results <- .calculate_consistency_all_levels(
    df3,
    minimal_consistency
  )

  # logger::log_trace("Splitting already computed predictions")
  if ("feature_pred_tax_cla_02sup_val" %in% colnames(annotations_distinct)) {
    df1 <- annotations_distinct |>
      tidytable::filter(!is.na(feature_pred_tax_cla_02sup_val))

    df1b <- df1 |>
      tidytable::select(-tidyselect::contains(match = "feature_pred_tax"))

    df2 <- annotations_distinct |>
      tidytable::select(
        -tidyselect::contains(match = "feature_pred_tax")
      ) |>
      tidytable::anti_join(y = df1) |>
      tidytable::bind_rows(df1b)
  } else {
    df1 <- tidytable::tidytable()
    df1b <- tidytable::tidytable()
    df2 <- annotations_distinct
  }
  rm(annotations_distinct)

  # logger::log_trace("Joining all except -1 together")
  supp_tables <- consistency_results

  annot_table_wei_bio_preclean <- purrr::reduce(
    .x = supp_tables,
    .init = df2,
    .f = tidytable::left_join,
    by = stats::setNames(object = "feature_source", nm = "feature_id")
  ) |>
    tidytable::select(feature_id, tidyselect::everything()) |>
    tidytable::mutate(tidytable::across(
      .cols = tidyselect::where(fn = is.logical),
      .fns = as.character
    ))
  # Conditional coalescing
  coalesce_if_present <- function(df, col, default) {
    if (col %in% colnames(df)) {
      df[[col]] <- tidytable::coalesce(df[[col]], default)
    } else {
      df[[col]] <- default
    }
    df
  }
  annot_table_wei_bio_preclean <- annot_table_wei_bio_preclean |>
    coalesce_if_present("feature_pred_tax_cla_01kin_val", "empty") |>
    coalesce_if_present("consistency_structure_cla_kin", 1) |>
    coalesce_if_present("feature_pred_tax_cla_01kin_score", 0) |>
    coalesce_if_present("feature_pred_tax_npc_01pat_val", "empty") |>
    coalesce_if_present("consistency_structure_npc_pat", 1) |>
    coalesce_if_present("feature_pred_tax_npc_01pat_score", 0) |>
    coalesce_if_present("feature_pred_tax_cla_02sup_val", "empty") |>
    coalesce_if_present("consistency_structure_cla_sup", 1) |>
    coalesce_if_present("feature_pred_tax_cla_02sup_score", 0) |>
    coalesce_if_present("feature_pred_tax_npc_02sup_val", "empty") |>
    coalesce_if_present("consistency_structure_npc_sup", 1) |>
    coalesce_if_present("feature_pred_tax_npc_02sup_score", 0) |>
    coalesce_if_present("feature_pred_tax_cla_03cla_val", "empty") |>
    coalesce_if_present("consistency_structure_cla_cla", 1) |>
    coalesce_if_present("feature_pred_tax_cla_03cla_score", 0) |>
    coalesce_if_present("feature_pred_tax_npc_03cla_val", "empty") |>
    coalesce_if_present("consistency_structure_npc_cla", 1) |>
    coalesce_if_present("feature_pred_tax_npc_03cla_score", 0) |>
    coalesce_if_present("feature_pred_tax_cla_04dirpar_val", "empty") |>
    coalesce_if_present("consistency_structure_cla_par", 1) |>
    coalesce_if_present("feature_pred_tax_cla_04dirpar_score", 0)
  rm(df2, supp_tables)

  # logger::log_trace("Adding already computed predictions back")
  if (nrow(df1b) == 0L) {
    return(annot_table_wei_bio_preclean)
  }
  annot_table_wei_bio_clean <- annot_table_wei_bio_preclean |>
    tidytable::anti_join(y = df1b, by = "feature_id") |>
    tidytable::bind_rows(df1)

  rm(
    annot_table_wei_bio_preclean,
    df1,
    df1b
  )

  return(annot_table_wei_bio_clean)
}

# Internal Helper Functions ----

#' Add default prediction columns to annotation table
#'
#' @description
#' Adds default (empty) taxonomy prediction columns when no consistency
#' calculation is possible (e.g., no edges or no neighbors).
#'
#' @param df Data frame to add columns to
#'
#' @return Data frame with default prediction columns added
#'
#' @keywords internal
.add_default_prediction_columns <- function(df) {
  df |>
    tidytable::mutate(
      feature_pred_tax_cla_01kin_val = "empty",
      consistency_structure_cla_kin = 1,
      feature_pred_tax_cla_01kin_score = 0,
      feature_pred_tax_npc_01pat_val = "empty",
      consistency_structure_npc_pat = 1,
      feature_pred_tax_npc_01pat_score = 0,
      feature_pred_tax_cla_02sup_val = "empty",
      consistency_structure_cla_sup = 1,
      feature_pred_tax_cla_02sup_score = 0,
      feature_pred_tax_npc_02sup_val = "empty",
      consistency_structure_npc_sup = 1,
      feature_pred_tax_npc_02sup_score = 0,
      feature_pred_tax_cla_03cla_val = "empty",
      consistency_structure_cla_cla = 1,
      feature_pred_tax_cla_03cla_score = 0,
      feature_pred_tax_npc_03cla_val = "empty",
      consistency_structure_npc_cla = 1,
      feature_pred_tax_npc_03cla_score = 0,
      feature_pred_tax_cla_04dirpar_val = "empty",
      consistency_structure_cla_par = 1,
      feature_pred_tax_cla_04dirpar_score = 0
    )
}

#' Extract distinct structure-taxonomy pairs from annotations
#'
#' @description
#' Extracts unique structure-taxonomy combinations from annotation table,
#' removing duplicates while preserving all relevant columns.
#'
#' @param annot_table Data frame containing annotations
#'
#' @return Data frame with distinct structure-taxonomy pairs
#'
#' @keywords internal
.extract_distinct_taxonomy_pairs <- function(annot_table) {
  annot_table |>
    tidytable::distinct(
      feature_id,
      candidate_structure_inchikey_connectivity_layer,
      candidate_structure_tax_cla_01kin,
      candidate_structure_tax_npc_01pat,
      candidate_structure_tax_cla_02sup,
      candidate_structure_tax_npc_02sup,
      candidate_structure_tax_cla_03cla,
      candidate_structure_tax_npc_03cla,
      candidate_structure_tax_cla_04dirpar,
      score_weighted_bio,
      .keep_all = TRUE
    )
}

#' Filter edges for consistency calculation
#'
#' @description
#' Filters edges to keep only features with at least 2 neighbors and
#' removes self-loops. Optionally filters by spectral entropy and labels.
#'
#' @param edges_table Data frame containing network edges
#' @param min_neighbors Integer minimum number of neighbors required (default: 2)
#' @param min_entropy Numeric minimum spectral entropy (default: 0)
#'
#' @return Filtered edges data frame
#'
#' @keywords internal
#'
#' @note
#' TODO: Implement more sophisticated filtering criteria:
#' - Configurable entropy thresholds for spectral quality
#' - Similarity score minimums
#' - Consider edge filtering during edge creation step
.filter_edges_for_consistency <- function(
  edges_table,
  min_neighbors = 2L,
  min_entropy = 0
) {
  # Remove self-loops
  edges_filtered <- edges_table |>
    tidytable::filter(feature_source != feature_target)

  # Filter by entropy or label presence
  if ("label" %in% colnames(edges_filtered)) {
    edges_filtered <- edges_filtered |>
      tidytable::filter(
        feature_spectrum_entropy > min_entropy | !is.na(label)
      )
  } else {
    edges_filtered <- edges_filtered |>
      tidytable::filter(feature_spectrum_entropy > min_entropy)
  }

  # Keep only features with at least min_neighbors neighbors
  edges_filtered |>
    tidytable::distinct(feature_source, feature_target) |>
    tidytable::group_by(feature_source) |>
    tidytable::add_count() |>
    tidytable::ungroup() |>
    tidytable::filter(n >= min_neighbors) |>
    tidytable::select(-n)
}

#' Calculate consistency per taxonomic level
#'
#' @description
#' Calculates chemical consistency scores for a single taxonomic level by
#' comparing classifications across network neighbors.
#'
#' @param df Data frame with joined edges and annotations
#' @param candidates Character name of the candidate taxonomy column
#' @param consistency_name Character name for output consistency column
#' @param feature_score_name Character name for output score column
#' @param feature_val_name Character name for output value column
#' @param minimal_consistency Numeric minimum consistency threshold (0-1)
#'
#' @return Data frame with consistency scores for this level
#'
#' @keywords internal
.calculate_consistency_per_level <- function(
  df,
  candidates,
  consistency_name,
  feature_score_name,
  feature_val_name,
  minimal_consistency
) {
  df |>
    tidytable::distinct(
      feature_source,
      feature_target,
      !!as.name(candidates),
      score_weighted_bio
    ) |>
    tidytable::mutate(
      count = tidytable::n_distinct(feature_target),
      .by = c(feature_source, !!as.name(candidates))
    ) |>
    tidytable::mutate(
      !!as.name(consistency_name) := count /
        tidytable::n_distinct(feature_target),
      .by = c(feature_source)
    ) |>
    tidytable::distinct(
      feature_source,
      !!as.name(candidates),
      .keep_all = TRUE
    ) |>
    tidytable::mutate(
      !!as.name(feature_score_name) := !!as.name(consistency_name) *
        score_weighted_bio,
      .by = c(feature_source, !!as.name(candidates))
    ) |>
    tidytable::arrange(-!!as.name(feature_score_name)) |>
    tidytable::distinct(feature_source, .keep_all = TRUE) |>
    tidytable::select(
      feature_source,
      !!as.name(feature_val_name) := !!as.name(candidates),
      !!as.name(consistency_name),
      !!as.name(feature_score_name)
    ) |>
    tidytable::mutate(
      !!as.name(feature_val_name) := tidytable::if_else(
        condition = !!as.name(feature_score_name) >= minimal_consistency,
        true = !!as.name(feature_val_name),
        false = "notConsistent"
      )
    )
}

#' Calculate consistency scores for all taxonomic levels
#'
#' @description
#' Wrapper function that calculates consistency scores across all taxonomic
#' levels (ClassyFire and NPC hierarchies).
#'
#' @param df3 Data frame with joined edges and annotations
#' @param minimal_consistency Numeric minimum consistency threshold (0-1)
#'
#' @return List of data frames with consistency scores for each level
#'
#' @keywords internal
.calculate_consistency_all_levels <- function(df3, minimal_consistency) {
  # Define taxonomic levels to process
  levels <- list(
    list(
      col = "candidate_structure_tax_cla_01kin",
      consistency = "consistency_structure_cla_kin",
      score = "feature_pred_tax_cla_01kin_score",
      val = "feature_pred_tax_cla_01kin_val"
    ),
    list(
      col = "candidate_structure_tax_npc_01pat",
      consistency = "consistency_structure_npc_pat",
      score = "feature_pred_tax_npc_01pat_score",
      val = "feature_pred_tax_npc_01pat_val"
    ),
    list(
      col = "candidate_structure_tax_cla_02sup",
      consistency = "consistency_structure_cla_sup",
      score = "feature_pred_tax_cla_02sup_score",
      val = "feature_pred_tax_cla_02sup_val"
    ),
    list(
      col = "candidate_structure_tax_npc_02sup",
      consistency = "consistency_structure_npc_sup",
      score = "feature_pred_tax_npc_02sup_score",
      val = "feature_pred_tax_npc_02sup_val"
    ),
    list(
      col = "candidate_structure_tax_cla_03cla",
      consistency = "consistency_structure_cla_cla",
      score = "feature_pred_tax_cla_03cla_score",
      val = "feature_pred_tax_cla_03cla_val"
    ),
    list(
      col = "candidate_structure_tax_npc_03cla",
      consistency = "consistency_structure_npc_cla",
      score = "feature_pred_tax_npc_03cla_score",
      val = "feature_pred_tax_npc_03cla_val"
    ),
    list(
      col = "candidate_structure_tax_cla_04dirpar",
      consistency = "consistency_structure_cla_par",
      score = "feature_pred_tax_cla_04dirpar_score",
      val = "feature_pred_tax_cla_04dirpar_val"
    )
  )

  # Calculate consistency for each level
  lapply(levels, function(level) {
    .calculate_consistency_per_level(
      df3,
      candidates = level$col,
      consistency_name = level$consistency,
      feature_score_name = level$score,
      feature_val_name = level$val,
      minimal_consistency = minimal_consistency
    )
  })
}
