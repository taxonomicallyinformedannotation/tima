#' @title Clean bio
#'
#' @description This function cleans and filters biologically weighted
#'     annotation results by calculating chemical consistency scores across
#'     network neighbors. Only features with at least 2 neighbors are evaluated.
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
#' @examples NULL
clean_bio <- function(
  annot_table_wei_bio = get("annot_table_wei_bio", envir = parent.frame()),
  edges_table = get("edges_table", envir = parent.frame()),
  minimal_consistency = get("minimal_consistency", envir = parent.frame())
) {
  # ============================================================================
  # Input Validation
  # ============================================================================

  # Validate data frames
  if (
    !is.data.frame(annot_table_wei_bio) && !inherits(annot_table_wei_bio, "tbl")
  ) {
    stop("annot_table_wei_bio must be a data frame or tibble")
  }

  if (!is.data.frame(edges_table) && !inherits(edges_table, "tbl")) {
    stop("edges_table must be a data frame or tibble")
  }

  # Validate consistency threshold
  if (
    !is.numeric(minimal_consistency) ||
      minimal_consistency < 0 ||
      minimal_consistency > 1
  ) {
    stop(
      "minimal_consistency must be between 0 and 1, got: ",
      minimal_consistency
    )
  }

  # Early exit for empty inputs
  if (nrow(annot_table_wei_bio) == 0L) {
    logger::log_warn("Empty annotation table provided")
    return(annot_table_wei_bio)
  }

  add_default_pred_cols <- function(df) {
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

  if (nrow(edges_table) == 0L) {
    logger::log_warn("Empty edges table provided, cannot calculate consistency")
    return(add_default_pred_cols(annot_table_wei_bio))
  }

  # ============================================================================
  # Extract Distinct Structure-Taxonomy Pairs
  # ============================================================================

  # logger::log_trace("Extracting distinct structure-taxonomy pairs")

  # Extract unique structure-taxonomy combinations
  annotations_distinct <- annot_table_wei_bio |>
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

  # ============================================================================
  # Filter Edges for Consistency Calculation
  # ============================================================================

  # logger::log_trace(
  #   "Calculating chemical consistency for features with at least 2 neighbors"
  # )

  # Filter edges: keep features with at least 2 neighbors
  # TODO: Implement more sophisticated filtering criteria:
  # - Entropy thresholds for spectral quality
  # - Similarity score minimums
  # - Consider edge filtering during edge creation step
  edges_filtered <- edges_table |>
    tidytable::filter(feature_source != feature_target)
  if ("label" %in% colnames(edges_filtered)) {
    edges_filtered <- edges_filtered |>
      tidytable::filter(feature_spectrum_entropy > 0 | !is.na(label))
  } else {
    edges_filtered <- edges_filtered |>
      tidytable::filter(feature_spectrum_entropy > 0)
  }
  edges_filtered <- edges_filtered |>
    tidytable::distinct(feature_source, feature_target) |>
    tidytable::group_by(feature_source) |>
    tidytable::add_count() |>
    tidytable::ungroup() |>
    tidytable::filter(n >= 2L) |>
    tidytable::select(-n)

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
    return(add_default_pred_cols(annot_table_wei_bio))
  }

  # ============================================================================
  # Join Edges with Annotations
  # ============================================================================

  # Join edges with annotations
  df3 <- tidytable::right_join(
    edges_filtered,
    annotations_distinct |>
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
    by = stats::setNames("feature_id", "feature_target")
  ) |>
    tidytable::filter(!is.na(feature_source))

  # logger::log_trace("Calculating consistency scores across network edges")

  # ============================================================================
  # Calculate Consistency Scores
  # ============================================================================

  # Function to calculate consistency per taxonomic level
  clean_per_level_bio <- function(
    df,
    candidates,
    consistency_name,
    feature_score_name,
    feature_val_name
  ) {
    freq <- df |>
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

  # logger::log_trace("... at the (classyfire) kingdom level")
  freq_cla_kin <- df3 |>
    clean_per_level_bio(
      candidates = "candidate_structure_tax_cla_01kin",
      consistency_name = "consistency_structure_cla_kin",
      feature_score_name = "feature_pred_tax_cla_01kin_score",
      feature_val_name = "feature_pred_tax_cla_01kin_val"
    )
  # logger::log_trace("... at the (NPC) pathway level")
  freq_npc_pat <- df3 |>
    clean_per_level_bio(
      candidates = "candidate_structure_tax_npc_01pat",
      consistency_name = "consistency_structure_npc_pat",
      feature_score_name = "feature_pred_tax_npc_01pat_score",
      feature_val_name = "feature_pred_tax_npc_01pat_val"
    )
  # logger::log_trace("... at the (classyfire) superclass level")
  freq_cla_sup <- df3 |>
    clean_per_level_bio(
      candidates = "candidate_structure_tax_cla_02sup",
      consistency_name = "consistency_structure_cla_sup",
      feature_score_name = "feature_pred_tax_cla_02sup_score",
      feature_val_name = "feature_pred_tax_cla_02sup_val"
    )
  # logger::log_trace("... at the (NPC) superclass level")
  freq_npc_sup <- df3 |>
    clean_per_level_bio(
      candidates = "candidate_structure_tax_npc_02sup",
      consistency_name = "consistency_structure_npc_sup",
      feature_score_name = "feature_pred_tax_npc_02sup_score",
      feature_val_name = "feature_pred_tax_npc_02sup_val"
    )
  # logger::log_trace("... at the (classyfire) class level")
  freq_cla_cla <- df3 |>
    clean_per_level_bio(
      candidates = "candidate_structure_tax_cla_03cla",
      consistency_name = "consistency_structure_cla_cla",
      feature_score_name = "feature_pred_tax_cla_03cla_score",
      feature_val_name = "feature_pred_tax_cla_03cla_val"
    )
  # logger::log_trace("... at the (NPC) class level")
  freq_npc_cla <- df3 |>
    clean_per_level_bio(
      candidates = "candidate_structure_tax_npc_03cla",
      consistency_name = "consistency_structure_npc_cla",
      feature_score_name = "feature_pred_tax_npc_03cla_score",
      feature_val_name = "feature_pred_tax_npc_03cla_val"
    )
  # logger::log_trace("... at the (classyfire) parent level")
  freq_cla_par <- df3 |>
    clean_per_level_bio(
      candidates = "candidate_structure_tax_cla_04dirpar",
      consistency_name = "consistency_structure_cla_par",
      feature_score_name = "feature_pred_tax_cla_04dirpar_score",
      feature_val_name = "feature_pred_tax_cla_04dirpar_val"
    )
  rm(df3)

  # logger::log_trace("Splitting already computed predictions")
  if ("feature_pred_tax_cla_02sup_val" %in% colnames(annotations_distinct)) {
    df1 <- annotations_distinct |>
      tidytable::filter(!is.na(feature_pred_tax_cla_02sup_val))

    df1b <- df1 |>
      tidytable::select(-tidyselect::contains("feature_pred_tax"))

    df2 <- annotations_distinct |>
      tidytable::select(-tidyselect::contains("feature_pred_tax")) |>
      tidytable::anti_join(df1) |>
      tidytable::bind_rows(df1b)
  } else {
    df1 <- tidytable::tidytable()
    df1b <- tidytable::tidytable()
    df2 <- annotations_distinct
  }
  rm(annotations_distinct)

  # logger::log_trace("Joining all except -1 together")
  supp_tables <- list(
    freq_cla_kin,
    freq_npc_pat,
    freq_cla_sup,
    freq_npc_sup,
    freq_cla_cla,
    freq_npc_cla,
    freq_cla_par
  )
  rm(
    freq_cla_kin,
    freq_npc_pat,
    freq_cla_sup,
    freq_npc_sup,
    freq_cla_cla,
    freq_npc_cla,
    freq_cla_par
  )

  annot_table_wei_bio_preclean <- purrr::reduce(
    .x = supp_tables,
    .init = df2,
    .f = function(x, y) {
      tidytable::left_join(
        x,
        y,
        by = stats::setNames("feature_source", "feature_id")
      )
    }
  ) |>
    tidytable::select(feature_id, tidyselect::everything()) |>
    tidytable::mutate(tidytable::across(
      .cols = tidyselect::where(is.logical),
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
    tidytable::anti_join(df1b, by = "feature_id") |>
    tidytable::bind_rows(df1)

  rm(
    annot_table_wei_bio_preclean,
    df1,
    df1b
  )

  return(annot_table_wei_bio_clean)
}
