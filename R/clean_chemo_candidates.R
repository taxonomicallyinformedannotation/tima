#' Resolve tied candidates via cross-feature neutral-mass anchors
#'
#' @include clean_chemo_preprocessing.R
#' @include constants.R
#' @include logs_utils.R
#'
#' @description Splits candidates per (feature_id, candidate_adduct,
#'     rank_final) into untied rows (always kept) and tied rows. A tied
#'     group is collapsed to just the candidates whose InChIKey matches
#'     a rank-1 InChIKey of ANOTHER feature at the same neutral mass
#'     (within tolerance). Tied groups with NO cross-feature anchor are
#'     NOT dropped: they are kept as-is if `.n_per_group <= max_per_score`,
#'     otherwise sampled down to `max_per_score` (RT-error candidates
#'     prioritized when the column is available).
#'
#' @param df Data frame with ranked candidates
#' @param max_per_score Integer, maximum candidates to keep per group
#' @param seed Integer, random seed for reproducibility
#'
#' @return List with three elements:
#'   \describe{
#'     \item{df}{Filtered data frame with sampled / collapsed candidates}
#'     \item{n_sampled_features}{Number of features whose tied groups
#'         required sampling (n > max_per_score)}
#'     \item{annotation_notes}{Per-group annotation notes (anchor / sampling)}
#'   }
#' @keywords internal

sample_candidates_per_group <- function(df, max_per_score, seed = 42L) {
  if (nrow(df) == 0L) {
    return(list(df = df, n_sampled_features = 0L))
  }

  # Add group sizes per (feature_id, candidate_adduct, rank_final)
  df <- df |>
    tidytable::mutate(
      .n_per_group = tidytable::n(),
      .by = c(feature_id, candidate_adduct, rank_final)
    )

  has_ik_col <- "candidate_structure_inchikey_connectivity_layer" %in% names(df)
  has_M_cols <- "mz" %in% names(df) && "candidate_adduct" %in% names(df)
  has_rt_feature_col <- "rt" %in% names(df)

  # Derive per-row neutral mass M from (feature mz, candidate_adduct). Rows
  # from DIFFERENT features whose M values match within a small tolerance
  # are interpretations of the same molecular entity under different
  # adducts (e.g. `[M+NH4]+` on feature X and `[2M+Na]+` on feature Y).
  if (has_M_cols) {
    df <- df |>
      tidytable::mutate(
        .candidate_M = compute_candidate_M(mz, candidate_adduct)
      )
  }

  # Cross-feature anchor lookup: (M, RT, IK, feature_id) tuples sourced
  # from rank=1 rows that are THEMSELVES UNAMBIGUOUS (`.n_per_group == 1`)
  # — a tied sibling is not real evidence and cannot anchor anything.
  # Matching requires:
  #   (a) anchor_fid != row_fid (another feature),
  #   (b) same IK,
  #   (c) same neutral mass M within NEUTRAL_MASS_MATCH_TOLERANCE_DA,
  #   (d) same retention time within DEFAULT_HE_MAX_RT_ERROR_MIN.
  # Without the RT gate, unrelated co-mass features at different RTs can
  # cross-anchor each of a tied group's IKs individually, leaving every
  # tied row flagged and nothing collapsed. Sorted by M for fast lookup.
  anchor_M_vec <- numeric(0)
  anchor_IK_vec <- character(0)
  anchor_fid_vec <- character(0)
  anchor_rt_vec <- numeric(0)

  if (has_ik_col && has_M_cols) {
    anchor_tbl_base <- df |>
      tidytable::filter(
        rank_final == 1L,
        .n_per_group == 1L,
        !is.na(candidate_structure_inchikey_connectivity_layer),
        !is.na(.candidate_M)
      ) |>
      tidytable::arrange(.candidate_M)

    anchor_tbl <- if (has_rt_feature_col) {
      anchor_tbl_base |>
        tidytable::select(
          .candidate_M,
          .anchor_IK = candidate_structure_inchikey_connectivity_layer,
          .anchor_fid = feature_id,
          .anchor_rt = rt
        )
    } else {
      anchor_tbl_base |>
        tidytable::select(
          .candidate_M,
          .anchor_IK = candidate_structure_inchikey_connectivity_layer,
          .anchor_fid = feature_id
        )
    }

    if (nrow(anchor_tbl) > 0L) {
      anchor_M_vec <- anchor_tbl$.candidate_M
      anchor_IK_vec <- anchor_tbl$.anchor_IK
      anchor_fid_vec <- as.character(anchor_tbl$.anchor_fid)
      anchor_rt_vec <- if (has_rt_feature_col) {
        as.numeric(anchor_tbl$.anchor_rt)
      } else {
        rep(NA_real_, nrow(anchor_tbl))
      }
    }
  }

  # Count features that will need sampling (> max_per_score tied rows).
  n_sampled_features <- df |>
    tidytable::filter(.n_per_group > max_per_score) |>
    tidytable::distinct(feature_id) |>
    nrow()

  # Split: untied rows pass through. Tied rows go through anchor + (optional)
  # sampling. Anchor collapse runs at EVERY tied size — even groups below
  # max_per_score are collapsed when a cross-feature anchor exists.
  df_untied <- df |>
    tidytable::filter(.n_per_group == 1L)

  df_tied <- df |>
    tidytable::filter(.n_per_group >= 2L)

  has_rt_col <- "candidate_structure_error_rt" %in% names(df)

  df_anchor_kept <- tidytable::tidytable()
  df_needs_sampling <- df_tied

  if (
    nrow(df_tied) > 0L &&
      has_ik_col &&
      length(anchor_M_vec) > 0L
  ) {
    row_M <- df_tied$.candidate_M
    row_IK <- df_tied$candidate_structure_inchikey_connectivity_layer
    row_fid <- as.character(df_tied$feature_id)
    row_rt <- if (has_rt_feature_col) {
      as.numeric(df_tied$rt)
    } else {
      rep(NA_real_, nrow(df_tied))
    }
    tol <- NEUTRAL_MASS_MATCH_TOLERANCE_DA
    rt_tol <- DEFAULT_HE_MAX_RT_ERROR_MIN

    lo <- findInterval(row_M - tol, anchor_M_vec)
    hi <- findInterval(row_M + tol, anchor_M_vec)

    anchor_match <- logical(length(row_M))
    for (i in seq_along(row_M)) {
      if (is.na(row_M[[i]]) || is.na(row_IK[[i]])) {
        next
      }
      from <- lo[[i]] + 1L
      to <- hi[[i]]
      if (from > to) {
        next
      }
      rng <- from:to
      hit <- anchor_IK_vec[rng] == row_IK[[i]] &
        anchor_fid_vec[rng] != row_fid[[i]]
      # RT co-elution gate: the anchor feature must elute at (within
      # DEFAULT_HE_MAX_RT_ERROR_MIN of) the tied row's feature RT.
      # Without this, any unrelated feature sharing M acts as an anchor.
      if (has_rt_feature_col && !is.na(row_rt[[i]])) {
        rt_diff <- abs(anchor_rt_vec[rng] - row_rt[[i]])
        rt_ok <- is.na(rt_diff) | rt_diff <= rt_tol
        hit <- hit & rt_ok
      }
      if (any(hit, na.rm = TRUE)) {
        anchor_match[[i]] <- TRUE
      }
    }

    df_tied <- df_tied |>
      tidytable::mutate(.anchor_match = anchor_match)

    anchor_group_keys <- df_tied |>
      tidytable::filter(.anchor_match) |>
      tidytable::distinct(feature_id, candidate_adduct, rank_final)

    if (nrow(anchor_group_keys) > 0L) {
      # Tied groups with at least one anchor match -> keep ONLY the
      # anchor-matching rows (no max_per_score cap: cross-feature evidence
      # is stronger than the arbitrary cap).
      df_anchor_kept <- df_tied |>
        tidytable::inner_join(
          y = anchor_group_keys,
          by = c("feature_id", "candidate_adduct", "rank_final")
        ) |>
        tidytable::filter(.anchor_match) |>
        tidytable::arrange(
          feature_id,
          candidate_adduct,
          rank_final,
          tidytable::desc(score_weighted_chemo),
          tidytable::desc(candidate_score_pseudo_initial)
        ) |>
        tidytable::mutate(
          annotation_note = paste0(
            "Kept candidate matching best-supported InChIKey from sibling ",
            "feature with same neutral mass (different adduct)"
          )
        )

      # Tied groups without any anchor match fall through to the sampling
      # step below (kept as-is if small, sampled if oversized).
      df_needs_sampling <- df_tied |>
        tidytable::anti_join(
          y = anchor_group_keys,
          by = c("feature_id", "candidate_adduct", "rank_final")
        )
    }
  }

  # Tied groups without an anchor: keep as-is when below the cap, sample
  # down when above. We NEVER drop them — losing every tied candidate
  # when no sibling evidence exists would erase valid annotations.
  df_keep_remaining <- df_needs_sampling |>
    tidytable::filter(.n_per_group <= max_per_score)

  df_needs_sampling <- df_needs_sampling |>
    tidytable::filter(.n_per_group > max_per_score)

  if (nrow(df_needs_sampling) > 0L && has_rt_col) {
    df_needs_sampling <- df_needs_sampling |>
      tidytable::mutate(.rt_priority = !is.na(candidate_structure_error_rt))

    set.seed(seed)
    df_sampled <- df_needs_sampling |>
      tidytable::arrange(tidytable::desc(.rt_priority)) |>
      tidytable::slice_head(
        n = max_per_score,
        by = c(feature_id, candidate_adduct, rank_final)
      ) |>
      tidytable::mutate(
        annotation_note = paste0(
          "Sampled ",
          max_per_score,
          " of ",
          .n_per_group,
          " candidates with same score"
        )
      ) |>
      tidytable::select(-.rt_priority)
  } else if (nrow(df_needs_sampling) > 0L) {
    set.seed(seed)
    df_sampled <- df_needs_sampling |>
      tidytable::slice_sample(
        n = max_per_score,
        by = c(feature_id, candidate_adduct, rank_final)
      ) |>
      tidytable::mutate(
        annotation_note = paste0(
          "Sampled ",
          max_per_score,
          " of ",
          .n_per_group,
          " candidates with same score"
        )
      )
  } else {
    df_sampled <- tidytable::tidytable()
  }

  df_result <- tidytable::bind_rows(
    df_untied,
    df_keep_remaining,
    df_anchor_kept,
    df_sampled
  ) |>
    tidytable::select(
      -tidyselect::any_of(c(
        ".n_per_group",
        ".anchor_match",
        ".candidate_M"
      ))
    )

  # Extract annotation_note as a separate lookup table
  # annotation_note is per (feature_id, adduct, rank_final) group
  annotation_notes_lookup <- tidytable::tidytable()

  if ("annotation_note" %in% names(df_result)) {
    annotation_notes_lookup <- df_result |>
      tidytable::filter(!is.na(annotation_note)) |>
      tidytable::distinct(
        feature_id,
        candidate_adduct,
        rank_final,
        annotation_note
      )

    # Remove annotation_note from main data
    df_result <- df_result |>
      tidytable::select(-tidyselect::any_of("annotation_note"))
  }

  list(
    df = df_result,
    n_sampled_features = n_sampled_features,
    annotation_notes = annotation_notes_lookup
  )
}

#' Remove Compound Names from Results
#'
#' @description Internal helper to optionally remove compound names from
#'     all result tiers.
#'
#' @param results_list Named list with full, filtered, mini data frames
#' @param compounds_names Logical, if FALSE remove names
#'
#' @return Modified results list
#' @keywords internal
remove_compound_names <- function(results_list, compounds_names) {
  if (!compounds_names) {
    results_list$mini <- results_list$mini |>
      tidytable::select(
        -tidyselect::any_of(x = "candidate_structure_name")
      )
    results_list$filtered <- results_list$filtered |>
      tidytable::select(
        -tidyselect::any_of(x = "candidate_structure_name")
      )
    results_list$full <- results_list$full |>
      tidytable::select(
        -tidyselect::any_of(x = "candidate_structure_name")
      )
  }
  results_list
}

#' Coerce score columns to numeric
#'
#' @description Internal helper that normalizes score-like columns loaded as
#'     character values (e.g., from TSV/CSV inputs) before numeric operations.
#'
#' @param annot_table_wei_chemo Data frame with annotation scores
#'
#' @return Data frame with existing score columns coerced to numeric
#' @keywords internal
coerce_score_columns <- function(annot_table_wei_chemo) {
  score_columns <- c(
    "score_biological",
    "score_chemical",
    "score_weighted_chemo",
    "candidate_score_pseudo_initial",
    "candidate_score_similarity",
    "candidate_score_similarity_forward",
    "candidate_score_similarity_reverse",
    "candidate_score_sirius_csi",
    "candidate_score_sirius_confidence"
  )

  cols_to_convert <- intersect(score_columns, names(annot_table_wei_chemo))
  if (length(cols_to_convert) == 0L) {
    return(annot_table_wei_chemo)
  }

  annot_table_wei_chemo |>
    tidytable::mutate(
      tidytable::across(
        .cols = tidyselect::all_of(cols_to_convert),
        .fns = as.numeric
      )
    )
}

#' Prepare ranked candidate tables for downstream tiers
#'
#' @param annot_table_wei_chemo Data frame with normalized score columns
#' @param minimal_ms1_bio Numeric threshold for biological score
#' @param minimal_ms1_chemo Numeric threshold for chemical score
#' @param minimal_ms1_condition Character filter mode ("OR" or "AND")
#' @param best_percentile Numeric percentile threshold
#' @param max_per_score Integer max candidates per score group
#' @param components_table Optional data frame with feature_id and component_id
#'
#' @return Named list with ranked/percentile tables and candidate counts
#' @keywords internal
prepare_ranked_candidates <- function(
  annot_table_wei_chemo,
  minimal_ms1_bio,
  minimal_ms1_chemo,
  minimal_ms1_condition,
  best_percentile,
  max_per_score,
  components_table = NULL
) {
  df_base <- filter_ms1_annotations(
    annot_table_wei_chemo = annot_table_wei_chemo,
    minimal_ms1_bio = minimal_ms1_bio,
    minimal_ms1_chemo = minimal_ms1_chemo,
    minimal_ms1_condition = minimal_ms1_condition
  )

  df_ranked <- rank_and_deduplicate(df_base)

  # Enforce cluster-level entity consensus if components_table is provided
  ## TODO Not fully correct for now
  # if (!is.null(components_table) && nrow(components_table) > 0L) {
  #   df_ranked <- enforce_cluster_entity_consensus(df_ranked, components_table)
  # }

  sampling_result <- sample_candidates_per_group(
    df = df_ranked,
    max_per_score = max_per_score,
    seed = 42L
  )

  df_ranked <- sampling_result$df
  df_percentile <- apply_percentile_filter(
    df = df_ranked,
    best_percentile = best_percentile
  )

  list(
    df_ranked = df_ranked,
    df_percentile = df_percentile,
    results_candidates = count_candidates(df_ranked, df_percentile),
    n_sampled_features = sampling_result$n_sampled_features,
    annotation_notes_lookup = sampling_result$annotation_notes
  )
}

#' Build mini-tier taxonomy table
#'
#' @param df_percentile Percentile-filtered candidate table
#' @param score_chemical_cla_kingdom ClassyFire kingdom weight
#' @param score_chemical_cla_superclass ClassyFire superclass weight
#' @param score_chemical_cla_class ClassyFire class weight
#' @param score_chemical_cla_parent ClassyFire direct parent weight
#' @param score_chemical_npc_pathway NPClassifier pathway weight
#' @param score_chemical_npc_superclass NPClassifier superclass weight
#' @param score_chemical_npc_class NPClassifier class weight
#'
#' @return Mini-tier taxonomy table keyed by feature_id
#' @keywords internal
build_mini_taxonomy_table <- function(
  df_percentile,
  score_chemical_cla_kingdom,
  score_chemical_cla_superclass,
  score_chemical_cla_class,
  score_chemical_cla_parent,
  score_chemical_npc_pathway,
  score_chemical_npc_superclass,
  score_chemical_npc_class
) {
  if (!"score_weighted_chemo" %in% names(df_percentile)) {
    df_percentile$score_weighted_chemo <- NA_real_
  }

  normalize_tax_label <- function(x) {
    vals <- trimws(as.character(x))
    vals[
      vals %in% c("", "notClassified", "empty", "N/A", "null")
    ] <- NA_character_
    vals
  }

  # Prepare all candidates with their structure taxonomy scored by
  # score_weighted_chemo so they compete on equal footing with predicted labels.
  df_all <- df_percentile |>
    tidytable::mutate(
      score_weighted_chemo = as.numeric(score_weighted_chemo),
      has_inchikey = !is.na(candidate_structure_inchikey_connectivity_layer),
      tidytable::across(
        .cols = tidyselect::starts_with(match = "candidate_structure_tax_"),
        .fns = normalize_tax_label
      ),
      .label_cla_str = tidytable::coalesce(
        candidate_structure_tax_cla_04dirpar,
        candidate_structure_tax_cla_03cla,
        candidate_structure_tax_cla_02sup,
        candidate_structure_tax_cla_01kin
      ),
      .label_npc_str = tidytable::coalesce(
        candidate_structure_tax_npc_03cla,
        candidate_structure_tax_npc_02sup,
        candidate_structure_tax_npc_01pat
      )
    )

  # has_inchikey flag at feature level
  df_has_ik <- df_all |>
    tidytable::summarize(
      has_inchikey = any(has_inchikey, na.rm = TRUE),
      .by = feature_id
    )

  # Best structure label per feature (highest score_weighted_chemo wins)
  df_str_cla <- df_all |>
    tidytable::filter(!is.na(.label_cla_str)) |>
    tidytable::arrange(tidytable::desc(score_weighted_chemo)) |>
    tidytable::distinct(feature_id, .keep_all = TRUE) |>
    tidytable::select(
      feature_id,
      label_classyfire_str = .label_cla_str,
      score_cla_str = score_weighted_chemo
    )

  df_str_npc <- df_all |>
    tidytable::filter(!is.na(.label_npc_str)) |>
    tidytable::arrange(tidytable::desc(score_weighted_chemo)) |>
    tidytable::distinct(feature_id, .keep_all = TRUE) |>
    tidytable::select(
      feature_id,
      label_npclassifier_str = .label_npc_str,
      score_npc_str = score_weighted_chemo
    )

  # Best predicted label per feature (highest prediction score wins)
  df_pred_tax <- df_percentile |>
    tidytable::select(
      tidyselect::contains(match = c("feature_id", "feature_pred"))
    ) |>
    tidytable::mutate(
      tidytable::across(
        tidyselect::contains(match = "score"),
        as.numeric
      )
    )

  if (nrow(df_pred_tax) > 0L) {
    weights <- list(
      w_cla_kin = score_chemical_cla_kingdom,
      w_cla_sup = score_chemical_cla_superclass,
      w_cla_cla = score_chemical_cla_class,
      w_cla_par = score_chemical_cla_parent,
      w_npc_pat = score_chemical_npc_pathway,
      w_npc_sup = score_chemical_npc_superclass,
      w_npc_cla = score_chemical_npc_class
    )

    df_pred_cla <- compute_classyfire_taxonomy(df_pred_tax, weights) |>
      tidytable::arrange(tidytable::desc(score_classyfire)) |>
      tidytable::distinct(feature_id, .keep_all = TRUE)

    df_pred_npc <- compute_npclassifier_taxonomy(df_pred_tax, weights) |>
      tidytable::arrange(tidytable::desc(score_npclassifier)) |>
      tidytable::distinct(feature_id, .keep_all = TRUE)
  } else {
    df_pred_cla <- tidytable::tidytable(
      feature_id = character(0),
      label_classyfire_predicted = character(0),
      score_classyfire = numeric(0)
    )
    df_pred_npc <- tidytable::tidytable(
      feature_id = character(0),
      label_npclassifier_predicted = character(0),
      score_npclassifier = numeric(0)
    )
  }

  # pmax across both pools: structure label wins when its score_weighted_chemo
  # >= prediction score, otherwise the predicted label wins.
  # Combine all joins into single operation to avoid multiple table scans
  joined <- purrr::reduce(
    .x = list(df_str_cla, df_str_npc, df_pred_cla, df_pred_npc),
    .init = df_has_ik,
    .f = function(acc, tbl) tidytable::left_join(x = acc, y = tbl)
  )

  joined |>
    tidytable::mutate(
      label_classyfire = tidytable::case_when(
        !is.na(score_cla_str) &
          !is.na(score_classyfire) &
          score_cla_str >= score_classyfire ~ label_classyfire_str,
        !is.na(score_classyfire) ~ label_classyfire_predicted,
        !is.na(label_classyfire_str) ~ label_classyfire_str,
        TRUE ~ NA_character_
      ),
      # No uncertainty score when label comes from a confirmed InChIKey structure
      score_classyfire = tidytable::case_when(
        !is.na(score_cla_str) &
          !is.na(score_classyfire) &
          score_cla_str >= score_classyfire ~ NA_real_,
        !is.na(score_classyfire) ~ score_classyfire,
        TRUE ~ NA_real_
      ),
      label_npclassifier = tidytable::case_when(
        !is.na(score_npc_str) &
          !is.na(score_npclassifier) &
          score_npc_str >= score_npclassifier ~ label_npclassifier_str,
        !is.na(score_npclassifier) ~ label_npclassifier_predicted,
        !is.na(label_npclassifier_str) ~ label_npclassifier_str,
        TRUE ~ NA_character_
      ),
      score_npclassifier = tidytable::case_when(
        !is.na(score_npc_str) &
          !is.na(score_npclassifier) &
          score_npc_str >= score_npclassifier ~ NA_real_,
        !is.na(score_npclassifier) ~ score_npclassifier,
        TRUE ~ NA_real_
      )
    ) |>
    tidytable::select(
      feature_id,
      has_inchikey,
      label_classyfire,
      label_npclassifier,
      score_classyfire,
      score_npclassifier
    ) |>
    tidytable::distinct()
}

#' Build mini-tier result table
#'
#' @param features_table Feature metadata table
#' @param df_classes_mini Mini taxonomy table keyed by feature_id
#' @param results_filtered Filtered-tier summarized results
#' @param df_filtered Filtered candidate table prior to summarization
#' @param xrefs_table Optional cross-reference table
#'
#' @return Mini-tier output table
#' @keywords internal
build_mini_results_table <- function(
  features_table,
  df_classes_mini,
  results_filtered,
  df_filtered,
  xrefs_table = NULL
) {
  results_mini <- purrr::reduce(
    .x = list(
      df_classes_mini,
      results_filtered,
      df_filtered |>
        tidytable::select(
          tidyselect::any_of(c(
            "feature_id",
            "candidate_structure_name",
            "candidate_adduct",
            "candidate_structure_smiles_no_stereo",
            "candidate_structure_inchikey_connectivity_layer",
            "candidate_library",
            "candidate_structure_error_mz",
            "candidate_structure_error_rt",
            "candidate_structure_organism_occurrence_closest",
            "candidate_structure_tag",
            "score_weighted_chemo"
          ))
        )
    ),
    .init = features_table,
    .f = function(acc, tbl) tidytable::left_join(x = acc, y = tbl)
  ) |>
    tidytable::rename(
      label_compound = candidate_structure_name,
      adduct = candidate_adduct,
      smiles_no_stereo = candidate_structure_smiles_no_stereo,
      inchikey_connectivity_layer = candidate_structure_inchikey_connectivity_layer,
      library = candidate_library,
      error_mz = candidate_structure_error_mz,
      error_rt = candidate_structure_error_rt,
      organism_closest = candidate_structure_organism_occurrence_closest,
      score = score_weighted_chemo
    ) |>
    tidytable::mutate(
      candidates_evaluated = tidytable::if_else(
        is.na(inchikey_connectivity_layer),
        NA_integer_,
        candidates_evaluated
      ),
      candidates_best = tidytable::if_else(
        is.na(inchikey_connectivity_layer),
        NA_integer_,
        candidates_best
      ),
      .pred_denom = as.numeric(!is.na(score_classyfire)) +
        as.numeric(!is.na(score_npclassifier)),
      .pred_sum = tidytable::coalesce(score_classyfire, 0) +
        tidytable::coalesce(score_npclassifier, 0),
      .pred_avg = tidytable::if_else(
        .pred_denom > 0,
        .pred_sum / .pred_denom,
        NA_real_
      ),
      score = tidytable::if_else(
        is.na(inchikey_connectivity_layer),
        .pred_avg,
        score
      )
    ) |>
    tidytable::select(
      tidyselect::any_of(
        x = c(
          "feature_id",
          "rt",
          "mz",
          "label_classyfire",
          "label_npclassifier",
          "label_compound",
          "adduct",
          "smiles_no_stereo",
          "inchikey_connectivity_layer",
          "library",
          "error_mz",
          "error_rt",
          "organism_closest",
          "tag" = "candidate_structure_tag",
          "score",
          "candidates_evaluated",
          "candidates_best",
          "note" = "annotation_note"
        )
      )
    ) |>
    tidytable::distinct() |>
    tidytable::mutate(
      label_classyfire = tidytable::if_else(
        condition = !is.na(score),
        true = label_classyfire,
        false = NA_character_
      ),
      label_npclassifier = tidytable::if_else(
        condition = !is.na(score),
        true = label_npclassifier,
        false = NA_character_
      )
    )

  if (!is.null(xrefs_table) && nrow(xrefs_table) > 0L) {
    results_mini <- results_mini |>
      tidytable::rename(
        candidate_structure_inchikey_connectivity_layer = inchikey_connectivity_layer
      ) |>
      .add_xrefs_to_df(xrefs = xrefs_table) |>
      tidytable::rename(
        inchikey_connectivity_layer = candidate_structure_inchikey_connectivity_layer
      )

    mini_id_cols <- grep(
      pattern = "^candidate_structure_id_",
      x = names(results_mini),
      value = TRUE
    )

    if (length(mini_id_cols) > 0L) {
      results_mini <- results_mini |>
        tidytable::rename_with(
          ~ sub("^candidate_structure_", "", .x),
          .cols = tidyselect::all_of(mini_id_cols)
        )
    }
  }

  results_mini
}

#' Enforce cluster-level entity consensus
#'
#' @description Ensures that all features within the same MS1 component/cluster
#'     with the same neutral mass M must agree on their best InChIKey candidate.
#'     This is a fundamental design constraint: if multiple features form an
#'     MS1-based cluster via adduct/loss edges, they should all resolve to the
#'     same chemical entity.
#'
#'     For each (component_id, neutral_M) group:
#'     - Identify the best-ranked feature by score_weighted_chemo
#'     - Extract its best InChIKey from rank_final == 1
#'     - All other features in the group reorder their rank=1 candidates
#'       such that the anchoring InChIKey becomes rank=1 (if present)
#'
#' @param df_ranked Data frame with ranked candidates
#' @param components_table Data frame with feature_id and component_id columns
#'
#' @return df_ranked with cluster-consensus-enforced rankings
#' @keywords internal
enforce_cluster_entity_consensus <- function(df_ranked, components_table) {
  if (nrow(df_ranked) == 0L) {
    return(df_ranked)
  }

  # Check if required columns exist
  has_component_col <- "component_id" %in% names(components_table)
  has_rank_col <- "rank_final" %in% names(df_ranked)
  has_ik_col <- "candidate_structure_inchikey_connectivity_layer" %in%
    names(df_ranked)
  has_score_col <- "score_weighted_chemo" %in% names(df_ranked)
  has_exact_mass_col <- "candidate_structure_exact_mass" %in% names(df_ranked)
  # mz/adduct fallback retained for legacy data that lacks exact_mass
  has_mz_col <- "mz" %in% names(df_ranked)
  has_adduct_col <- "candidate_adduct" %in% names(df_ranked)
  has_M_source <- has_exact_mass_col || (has_mz_col && has_adduct_col)

  if (
    !all(c(
      has_component_col,
      has_rank_col,
      has_ik_col,
      has_score_col,
      has_M_source
    ))
  ) {
    log_debug(
      "Skipping cluster entity consensus (missing required columns: %s)",
      paste(
        c(
          "component_id",
          "rank_final",
          "InChIKey",
          "score",
          "candidate_structure_exact_mass (or mz + adduct)"
        )[
          !c(
            has_component_col,
            has_rank_col,
            has_ik_col,
            has_score_col,
            has_M_source
          )
        ],
        collapse = ", "
      )
    )
    return(df_ranked)
  }

  # Add component_id and neutral mass to ranked candidates
  df_with_comp <- df_ranked |>
    tidytable::left_join(
      y = components_table |> tidytable::distinct(feature_id, component_id),
      by = "feature_id"
    )

  has_similarity_col <- "candidate_score_similarity" %in% names(df_with_comp)
  has_sirius_csi_col <- "candidate_score_sirius_csi" %in% names(df_with_comp)
  has_sirius_conf_col <- "candidate_score_sirius_confidence" %in%
    names(df_with_comp)

  # Prefer anchors with direct MS2 evidence when available so consensus does
  # not systematically promote MS1-only rows to rank 1.
  df_with_comp <- df_with_comp |>
    tidytable::mutate(
      .has_ms2_evidence = (if (has_similarity_col) {
        !is.na(candidate_score_similarity)
      } else {
        FALSE
      }) |
        (if (has_sirius_csi_col) {
          !is.na(candidate_score_sirius_csi)
        } else {
          FALSE
        }) |
        (if (has_sirius_conf_col) {
          !is.na(candidate_score_sirius_confidence) &
            as.numeric(candidate_score_sirius_confidence) > 0
        } else {
          FALSE
        })
    )

  # Compute neutral mass for grouping.
  # Primary key: candidate_structure_exact_mass (formula-derived, adduct
  # independent). Row-level fallback: invert mz+adduct when exact mass is
  # missing, so partially populated exact-mass columns still propagate
  # consensus.
  has_mz_adduct_fallback <- has_mz_col && has_adduct_col
  if (!has_exact_mass_col) {
    log_warn(paste0(
      "candidate_structure_exact_mass not found; ",
      "falling back to mz/adduct inversion for neutral-mass grouping. ",
      "Results may be less reliable for cross-adduct consensus."
    ))
  }

  df_with_comp <- df_with_comp |>
    tidytable::mutate(
      .candidate_M_exact = if (has_exact_mass_col) {
        as.numeric(candidate_structure_exact_mass)
      } else {
        NA_real_
      },
      .candidate_M_mz = if (has_mz_adduct_fallback) {
        compute_candidate_M(mz, candidate_adduct)
      } else {
        NA_real_
      },
      .candidate_M = tidytable::coalesce(.candidate_M_exact, .candidate_M_mz),
      .candidate_M_key = round(.candidate_M, NEUTRAL_MASS_GROUP_DECIMALS)
    )

  if (has_exact_mass_col && has_mz_adduct_fallback) {
    n_fallback_rows <- df_with_comp |>
      tidytable::filter(is.na(.candidate_M_exact), !is.na(.candidate_M_mz)) |>
      nrow()
    if (n_fallback_rows > 0L) {
      log_info(
        "Cluster-consensus mass key used mz/adduct fallback for %d row(s) with missing exact mass",
        n_fallback_rows
      )
    }
  }

  # For each (component_id, .candidate_M_key) group with rank_final == 1
  # identify the anchor InChIKey from the best-scored feature
  anchor_lookup <- df_with_comp |>
    tidytable::filter(
      rank_final == 1L,
      !is.na(component_id),
      !is.na(.candidate_M_key),
      !is.na(candidate_structure_inchikey_connectivity_layer)
    ) |>
    tidytable::arrange(
      component_id,
      .candidate_M_key,
      tidytable::desc(.has_ms2_evidence),
      tidytable::desc(score_weighted_chemo)
    ) |>
    tidytable::distinct(component_id, .candidate_M_key, .keep_all = TRUE) |>
    tidytable::select(
      component_id,
      .candidate_M_key,
      .anchor_feature_id = feature_id,
      .anchor_ik = candidate_structure_inchikey_connectivity_layer
    ) |>
    tidytable::mutate(
      cluster_consensus_group_id = paste(
        component_id,
        sprintf("%.*f", NEUTRAL_MASS_GROUP_DECIMALS, .candidate_M_key),
        sep = "::"
      )
    )

  n_anchor_groups <- nrow(anchor_lookup)

  if (n_anchor_groups == 0L) {
    log_debug("No cluster entity consensus groups to enforce")
    return(df_ranked |> tidytable::select(-tidyselect::any_of(".candidate_M")))
  }

  # For each feature in a consensus group, if it has a different rank=1 candidate
  # than the anchor, and the anchor is present in its candidates, reorder to make
  # the anchor the rank=1
  df_to_reorder <- df_with_comp |>
    tidytable::left_join(
      y = anchor_lookup,
      by = c("component_id", ".candidate_M_key")
    ) |>
    tidytable::filter(
      !is.na(.anchor_ik),
      rank_final == 1L,
      !.has_ms2_evidence,
      candidate_structure_inchikey_connectivity_layer != .anchor_ik
    )

  if (nrow(df_to_reorder) > 0L) {
    # Check if anchor is present as a candidate (any rank) for these features
    reorder_fids <- df_to_reorder$feature_id

    df_anchored <- df_with_comp |>
      tidytable::left_join(
        y = anchor_lookup,
        by = c("component_id", ".candidate_M_key")
      ) |>
      tidytable::filter(feature_id %in% reorder_fids) |>
      # For each feature in reorder set, check if it HAS the anchor IK
      tidytable::mutate(
        .has_anchor_ik = candidate_structure_inchikey_connectivity_layer ==
          .anchor_ik,
        .is_anchor_feature = feature_id == .anchor_feature_id
      )

    # Identify which (feature_id, component_id, .candidate_M_key) groups actually
    # have the anchor InChIKey as a candidate
    groups_with_anchor <- df_anchored |>
      tidytable::filter(.has_anchor_ik) |>
      tidytable::distinct(feature_id, component_id, .candidate_M_key)

    if (nrow(groups_with_anchor) > 0L) {
      # Reorder: make the anchor IK == rank_final 1, shift others down
      has_annotation_note <- "annotation_note" %in% names(df_anchored)
      df_part_reorder <- df_anchored |>
        tidytable::inner_join(
          y = groups_with_anchor,
          by = c("feature_id", "component_id", ".candidate_M_key")
        ) |>
        tidytable::arrange(
          feature_id,
          .candidate_M_key,
          tidytable::desc(.has_anchor_ik),
          rank_final
        ) |>
        tidytable::mutate(
          rank_final = tidytable::row_number(),
          .by = c(feature_id, component_id, .candidate_M_key),
          cluster_consensus_group_id = tidytable::coalesce(
            cluster_consensus_group_id,
            paste(
              component_id,
              sprintf(
                "%.*f",
                NEUTRAL_MASS_GROUP_DECIMALS,
                .candidate_M_key
              ),
              sep = "::"
            )
          ),
          cluster_consensus_anchor_feature_id = .anchor_feature_id,
          cluster_consensus_anchor_inchikey = .anchor_ik,
          cluster_consensus_applied = TRUE,
          cluster_consensus_promoted_from_anchor = .has_anchor_ik &
            rank_final == 1L &
            !.is_anchor_feature,
          .existing_note = if (has_annotation_note) {
            annotation_note
          } else {
            NA_character_
          },
          annotation_note = tidytable::if_else(
            condition = cluster_consensus_promoted_from_anchor,
            true = "Promoted to rank 1: cluster entity consensus (same M across component)",
            false = .existing_note
          )
        ) |>
        tidytable::select(
          -tidyselect::all_of(c(
            ".candidate_M",
            ".candidate_M_exact",
            ".candidate_M_mz",
            ".candidate_M_key",
            ".has_anchor_ik",
            ".is_anchor_feature",
            ".anchor_ik",
            ".anchor_feature_id",
            ".existing_note",
            ".has_ms2_evidence"
          ))
        )

      # Combine reordered with unchanged rows
      df_unchanged <- df_with_comp |>
        tidytable::anti_join(
          y = groups_with_anchor,
          by = c("feature_id", "component_id", ".candidate_M_key")
        ) |>
        tidytable::mutate(
          cluster_consensus_applied = FALSE,
          cluster_consensus_promoted_from_anchor = FALSE
        ) |>
        tidytable::select(
          -tidyselect::all_of(c(
            ".candidate_M",
            ".candidate_M_exact",
            ".candidate_M_mz",
            ".candidate_M_key",
            "component_id",
            ".has_ms2_evidence"
          ))
        )

      df_result <- df_part_reorder |>
        tidytable::bind_rows(df_unchanged) |>
        tidytable::arrange(feature_id, rank_final)

      log_info(
        "Enforced cluster entity consensus for %d features (anchor InChIKey promoted to rank 1)",
        tidytable::n_distinct(groups_with_anchor$feature_id)
      )

      return(df_result)
    }
  }

  # No reordering needed; clean up temporary columns
  df_with_comp |>
    tidytable::mutate(
      cluster_consensus_applied = FALSE,
      cluster_consensus_promoted_from_anchor = FALSE
    ) |>
    tidytable::select(
      -tidyselect::any_of(c(
        ".candidate_M",
        ".candidate_M_exact",
        ".candidate_M_mz",
        ".candidate_M_key",
        "component_id",
        ".has_ms2_evidence"
      ))
    )
}
