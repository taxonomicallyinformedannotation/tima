library(testthat)
sample_candidates_per_group <- sample_candidates_per_group
remove_compound_names <- remove_compound_names
coerce_score_columns <- coerce_score_columns
enforce_cluster_entity_consensus <- enforce_cluster_entity_consensus
prepare_ranked_candidates <- prepare_ranked_candidates

# ── sample_candidates_per_group ───────────────────────────────────────────────

test_that("sample_candidates_per_group returns empty list for 0-row input", {
  df <- tidytable::tidytable(
    feature_id = character(),
    candidate_adduct = character(),
    rank_final = integer()
  )
  result <- sample_candidates_per_group(df, max_per_score = 3L)
  expect_equal(nrow(result$df), 0L)
  expect_equal(result$n_sampled_features, 0L)
})

test_that("sample_candidates_per_group passes through untied rows unchanged", {
  df <- tidytable::tidytable(
    feature_id = c("F1", "F2"),
    candidate_adduct = c("[M+H]+", "[M+H]+"),
    rank_final = c(1L, 1L),
    score_weighted_chemo = c(0.9, 0.8),
    candidate_score_pseudo_initial = c(0.5, 0.4)
  )
  result <- sample_candidates_per_group(df, max_per_score = 5L)
  expect_equal(nrow(result$df), 2L)
  expect_equal(result$n_sampled_features, 0L)
})

test_that("sample_candidates_per_group keeps all tied rows below cap", {
  df <- tidytable::tidytable(
    feature_id = rep("F1", 3L),
    candidate_adduct = rep("[M+H]+", 3L),
    rank_final = rep(1L, 3L),
    score_weighted_chemo = c(0.9, 0.9, 0.9),
    candidate_score_pseudo_initial = c(0.5, 0.5, 0.5)
  )
  result <- sample_candidates_per_group(df, max_per_score = 5L)
  # All 3 tied rows below cap of 5 -> keep all
  expect_equal(nrow(result$df), 3L)
})

test_that("sample_candidates_per_group samples down when above cap", {
  df <- tidytable::tidytable(
    feature_id = rep("F1", 6L),
    candidate_adduct = rep("[M+H]+", 6L),
    rank_final = rep(1L, 6L),
    score_weighted_chemo = rep(0.9, 6L),
    candidate_score_pseudo_initial = rep(0.5, 6L)
  )
  result <- sample_candidates_per_group(df, max_per_score = 3L, seed = 42L)
  # 6 ties > cap(3) -> sample to 3
  expect_equal(nrow(result$df), 3L)
  expect_equal(result$n_sampled_features, 1L)
})

test_that("sample_candidates_per_group stores annotation notes for sampled groups", {
  df <- tidytable::tidytable(
    feature_id = rep("F1", 5L),
    candidate_adduct = rep("[M+H]+", 5L),
    rank_final = rep(1L, 5L),
    score_weighted_chemo = rep(0.8, 5L),
    candidate_score_pseudo_initial = rep(0.5, 5L)
  )
  result <- sample_candidates_per_group(df, max_per_score = 2L, seed = 1L)
  expect_true(nrow(result$annotation_notes) >= 1L)
})

test_that("sample_candidates_per_group prefers the strongest tied rows", {
  df <- tidytable::tidytable(
    feature_id = rep("F1", 4L),
    candidate_adduct = rep("[M+H]+", 4L),
    rank_final = rep(1L, 4L),
    score_weighted_chemo = c(0.1, 0.9, 0.2, 0.3),
    candidate_score_pseudo_initial = c(0.1, 0.2, 0.3, 0.4)
  )
  result <- sample_candidates_per_group(df, max_per_score = 2L, seed = 42L)

  expect_equal(nrow(result$df), 2L)
  expect_true(all(result$df$score_weighted_chemo %in% c(0.9, 0.3)))
})

test_that("sample_candidates_per_group preserves consensus-promoted rows", {
  df <- tidytable::tidytable(
    feature_id = rep("F1", 4L),
    candidate_adduct = rep("[M+H]+", 4L),
    rank_final = rep(1L, 4L),
    score_weighted_chemo = c(0.1, 0.9, 0.8, 0.7),
    candidate_score_pseudo_initial = c(0.1, 0.2, 0.3, 0.4),
    cluster_consensus_promoted_from_anchor = c(FALSE, TRUE, FALSE, FALSE)
  )

  result <- sample_candidates_per_group(df, max_per_score = 2L, seed = 42L)

  expect_equal(nrow(result$df), 2L)
  expect_true(any(result$df$cluster_consensus_promoted_from_anchor %in% TRUE))
  expect_true(
    all(
      result$df$cluster_consensus_promoted_from_anchor[1:2] %in% c(TRUE, FALSE)
    )
  )
})

# ── remove_compound_names ──────────────────────────────────────────────────────

test_that("remove_compound_names strips name column when compounds_names=FALSE", {
  tbl <- tidytable::tidytable(
    feature_id = c("F1", "F2"),
    candidate_structure_name = c("Compound A", "Compound B"),
    score = c(0.8, 0.7)
  )
  results_list <- list(full = tbl, filtered = tbl, mini = tbl)

  result <- remove_compound_names(results_list, compounds_names = FALSE)
  expect_false("candidate_structure_name" %in% names(result$full))
  expect_false("candidate_structure_name" %in% names(result$filtered))
  expect_false("candidate_structure_name" %in% names(result$mini))
})

test_that("remove_compound_names keeps name column when compounds_names=TRUE", {
  tbl <- tidytable::tidytable(
    feature_id = c("F1"),
    candidate_structure_name = c("Compound A"),
    score = c(0.8)
  )
  results_list <- list(full = tbl, filtered = tbl, mini = tbl)

  result <- remove_compound_names(results_list, compounds_names = TRUE)
  expect_true("candidate_structure_name" %in% names(result$full))
})

# ── coerce_score_columns ──────────────────────────────────────────────────────

test_that("coerce_score_columns converts character score columns to numeric", {
  df <- tidytable::tidytable(
    feature_id = c("F1", "F2"),
    score_biological = c("0.8", "0.5"),
    score_chemical = c("0.7", "0.3"),
    candidate_score_similarity = c("0.9", NA_character_)
  )
  result <- coerce_score_columns(df)
  expect_true(is.numeric(result$score_biological))
  expect_true(is.numeric(result$score_chemical))
  expect_true(is.numeric(result$candidate_score_similarity))
  expect_equal(result$score_biological, c(0.8, 0.5))
})

test_that("coerce_score_columns is a no-op when no score columns present", {
  df <- tidytable::tidytable(
    feature_id = c("F1"),
    other_col = c("abc")
  )
  result <- coerce_score_columns(df)
  expect_equal(names(result), names(df))
})

test_that("coerce_score_columns handles all expected score column names", {
  cols <- c(
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
  df_vals <- setNames(as.list(as.character(seq_along(cols) * 0.1)), cols)
  df <- tidytable::as_tidytable(as.data.frame(
    df_vals,
    stringsAsFactors = FALSE
  ))
  result <- coerce_score_columns(df)
  for (col in cols) {
    expect_true(is.numeric(result[[col]]), info = col)
  }
})

test_that("enforce_cluster_entity_consensus annotates promoted children with anchor metadata", {
  # Feature A detected as [M+H]+ (mz ≈ 201), Feature B as [M+Na]+ (mz ≈ 223).
  # Both point to the same neutral molecule (exact_mass = 200.0).
  # candidate_structure_exact_mass is the formula-derived monoisotopic mass —
  # adduct-independent and exact, so the exact-mass grouping inside the
  # function is exact.
  df_ranked <- tidytable::tidytable(
    feature_id = c("A", "A", "B", "B"),
    mz = c(201.007, 201.007, 222.989, 222.989),
    candidate_adduct = c("[M+H]+", "[M+H]+", "[M+Na]+", "[M+Na]+"),
    candidate_structure_inchikey_connectivity_layer = c(
      "IK1",
      "IK2",
      "IK2",
      "IK1"
    ),
    candidate_structure_exact_mass = rep(200.0, 4L),
    score_weighted_chemo = c(0.95, 0.60, 0.99, 0.40),
    rank_final = c(1L, 2L, 1L, 2L)
  )
  components <- tidytable::tidytable(
    feature_id = c("A", "B"),
    component_id = c("C1", "C1")
  )

  out <- enforce_cluster_entity_consensus(df_ranked, components)

  top <- out |>
    tidytable::filter(rank_final == 1L)

  expect_equal(nrow(top), 2L)
  expect_true("cluster_consensus_group_id" %in% names(out))
  expect_true("cluster_consensus_anchor_feature_id" %in% names(out))
  expect_true("cluster_consensus_promoted_from_anchor" %in% names(out))

  promoted <- out |>
    tidytable::filter(cluster_consensus_promoted_from_anchor %in% TRUE)
  expect_true(nrow(promoted) >= 1L)

  promoted_anchor <- promoted |>
    tidytable::distinct(cluster_consensus_anchor_feature_id)
  expect_equal(nrow(promoted_anchor), 1L)

  anchor_feature <- out |>
    tidytable::filter(feature_id == "B", rank_final == 1L)
  expect_true(all(anchor_feature$cluster_consensus_applied %in% TRUE))
  expect_false(any(
    anchor_feature$cluster_consensus_promoted_from_anchor %in% TRUE
  ))
})

# ── end-to-end integration: consensus promotion → percentile pass ──────────────

# Scenario
# --------
# Feature A: detected as [M+H]+  at mz ≈ 201.007
# Feature B: detected as [M+Na]+ at mz ≈ 222.989
# Both encode the SAME underlying compound (candidate_structure_exact_mass = 200.0).
# The exact mass comes from the molecular formula — it is adduct-independent and
# the same double value for all rows here, so the (component_id, exact_mass)
# grouping inside enforce_cluster_entity_consensus is exact with no tolerance
# needed.
#
# Scores:
#   Feature A rank-1 → IK1 (score 0.95), rank-2 → IK2 (score 0.60)
#   Feature B rank-1 → IK2 (score 0.99), rank-2 → IK1 (score 0.40)
#
# enforce_cluster_entity_consensus picks Feature B's IK2 (best rank-1 score)
# as anchor and promotes IK2 to rank-1 in Feature A (carrying score 0.60).
#
# apply_percentile_filter at 0.9 would normally drop IK2 from Feature A
# because 0.60 < 0.9 × 0.95 = 0.855.  The .keep_by_consensus guard keeps it.

test_that("prepare_ranked_candidates: consensus-promoted row survives percentile filter", {
  annot <- tidytable::tidytable(
    feature_id = c("A", "A", "B", "B"),
    mz = c(201.007, 201.007, 222.989, 222.989),
    candidate_adduct = c("[M+H]+", "[M+H]+", "[M+Na]+", "[M+Na]+"),
    candidate_structure_inchikey_connectivity_layer = c(
      "IK1",
      "IK2",
      "IK2",
      "IK1"
    ),
    # Formula-derived monoisotopic mass — exact and adduct-independent.
    # All rows share the same M so the exact-mass grouping is exact.
    candidate_structure_exact_mass = rep(200.0, 4L),
    score_biological = c(0.5, 0.5, 0.5, 0.5),
    score_chemical = c(0.5, 0.5, 0.5, 0.5),
    score_weighted_chemo = c(0.95, 0.60, 0.99, 0.40),
    candidate_score_pseudo_initial = c(0.80, 0.50, 0.90, 0.35),
    candidate_score_similarity = rep(NA_real_, 4L),
    candidate_score_sirius_csi = rep(NA_real_, 4L)
  )

  components <- tidytable::tidytable(
    feature_id = c("A", "B"),
    component_id = c("C1", "C1")
  )

  result <- prepare_ranked_candidates(
    annot_table_wei_chemo = annot,
    minimal_ms1_bio = 0.0,
    minimal_ms1_chemo = 0.0,
    minimal_ms1_condition = "OR",
    best_percentile = 0.9,
    max_per_score = 10L,
    components_table = components
  )

  df_ranked <- result$df_ranked
  df_percentile <- result$df_percentile

  # 1. After consensus enforcement, Feature A's rank-1 candidate must be IK2
  #    (promoted from anchor Feature B).
  rank1_A <- df_ranked |>
    tidytable::filter(feature_id == "A", rank_final == 1L)
  expect_equal(
    rank1_A$candidate_structure_inchikey_connectivity_layer,
    "IK2"
  )

  # 2. The promoted row carries the consensus metadata flag.
  expect_true(
    any(rank1_A$cluster_consensus_promoted_from_anchor %in% TRUE)
  )

  # 3. After percentile filtering, Feature A's promoted IK2 (score 0.60)
  #    must still be present even though 0.60 < 0.9 × max-score(A).
  #    The .keep_by_consensus guard in apply_percentile_filter is responsible.
  pct_A <- df_percentile |>
    tidytable::filter(feature_id == "A")
  expect_true(
    "IK2" %in% pct_A$candidate_structure_inchikey_connectivity_layer,
    info = paste(
      "IK2 should survive percentile filter via consensus guard;",
      "found IKs:",
      paste(
        pct_A$candidate_structure_inchikey_connectivity_layer,
        collapse = ", "
      )
    )
  )

  # 4. Feature B is unaffected: its rank-1 candidate remains IK2.
  rank1_B <- df_ranked |>
    tidytable::filter(feature_id == "B", rank_final == 1L)
  expect_equal(
    rank1_B$candidate_structure_inchikey_connectivity_layer,
    "IK2"
  )

  # 5. Feature B's IK2 (score 0.99) also passes the percentile filter
  #    on its own merits (max score for B is 0.99, threshold = 0.891).
  pct_B <- df_percentile |>
    tidytable::filter(feature_id == "B")
  expect_true(
    "IK2" %in% pct_B$candidate_structure_inchikey_connectivity_layer
  )
})

test_that("prepare_ranked_candidates: no consensus table → percentile filter drops low scorer", {
  # Same realistic setup (different adducts/mz, same exact_mass = 200.0)
  # but WITHOUT components_table.  IK2 for Feature A (score 0.60) is NOT
  # promoted and is NOT guarded → percentile at 0.9 must drop it.
  annot <- tidytable::tidytable(
    feature_id = c("A", "A", "B", "B"),
    mz = c(201.007, 201.007, 222.989, 222.989),
    candidate_adduct = c("[M+H]+", "[M+H]+", "[M+Na]+", "[M+Na]+"),
    candidate_structure_inchikey_connectivity_layer = c(
      "IK1",
      "IK2",
      "IK2",
      "IK1"
    ),
    candidate_structure_exact_mass = rep(200.0, 4L),
    score_biological = c(0.5, 0.5, 0.5, 0.5),
    score_chemical = c(0.5, 0.5, 0.5, 0.5),
    score_weighted_chemo = c(0.95, 0.60, 0.99, 0.40),
    candidate_score_pseudo_initial = c(0.80, 0.50, 0.90, 0.35),
    candidate_score_similarity = rep(NA_real_, 4L),
    candidate_score_sirius_csi = rep(NA_real_, 4L)
  )

  result <- prepare_ranked_candidates(
    annot_table_wei_chemo = annot,
    minimal_ms1_bio = 0.0,
    minimal_ms1_chemo = 0.0,
    minimal_ms1_condition = "OR",
    best_percentile = 0.9,
    max_per_score = 10L,
    components_table = NULL
  )

  pct_A <- result$df_percentile |>
    tidytable::filter(feature_id == "A")

  # Without consensus guard IK2 (0.60) is below the percentile threshold.
  expect_false(
    "IK2" %in% pct_A$candidate_structure_inchikey_connectivity_layer,
    info = "IK2 should be filtered out without consensus promotion"
  )
  # IK1 (0.95 == max) must survive.
  expect_true(
    "IK1" %in% pct_A$candidate_structure_inchikey_connectivity_layer
  )
})

test_that("enforce_cluster_entity_consensus does not promote across mixed exact masses", {
  # Same component, but the best-supported rank-1 rows resolve to different
  # exact masses. That must prevent any cross-feature consensus promotion.
  df_ranked <- tidytable::tidytable(
    feature_id = c("A", "A", "B", "B"),
    candidate_adduct = c("[M+H]+", "[M+H]+", "[M+Na]+", "[M+Na]+"),
    candidate_structure_inchikey_connectivity_layer = c(
      "IK_A",
      "IK_B",
      "IK_B",
      "IK_A"
    ),
    candidate_structure_exact_mass = c(200.0, 200.0, 201.0, 201.0),
    score_weighted_chemo = c(0.95, 0.60, 0.99, 0.40),
    rank_final = c(1L, 2L, 1L, 2L)
  )

  components <- tidytable::tidytable(
    feature_id = c("A", "B"),
    component_id = c("C1", "C1")
  )

  out <- enforce_cluster_entity_consensus(df_ranked, components)

  expect_false(any(out$cluster_consensus_promoted_from_anchor %in% TRUE))

  rank1 <- out |>
    tidytable::filter(rank_final == 1L) |>
    tidytable::arrange(feature_id)
  expect_equal(
    rank1$candidate_structure_inchikey_connectivity_layer,
    c("IK_A", "IK_B")
  )
})

test_that("enforce_cluster_entity_consensus falls back to mz/adduct when exact mass is absent", {
  # Legacy input: no candidate_structure_exact_mass column. The function must
  # still infer the shared neutral mass from mz + adduct and promote the
  # correct anchor across features.
  M_target <- 200.0
  mz_A <- calculate_mz_from_mass(M_target, "[M+H]+")
  mz_B <- calculate_mz_from_mass(M_target, "[M+Na]+")

  df_ranked <- tidytable::tidytable(
    feature_id = c("A", "A", "B", "B"),
    mz = c(mz_A, mz_A, mz_B, mz_B),
    candidate_adduct = c("[M+H]+", "[M+H]+", "[M+Na]+", "[M+Na]+"),
    candidate_structure_inchikey_connectivity_layer = c(
      "IK_A",
      "IK_B",
      "IK_B",
      "IK_A"
    ),
    score_weighted_chemo = c(0.95, 0.60, 0.99, 0.40),
    rank_final = c(1L, 2L, 1L, 2L)
  )

  components <- tidytable::tidytable(
    feature_id = c("A", "B"),
    component_id = c("C1", "C1")
  )

  out <- enforce_cluster_entity_consensus(df_ranked, components)

  rank1 <- out |>
    tidytable::filter(rank_final == 1L) |>
    tidytable::arrange(feature_id)

  expect_equal(
    rank1$candidate_structure_inchikey_connectivity_layer,
    c("IK_B", "IK_B")
  )
  expect_true(any(out$cluster_consensus_promoted_from_anchor %in% TRUE))
})

test_that("enforce_cluster_entity_consensus uses row-level mz/adduct fallback when exact mass is partially missing", {
  M_target <- 200.0
  mz_A <- calculate_mz_from_mass(M_target, "[M+H]+")
  mz_B <- calculate_mz_from_mass(M_target, "[M+Na]+")

  # exact_mass exists but is missing for B rows. Consensus must still work
  # via per-row mz/adduct fallback.
  df_ranked <- tidytable::tidytable(
    feature_id = c("A", "A", "B", "B"),
    mz = c(mz_A, mz_A, mz_B, mz_B),
    candidate_adduct = c("[M+H]+", "[M+H]+", "[M+Na]+", "[M+Na]+"),
    candidate_structure_inchikey_connectivity_layer = c(
      "IK_A",
      "IK_B",
      "IK_B",
      "IK_A"
    ),
    candidate_structure_exact_mass = c(200.0, 200.0, NA_real_, NA_real_),
    score_weighted_chemo = c(0.95, 0.60, 0.99, 0.40),
    rank_final = c(1L, 2L, 1L, 2L)
  )

  components <- tidytable::tidytable(
    feature_id = c("A", "B"),
    component_id = c("C1", "C1")
  )

  out <- enforce_cluster_entity_consensus(df_ranked, components)

  rank1 <- out |>
    tidytable::filter(rank_final == 1L) |>
    tidytable::arrange(feature_id)

  expect_equal(
    rank1$candidate_structure_inchikey_connectivity_layer,
    c("IK_B", "IK_B")
  )
  expect_true(any(out$cluster_consensus_promoted_from_anchor %in% TRUE))
})

test_that("enforce_cluster_entity_consensus does not override rank-1 rows that already have MS2 evidence", {
  # Two features (A, C) both have MS2 evidence but disagree on rank-1 IK.
  # C has the higher score and becomes the anchor. A must keep its own rank-1
  # because it already has direct MS2 evidence; only B (no MS2) is promotable.
  df_ranked <- tidytable::tidytable(
    feature_id = c("A", "A", "B", "B", "C", "C"),
    candidate_structure_inchikey_connectivity_layer = c(
      "IK1",
      "IK2",
      "IK3",
      "IK2",
      "IK2",
      "IK1"
    ),
    candidate_structure_exact_mass = rep(200.0, 6L),
    score_weighted_chemo = c(0.80, 0.60, 0.70, 0.50, 0.95, 0.40),
    rank_final = c(1L, 2L, 1L, 2L, 1L, 2L),
    candidate_score_similarity = c(
      0.92,
      NA_real_,
      NA_real_,
      NA_real_,
      0.98,
      NA_real_
    )
  )

  components <- tidytable::tidytable(
    feature_id = c("A", "B", "C"),
    component_id = c("C1", "C1", "C1")
  )

  out <- enforce_cluster_entity_consensus(df_ranked, components)

  rank1 <- out |>
    tidytable::filter(rank_final == 1L) |>
    tidytable::arrange(feature_id)

  expect_equal(
    rank1$candidate_structure_inchikey_connectivity_layer,
    c("IK1", "IK2", "IK2")
  )

  promoted <- out |>
    tidytable::filter(cluster_consensus_promoted_from_anchor %in% TRUE)
  expect_true(all(promoted$feature_id != "A"))
  expect_true(any(promoted$feature_id == "B"))
})
