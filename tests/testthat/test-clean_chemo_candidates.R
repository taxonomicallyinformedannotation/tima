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
    candidate_score_pseudo_initial = c(0.5, 0.4),
    candidate_structure_inchikey_connectivity_layer = c("IK1", "IK2")
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
    candidate_score_pseudo_initial = c(0.5, 0.5, 0.5),
    candidate_structure_inchikey_connectivity_layer = paste0("IK", 1:3)
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
    candidate_score_pseudo_initial = rep(0.5, 6L),
    candidate_structure_inchikey_connectivity_layer = paste0("IK", 1:6)
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
    candidate_score_pseudo_initial = rep(0.5, 5L),
    candidate_structure_inchikey_connectivity_layer = paste0("IK", 1:5)
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
    candidate_score_pseudo_initial = c(0.1, 0.2, 0.3, 0.4),
    candidate_structure_inchikey_connectivity_layer = paste0("IK", 1:4)
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
    cluster_consensus_promoted_from_anchor = c(FALSE, TRUE, FALSE, FALSE),
    candidate_structure_inchikey_connectivity_layer = paste0("IK", 1:4)
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
  df_vals <- stats::setNames(as.list(as.character(seq_along(cols) * 0.1)), cols)
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

test_that("sample_candidates_per_group handles missing candidate_score_pseudo_initial during anchor collapsing", {
  M_target <- 250.0
  mz_pos_h <- calculate_mz_from_mass(M_target, "[M+H]+")
  mz_pos_na <- calculate_mz_from_mass(M_target, "[M+Na]+")

  df <- tidytable::tidytable(
    feature_id = c("F_anchor", "F_tied", "F_tied", "F_tied"),
    candidate_adduct = c("[M+H]+", "[M+Na]+", "[M+Na]+", "[M+Na]+"),
    rank_final = c(1L, 1L, 1L, 1L),
    score_weighted_chemo = c(0.95, 0.80, 0.80, 0.80),
    candidate_structure_inchikey_connectivity_layer = c(
      "IK_ANCHOR",
      "IK_OTHER1",
      "IK_ANCHOR",
      "IK_OTHER2"
    ),
    mz = c(mz_pos_h, mz_pos_na, mz_pos_na, mz_pos_na),
    candidate_structure_error_mz = c(0.1, 0.2, 0.1, 0.3)
  )

  res <- sample_candidates_per_group(
    df = df,
    max_per_score = 2L,
    apply_anchor_collapsing = TRUE
  )
  tied_kept <- res$df[res$df$feature_id == "F_tied", ]
  expect_identical(nrow(tied_kept), 1L)
  expect_identical(
    tied_kept$candidate_structure_inchikey_connectivity_layer,
    "IK_ANCHOR"
  )
})

test_that("clean_chemo_candidates helpers cover edge cases, complete linkage, and cross-RT disambiguation", {
  # 1. rank_and_deduplicate edge cases: plain data.frame, missing optional columns,
  # missing candidate_adduct, missing InChIKey, and annotation_note preservation on ties
  df_plain <- data.frame(
    feature_id = c("F1", "F1"),
    stringsAsFactors = FALSE
  )
  rk_plain <- rank_and_deduplicate(df_plain)
  expect_identical(nrow(rk_plain), 2L)

  df_notes <- tidytable::tidytable(
    feature_id = c("F1", "F1", "F1"),
    candidate_adduct = c("[M+H]+", "[M+H]+", "[M+H]+"),
    candidate_structure_inchikey_connectivity_layer = c("IK1", "IK1", "IK2"),
    score_weighted_chemo = c(0.8, 0.9, 0.7),
    candidate_score_pseudo_initial = c(0.5, 0.5, 0.4),
    candidate_structure_error_rt = c(0.02, 0.01, 0.03),
    candidate_structure_error_mz = c(0.002, 0.001, 0.003),
    annotation_note = c(NA_character_, "Kept note", "   ")
  )
  rk_notes <- rank_and_deduplicate(df_notes)
  expect_identical(nrow(rk_notes), 2L)
  expect_identical(
    rk_notes$annotation_note[rk_notes$rank_final == 1L],
    "Kept note"
  )

  # 2. sample_candidates_per_group and .match_tied_rows_to_anchors edge cases:
  expect_identical(
    .match_tied_rows_to_anchors(
      tidytable::tidytable(),
      tidytable::tidytable(),
      0.01,
      0.05,
      FALSE
    ),
    logical(0)
  )
  # Out-of-mass-range anchor, non-matching IK, and duplicate-IK anchors (merge path: mass mismatch, RT mismatch, and match)
  df_t_direct <- tidytable::tidytable(
    feature_id = c("FT1", "FT2"),
    rt = c(2.0, 2.0),
    .candidate_anchor_mass = c(200.0, 200.0),
    candidate_structure_inchikey = c("IK_A", "IK_B")
  )
  expect_identical(
    .match_tied_rows_to_anchors(
      df_t_direct,
      tidytable::tidytable(
        feature_id = "FA",
        rt = 2.0,
        .candidate_anchor_mass = 500.0,
        candidate_structure_inchikey = "IK_A"
      ),
      0.01,
      0.05,
      TRUE
    ),
    c(FALSE, FALSE)
  )
  expect_identical(
    .match_tied_rows_to_anchors(
      df_t_direct,
      tidytable::tidytable(
        feature_id = "FA",
        rt = 2.0,
        .candidate_anchor_mass = 200.0,
        candidate_structure_inchikey = "IK_OTHER"
      ),
      0.01,
      0.05,
      TRUE
    ),
    c(FALSE, FALSE)
  )
  # Duplicate anchor IKs (triggers merge() path):
  # (a) same feature_id -> filtered out at mass/fid step
  expect_identical(
    .match_tied_rows_to_anchors(
      df_t_direct,
      tidytable::tidytable(
        feature_id = c("FT1", "FT1"),
        rt = c(2.0, 2.0),
        .candidate_anchor_mass = c(200.0, 200.0),
        candidate_structure_inchikey = c("IK_A", "IK_A")
      ),
      0.01,
      0.05,
      TRUE
    ),
    c(FALSE, FALSE)
  )
  # (b) different feature_id but RT outside rt_tol -> filtered out at RT step
  expect_identical(
    .match_tied_rows_to_anchors(
      df_t_direct,
      tidytable::tidytable(
        feature_id = c("FA1", "FA2"),
        rt = c(9.0, 9.5),
        .candidate_anchor_mass = c(200.0, 200.0),
        candidate_structure_inchikey = c("IK_A", "IK_A")
      ),
      0.01,
      0.05,
      TRUE
    ),
    c(FALSE, FALSE)
  )
  # (c) different feature_id and RT within rt_tol -> matches row 1
  expect_identical(
    .match_tied_rows_to_anchors(
      df_t_direct,
      tidytable::tidytable(
        feature_id = c("FA1", "FA2"),
        rt = c(2.01, 2.02),
        .candidate_anchor_mass = c(200.0, 200.0),
        candidate_structure_inchikey = c("IK_A", "IK_A")
      ),
      0.01,
      0.05,
      TRUE
    ),
    c(TRUE, FALSE)
  )

  # - missing rank_final, missing candidate_adduct, missing IK, NA in cluster_consensus_promoted_from_anchor
  df_no_rk_ik <- tidytable::tidytable(
    feature_id = c("F1", "F1", "F1"),
    score_weighted_chemo = c(0.9, 0.9, 0.9),
    candidate_score_sirius_confidence = c(0.8, 0.5, 0.2),
    cluster_consensus_promoted_from_anchor = c(NA, FALSE, FALSE)
  )
  res_no_ik <- sample_candidates_per_group(df_no_rk_ik, max_per_score = 1L)
  expect_identical(nrow(res_no_ik$df), 1L)
  expect_false(anyNA(res_no_ik$df$cluster_consensus_promoted_from_anchor))

  # - anchor collapsing with rt column, existing annotation_note merging, and sampling when no anchor matches
  M_val <- 200.0
  mz_h <- calculate_mz_from_mass(M_val, "[M+H]+")
  mz_na <- calculate_mz_from_mass(M_val, "[M+Na]+")
  df_anc_rt <- tidytable::tidytable(
    feature_id = c("FA", "FB", "FB", "FC", "FC", "FC"),
    rt = c(2.00, 2.01, 2.01, 5.00, 5.00, 5.00),
    mz = c(mz_h, mz_na, mz_na, 350.0, 350.0, 350.0),
    candidate_adduct = c(
      "[M+H]+",
      "[M+Na]+",
      "[M+Na]+",
      "[M+H]+",
      "[M+H]+",
      "[M+H]+"
    ),
    rank_final = rep(1L, 6L),
    score_weighted_chemo = c(0.95, 0.80, 0.80, 0.70, 0.70, 0.70),
    candidate_structure_inchikey_connectivity_layer = c(
      "IK_ANC",
      "IK_ANC",
      "IK_OTHER",
      "IK_C1",
      "IK_C1",
      "IK_C2"
    ),
    annotation_note = c(
      NA_character_,
      "Prior note",
      NA_character_,
      "Note C1",
      "Note C2",
      NA_character_
    )
  )
  res_anc_rt <- sample_candidates_per_group(
    df_anc_rt,
    max_per_score = 1L,
    apply_anchor_collapsing = TRUE
  )
  expect_true(any(grepl(
    "Prior note \\| Kept candidate",
    res_anc_rt$annotation_notes$annotation_note
  )))

  # Duplicate note keys across different rows in annotation_notes_lookup
  df_dup_notes <- tidytable::tidytable(
    feature_id = c("FD", "FD", "FD"),
    candidate_adduct = c("[M+H]+", "[M+H]+", "[M+H]+"),
    rank_final = c(1L, 2L, 2L),
    candidate_structure_inchikey_connectivity_layer = c(
      "IK_D",
      "IK_D",
      "IK_D2"
    ),
    annotation_note = c("Note 1", "Note 2", "Note 3")
  )
  res_dup_notes <- sample_candidates_per_group(df_dup_notes, max_per_score = 1L)
  expect_true(any(grepl("\\|", res_dup_notes$annotation_notes$annotation_note)))

  # 3. build_mini_taxonomy_table & build_mini_results_table edge cases
  empty_pct <- tidytable::tidytable(
    feature_id = character(0),
    candidate_structure_inchikey_connectivity_layer = character(0)
  )
  tax_empty <- build_mini_taxonomy_table(
    empty_pct,
    0.1,
    0.2,
    0.3,
    0.4,
    0.1,
    0.2,
    0.3
  )
  expect_identical(nrow(tax_empty), 0L)

  ft_dup <- tidytable::tidytable(
    feature_id = c("F1", "F1", "F2"),
    rt = c(1.0, 2.0, 3.0),
    mz = c(100.0, 100.0, 200.0)
  )
  rf_alt <- tidytable::tidytable(
    feature_id = c("F1", "F2"),
    rank_final = c(1L, 1L),
    score_weighted_chemo = c(0.8, 0.7),
    candidate_score_pseudo_initial = c(0.5, 0.4),
    candidate_structure_error_rt = c(0.01, 0.02),
    candidate_structure_error_mz = c(0.001, 0.002),
    inchikey_connectivity_layer = c("IK1", "IK2")
  )
  mini_alt <- build_mini_results_table(
    features_table = ft_dup,
    df_classes_mini = tidytable::tidytable(feature_id = "F1"),
    results_filtered = rf_alt
  )
  expect_true(nrow(mini_alt) >= 2L)

  rf_minimal <- tidytable::tidytable(
    feature_id = "F1",
    rank_final = 1L,
    score_final = 0.9,
    inchikey_connectivity_layer = "IK1"
  )
  mini_min <- build_mini_results_table(
    features_table = tidytable::tidytable(
      feature_id = "F1",
      rt = 1.0,
      mz = 100.0
    ),
    df_classes_mini = tidytable::tidytable(),
    results_filtered = rf_minimal
  )
  expect_identical(nrow(mini_min), 1L)
  mini_min_noscore <- build_mini_results_table(
    features_table = tidytable::tidytable(
      feature_id = "F1",
      rt = 1.0,
      mz = 100.0
    ),
    df_classes_mini = tidytable::tidytable(),
    results_filtered = tidytable::tidytable(
      feature_id = "F1",
      rank_final = 1L,
      inchikey_connectivity_layer = "IK1"
    )
  )
  expect_identical(nrow(mini_min_noscore), 1L)

  # 4. enforce_cluster_entity_consensus with missing required columns
  expect_equal(
    nrow(enforce_cluster_entity_consensus(
      tidytable::tidytable(feature_id = "F1", rank_final = 1L),
      tidytable::tidytable(feature_id = "F1")
    )),
    1L
  )

  # 5. .cluster_1d_complete_linkage and .compute_feature_rt_clusters edge cases
  expect_identical(.cluster_1d_complete_linkage(numeric(0), 0.05), integer(0))
  expect_identical(.cluster_1d_complete_linkage(1.0, 0.05), 1L)
  # Chain where single-linkage has diameter 0.08 > 0.05, so complete-linkage splits at 0.08
  expect_identical(
    .cluster_1d_complete_linkage(c(0.0, 0.04, 0.08), 0.05),
    c(1L, 1L, 2L)
  )
  expect_identical(
    .cluster_1d_complete_linkage(
      c(0.0, 0.04, 0.08, 0.09),
      0.05,
      group_sorted = c(1L, 1L, 1L, 2L)
    ),
    c(1L, 1L, 2L, 3L)
  )

  expect_identical(
    .compute_feature_rt_clusters(character(0), numeric(0)),
    integer(0)
  )
  expect_identical(
    .compute_feature_rt_clusters(c(NA_character_, NA_character_), c(1.0, 2.0)),
    c(NA_integer_, NA_integer_)
  )
  expect_identical(
    .compute_feature_rt_clusters(c("F1", "F1"), c(1.0, 1.0)),
    c(NA_integer_, NA_integer_)
  )
  expect_identical(
    .compute_feature_rt_clusters(
      c("F1", "F2"),
      c(1.0, 1.01),
      rt_tolerance = 0.05
    ),
    c(NA_integer_, NA_integer_)
  )

  # 6. .cross_rt_row_priority_and_weights and .solve_bipartite_max_weight edge cases
  pw_edge <- .cross_rt_row_priority_and_weights(tidytable::tidytable(
    feature_id = c("F1", "F2"),
    candidate_score_sirius_confidence = c(0.8, 0.0),
    candidate_structure_error_rt = c(0.01, 0.05),
    candidate_structure_error_mz = c(0.001, 0.005)
  ))
  expect_length(pw_edge$weights, 2L)
  expect_identical(
    .solve_bipartite_max_weight(matrix(numeric(0), nrow = 0L, ncol = 0L)),
    integer(0)
  )

  # 7. resolve_cross_rt_duplicate_candidates and drop_cross_rt_duplicate_final_candidates edge cases
  expect_identical(
    nrow(resolve_cross_rt_duplicate_candidates(tidytable::tidytable(
      feature_id = "F1"
    ))),
    1L
  )
  expect_identical(
    nrow(resolve_cross_rt_duplicate_candidates(tidytable::tidytable(
      feature_id = c("F1", "F2"),
      rank_final = c(1L, 1L),
      candidate_structure_inchikey_connectivity_layer = c("IK1", "IK1")
    ))),
    2L
  )
  expect_identical(
    nrow(resolve_cross_rt_duplicate_candidates(tidytable::tidytable(
      feature_id = c("F1", "F2"),
      rt = c(NA_real_, NA_real_),
      rank_final = c(1L, 1L),
      candidate_structure_inchikey_connectivity_layer = c("IK1", "IK1")
    ))),
    2L
  )
  expect_identical(
    nrow(drop_cross_rt_duplicate_final_candidates(tidytable::tidytable(
      feature_id = c("F1", "F2"),
      rt = c(NA_real_, NA_real_),
      rank_final = c(1L, 1L),
      candidate_structure_inchikey_connectivity_layer = c("IK1", "IK1")
    ))),
    2L
  )
  # Pure tie in drop_cross_rt_duplicate_final_candidates with no rank-2 candidates -> all(keep_row) && !added_note
  expect_identical(
    nrow(drop_cross_rt_duplicate_final_candidates(tidytable::tidytable(
      feature_id = c("F1", "F2"),
      rt = c(1.0, 5.0),
      rank_final = c(1L, 1L),
      candidate_structure_inchikey_connectivity_layer = c("IK_TIE", "IK_TIE"),
      score_weighted_chemo = c(0.9, 0.9)
    ))),
    2L
  )
  # Co-eluting features in same RT cluster (no cross-RT conflict) + feature_rt column
  df_same_rt <- tidytable::tidytable(
    feature_id = c("F1", "F2", "F3"),
    feature_rt = c(1.00, 1.01, 5.00),
    rank_final = c(1L, 1L, 1L),
    candidate_structure_inchikey_connectivity_layer = c("IK1", "IK1", "IK2"),
    score_weighted_chemo = c(0.9, 0.8, 0.7)
  )
  expect_identical(
    resolve_cross_rt_duplicate_candidates(df_same_rt)$rank_final,
    c(1L, 1L, 1L)
  )

  # Direct call to .solve_optimal_rt_cluster_assignment with < 2 rank-1 rows and with same-cluster dup
  expect_null(.solve_optimal_rt_cluster_assignment(
    fids = "F1",
    rt_cid = 1L,
    iks = "IK1",
    valid_ik = TRUE,
    col_rk = 1L,
    row_prio_rank = 1L,
    row_weights = 1.0
  ))
  expect_null(.solve_optimal_rt_cluster_assignment(
    fids = c("F1", "F2"),
    rt_cid = c(1L, 1L),
    iks = c("IK1", "IK1"),
    valid_ik = c(TRUE, TRUE),
    col_rk = c(1L, 1L),
    row_prio_rank = c(1L, 2L),
    row_weights = c(1.0, 0.9)
  ))
  expect_null(.solve_optimal_rt_cluster_assignment(
    fids = c("F1", "F2"),
    rt_cid = c(1L, 2L),
    iks = c("IK1", "IK2"),
    valid_ik = c(TRUE, TRUE),
    col_rk = c(1L, 1L),
    row_prio_rank = c(1L, 2L),
    row_weights = c(1.0, 0.9)
  ))

  # Multi-step conflict closure + unused IK in active component + exact cross-RT tie + fallback
  df_closure_and_tie <- tidytable::tidytable(
    feature_id = c(
      "C1_A",
      "C1_A",
      "C1_B",
      "C2_A",
      "C2_A",
      "C3_A",
      "C3_A",
      "T1",
      "T1",
      "T2",
      "T2"
    ),
    rt = c(
      1.0,
      1.0,
      1.01,
      3.0,
      3.0,
      5.0,
      5.0,
      8.0,
      8.0,
      10.0,
      10.0
    ),
    rank_final = c(
      1L,
      2L,
      2L,
      1L,
      2L,
      1L,
      2L,
      1L,
      2L,
      1L,
      2L
    ),
    candidate_structure_inchikey_connectivity_layer = c(
      "IK_AB",
      "IK_BC",
      NA_character_,
      "IK_AB",
      "IK_UNUSED",
      "IK_BC",
      "IK_C3",
      "IK_TIE",
      "IK_T1_ALT",
      "IK_TIE",
      "IK_T2_ALT"
    ),
    score_weighted_chemo = c(
      0.95,
      0.85,
      0.50,
      0.90,
      0.10,
      0.99,
      0.80,
      0.88,
      0.70,
      0.88,
      0.70
    )
  )
  res_ct <- resolve_cross_rt_duplicate_candidates(df_closure_and_tie)
  expect_true(any(grepl(
    "tied with feature",
    res_ct$annotation_note,
    fixed = TRUE
  )))

  # Feature where only candidate is taken by another RT cluster (triggers fallback / no target row)
  df_only_one <- tidytable::tidytable(
    feature_id = c("F_WIN", "F_LOSE", "F_LOSE"),
    rt = c(1.0, 5.0, 5.0),
    rank_final = c(1L, 1L, 2L),
    candidate_structure_inchikey_connectivity_layer = c(
      "IK_SHARED",
      "IK_SHARED",
      NA_character_
    ),
    score_weighted_chemo = c(0.95, 0.50, 0.40)
  )
  res_only_one <- resolve_cross_rt_duplicate_candidates(df_only_one)
  expect_identical(nrow(res_only_one), 3L)

  # 8. drop_cross_rt_duplicate_final_candidates edge cases
  expect_identical(
    nrow(drop_cross_rt_duplicate_final_candidates(tidytable::tidytable(
      feature_id = "F1"
    ))),
    1L
  )
  expect_identical(
    nrow(drop_cross_rt_duplicate_final_candidates(tidytable::tidytable(
      feature_id = c("F1", "F2"),
      rank_final = c(1L, 1L)
    ))),
    2L
  )
  expect_identical(
    nrow(drop_cross_rt_duplicate_final_candidates(tidytable::tidytable(
      feature_id = c("F1", "F2"),
      rank_final = c(1L, 1L),
      candidate_structure_inchikey_connectivity_layer = c("IK1", "IK2")
    ))),
    2L
  )
  expect_identical(
    nrow(drop_cross_rt_duplicate_final_candidates(df_same_rt)),
    3L
  )

  # drop_cross_rt_duplicate_final_candidates with displaced IK, tied IK, NA IK row, and all-dropped feature
  df_filt_cross <- tidytable::tidytable(
    feature_id = c(
      "F1",
      "F1",
      "F2",
      "F2",
      "F2",
      "F_DROP",
      "T1",
      "T1",
      "T2",
      "T2"
    ),
    feature_rt = c(
      1.0,
      1.0,
      4.0,
      4.0,
      4.0,
      6.0,
      7.0,
      7.0,
      9.0,
      9.0
    ),
    rank_final = c(
      1L,
      2L,
      1L,
      2L,
      3L,
      1L,
      1L,
      2L,
      1L,
      2L
    ),
    candidate_structure_inchikey_connectivity_layer = c(
      "IK_1",
      "IK_1B",
      "IK_1",
      "IK_2B",
      NA_character_,
      "IK_1",
      "IK_TIE",
      "IK_T1B",
      "IK_TIE",
      "IK_T2B"
    ),
    score_weighted_chemo = c(
      0.95,
      0.80,
      0.85,
      0.75,
      0.50,
      0.40,
      0.90,
      0.70,
      0.90,
      0.70
    ),
    candidate_score_sirius_csi = rep(-5.0, 10L),
    candidate_score_pseudo_initial = rep(0.6, 10L),
    candidate_adduct = rep("[M+H]+", 10L),
    annotation_note = rep(NA_character_, 10L)
  )
  out_filt_cross <- drop_cross_rt_duplicate_final_candidates(df_filt_cross)
  expect_true(nrow(out_filt_cross) < nrow(df_filt_cross))
  expect_true(any(grepl(
    "assigned to feature",
    out_filt_cross$annotation_note,
    fixed = TRUE
  )))
  expect_true(any(grepl(
    "tied with feature",
    out_filt_cross$annotation_note,
    fixed = TRUE
  )))

  # 9. Additional edge cases for 100% coverage of R/clean_chemo_candidates.R
  res_rm_null <- remove_compound_names(
    list(
      mini = NULL,
      filtered = tidytable::tidytable(feature_id = "F1"),
      full = NULL
    ),
    compounds_names = FALSE
  )
  expect_null(res_rm_null$mini)
  expect_identical(
    nrow(select_clean_chemo_working_columns(tidytable::tidytable(
      feature_id = "F1"
    ))),
    1L
  )
  expect_identical(
    ncol(select_clean_chemo_working_columns(tidytable::tidytable(
      feature_id = "F1",
      unused_col = 1L
    ))),
    1L
  )
  expect_identical(
    nrow(build_mini_results_table(
      features_table = tidytable::tidytable(
        feature_id = "F1",
        rt = 1.0,
        mz = 100.0
      ),
      df_classes_mini = tidytable::tidytable(),
      results_filtered = tidytable::tidytable()
    )),
    1L
  )
  expect_identical(
    nrow(build_mini_results_table(
      features_table = tidytable::tidytable(
        feature_id = "F1",
        rt = 1.0,
        mz = 100.0
      ),
      df_classes_mini = tidytable::tidytable(),
      results_filtered = tidytable::tidytable(
        feature_id = "F1",
        rank_final = 1L,
        score_initial = 0.8
      )
    )),
    1L
  )
  expect_identical(
    nrow(enforce_cluster_entity_consensus(
      tidytable::tidytable(),
      tidytable::tidytable(feature_id = "F1", component_id = "C1")
    )),
    0L
  )
  expect_identical(
    nrow(enforce_cluster_entity_consensus(
      df_ranked = tidytable::tidytable(
        feature_id = c("F1", "F2", "F2"),
        feature_rt = c(1.0, 1.0, 1.0),
        rank_final = c(1L, 1L, 2L),
        candidate_structure_exact_mass = c(180.06, 180.06, 180.06),
        candidate_structure_inchikey_connectivity_layer = c(
          "IK_A",
          "IK_B",
          "IK_A"
        ),
        score_weighted_chemo = c(0.95, 0.80, 0.70)
      ),
      components_table = tidytable::tidytable(
        feature_id = c("F1", "F2"),
        component_id = c("C1", "C1")
      )
    )),
    3L
  )
  expect_identical(
    nrow(enforce_cluster_entity_consensus(
      df_ranked = tidytable::tidytable(
        feature_id = c("F1", "F2"),
        rt = c(1.0, 1.0),
        rank_final = c(1L, 1L),
        candidate_structure_exact_mass = c(180.06, 180.06),
        candidate_structure_inchikey_connectivity_layer = c("IK_A", "IK_A"),
        score_weighted_chemo = c(0.95, 0.80)
      ),
      components_table = tidytable::tidytable(
        feature_id = c("F1", "F2"),
        component_id = c("C1", "C1")
      )
    )),
    2L
  )
  expect_identical(
    nrow(resolve_cross_rt_duplicate_candidates(tidytable::tidytable())),
    0L
  )

  # Two unrelated mass families co-eluting in the same global RT window (1.00 & 1.01)
  df_coelute_unrel <- tidytable::tidytable(
    feature_id = c(
      "M1_early",
      "M1_early",
      "M1_late",
      "M1_late",
      "M2_early",
      "M2_early",
      "M2_late",
      "M2_late"
    ),
    rt = c(1.00, 1.00, 5.00, 5.00, 1.01, 1.01, 8.00, 8.00),
    rank_final = c(1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L),
    candidate_structure_inchikey_connectivity_layer = c(
      "IK_M1_A",
      "IK_M1_B",
      "IK_M1_A",
      "IK_M1_B",
      "IK_M2_A",
      "IK_M2_B",
      "IK_M2_A",
      "IK_M2_B"
    ),
    score_weighted_chemo = c(0.60, 0.55, 0.90, 0.50, 0.99, 0.50, 0.80, 0.75)
  )
  res_coelute <- resolve_cross_rt_duplicate_candidates(df_coelute_unrel)
  r1_m1_early <- res_coelute$candidate_structure_inchikey_connectivity_layer[
    res_coelute$feature_id == "M1_early" & res_coelute$rank_final == 1L
  ]
  r1_m2_early <- res_coelute$candidate_structure_inchikey_connectivity_layer[
    res_coelute$feature_id == "M2_early" & res_coelute$rank_final == 1L
  ]
  expect_identical(r1_m1_early, "IK_M1_B")
  expect_identical(r1_m2_early, "IK_M2_A")
})
