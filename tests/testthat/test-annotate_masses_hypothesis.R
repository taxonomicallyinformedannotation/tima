# Test Suite: annotate_masses_hypothesis ----
# Tests for adduct hypothesis helpers in annotate_masses_hypothesis.R

library(testthat)

# Helper to create minimal adduct edge tables
.make_adduct_edges <- function(
  fid1 = "f1",
  adduct1 = "[M+H]+",
  fid2 = "f2",
  adduct2 = "[M+Na]+"
) {
  tidytable::tidytable(
    feature_id = fid1,
    adduct = adduct1,
    feature_id_dest = fid2,
    adduct_dest = adduct2
  )
}

# ── match_pairs_to_adduct_diffs ───────────────────────────────────────────────

test_that("match_pairs_to_adduct_diffs returns empty table when pairs is empty", {
  pairs <- tidytable::tidytable(
    feature_id = character(),
    feature_id_dest = character(),
    delta_min = numeric(),
    delta_max = numeric()
  )
  diffs <- tidytable::tidytable(
    Distance = 21.982,
    Group1 = "[M+H]+",
    Group2 = "[M+Na]+"
  )
  result <- match_pairs_to_adduct_diffs(pairs, diffs)
  expect_equal(nrow(result), 0L)
  expect_true("feature_id" %in% colnames(result))
  expect_true("adduct" %in% colnames(result))
})

test_that("match_pairs_to_adduct_diffs returns empty table when diffs is empty", {
  pairs <- tidytable::tidytable(
    feature_id = c("f1"),
    feature_id_dest = c("f2"),
    delta_min = c(21.0),
    delta_max = c(23.0)
  )
  diffs <- tidytable::tidytable(
    Distance = numeric(),
    Group1 = character(),
    Group2 = character()
  )
  result <- match_pairs_to_adduct_diffs(pairs, diffs)
  expect_equal(nrow(result), 0L)
})

test_that("match_pairs_to_adduct_diffs matches within tolerance range", {
  pairs <- tidytable::tidytable(
    feature_id = c("f1"),
    feature_id_dest = c("f2"),
    delta_min = c(21.0),
    delta_max = c(23.0)
  )
  # H -> Na difference: 22.989 - 1.008 = 21.982 Da
  diffs <- tidytable::tidytable(
    Distance = 21.982,
    Group1 = "[M+H]+",
    Group2 = "[M+Na]+"
  )
  result <- match_pairs_to_adduct_diffs(pairs, diffs)
  expect_true(nrow(result) >= 1L)
  expect_equal(result$feature_id[[1L]], "f1")
  expect_equal(result$feature_id_dest[[1L]], "f2")
})

test_that("match_pairs_to_adduct_diffs does not match outside tolerance range", {
  pairs <- tidytable::tidytable(
    feature_id = c("f1"),
    feature_id_dest = c("f2"),
    delta_min = c(1.0),
    delta_max = c(2.0)
  )
  diffs <- tidytable::tidytable(
    Distance = 21.982,
    Group1 = "[M+H]+",
    Group2 = "[M+Na]+"
  )
  result <- match_pairs_to_adduct_diffs(pairs, diffs)
  expect_equal(nrow(result), 0L)
})

# ── dedupe_node_hypotheses ────────────────────────────────────────────────────

test_that("dedupe_node_hypotheses returns empty for empty input", {
  hyps <- tidytable::tidytable(
    feature_id = character(),
    adduct = character(),
    source = character(),
    is_preassigned = logical(),
    adduct_support = integer(),
    mz = numeric(),
    rt = numeric()
  )
  result <- dedupe_node_hypotheses(hyps)
  expect_equal(nrow(result), 0L)
})

test_that("dedupe_node_hypotheses deduplicates same feature/adduct", {
  hyps <- tidytable::tidytable(
    feature_id = c("f1", "f1"),
    adduct = c("[M+H]+", "[M+H]+"),
    source = c("pair", "baseline"),
    is_preassigned = c(FALSE, FALSE),
    adduct_support = c(1L, 0L),
    mz = c(100.0, 100.0),
    rt = c(1.0, 1.0)
  )
  result <- dedupe_node_hypotheses(hyps)
  expect_equal(nrow(result), 1L)
  # pair has higher priority (lower rank) than baseline
  expect_equal(result$source[[1L]], "pair")
})

test_that("dedupe_node_hypotheses keeps different adducts for same feature", {
  hyps <- tidytable::tidytable(
    feature_id = c("f1", "f1"),
    adduct = c("[M+H]+", "[M+Na]+"),
    source = c("pair", "pair"),
    is_preassigned = c(FALSE, FALSE),
    adduct_support = c(1L, 1L),
    mz = c(100.0, 100.0),
    rt = c(1.0, 1.0)
  )
  result <- dedupe_node_hypotheses(hyps)
  expect_equal(nrow(result), 2L)
})

test_that("dedupe_node_hypotheses keeps different features with same adduct", {
  hyps <- tidytable::tidytable(
    feature_id = c("f1", "f2"),
    adduct = c("[M+H]+", "[M+H]+"),
    source = c("baseline", "baseline"),
    is_preassigned = c(FALSE, FALSE),
    adduct_support = c(0L, 0L),
    mz = c(100.0, 200.0),
    rt = c(1.0, 2.0)
  )
  result <- dedupe_node_hypotheses(hyps)
  expect_equal(nrow(result), 2L)
})

# ── evidence_adduct_requires_pairwise_support ─────────────────────────────────

test_that("evidence_adduct_requires_pairwise_support returns FALSE for simple protonated adducts", {
  expect_false(evidence_adduct_requires_pairwise_support("[M+H]+"))
  expect_false(evidence_adduct_requires_pairwise_support("[M-H]-"))
  expect_false(evidence_adduct_requires_pairwise_support("[M+Na]+"))
  expect_false(evidence_adduct_requires_pairwise_support("[M+2H]2+"))
  expect_false(evidence_adduct_requires_pairwise_support("[2M+H]+"))
})

test_that("evidence_adduct_requires_pairwise_support returns TRUE for modifier-bearing adducts", {
  # Adducts with non-carrier modifiers like ACN require pairwise support
  expect_true(evidence_adduct_requires_pairwise_support("[M+ACN+H]+"))
  expect_true(evidence_adduct_requires_pairwise_support("[M-H2O+H]+"))
})

test_that("evidence_adduct_requires_pairwise_support returns FALSE for NULL/NA/empty", {
  expect_false(evidence_adduct_requires_pairwise_support(NULL))
  expect_false(evidence_adduct_requires_pairwise_support(NA_character_))
  expect_false(evidence_adduct_requires_pairwise_support(""))
})

# ── generate_multi_hypotheses_from_node_masses ────────────────────────────────

test_that("generate_multi_hypotheses_from_node_masses returns empty for empty inputs", {
  node_hyps <- tidytable::tidytable(
    feature_id = character(),
    adduct = character(),
    source = character(),
    adduct_support = integer(),
    mz = numeric(),
    rt = numeric(),
    mass = numeric()
  )
  multi_adducts <- tidytable::tidytable(adduct = character())
  result <- generate_multi_hypotheses_from_node_masses(
    node_hypotheses = node_hyps,
    multi_adducts = multi_adducts,
    tolerance_ppm = 10
  )
  expect_equal(nrow(result), 0L)
})

test_that("generate_multi_hypotheses_from_node_masses returns empty when no non-multi base", {
  # No base neutral masses exist (all "multi") -> no expansion possible
  node_hyps <- tidytable::tidytable(
    feature_id = c("f1"),
    adduct = c("[2M+H]+"),
    source = c("multi"),
    adduct_support = c(0L),
    mz = c(201.0),
    rt = c(1.0),
    mass = c(200.0)
  )
  multi_adducts <- tidytable::tidytable(adduct = c("[3M+H]+"))
  result <- generate_multi_hypotheses_from_node_masses(
    node_hyps,
    multi_adducts,
    tolerance_ppm = 10
  )
  expect_equal(nrow(result), 0L)
})

# ── prune_candidates_by_network_consensus ─────────────────────────────────────

test_that("prune_candidates_by_network_consensus returns empty for empty matched", {
  matched <- tidytable::tidytable(
    feature_id = character(),
    adduct = character(),
    source = character()
  )
  adduct_edges <- tidytable::tidytable(
    feature_id = character(),
    adduct = character(),
    feature_id_dest = character(),
    adduct_dest = character()
  )
  result <- prune_candidates_by_network_consensus(matched, adduct_edges)
  expect_equal(nrow(result), 0L)
})

test_that("prune_candidates_by_network_consensus keeps all when no adduct edges", {
  matched <- tidytable::tidytable(
    feature_id = c("f1", "f1"),
    adduct = c("[M+H]+", "[M+Na]+"),
    source = c("baseline", "multi"),
    structure_exact_mass = c(100.0, 100.0)
  )
  adduct_edges <- tidytable::tidytable(
    feature_id = character(),
    adduct = character(),
    feature_id_dest = character(),
    adduct_dest = character()
  )
  result <- prune_candidates_by_network_consensus(matched, adduct_edges)
  # Without support, all candidates stay (no candidate is strictly better)
  expect_gte(nrow(result), 1L)
})
