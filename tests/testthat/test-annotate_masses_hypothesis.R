# Test Suite: annotate_masses_hypothesis ----
# Tests for adduct hypothesis helpers in annotate_masses_hypothesis.R

library(testthat)


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

# match_candidates_to_library: either-tolerance rule ----

.match_case <- function(exact, observed, ppm = 10, dalton = 0.0025) {
  candidates <- tidytable::tidytable(
    feature_id = paste0("f", seq_along(observed)),
    rt = 1,
    mz = observed + 1.007276,
    adduct = "[M+H]+",
    mass = observed,
    source = "test",
    adduct_support = 0L
  )
  # Wide library window so only the post-filter decides
  library_em <- tidytable::tidytable(
    exact_mass = exact,
    value_min = exact - 0.05,
    value_max = exact + 0.05
  )
  match_candidates_to_library(
    candidates = candidates,
    library_em = library_em,
    tolerance_ppm = ppm,
    tolerance_dalton = dalton
  )
}

test_that("a match within the Da floor is kept at low mass even beyond ppm", {
  # 2 mDa at M = 100 is 20 ppm: fails 10 ppm, passes 2.5 mDa
  kept <- .match_case(exact = 100, observed = 100.002)
  expect_identical(nrow(kept), 1L)
  expect_equal(kept$error_mz, -0.002, tolerance = 1e-9)

  # 3 mDa at M = 100 is 30 ppm: fails both
  expect_identical(nrow(.match_case(exact = 100, observed = 100.003)), 0L)
})

test_that("a match within ppm is kept at high mass even beyond the Da floor", {
  # 4 mDa at M = 500 is 8 ppm: fails 2.5 mDa, passes 10 ppm
  expect_identical(nrow(.match_case(exact = 500, observed = 500.004)), 1L)
  # 6 mDa at M = 500 is 12 ppm: fails both
  expect_identical(nrow(.match_case(exact = 500, observed = 500.006)), 0L)
})

test_that("the ppm error is relative to the library exact mass", {
  # 10 ppm of 500 = 5 mDa; of 500.005 it would be 5.00005 mDa
  at_edge <- .match_case(
    exact = 500,
    observed = 500 + 500 * 10e-6 * (1 - 1e-9),
    dalton = NULL
  )
  beyond <- .match_case(
    exact = 500,
    observed = 500 + 500 * 10e-6 * (1 + 1e-6),
    dalton = NULL
  )
  expect_identical(nrow(at_edge), 1L)
  expect_identical(nrow(beyond), 0L)
})

test_that("a single tolerance alone decides when the other is missing", {
  # ppm only: the Da floor no longer rescues the low-mass match
  expect_identical(
    nrow(.match_case(exact = 100, observed = 100.002, dalton = NULL)),
    0L
  )
  # Da only: the high-mass ppm match is lost
  expect_identical(
    nrow(.match_case(exact = 500, observed = 500.004, ppm = NULL)),
    0L
  )
  # neither: the library window alone decides
  expect_identical(
    nrow(.match_case(
      exact = 500,
      observed = 500.04,
      ppm = NULL,
      dalton = NULL
    )),
    1L
  )
})

test_that("each candidate matches every library mass within tolerance", {
  candidates <- tidytable::tidytable(
    feature_id = "f1",
    rt = 1,
    mz = 101.007276,
    adduct = "[M+H]+",
    mass = 100,
    source = "test",
    adduct_support = 0L
  )
  library_em <- tidytable::tidytable(
    exact_mass = c(99.999, 100.001, 100.1),
    value_min = c(99.999, 100.001, 100.1) - 0.05,
    value_max = c(99.999, 100.001, 100.1) + 0.05
  )
  matched <- match_candidates_to_library(
    candidates,
    library_em,
    tolerance_ppm = 10,
    tolerance_dalton = 0.0025
  )
  expect_setequal(matched$structure_exact_mass, c(99.999, 100.001))
  expect_false(any(matched$is_preassigned))
  expect_true(all(matched$candidate_adduct_origin == "supported"))
})

test_that(".fast_group_median matches stats::ave median across odd, even, and singleton groups", {
  expect_identical(
    .fast_group_median(numeric(0), character(0)),
    list(groups = character(), medians = numeric(), per_obs = numeric())
  )

  vals <- c(10.0, 20.0, 30.0, 4.0, 8.0, 99.0, 2.0, 6.0, 10.0, 14.0)
  grps <- c(
    "G_odd",
    "G_odd",
    "G_odd",
    "G_even2",
    "G_even2",
    "G_single",
    "G_even4",
    "G_even4",
    "G_even4",
    "G_even4"
  )

  res <- .fast_group_median(vals, grps)
  expected_per_obs <- stats::ave(vals, grps, FUN = stats::median)
  expect_equal(res$per_obs, expected_per_obs)
  expect_equal(
    stats::setNames(res$medians, res$groups)[c(
      "G_odd",
      "G_even2",
      "G_single",
      "G_even4"
    )],
    c(G_odd = 20.0, G_even2 = 6.0, G_single = 99.0, G_even4 = 8.0)
  )
})

test_that(".simplify_adduct_after_modifier deduplicates repeated strings and preserves NAs", {
  inp <- c(
    "[M+CH2O2+Na-H2]-",
    NA_character_,
    "",
    "[M+CH2O2+Na-H2]-",
    "[M-H2O+H]+",
    "[M+H4N-H3N]+"
  )
  out <- .simplify_adduct_after_modifier(inp)
  expect_length(out, length(inp))
  expect_true(is.na(out[[2L]]))
  expect_identical(out[[3L]], "")
  expect_identical(out[[1L]], out[[4L]])
  expect_identical(out[[5L]], "[M-H2O+H]+")
  expect_identical(out[[6L]], "[M-H3N+H4N]+")
})

test_that("prune_candidates_by_network_consensus drops unsupported candidates and multi hits when alternatives exist", {
  matched <- tidytable::tidytable(
    feature_id = c("F1", "F1", "F2", "F2", "F3"),
    adduct = c("[M+H]+", "[M+K]+", "[M+Na]+", "[2M+Fe]2+", "[2M+Fe]2+"),
    source = c("pair", "pair", "loss", "multi", "multi"),
    structure_exact_mass = c(200, 200, 300, 300, 400),
    error_mz = rep(0.001, 5L)
  )
  adduct_edges <- tidytable::tidytable(
    feature_id = "F1",
    adduct = "[M+H]+",
    feature_id_dest = "F9",
    adduct_dest = "[M+Na]+"
  )
  pruned <- prune_candidates_by_network_consensus(
    matched = matched,
    adduct_edges = adduct_edges,
    baseline_adduct = NULL
  )
  # F1: [M+H]+ has edge support, [M+K]+ has 0 support -> [M+K]+ dropped
  expect_identical(pruned$adduct[pruned$feature_id == "F1"], "[M+H]+")
  # F2: no edge support, but has non-multi ("loss") so speculative "multi" [2M+Fe]2+ is dropped
  expect_identical(pruned$adduct[pruned$feature_id == "F2"], "[M+Na]+")
  # F3: sole candidate is kept
  expect_identical(pruned$adduct[pruned$feature_id == "F3"], "[2M+Fe]2+")
})

test_that("generate_multi_hypotheses_from_node_masses matches within ppm or dalton tolerance and preserves order", {
  # Feature F1: base neutral mass = 200.0; mz = 401.007276 implies [2M+H]+ neutral mass = 200.0
  # Feature F2: base neutral mass = 300.0; mz = 151.007276 implies [M+2H]2+ neutral mass = 300.0
  # Feature F3: base neutral mass = 250.0; mz = 251.007276 does not match any multi adduct
  m_h <- 1.007276466812
  node_hyps <- tidytable::tidytable(
    feature_id = c("F1", "F2", "F3"),
    adduct = c("[M+H]+", "[M+H]+", "[M+H]+"),
    source = c("pair", "pair", "baseline"),
    is_preassigned = c(FALSE, FALSE, FALSE),
    adduct_support = c(1L, 1L, 0L),
    mz = c(2 * 200.0 + m_h, (300.0 + 2 * m_h) / 2, 250.0 + m_h),
    rt = c(1.5, 2.5, 3.5),
    mass = c(200.0, 300.0, 250.0)
  )
  multi_adducts <- tidytable::tidytable(
    adduct = c("[2M+H]+", "[M+2H]2+", "[3M+H]+")
  )
  lookup <- build_adduct_lookup_from_strings(c("[M+H]+", multi_adducts$adduct))

  res <- generate_multi_hypotheses_from_node_masses(
    node_hypotheses = node_hyps,
    multi_adducts = multi_adducts,
    adduct_lookup = lookup,
    tolerance_ppm = 10,
    tolerance_dalton = 0.0025
  )
  expect_identical(res$feature_id, c("F1", "F2"))
  expect_identical(res$adduct, c("[2M+H]+", "[M+2H]2+"))
  expect_identical(res$source, c("multi", "multi"))
  expect_identical(res$adduct_support, c(0L, 0L))
  expect_equal(res$mass, c(200.0, 300.0), tolerance = 1e-5)
})

test_that("append_component_weak_hypotheses expands weak hypotheses only for unresolved component features", {
  m_h <- 1.007276466812
  m_na <- 22.989218
  features_table <- tidytable::tidytable(
    feature_id = c("F1", "F2", "F3"),
    mz = c(200.0 + m_h, 200.0 + m_na, 350.0 + m_h),
    rt = c(5.0, 5.0, 8.0)
  )
  # F1 is resolved by pair edge; F2 is in component 1 with only baseline hypothesis
  node_hyps <- tidytable::tidytable(
    feature_id = c("F1", "F2"),
    adduct = c("[M+H]+", "[M+H]+"),
    source = c("pair", "baseline"),
    is_preassigned = c(FALSE, FALSE),
    candidate_adduct_origin = c("supported", "baseline"),
    adduct_support = c(1L, 0L),
    mz = c(200.0 + m_h, 200.0 + m_na),
    rt = c(5.0, 5.0),
    mass = c(200.0, 200.0 + m_na - m_h)
  )
  attr(node_hyps, "component_membership") <- tidytable::tidytable(
    feature_id = c("F1", "F2"),
    component_id = c(1L, 1L)
  )
  attr(node_hyps, "feature_m_map") <- tidytable::tidytable(
    feature_id = "F1",
    component_id = 1L,
    neutral_mass = 200.0
  )
  lookup <- build_adduct_lookup_from_strings(c("[M+H]+", "[M+Na]+"))

  out <- append_component_weak_hypotheses(
    node_hypotheses = node_hyps,
    features_table = features_table,
    neutral_losses_list = character(),
    clusters = c("Na"),
    baseline_adducts = c("[M+H]+"),
    adduct_lookup = lookup,
    tolerance_ppm = 10,
    tolerance_dalton = 0.0025
  )
  f2_rows <- out[out$feature_id == "F2", ]
  expect_true(nrow(f2_rows) >= 1L)
})
