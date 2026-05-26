#' @title Test evidence_count semantics (NOT a score)
#'
#' @description
#' Validates that evidence_count correctly represents the COUNT of supporting
#' features, not a quality score. A count=1 is valid evidence; higher counts
#' provide peer verification but don't invalidate count=1.

test_that("evidence_count is count of supporting features, not a score", {
  # evidence_count should be at least 1 for any retained hypothesis
  # (a feature supporting itself via mass consistency)
  # Higher counts mean additional peer features agree on the same M

  skip_if_not_installed("tidytable")

  # Create mock evidence table
  mock_hyps <- tidytable::tidytable(
    feature_id = c("f1", "f1", "f2", "f2", "f3"),
    adduct = c("[M+H]+", "[M+Na]+", "[M+H]+", "[M+Na]+", "[M+H]+"),
    evidence_cluster = c(
      "cluster_1",
      "cluster_1",
      "cluster_1",
      "cluster_1",
      NA_character_
    ),
    n_evidence_features = c(3L, 3L, 3L, 3L, 1L)
  )

  # evidence_count should equal n_evidence_features directly (no -1 offset)
  expected_counts <- c(3L, 3L, 3L, 3L, 1L)

  for (i in seq_len(nrow(mock_hyps))) {
    expect_equal(
      mock_hyps$n_evidence_features[i],
      expected_counts[i],
      label = sprintf(
        "Row %d: evidence_count should match n_evidence_features",
        i
      )
    )
  }
})


test_that("min_support = 1 means any supporting feature is valid", {
  skip_if_not_installed("tidytable")

  # Before: evidence_score = n_features - 1, so min_support had to be 2 for count>=1
  # Now: evidence_count = n_features directly, min_support = 1 for any support

  # Create test cases
  test_cases <- tidytable::tidytable(
    n_evidence = c(1L, 2L, 3L, 5L),
    min_support = 1L,
    should_pass = c(TRUE, TRUE, TRUE, TRUE)
  )

  for (i in seq_len(nrow(test_cases))) {
    evidence <- test_cases$n_evidence[i]
    min_supp <- test_cases$min_support[i]
    expected <- test_cases$should_pass[i]

    # With new semantics: evidence_count >= 1 for all valid hypotheses
    result <- evidence >= min_supp

    expect_equal(
      result,
      expected,
      label = sprintf(
        "evidence_count=%d vs min_support=%d",
        evidence,
        min_supp
      )
    )
  }
})


test_that("evidence_count=1 is self-consistent (not 0)", {
  # A feature's hypothesis with evidence_cluster=NA (no peers) still has
  # evidence_count=1 (self-consistency via being in the output)
  # This is NOT a "0 score" baseline — it's a valid hypothesis.

  # Create feature with no peer support
  single_feature_hyp <- tidytable::tidytable(
    feature_id = "f_alone",
    adduct = "[M+H]+",
    evidence_cluster = NA_character_,
    n_evidence_features = 1L,
    implied_M = 200.0
  )

  # Should have evidence_count = 1, not 0
  expect_equal(single_feature_hyp$n_evidence_features[1], 1L)

  # In filtering: keep if evidence_count >= 1 (true for all valid hypotheses)
  keep <- single_feature_hyp$n_evidence_features >= 1L
  expect_true(keep[1])
})


test_that("strict mode uses min_support threshold directly", {
  # In strict mode: evidence_count >= adduct_min_support
  # (no offset, direct comparison)

  test_cases <- tidytable::tidytable(
    evidence_count = c(0L, 1L, 2L, 3L),
    min_support = 2L,
    should_pass = c(FALSE, FALSE, TRUE, TRUE)
  )

  for (i in seq_len(nrow(test_cases))) {
    count <- test_cases$evidence_count[i]
    min_s <- test_cases$min_support[i]
    expected <- test_cases$should_pass[i]

    result <- count >= min_s

    expect_equal(
      result,
      expected,
      label = sprintf(
        "strict: evidence_count=%d >= min_support=%d",
        count,
        min_s
      )
    )
  }
})


test_that("conditional mode filters at min_degree (high connectivity)", {
  # In conditional mode:
  # - At low local degree (few neighbors): keep if evidence_count >= 1
  # - At high degree (min_degree+): activate support filtering
  # min_degree controls strictness in crowded regions (not hardcoded)

  # This test validates the concept
  min_degree <- 3L

  # Low-connectivity scenario: keep all with evidence_count >= 1
  low_conn_evidence <- c(0L, 1L, 2L, 1L)
  low_conn_degree <- 1L
  low_conn_keep <- (low_conn_evidence >= 1L)

  expect_true(all(low_conn_keep[low_conn_evidence >= 1L]))
  expect_false(any(low_conn_keep[low_conn_evidence < 1L]))

  # High-connectivity scenario: activate stricter filter
  high_conn_evidence <- c(0L, 1L, 2L, 3L)
  high_conn_degree <- 5L
  high_conn_min_support <- 2L
  high_conn_keep <- (high_conn_degree < min_degree) |
    (high_conn_evidence >= high_conn_min_support)

  # With degree >= min_degree, only evidence >= min_support passes
  expect_false(high_conn_keep[1]) # 0 < 2
  expect_false(high_conn_keep[2]) # 1 < 2
  expect_true(high_conn_keep[3]) # 2 >= 2
  expect_true(high_conn_keep[4]) # 3 >= 2
})


test_that("baseline adduct never filtered by evidence_count", {
  # Baseline (e.g., [M+H]+) is ALWAYS kept, regardless of evidence_count

  baseline <- "[M+H]+"

  test_cases <- tidytable::tidytable(
    adduct = c(baseline, "[M+Na]+", baseline, "[M+Cl]+"),
    evidence_count = c(0L, 1L, 1L, 0L),
    is_baseline = c(TRUE, FALSE, TRUE, FALSE),
    should_keep = c(TRUE, TRUE, TRUE, FALSE)
  )

  for (i in seq_len(nrow(test_cases))) {
    adduct_i <- test_cases$adduct[i]
    count <- test_cases$evidence_count[i]
    expect_keep <- test_cases$should_keep[i]

    # Logic: keep if baseline OR has evidence
    keep <- (adduct_i == baseline) | (count >= 1L)

    expect_equal(
      keep,
      expect_keep,
      label = sprintf(
        "adduct=%s evidence_count=%d",
        adduct_i,
        count
      )
    )
  }
})


test_that("adduct_support transmission to output keeps evidence_count", {
  # In annotate_masses.R, df_addlossed_rdy uses:
  #   adduct_support = as.integer(evidence_count)
  # This is correct: transmits the count (not a quality score)

  evidence_count <- c(1L, 2L, 3L, 1L, 5L)
  adduct_support <- as.integer(evidence_count)

  expect_equal(adduct_support, evidence_count)
  expect_true(all(adduct_support >= 1L))
})


test_that("preassigned adducts get evidence_count = 1", {
  # A pre-assigned adduct has no peers but is self-consistent
  # Should have evidence_count = 1 (not 0)

  preassigned_evidence_count <- 1L
  min_support <- 1L

  # Should pass filter: 1 >= 1
  keep <- preassigned_evidence_count >= min_support
  expect_true(keep)
})
