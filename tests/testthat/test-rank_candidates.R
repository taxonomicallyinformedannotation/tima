# Test Suite: rank_candidates ----

library(testthat)

.rank_input <- function() {
  data.frame(
    feature_id = c("F1", "F1", "F1", "F2", "F2"),
    candidate_structure_inchikey_connectivity_layer = c("A", "B", "A", "C", "D"),
    score_weighted_chemo = c(0.9, 0.5, 0.9, 0.2, 0.7),
    candidate_score_pseudo_initial = c(0.1, 0.9, 0.9, 0.4, 0.4),
    stringsAsFactors = FALSE
  )
}

test_that("rank_candidates ranks descending within each feature_id", {
  out <- rank_candidates(.rank_input())

  # F1 has three pre-de-duplication rows (0.9, 0.5, 0.9): min_rank gives the
  # two ties rank 1 and the 0.5 rank 3. F2 (0.2, 0.7) gives 2 and 1.
  f1 <- out[out$feature_id == "F1", ]
  f2 <- out[out$feature_id == "F2", ]
  expect_equal(
    f1$rank_final[order(f1$candidate_structure_inchikey_connectivity_layer)],
    c(1L, 3L)
  )
  expect_equal(
    f2$rank_final[order(f2$candidate_structure_inchikey_connectivity_layer)],
    c(2L, 1L)
  )
  # Every feature_id restarts at rank 1.
  expect_equal(min(f1$rank_final), 1L)
  expect_equal(min(f2$rank_final), 1L)
})

test_that("rank_candidates uses score_final when score_weighted_chemo is absent", {
  d <- .rank_input()
  d$score_final <- c(0.1, 0.4, 0.1, 0.8, 0.6)
  d$score_weighted_chemo <- NULL

  out <- rank_candidates(d)
  # Ranks follow score_final (0.4 highest in F1), not the removed column:
  # A has 0.1 and ranks 2, B has 0.4 and ranks 1.
  f1 <- out[out$feature_id == "F1", ]
  expect_equal(
    f1$rank_final[order(f1$candidate_structure_inchikey_connectivity_layer)],
    c(2L, 1L)
  )
  # The score_final column is a working column and must not leak out.
  expect_false("score_final" %in% names(out))
  expect_false("score_initial" %in% names(out))
})

test_that("score_weighted_chemo wins over score_final when both exist", {
  d <- .rank_input()
  d$score_final <- c(0.1, 0.4, 0.1, 0.8, 0.6)

  out <- rank_candidates(d)
  f1 <- out[out$feature_id == "F1", ]
  # 0.9 ranks 1 and 0.5 ranks 3, i.e. the weighted column, not score_final
  # (0.1 and 0.4) which would have given 2 and 1.
  expect_equal(
    f1$rank_final[order(f1$candidate_structure_inchikey_connectivity_layer)],
    c(1L, 3L)
  )
})

test_that("rank_candidates gives NA ranks and warns without any score column", {
  d <- .rank_input()[, c("feature_id", "candidate_structure_inchikey_connectivity_layer")]

  expect_warning(out <- rank_candidates(d), "Neither score_weighted_chemo")
  expect_true(all(is.na(out$rank_final)))
  # The promotion flag is still added so downstream code sees a stable schema.
  expect_true("cluster_consensus_promoted_from_anchor" %in% names(out))
  expect_false(any(out$cluster_consensus_promoted_from_anchor))
  expect_equal(nrow(out), nrow(d))
})

test_that("rank_candidates clamps negative scores to zero", {
  d <- data.frame(
    feature_id = c("F1", "F1", "F1"),
    candidate_structure_inchikey_connectivity_layer = c("A", "B", "C"),
    score_weighted_chemo = c(-0.5, 0.5, -0.1),
    stringsAsFactors = FALSE
  )
  out <- rank_candidates(d)
  # -0.5 and -0.1 both clamp to 0, so they tie at rank 2 below the 0.5.
  expect_equal(out$rank_final, c(2L, 1L, 2L))
  # The scores themselves are not rewritten, only the ranking input.
  expect_equal(out$score_weighted_chemo, c(-0.5, 0.5, -0.1))
})

test_that("rank_candidates turns non-finite scores into NA ranks", {
  d <- data.frame(
    feature_id = c("F1", "F1", "F1", "F1"),
    candidate_structure_inchikey_connectivity_layer = c("A", "B", "C", "D"),
    score_weighted_chemo = c(0.9, Inf, -Inf, NaN),
    stringsAsFactors = FALSE
  )
  out <- rank_candidates(d)
  expect_equal(out$rank_final[1L], 1L)
  # +Inf and NaN have no rank.
  expect_true(all(is.na(out$rank_final[c(2L, 4L)])))
  # -Inf clamps to 0 first, which *is* finite, so it ranks normally.
  expect_equal(out$rank_final[3L], 2L)
})

test_that("rank_candidates handles a missing pseudo-initial score column", {
  d <- .rank_input()
  d$candidate_score_pseudo_initial <- NULL

  out <- rank_candidates(d)
  expect_true("rank_initial" %in% names(out))
  expect_true(all(is.na(out$rank_initial)))
  # The final ranks are unaffected.
  expect_equal(sort(out$rank_final), c(1L, 1L, 2L, 3L))
})

test_that("rank_candidates clamps a negative pseudo-initial score", {
  d <- .rank_input()
  d$candidate_score_pseudo_initial <- c(-1, 0.9, 0.9, 0.4, 0.4)

  out <- rank_candidates(d)
  f1 <- out[out$feature_id == "F1", ]
  # -1 clamps to 0, the lowest pseudo-initial score in F1, so the surviving
  # (F1, A) row gets rank 3 while the 0.9 row gets rank 1.
  expect_equal(
    f1$rank_initial[order(f1$candidate_structure_inchikey_connectivity_layer)],
    c(3L, 1L)
  )
  # A pseudo-initial of exactly 0 must not be confused with "missing".
  expect_false(is.na(f1$rank_initial[[1L]]))
})

test_that("rank_candidates de-duplicates candidate rows per feature", {
  # (F1, A) appears twice with identical scores; only the first survives.
  d <- .rank_input()
  out <- rank_candidates(d)

  expect_equal(nrow(out), 4L)
  expect_equal(sum(out$feature_id == "F1" & out$candidate_structure_inchikey_connectivity_layer == "A"), 1L)
  # Columns other than the ranking key are preserved.
  expect_true("score_weighted_chemo" %in% names(out))
  expect_true("candidate_score_pseudo_initial" %in% names(out))
})

test_that("rank_candidates honours an existing promotion flag", {
  d <- .rank_input()
  d$cluster_consensus_promoted_from_anchor <- c(TRUE, FALSE, TRUE, FALSE, FALSE)

  out <- rank_candidates(d)
  expect_true("cluster_consensus_promoted_from_anchor" %in% names(out))
  expect_equal(
    out$cluster_consensus_promoted_from_anchor,
    c(TRUE, FALSE, FALSE, FALSE)[seq_len(nrow(out))]
  )
  expect_true(out$cluster_consensus_promoted_from_anchor[[1L]])
  expect_false(any(out$cluster_consensus_promoted_from_anchor[-1L]))
})

test_that("rank_candidates returns an empty input untouched", {
  d <- .rank_input()[0, ]
  out <- rank_candidates(d)

  expect_equal(nrow(out), 0L)
  expect_named(out, names(d))
})
