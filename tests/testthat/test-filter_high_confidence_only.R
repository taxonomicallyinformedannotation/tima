# Test Suite: filter_high_confidence_only ----

library(testthat)

test_that("filter_high_confidence_only handles empty input", {
  empty <- tidytable::tidytable()
  result <- filter_high_confidence_only(empty)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0L)
})

# Basic score-based filtering across primary thresholds
test_that("filter_high_confidence_only keeps rows meeting any primary threshold", {
  df <- tidytable::tidytable(
    feature_id = c("F1", "F2", "F3"),
    score_biological = c(0.9, 0.1, 0.1),
    candidate_score_pseudo_initial = c(0.1, 0.96, 0.1),
    score_weighted_chemo = c(0.1, 0.1, 0.8)
  )
  res <- filter_high_confidence_only(
    df,
    score_bio_min = 0.85,
    score_ini_min = 0.95,
    score_final_min = 0.75,
    confidence_sirius_min = NULL,
    similarity_spectral_min = NULL,
    matched_peaks_min = NULL
  )
  expect_setequal(res$feature_id, c("F1", "F2", "F3"))
})

test_that("filter_high_confidence_only validates threshold ranges", {
  df <- tidytable::tidytable(
    feature_id = "F1",
    score_biological = 0.9,
    candidate_score_pseudo_initial = 0.9,
    score_weighted_chemo = 0.9
  )
  expect_error(
    filter_high_confidence_only(df, score_bio_min = -0.1),
    "between 0 and 1"
  )
  expect_error(
    filter_high_confidence_only(df, score_ini_min = 1.1),
    "between 0 and 1"
  )
  expect_error(
    filter_high_confidence_only(df, score_final_min = 2),
    "between 0 and 1"
  )
  expect_error(
    filter_high_confidence_only(df, error_rt_max = 0),
    "positive"
  )
})

# RT error filter (error in minutes). NA allowed
test_that("filter_high_confidence_only applies RT error filter (minutes)", {
  df <- tidytable::tidytable(
    feature_id = c("F1", "F2", "F3"),
    score_biological = c(0.9, 0.1, 0.1),
    candidate_score_pseudo_initial = c(0.1, 0.96, 0.1),
    score_weighted_chemo = c(0.1, 0.1, 0.8),
    candidate_structure_error_rt = c(0.05, 0.2, NA_real_)
  )
  result <- filter_high_confidence_only(
    df,
    score_bio_min = 0.85,
    score_ini_min = 0.95,
    score_final_min = 0.75,
    error_rt_max = 0.1,
    confidence_sirius_min = NULL,
    similarity_spectral_min = NULL,
    matched_peaks_min = NULL
  )
  # F2 should be removed due to RT error 0.2; F1 passes bio; F3 passes chemo with NA RT
  expect_setequal(result$feature_id, c("F1", "F3"))
})

# Optional SIRIUS confidence and spectral similarity filters (OR logic)
test_that("filter_high_confidence_only applies confidence and similarity with OR logic", {
  df <- tidytable::tidytable(
    feature_id = c("F1", "F2", "F3", "F4", "F5"),
    score_biological = c(0.2, 0.2, 0.2, 0.2, 0.2),
    score_weighted_chemo = c(0.9, 0.9, 0.9, 0.9, 0.9),
    candidate_score_sirius_confidence = c(0.95, 0.7, 0.99, NA_real_, 0.94),
    candidate_score_pseudo_initial = c(0.8, 0.95, 0.6, NA_real_, NA_real_)
  )
  # With confidence >= 0.9 and similarity >= 0.75 (OR logic):
  # F1: conf=0.95 ✓ OR sim=0.8 ✓ → PASS (both pass)
  # F2: conf=0.7 ✗ OR sim=0.95 ✓ → PASS (spectral passes, uses OR)
  # F3: conf=0.99 ✓ OR sim=0.6 ✗ → PASS (SIRIUS passes, uses OR)
  # F4: conf=NA ✓, sim=NA ✓ → PASS (both NA allowed)
  # F5: conf=0.95 ✓, sim=NA ✓ → PASS (SIRIUS passes, spectral NA allowed)
  res <- filter_high_confidence_only(
    df,
    confidence_sirius_min = 0.9,
    similarity_spectral_min = 0.75
  )
  expect_equal(nrow(res), 5L)
  expect_setequal(res$feature_id, c("F1", "F2", "F3", "F4", "F5"))
})

test_that("filter_high_confidence_only allows NA SIRIUS confidence when threshold set", {
  df <- tidytable::tidytable(
    feature_id = c("F1", "F2", "F3"),
    score_biological = c(0.2, 0.2, 0.2),
    score_weighted_chemo = c(0.9, 0.9, 0.9),
    candidate_score_pseudo_initial = c(0.95, 0.5, NA_real_),
    candidate_score_sirius_confidence = c(0.95, 0.5, NA_real_)
  )

  res <- filter_high_confidence_only(
    df,
    confidence_sirius_min = 0.8,
    similarity_spectral_min = NULL,
    matched_peaks_min = NULL
  )

  # F1: confidence ✓ → PASS
  # F2: confidence ✗ → FAIL
  # F3: NA ✓ → PASS (NA allowed)
  expect_setequal(res$feature_id, c("F1", "F3"))
})

test_that("filter_high_confidence_only allows NA spectral similarity when threshold set", {
  df <- tidytable::tidytable(
    feature_id = c("F1", "F2", "F3"),
    score_biological = c(0.2, 0.2, 0.2),
    score_weighted_chemo = c(0.9, 0.9, 0.9),
    candidate_score_pseudo_initial = c(0.85, 0.4, NA_real_)
  )

  res <- filter_high_confidence_only(
    df,
    similarity_spectral_min = 0.7,
    confidence_sirius_min = NULL,
    matched_peaks_min = NULL
  )

  # F1: similarity ✓ → PASS
  # F2: similarity ✗ → FAIL
  # F3: NA ✓ → PASS (NA allowed)
  expect_setequal(res$feature_id, c("F1", "F3"))
})

test_that("filter_high_confidence_only filters out spectral similarity = 0", {
  # Spectral similarity = 0 is always filtered (invalid MS2 data)
  df <- tidytable::tidytable(
    feature_id = c("Spec0", "SpecNA", "SpecValid"),
    score_biological = c(0.9, 0.9, 0.9),
    score_weighted_chemo = c(0.5, 0.5, 0.5),
    candidate_score_pseudo_initial = c(0, NA_real_, 0.85)
  )

  result <- filter_high_confidence_only(
    df,
    similarity_spectral_min = 0.5
  )

  # Spec0: similarity=0 ✗ (always filtered) → FAIL
  # SpecNA: similarity=NA ✓ → PASS
  # SpecValid: similarity=0.85 ✓ → PASS
  expect_setequal(result$feature_id, c("SpecNA", "SpecValid"))
  expect_false("Spec0" %in% result$feature_id)
})

test_that("filter_high_confidence_only SIRIUS OR spectral both fail", {
  # When both SIRIUS and spectral are below threshold, candidate fails
  df <- tidytable::tidytable(
    feature_id = c("BothLow"),
    score_biological = c(0.2),
    score_weighted_chemo = c(0.9),
    candidate_score_sirius_confidence = c(0.5),
    candidate_score_pseudo_initial = c(0.5)
  )

  result <- filter_high_confidence_only(
    df,
    confidence_sirius_min = 0.8,
    similarity_spectral_min = 0.8
  )

  # BothLow: sirius=0.5 ✗ AND spectral=0.5 ✗ → FAIL
  expect_equal(nrow(result), 0L)
})

## MS1-only hits handling ----

test_that("filter_high_confidence_only retains MS1-only hits with NA initial score", {
  # MS1-only candidates have candidate_score_pseudo_initial = NA (not 0, which means invalid)
  # They should be kept if they pass biological or final score thresholds
  df <- tidytable::tidytable(
    feature_id = c("MS1_bio", "MS1_final", "MS1_none", "MS2"),
    score_biological = c(0.9, 0.1, 0.1, 0.1),
    candidate_score_pseudo_initial = c(NA_real_, NA_real_, NA_real_, 0.98),
    score_weighted_chemo = c(0.1, 0.8, 0.1, 0.1)
  )

  result <- filter_high_confidence_only(
    df,
    score_bio_min = 0.85,
    score_ini_min = 0.95,
    score_final_min = 0.75
  )

  # MS1_bio: ini=NA (MS1-only), bio=0.9 ✓ → PASS
  # MS1_final: ini=NA (MS1-only), final=0.8 ✓ → PASS
  # MS1_none: ini=NA (MS1-only), no other score passes → FAIL
  # MS2: ini=0.98 ✓ → PASS
  expect_setequal(result$feature_id, c("MS1_bio", "MS1_final", "MS2"))
  expect_false("MS1_none" %in% result$feature_id)
})

test_that("filter_high_confidence_only removes candidates with initial score = 0 (invalid)", {
  # Score = 0 is considered invalid/missing data and should be filtered out
  df <- tidytable::tidytable(
    feature_id = c("Score0_invalid", "Score0_lowbio", "ScoreNA_MS1"),
    score_biological = c(0.9, 0.5, 0.9),
    candidate_score_pseudo_initial = c(0, 0, NA_real_),
    score_weighted_chemo = c(0.1, 0.1, 0.1)
  )

  result <- filter_high_confidence_only(
    df,
    score_bio_min = 0.85,
    score_ini_min = 0.95,
    score_final_min = 0.75,
    confidence_sirius_min = NULL,
    similarity_spectral_min = NULL,
    matched_peaks_min = NULL
  )

  # Score0_invalid: ini=0 (invalid, filtered), bio=0.9 ✓ → PASS (bio still satisfies)
  # Score0_lowbio: ini=0 (invalid, filtered), bio=0.5 ✗ → FAIL
  # ScoreNA_MS1: ini=NA (MS1-only), bio=0.9 ✓ → PASS
  expect_setequal(result$feature_id, c("Score0_invalid", "ScoreNA_MS1"))
  expect_false("Score0_lowbio" %in% result$feature_id)
})

test_that("filter_high_confidence_only: MS1-only hits fail when no score passes", {
  # MS1-only candidate with score=NA, and no other score passes threshold
  df <- tidytable::tidytable(
    feature_id = c("MS1_only"),
    score_biological = c(0.5),
    candidate_score_pseudo_initial = c(NA_real_),
    score_weighted_chemo = c(0.5)
  )

  result <- filter_high_confidence_only(
    df,
    score_bio_min = 0.85,
    score_ini_min = 0.95,
    score_final_min = 0.75
  )

  # MS1_only: bio=0.5 ✗, ini=NA (MS1-only), final=0.5 ✗ → FAIL
  expect_equal(nrow(result), 0L)
})

test_that("filter_high_confidence_only removes candidates with 0 matched peaks", {
  # Candidates with 0 matched peaks are invalid (no peaks matched)
  df <- tidytable::tidytable(
    feature_id = c("Peaks0", "PeaksNA_nodata", "Peaks5"),
    score_biological = c(0.9, 0.9, 0.9),
    candidate_score_pseudo_initial = c(0.5, 0.5, 0.5),
    score_weighted_chemo = c(0.5, 0.5, 0.5),
    candidate_count_similarity_peaks_matched = c(0, NA_integer_, 5)
  )

  result <- filter_high_confidence_only(
    df,
    matched_peaks_min = 1 # Require at least 1 peak
  )

  # Peaks0: peaks=0 ✗ (filtered) → FAIL
  # PeaksNA_nodata: peaks=NA ✓ → PASS
  # Peaks5: peaks=5 ✓ → PASS
  expect_setequal(result$feature_id, c("PeaksNA_nodata", "Peaks5"))
  expect_false("Peaks0" %in% result$feature_id)
})

test_that("filter_high_confidence_only allows NA matched peaks when threshold set", {
  df <- tidytable::tidytable(
    feature_id = c("F1", "F2", "F3"),
    score_biological = c(0.2, 0.2, 0.2),
    candidate_score_pseudo_initial = c(0.2, 0.2, 0.2),
    score_weighted_chemo = c(0.9, 0.9, 0.9),
    candidate_count_similarity_peaks_matched = c(12, 5, NA_integer_)
  )

  res <- filter_high_confidence_only(
    df,
    matched_peaks_min = 10,
    confidence_sirius_min = NULL,
    similarity_spectral_min = NULL
  )

  # F1: matched_peaks=12 ✓ → PASS
  # F2: matched_peaks=5 ✗ → FAIL (below threshold)
  # F3: NA ✓ → PASS (NA allowed)
  expect_setequal(res$feature_id, c("F1", "F3"))
})

test_that("filter_high_confidence_only filters matched peaks correctly", {
  df <- tidytable::tidytable(
    feature_id = c("F1", "F2", "F3", "F4"),
    score_biological = c(0.2, 0.2, 0.2, 0.2),
    score_weighted_chemo = c(0.9, 0.9, 0.9, 0.9),
    candidate_score_pseudo_initial = c(0.9, 0.9, 0.9, 0.9),
    candidate_count_similarity_peaks_matched = c(15, 8, 6, NA_integer_)
  )

  res <- filter_high_confidence_only(
    df,
    matched_peaks_min = 10
  )

  # F1: 15 ✓ → PASS
  # F2: 8 ✗ → FAIL
  # F3: 6 ✗ → FAIL
  # F4: NA ✓ → PASS
  expect_setequal(res$feature_id, c("F1", "F4"))
  expect_equal(nrow(res), 2L)
})

test_that("filter_high_confidence_only allows zero matched peaks threshold", {
  df <- tidytable::tidytable(
    feature_id = c("F1", "F2", "F3"),
    score_biological = c(0.2, 0.2, 0.2),
    score_weighted_chemo = c(0.9, 0.9, 0.9),
    candidate_score_pseudo_initial = c(0.9, 0.9, 0.9),
    candidate_count_similarity_peaks_matched = c(3, 0, NA_integer_)
  )

  res <- filter_high_confidence_only(
    df,
    matched_peaks_min = 0
  )

  # F1: peaks=3 ✓ → PASS
  # F2: peaks=0 ✗ (zero peaks filtered) → FAIL
  # F3: NA ✓ → PASS (NA allowed)
  expect_setequal(res$feature_id, c("F1", "F3"))
  expect_equal(nrow(res), 2L)
})

test_that("filter_high_confidence_only validates matched peaks threshold", {
  df <- tidytable::tidytable(
    feature_id = "F1",
    score_biological = 0.9,
    candidate_score_pseudo_initial = 0.9,
    score_weighted_chemo = 0.9
  )

  # Negative threshold should error
  expect_error(
    filter_high_confidence_only(df, matched_peaks_min = -1),
    "non-negative"
  )

  # Non-numeric should error
  expect_error(
    filter_high_confidence_only(df, matched_peaks_min = "five"),
    "non-negative"
  )
})

test_that("filter_high_confidence_only with all optional filters combined (OR logic)", {
  df <- tidytable::tidytable(
    feature_id = c("F1", "F2", "F3", "F4", "F5", "F6"),
    score_biological = c(0.2, 0.2, 0.2, 0.2, 0.2, 0.2),
    score_weighted_chemo = c(0.9, 0.9, 0.9, 0.9, 0.9, 0.9),
    candidate_score_sirius_confidence = as.numeric(c(
      0.95,
      0.5,
      0.99,
      0.99,
      NA,
      0.5
    )),
    candidate_score_pseudo_initial = as.numeric(c(
      0.8,
      0.95,
      0.6,
      0.85,
      NA,
      0.6
    )),
    candidate_count_similarity_peaks_matched = as.integer(c(
      12,
      15,
      15,
      8,
      12,
      12
    ))
  )

  res <- filter_high_confidence_only(
    df,
    confidence_sirius_min = 0.9,
    similarity_spectral_min = 0.75,
    matched_peaks_min = 10
  )

  # F1: conf=0.95 ✓ OR sim=0.8 ✓, peaks=12 ✓ → PASS
  # F2: conf=0.5 ✗ OR sim=0.95 ✓, peaks=15 ✓ → PASS (spectral passes)
  # F3: conf=0.99 ✓ OR sim=0.6 ✗, peaks=15 ✓ → PASS (SIRIUS passes)
  # F4: conf=0.99 ✓ OR sim=0.85 ✓, peaks=8 ✗ → FAIL (peaks below threshold)
  # F5: conf=NA ✓, sim=NA ✓, peaks=12 ✓ → PASS (both NA allowed)
  # F6: conf=0.5 ✗ OR sim=0.6 ✗ → FAIL (both below threshold)
  expect_setequal(res$feature_id, c("F1", "F2", "F3", "F5"))
  expect_equal(nrow(res), 4L)
})
