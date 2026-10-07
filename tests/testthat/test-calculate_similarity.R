# Test Suite: calculate_similarity ----

library(testthat)

## Internal Utility Helpers ----

make_spec <- function(mz, int) {
  matrix(c(mz, int), ncol = 2, dimnames = list(NULL, c("mz", "int")))
}

TEST_QUERY <- make_spec(c(100, 150, 200, 250), c(10, 200, 500, 50))
TEST_TARGET <- make_spec(c(100, 151, 199, 300), c(12, 180, 520, 40))

## Basic method tests ----

test_that("calculate_similarity works with entropy method", {
  score <- calculate_similarity(
    method = "entropy",
    query_spectrum = TEST_QUERY,
    target_spectrum = TEST_TARGET,
    query_precursor = 250,
    target_precursor = 300,
    dalton = 0.01,
    ppm = 10
  )
  expect_type(score, "double")
  expect_true(score >= 0)
})

test_that("entropy similarity is 1 for identical spectra and 0 for disjoint ones", {
  identical_score <- calculate_similarity(
    method = "entropy",
    query_spectrum = TEST_QUERY,
    target_spectrum = TEST_QUERY,
    query_precursor = 250,
    target_precursor = 250,
    dalton = 0.01,
    ppm = 10
  )
  expect_equal(identical_score, 1)

  disjoint <- calculate_similarity(
    method = "entropy",
    query_spectrum = make_spec(c(10, 20), c(1, 1)),
    target_spectrum = make_spec(c(300, 400), c(1, 1)),
    query_precursor = 1,
    target_precursor = 2,
    dalton = 0.01,
    ppm = 10
  )
  expect_equal(disjoint, 0)
})

test_that("entropy backend is called with the m/z window, noise and cleaning settings", {
  # A peak at 5100 Da is outside the configured max_mz = 5000 window, so it
  # must not contribute. Widening the window does change the score, which is
  # what pins the constant down.
  wide <- make_spec(c(100, 150, 200, 250, 5100), c(10, 200, 500, 50, 900))
  wide_t <- make_spec(c(100, 151, 199, 300, 5100), c(12, 180, 520, 40, 900))

  base <- calculate_similarity(
    "entropy",
    wide,
    wide_t,
    250,
    300,
    dalton = 0.01,
    ppm = 10
  )
  expect_true(base > 0)
  expect_lt(base, 1)

  # The 5100 pair is a perfect match that is being discarded: including it
  # raises the entropy similarity substantially.
  included <- msentropy::calculate_entropy_similarity(
    peaks_a = wide,
    peaks_b = wide_t,
    min_mz = 0,
    max_mz = 6000,
    noise_threshold = 0,
    ms2_tolerance_in_da = 0.01,
    ms2_tolerance_in_ppm = 10,
    max_peak_num = -1,
    clean_spectra = TRUE
  )
  expect_gt(included, base)

  # noise_threshold = 0 keeps every peak: raising it above the smallest
  # intensity collapses the score to 0.
  noisy <- msentropy::calculate_entropy_similarity(
    peaks_a = TEST_QUERY,
    peaks_b = TEST_TARGET,
    min_mz = 0,
    max_mz = 5000,
    noise_threshold = 1000000,
    ms2_tolerance_in_da = 0.01,
    ms2_tolerance_in_ppm = 10,
    max_peak_num = -1,
    clean_spectra = TRUE
  )
  expect_equal(noisy, 0)

  # max_peak_num = -1 keeps all peaks; capping at 2 changes the score, so a
  # truncated peak list would be visible.
  capped <- msentropy::calculate_entropy_similarity(
    peaks_a = TEST_QUERY,
    peaks_b = TEST_TARGET,
    min_mz = 0,
    max_mz = 5000,
    noise_threshold = 0,
    ms2_tolerance_in_da = 0.01,
    ms2_tolerance_in_ppm = 10,
    max_peak_num = 2,
    clean_spectra = TRUE
  )
  expect_false(isTRUE(all.equal(capped, calculate_similarity(
    "entropy",
    TEST_QUERY,
    TEST_TARGET,
    250,
    300,
    dalton = 0.01,
    ppm = 10
  ))))

  # clean_spectra = TRUE is what makes an unsorted/duplicated query usable:
  # with cleaning off the same matrix scores differently (and badly).
  dup <- make_spec(c(100, 100, 150), c(10, 500, 200))
  dup_clean <- msentropy::calculate_entropy_similarity(
    peaks_a = dup,
    peaks_b = TEST_TARGET,
    min_mz = 0,
    max_mz = 5000,
    noise_threshold = 0,
    ms2_tolerance_in_da = 0.01,
    ms2_tolerance_in_ppm = 10,
    max_peak_num = -1,
    clean_spectra = TRUE
  )
  dup_raw <- msentropy::calculate_entropy_similarity(
    peaks_a = dup,
    peaks_b = TEST_TARGET,
    min_mz = 0,
    max_mz = 5000,
    noise_threshold = 0,
    ms2_tolerance_in_da = 0.01,
    ms2_tolerance_in_ppm = 10,
    max_peak_num = -1,
    clean_spectra = FALSE
  )
  expect_false(isTRUE(all.equal(dup_clean, dup_raw)))
})

test_that("entropy backend receives the dalton and ppm tolerances unchanged", {
  q <- make_spec(c(100, 150, 200, 250), c(10, 200, 500, 50))
  t <- make_spec(c(100.001, 150.001, 200.001, 250.001), c(10, 200, 500, 50))
  # 1 mDa offsets: matched at 0.005 Da, not at 0.0001 Da.
  loose <- calculate_similarity(
    "entropy",
    q,
    t,
    250,
    250,
    dalton = 0.005,
    ppm = 10
  )
  tight <- calculate_similarity(
    "entropy",
    q,
    t,
    250,
    250,
    dalton = 0.0001,
    ppm = 0
  )
  expect_equal(loose, 1)
  expect_lt(tight, loose)
})

test_that("gnps returns the full score/matches/forward/reverse vector", {
  res <- calculate_similarity(
    method = "gnps",
    query_spectrum = TEST_QUERY,
    target_spectrum = TEST_TARGET,
    query_precursor = 250,
    target_precursor = 300,
    dalton = 0.01,
    ppm = 10,
    return_matched_peaks = TRUE
  )
  expect_length(res, 4L)
  # Identical spectra: perfect score, every peak matched, forward == reverse.
  same <- calculate_similarity(
    method = "gnps",
    query_spectrum = TEST_QUERY,
    target_spectrum = TEST_QUERY,
    query_precursor = 250,
    target_precursor = 250,
    dalton = 0.01,
    ppm = 10,
    return_matched_peaks = TRUE
  )
  expect_equal(same, c(1, nrow(TEST_QUERY), 1, 1))
})

test_that("gnps and cosine score-only output is a bare score, not a vector", {
  for (m in c("gnps", "cosine")) {
    res <- calculate_similarity(
      method = m,
      query_spectrum = TEST_QUERY,
      target_spectrum = TEST_TARGET,
      query_precursor = 250,
      target_precursor = 300,
      dalton = 0.01,
      ppm = 10
    )
    expect_length(res, 1L)
    expect_type(res, "double")
    expect_true(res >= 0 && res <= 1)
  }
})

test_that("entropy dalton and ppm tolerances are not interchangeable", {
  # The Da and the ppm tolerances enter separately, so their roles must not be
  # swapped. A 0.05 Da offset at m/z 100-200 is 250-500 ppm away: reachable
  # with a generous ppm window, but not with a generous Da window alone.
  q <- make_spec(c(100, 200), c(10, 20))
  t_off <- make_spec(c(100.05, 200.05), c(10, 20))
  call <- function(da, ppm) {
    calculate_similarity(
      "entropy",
      q,
      t_off,
      200,
      200,
      dalton = da,
      ppm = ppm
    )
  }

  # ppm carries the matching (500 ppm at 100 Da = 0.05 Da).
  expect_equal(call(0, 500), 1)
  # Da carries the matching.
  expect_equal(call(0.05, 0), 1)
  # Neither alone is enough.
  expect_equal(call(0, 100), 0)
  expect_equal(call(0.01, 0), 0)
  # Swapping the two arguments would give 500 Da / 0.05 ppm, which still
  # matches here, so also pin the asymmetric pair where it cannot.
  expect_equal(call(0.01, 500), 1)
  expect_equal(call(500, 0.01), 0)
})

test_that("gnps and cosine report distinct backend failure messages", {
  # The gnps and cosine branches share the computation but not the log
  # message, which is what distinguishes them at runtime.
  msgs <- character()
  collect <- function(...) {
    msgs <<- c(msgs, paste0(..., collapse = ""))
    invisible(NULL)
  }

  with_mocked_bindings(
    .gnps_chain_dp_call = function(...) stop("backend boom"),
    log_warn = collect,
    calculate_similarity(
      method = "gnps",
      query_spectrum = TEST_QUERY,
      target_spectrum = TEST_TARGET,
      query_precursor = 250,
      target_precursor = 300,
      dalton = 0.01,
      ppm = 10
    )
  )
  with_mocked_bindings(
    .gnps_chain_dp_call = function(...) stop("backend boom"),
    log_warn = collect,
    calculate_similarity(
      method = "cosine",
      query_spectrum = TEST_QUERY,
      target_spectrum = TEST_TARGET,
      query_precursor = 250,
      target_precursor = 300,
      dalton = 0.01,
      ppm = 10
    )
  )
  with_mocked_bindings(
    .entropy_similarity_call = function(...) stop("entropy boom"),
    log_warn = collect,
    calculate_similarity(
      method = "entropy",
      query_spectrum = TEST_QUERY,
      target_spectrum = TEST_TARGET,
      query_precursor = 250,
      target_precursor = 300,
      dalton = 0.01,
      ppm = 10
    )
  )

  expect_length(msgs, 3L)
  expect_match(msgs[1], "GNPS")
  expect_match(msgs[2], "Similarity")
  expect_match(msgs[3], "Entropy")
})

test_that("gnps matching widens with the dalton tolerance", {
  q <- make_spec(c(100, 200, 300, 400), c(100, 100, 100, 100))
  t <- make_spec(c(100.005, 200.005, 300.005, 400.005), c(100, 100, 100, 100))

  wide <- calculate_similarity(
    "gnps",
    q,
    t,
    400,
    400,
    dalton = 0.01,
    ppm = 10,
    return_matched_peaks = TRUE
  )
  tight <- calculate_similarity(
    "gnps",
    q,
    t,
    400,
    400,
    dalton = 0.0001,
    ppm = 0,
    return_matched_peaks = TRUE
  )
  expect_equal(wide[2L], 4L)
  expect_lt(tight[2L], wide[2L])
})

test_that("gnps method does not silently take the entropy path", {
  # The entropy and the chain-DP scores of these spectra differ, so a mutant
  # that reroutes "gnps" onto the cosine/entropy branch is visible.
  q <- make_spec(c(100, 150, 200, 250, 5100), c(10, 200, 500, 50, 900))
  t <- make_spec(c(100, 151, 199, 300, 5100), c(12, 180, 520, 40, 900))
  gnps_score <- calculate_similarity(
    "gnps",
    q,
    t,
    250,
    300,
    dalton = 0.01,
    ppm = 10
  )
  entropy_score <- calculate_similarity(
    "entropy",
    q,
    t,
    250,
    300,
    dalton = 0.01,
    ppm = 10
  )
  expect_false(isTRUE(all.equal(gnps_score, entropy_score)))
})

test_that("calculate_similarity works with gnps method returning matches", {
  res <- calculate_similarity(
    method = "gnps",
    query_spectrum = TEST_QUERY,
    target_spectrum = TEST_TARGET,
    query_precursor = 250,
    target_precursor = 300,
    dalton = 0.01,
    ppm = 10,
    return_matched_peaks = TRUE
  )
  expect_type(res[1L], "double")
  expect_true(res[2L] >= 0)
})

test_that("calculate_similarity works with cosine method", {
  score <- calculate_similarity(
    method = "cosine",
    query_spectrum = TEST_QUERY,
    target_spectrum = TEST_TARGET,
    query_precursor = 250,
    target_precursor = 300,
    dalton = 0.01,
    ppm = 10
  )
  expect_type(score, "double")
})

## Validation tests ----

test_that("calculate_similarity validates method", {
  expect_error(
    calculate_similarity(
      method = "foo",
      query_spectrum = TEST_QUERY,
      target_spectrum = TEST_TARGET,
      query_precursor = 1,
      target_precursor = 2,
      dalton = 0.01,
      ppm = 10
    ),
    "Invalid method"
  )
})

test_that("calculate_similarity returns 0 for empty spectra", {
  empty <- matrix(numeric(), ncol = 2)
  # Empty *query* and empty *target* must behave the same.
  for (m in c("entropy", "gnps", "cosine")) {
    for (rmp in c(FALSE, TRUE)) {
      res_q <- calculate_similarity(
        method = m,
        query_spectrum = empty,
        target_spectrum = TEST_TARGET,
        query_precursor = 1,
        target_precursor = 2,
        dalton = 0.01,
        ppm = 10,
        return_matched_peaks = rmp
      )
      res_t <- calculate_similarity(
        method = m,
        query_spectrum = TEST_QUERY,
        target_spectrum = empty,
        query_precursor = 1,
        target_precursor = 2,
        dalton = 0.01,
        ppm = 10,
        return_matched_peaks = rmp
      )
      expected <- if (rmp) c(0, 0) else 0
      expect_equal(res_q, expected)
      expect_equal(res_t, expected)
    }
  }
})

test_that("calculate_similarity short-circuits empty spectra before any backend", {
  # The zero-return guard must short-circuit: reaching the backend with a
  # 0-row matrix would raise, and the mocked backend records the call.
  called <- FALSE
  empty <- matrix(numeric(), ncol = 2)

  with_mocked_bindings(
    .entropy_similarity_call = function(...) {
      called <<- TRUE
      stop("backend must not be called")
    },
    res <- calculate_similarity(
      method = "entropy",
      query_spectrum = empty,
      target_spectrum = TEST_TARGET,
      query_precursor = 1,
      target_precursor = 2,
      dalton = 0.01,
      ppm = 10
    )
  )
  expect_false(called)
  expect_equal(res, 0)
})

test_that("calculate_similarity rejects negative tolerances", {
  expect_error(
    calculate_similarity(
      method = "cosine",
      query_spectrum = TEST_QUERY,
      target_spectrum = TEST_TARGET,
      query_precursor = 1,
      target_precursor = 2,
      dalton = -0.01,
      ppm = 10
    ),
    "must be between"
  )
  # ppm is bounded from below just like dalton.
  expect_error(
    calculate_similarity(
      method = "cosine",
      query_spectrum = TEST_QUERY,
      target_spectrum = TEST_TARGET,
      query_precursor = 1,
      target_precursor = 2,
      dalton = 0.01,
      ppm = -10
    ),
    "must be between"
  )
  # Zero is a legal tolerance for both.
  expect_no_error(calculate_similarity(
    method = "cosine",
    query_spectrum = TEST_QUERY,
    target_spectrum = TEST_TARGET,
    query_precursor = 1,
    target_precursor = 2,
    dalton = 0,
    ppm = 0
  ))
})

test_that("calculate_similarity rejects non-scalar tolerances", {
  expect_error(
    calculate_similarity(
      method = "cosine",
      query_spectrum = TEST_QUERY,
      target_spectrum = TEST_TARGET,
      query_precursor = 1,
      target_precursor = 2,
      dalton = c(0.01, 0.02),
      ppm = 10
    ),
    class = "tima_validation_error"
  )
  expect_error(
    calculate_similarity(
      method = "cosine",
      query_spectrum = TEST_QUERY,
      target_spectrum = TEST_TARGET,
      query_precursor = 1,
      target_precursor = 2,
      dalton = 0.01,
      ppm = c(10, 20)
    ),
    class = "tima_validation_error"
  )
  expect_error(
    calculate_similarity(
      method = "cosine",
      query_spectrum = TEST_QUERY,
      target_spectrum = TEST_TARGET,
      query_precursor = 1,
      target_precursor = 2,
      dalton = 0.01,
      ppm = "10"
    ),
    class = "tima_validation_error"
  )
})

test_that("calculate_similarity rejects a non-flag return_matched_peaks", {
  expect_error(
    calculate_similarity(
      method = "cosine",
      query_spectrum = TEST_QUERY,
      target_spectrum = TEST_TARGET,
      query_precursor = 1,
      target_precursor = 2,
      dalton = 0.01,
      ppm = 10,
      return_matched_peaks = "yes"
    ),
    class = "tima_validation_error"
  )
})

test_that("calculate_similarity validates matrix inputs and column counts", {
  expect_error(
    calculate_similarity(
      method = "cosine",
      query_spectrum = as.list(TEST_QUERY),
      target_spectrum = TEST_TARGET,
      query_precursor = 1,
      target_precursor = 2,
      dalton = 0.01,
      ppm = 10
    ),
    "must be matrices",
    class = "tima_validation_error"
  )

  one_col <- matrix(c(100, 200), ncol = 1)
  # Both the query *and* the target column count are checked.
  expect_error(
    calculate_similarity(
      method = "cosine",
      query_spectrum = one_col,
      target_spectrum = TEST_TARGET,
      query_precursor = 1,
      target_precursor = 2,
      dalton = 0.01,
      ppm = 10
    ),
    "at least 2 columns",
    class = "tima_validation_error"
  )
  expect_error(
    calculate_similarity(
      method = "cosine",
      query_spectrum = TEST_QUERY,
      target_spectrum = one_col,
      query_precursor = 1,
      target_precursor = 2,
      dalton = 0.01,
      ppm = 10
    ),
    "at least 2 columns",
    class = "tima_validation_error"
  )
  # A 2-column spectrum is fine; the check is a strict "fewer than two".
  expect_no_error(calculate_similarity(
    method = "cosine",
    query_spectrum = TEST_QUERY,
    target_spectrum = TEST_TARGET,
    query_precursor = 1,
    target_precursor = 2,
    dalton = 0.01,
    ppm = 10
  ))
})

test_that("calculate_similarity returns safe fallbacks when backends error", {
  expect_equal(
    with_mocked_bindings(
      .entropy_similarity_call = function(...) stop("entropy fail"),
      log_warn = function(...) invisible(NULL),
      calculate_similarity(
        method = "entropy",
        query_spectrum = TEST_QUERY,
        target_spectrum = TEST_TARGET,
        query_precursor = 250,
        target_precursor = 300,
        dalton = 0.01,
        ppm = 10
      )
    ),
    0
  )

  expect_equal(
    with_mocked_bindings(
      .entropy_similarity_call = function(...) stop("entropy fail"),
      log_warn = function(...) invisible(NULL),
      calculate_similarity(
        method = "entropy",
        query_spectrum = TEST_QUERY,
        target_spectrum = TEST_TARGET,
        query_precursor = 250,
        target_precursor = 300,
        dalton = 0.01,
        ppm = 10,
        return_matched_peaks = TRUE
      )
    ),
    c(0, 0)
  )

  expect_equal(
    with_mocked_bindings(
      .gnps_chain_dp_call = function(...) stop("gnps fail"),
      log_warn = function(...) invisible(NULL),
      calculate_similarity(
        method = "gnps",
        query_spectrum = TEST_QUERY,
        target_spectrum = TEST_TARGET,
        query_precursor = 250,
        target_precursor = 300,
        dalton = 0.01,
        ppm = 10,
        return_matched_peaks = TRUE
      )
    ),
    c(0, 0, 0, 0)
  )

  expect_equal(
    with_mocked_bindings(
      .gnps_chain_dp_call = function(...) c(0.75, 2, 0.7, 0.8),
      log_warn = function(...) invisible(NULL),
      calculate_similarity(
        method = "cosine",
        query_spectrum = TEST_QUERY,
        target_spectrum = TEST_TARGET,
        query_precursor = 250,
        target_precursor = 300,
        dalton = 0.01,
        ppm = 10,
        return_matched_peaks = TRUE
      )
    ),
    c(0.75, 2, 0.7, 0.8)
  )

  expect_equal(
    with_mocked_bindings(
      .gnps_chain_dp_call = function(...) stop("score fail"),
      log_warn = function(...) invisible(NULL),
      calculate_similarity(
        method = "cosine",
        query_spectrum = TEST_QUERY,
        target_spectrum = TEST_TARGET,
        query_precursor = 250,
        target_precursor = 300,
        dalton = 0.01,
        ppm = 10,
        return_matched_peaks = TRUE
      )
    ),
    c(0, 0, 0, 0)
  )
})

# test_that(
#   skip("Not implemented")
# )
# test_that("calculate_similarity rejects non-matrix input", {
#   expect_error(
#     calculate_similarity(
#       method = "cosine",
#       query_spectrum = as.list(TEST_QUERY),
#       target_spectrum = TEST_TARGET,
#       query_precursor = 1,
#       target_precursor = 2,
#       dalton = 0.01,
#       ppm = 10
#     ),
#     "Spectra must be matrices"
#   )
# })
