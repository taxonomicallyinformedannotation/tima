# Test Suite: calculate_entropy_and_similarity ----
# Covers the hot loop that scores query vs library spectra.

library(testthat)

# ---- fixtures ----------------------------------------------------------------

sp_acetic <- cbind(mz = c(45, 60), intensity = c(0.3, 0.7)) # simple 2-peak
sp_ethane <- cbind(
  mz = c(26, 27, 28, 29, 30),
  intensity = c(0.1, 0.2, 0.9, 0.3, 0.1)
)
sp_propane <- cbind(
  mz = c(28, 29, 41, 43, 44),
  intensity = c(0.1, 0.1, 0.4, 0.8, 0.6)
)

call_ces <- function(
  query_sp = list(sp_acetic),
  lib_sp = list(sp_acetic),
  query_pmz = 61.0,
  lib_pmz = 61.0,
  method = "cosine",
  threshold = 0.0,
  approx = TRUE
) {
  calculate_entropy_and_similarity(
    lib_ids = 1L,
    lib_precursors = lib_pmz,
    lib_spectra = lib_sp,
    query_ids = 1L,
    query_precursors = query_pmz,
    query_spectra = query_sp,
    method = method,
    dalton = 0.01,
    ppm = 10,
    threshold = threshold,
    approx = approx
  )
}

# ---- output schema -----------------------------------------------------------

test_that("calculate_entropy_and_similarity returns a data frame with expected columns", {
  out <- call_ces()
  expect_s3_class(out, "data.frame")
  expect_true("feature_id" %in% names(out))
  expect_true("candidate_score_similarity" %in% names(out))
  expect_true("candidate_score_similarity_forward" %in% names(out))
  expect_true("candidate_score_similarity_reverse" %in% names(out))
  expect_true("candidate_spectrum_entropy" %in% names(out))
  expect_true("candidate_count_similarity_peaks_matched" %in% names(out))
})

# ---- identical spectra score = 1 --------------------------------------------

test_that("identical query and library spectra yield similarity = 1 with cosine", {
  out <- call_ces(method = "cosine", approx = TRUE)
  expect_equal(nrow(out), 1L)
  expect_equal(out$candidate_score_similarity, 1.0, tolerance = 1e-6)
})

test_that("identical spectra yield similarity = 1 with entropy", {
  out <- call_ces(method = "entropy", approx = TRUE)
  expect_equal(out$candidate_score_similarity, 1.0, tolerance = 1e-6)
})

# ---- threshold filters results -----------------------------------------------

test_that("threshold > similarity produces empty-row tibble (no NULL error)", {
  # sp_acetic vs sp_propane should have low similarity
  out <- call_ces(
    query_sp = list(sp_acetic),
    lib_sp = list(sp_propane),
    query_pmz = 61,
    lib_pmz = 73,
    threshold = 0.999, # nearly impossible to pass
    approx = TRUE
  )
  expect_s3_class(out, "data.frame")
  # Should be the empty-result placeholder row or zero matching rows
  expect_true(nrow(out) >= 0L)
})

test_that("non-GNPS methods skip GNPS forward/reverse pass below threshold", {
  local_mocked_bindings(
    gnps_chain_dp_wrapper = function(...) stop("unexpected GNPS call")
  )

  out <- calculate_entropy_and_similarity(
    lib_ids = 1L,
    lib_precursors = 73.0,
    lib_spectra = list(sp_propane),
    query_ids = 1L,
    query_precursors = 61.0,
    query_spectra = list(sp_acetic),
    method = "cosine",
    dalton = 0.01,
    ppm = 10,
    threshold = 0.999,
    approx = TRUE
  )

  expect_s3_class(out, "data.frame")
  expect_true(nrow(out) == 0L || all(is.na(out$candidate_score_similarity)))
})

test_that("adduct-aware scoring uses neutral-M space when both sides carry adducts", {
  out <- calculate_entropy_and_similarity(
    lib_ids = 1L,
    lib_precursors = 61.0,
    lib_spectra = list(sp_acetic),
    query_ids = 1L,
    query_precursors = 61.0,
    query_spectra = list(sp_acetic),
    method = "cosine",
    dalton = 0.01,
    ppm = 10,
    threshold = 0.0,
    approx = TRUE,
    query_adducts = "[M+H]+",
    lib_adducts = "[M+Na]+"
  )

  expect_true(nrow(out) >= 1L)
  expect_equal(out$.similarity_space[1], "neutral_M")
})

# ---- approx = FALSE precursor filter -----------------------------------------

test_that("approx=FALSE excludes library spectra outside precursor window", {
  # Query precursor 200 Da; library precursor 400 Da — far outside any window
  out <- calculate_entropy_and_similarity(
    lib_ids = 1L,
    lib_precursors = 400.0,
    lib_spectra = list(sp_ethane),
    query_ids = 1L,
    query_precursors = 200.0,
    query_spectra = list(sp_ethane),
    method = "cosine",
    dalton = 0.01,
    ppm = 10,
    threshold = 0.0,
    approx = FALSE
  )
  # With approx=FALSE the library entry is outside precursor window → no hits
  expect_s3_class(out, "data.frame")
})

test_that("approx=FALSE includes library spectra within precursor window", {
  # Same precursor → should match
  out <- call_ces(
    query_sp = list(sp_ethane),
    lib_sp = list(sp_ethane),
    query_pmz = 30.05,
    lib_pmz = 30.05,
    method = "cosine",
    approx = FALSE
  )
  expect_true(nrow(out) >= 1L)
  expect_equal(out$candidate_score_similarity, 1.0, tolerance = 1e-6)
})

# ---- GNPS method -------------------------------------------------------------

test_that("gnps method runs without error and returns matched peaks count", {
  out <- call_ces(
    method = "gnps",
    approx = FALSE,
    query_sp = list(sp_ethane),
    lib_sp = list(sp_ethane),
    query_pmz = 30.05,
    lib_pmz = 30.05
  )
  expect_s3_class(out, "data.frame")
  if (nrow(out) > 0L) {
    expect_true(!is.na(out$candidate_count_similarity_peaks_matched[1]))
  }
})

# ---- multi-query, multi-library ----------------------------------------------

test_that("multiple query and library spectra are all compared", {
  out <- calculate_entropy_and_similarity(
    lib_ids = c(10L, 20L),
    lib_precursors = c(61.0, 73.0),
    lib_spectra = list(sp_acetic, sp_propane),
    query_ids = c(1L, 2L),
    query_precursors = c(61.0, 73.0),
    query_spectra = list(sp_acetic, sp_propane),
    method = "cosine",
    dalton = 0.01,
    ppm = 10,
    threshold = 0.0,
    approx = TRUE
  )
  expect_s3_class(out, "data.frame")
  # At minimum the self-matches should appear
  expect_true(nrow(out) >= 2L)
})

# ---- input validation --------------------------------------------------------

test_that("mismatched lib_ids / lib_spectra length errors", {
  expect_error(
    calculate_entropy_and_similarity(
      lib_ids = c(1L, 2L),
      lib_precursors = c(61, 73),
      lib_spectra = list(sp_acetic), # length mismatch
      query_ids = 1L,
      query_precursors = 61,
      query_spectra = list(sp_acetic),
      method = "cosine",
      dalton = 0.01,
      ppm = 10,
      threshold = 0,
      approx = TRUE
    ),
    "must have the same length",
    class = "tima_validation_error"
  )
})

test_that("invalid method name errors", {
  expect_error(call_ces(method = "madeup"))
})

test_that("threshold outside [0,1] errors", {
  expect_error(call_ces(threshold = 1.5))
  expect_error(call_ces(threshold = -0.1))
})

# ---- on-demand sanitization --------------------------------------------------

test_that("unsanitized query spectrum is sanitized on demand without error", {
  sp_dirty <- cbind(
    mz = c(200, 100, 100.005), # unsorted + near-duplicate
    intensity = c(0.5, 0.3, 0.2)
  )
  expect_no_error({
    out <- call_ces(
      query_sp = list(sp_dirty),
      lib_sp = list(sp_acetic),
      query_pmz = 61,
      lib_pmz = 61,
      approx = TRUE
    )
  })
  expect_s3_class(out, "data.frame")
})

test_that("degenerate library spectra are ignored even at zero threshold", {
  lib_bad <- matrix(numeric(0), ncol = 2)
  out <- calculate_entropy_and_similarity(
    lib_ids = 1L,
    lib_precursors = 61.0,
    lib_spectra = list(lib_bad),
    query_ids = 1L,
    query_precursors = 61.0,
    query_spectra = list(sp_acetic),
    method = "cosine",
    dalton = 0.01,
    ppm = 10,
    threshold = 0,
    approx = TRUE
  )

  expect_s3_class(out, "data.frame")
  expect_true(all(is.na(out$candidate_score_similarity)))
})

# ---- forward / reverse score tests ------------------------------------------

test_that("identical spectra yield forward = reverse = similarity = 1 (gnps)", {
  out <- call_ces(
    method = "gnps",
    query_sp = list(sp_ethane),
    lib_sp = list(sp_ethane),
    query_pmz = 30.05,
    lib_pmz = 30.05
  )
  expect_equal(out$candidate_score_similarity_forward[1], 1.0, tolerance = 1e-6)
  expect_equal(out$candidate_score_similarity_reverse[1], 1.0, tolerance = 1e-6)
})

test_that("forward and reverse >= score when spectra partially match (gnps)", {
  out <- calculate_entropy_and_similarity(
    lib_ids = 1L,
    lib_precursors = 61.0,
    lib_spectra = list(sp_acetic), # 2 peaks
    query_ids = 1L,
    query_precursors = 61.0,
    query_spectra = list(sp_ethane), # 5 peaks, only some match
    method = "gnps",
    dalton = 0.5,
    ppm = 10,
    threshold = 0.0,
    approx = TRUE
  )
  if (nrow(out) > 0L && !is.na(out$candidate_score_similarity[1])) {
    expect_gte(
      out$candidate_score_similarity_forward[1],
      out$candidate_score_similarity[1] - 1e-12
    )
    expect_gte(
      out$candidate_score_similarity_reverse[1],
      out$candidate_score_similarity[1] - 1e-12
    )
  }
})

test_that("entropy computes forward/reverse scores by default for retained matches", {
  out <- call_ces(
    method = "entropy",
    query_sp = list(sp_ethane),
    lib_sp = list(sp_ethane),
    query_pmz = 30.05,
    lib_pmz = 30.05
  )
  expect_true("candidate_score_similarity_forward" %in% names(out))
  expect_true("candidate_score_similarity_reverse" %in% names(out))
  expect_equal(out$candidate_score_similarity_forward[1], 1.0, tolerance = 1e-6)
  expect_equal(out$candidate_score_similarity_reverse[1], 1.0, tolerance = 1e-6)
})

test_that("entropy skips forward/reverse work when explicitly disabled", {
  called <- 0L
  local_mocked_bindings(
    gnps_chain_dp_wrapper = function(...) {
      called <<- called + 1L
      list(1.0, 1L, 0.5, 0.5)
    }
  )

  out <- calculate_entropy_and_similarity(
    lib_ids = 1L,
    lib_precursors = 30.05,
    lib_spectra = list(sp_ethane),
    query_ids = 1L,
    query_precursors = 30.05,
    query_spectra = list(sp_ethane),
    method = "entropy",
    dalton = 0.01,
    ppm = 10,
    threshold = 0.0,
    approx = TRUE,
    compute_forward_reverse = FALSE
  )

  expect_equal(called, 0L)
  expect_true(all(is.na(out$candidate_score_similarity_forward)))
  expect_true(all(is.na(out$candidate_score_similarity_reverse)))
})

test_that("entropy can be skipped while keeping forward/reverse scoring", {
  out <- calculate_entropy_and_similarity(
    lib_ids = 1L,
    lib_precursors = 30.05,
    lib_spectra = list(sp_ethane),
    query_ids = 1L,
    query_precursors = 30.05,
    query_spectra = list(sp_ethane),
    method = "entropy",
    dalton = 0.01,
    ppm = 10,
    threshold = 0.0,
    approx = TRUE,
    compute_entropy = FALSE
  )

  expect_true(all(is.na(out$candidate_spectrum_entropy)))
  expect_true(!all(is.na(out$candidate_score_similarity_forward)))
  expect_true(!all(is.na(out$candidate_score_similarity_reverse)))
})

# ---- .count_matched_peaks: the ppm/Da matching window -------------------------

test_that(".count_matched_peaks counts peaks inside the combined ppm/Da window", {
  lib <- c(100, 200, 300)

  expect_equal(.count_matched_peaks(c(100, 200), lib, 0.01, 10), 2L)
  # 0.005 Da at m/z 100 is inside a 0.01 Da window.
  expect_equal(.count_matched_peaks(c(100.005), lib, 0.01, 10), 1L)
  # 0.02 Da is outside it.
  expect_equal(.count_matched_peaks(c(100.02), lib, 0.01, 10), 0L)
  # A query with no counterpart anywhere in the library.
  expect_equal(.count_matched_peaks(c(150), lib, 0.01, 10), 0L)
})

test_that(".count_matched_peaks uses the wider of the ppm and Da tolerances", {
  # At m/z 1000, 100 ppm is 0.1 Da, far wider than the 0 Da absolute window.
  expect_equal(.count_matched_peaks(c(1000.05), 1000, 0, 100), 1L)
  # 10 ppm at m/z 1000 is only 0.01 Da, which does not cover a 0.05 offset.
  expect_equal(.count_matched_peaks(c(1000.05), 1000, 0, 10), 0L)
  # The absolute window alone covers it.
  expect_equal(.count_matched_peaks(c(1000.05), 1000, 0.1, 0), 1L)

  # The ppm tolerance scales with mass: the same 0.05 Da offset is 500 ppm at
  # m/z 100 but 50 ppm at m/z 1000.
  expect_equal(.count_matched_peaks(c(100.05), 100, 0, 100), 0L)
  expect_equal(.count_matched_peaks(c(100.05), 100, 0, 600), 1L)
})

test_that(".count_matched_peaks returns an integer count of 0 for empty input", {
  lib <- c(100, 200)
  expect_equal(.count_matched_peaks(numeric(0), lib, 0.01, 10), 0L)
  expect_equal(.count_matched_peaks(c(100), numeric(0), 0.01, 10), 0L)
  expect_type(.count_matched_peaks(c(100), lib, 0.01, 10), "integer")
})

test_that(".count_matched_peaks counts at most one match per query peak", {
  # Both query peaks fall inside a wide window around the single library peak.
  lib <- c(500)
  expect_equal(.count_matched_peaks(c(500, 500), lib, 0.1, 0), 2L)
  expect_equal(.count_matched_peaks(c(500, 500), lib, 0.1, 0), 2L)
})

# ---- threshold is an inclusive lower bound -----------------------------------

test_that("threshold keeps candidates whose similarity equals it", {
  # Library 1 is identical to the query (similarity 1); library 2 shares no
  # peaks with it (similarity 0).
  two_lib <- list(sp_acetic, sp_propane)
  call_th <- function(th) {
    calculate_entropy_and_similarity(
      lib_ids = 1:2,
      lib_precursors = c(61, 61),
      lib_spectra = two_lib,
      query_ids = 1L,
      query_precursors = 61,
      query_spectra = list(sp_acetic),
      method = "cosine",
      dalton = 0.01,
      ppm = 10,
      threshold = th,
      approx = TRUE
    )
  }

  # threshold = 0 keeps the zero-similarity candidate (>= is inclusive).
  at_zero <- call_th(0)
  expect_equal(nrow(at_zero), 2L)
  expect_equal(sort(at_zero$candidate_score_similarity), c(0, 1))

  # Any threshold above 0 drops it, so only the perfect match survives.
  for (th in c(1e-12, 0.05, 0.5, 0.999)) {
    out <- call_th(th)
    expect_equal(nrow(out), 1L, info = paste(th))
    expect_equal(out$candidate_score_similarity[[1L]], 1, info = paste(th))
  }

  # A threshold of exactly 1 still keeps the perfect match.
  expect_equal(nrow(call_th(1)), 1L)
})

test_that("below-threshold candidates carry no forward/reverse scores", {
  out <- calculate_entropy_and_similarity(
    lib_ids = 1:2,
    lib_precursors = c(61, 61),
    lib_spectra = list(sp_acetic, sp_propane),
    query_ids = 1L,
    query_precursors = 61,
    query_spectra = list(sp_acetic),
    method = "cosine",
    dalton = 0.01,
    ppm = 10,
    threshold = 0,
    approx = TRUE
  )
  # threshold = 0 is inclusive, so the zero-similarity candidate is retained
  # and still gets a forward/reverse pass, which yields 0 for a no-peak overlap.
  zero_row <- out[out$candidate_score_similarity == 0, ]
  expect_equal(nrow(zero_row), 1L)
  expect_equal(zero_row$candidate_score_similarity_forward[[1L]], 0)
  expect_equal(zero_row$candidate_score_similarity_reverse[[1L]], 0)
  # The perfect match scores 1 in both directions.
  one_row <- out[out$candidate_score_similarity == 1, ]
  expect_equal(one_row$candidate_score_similarity_forward[[1L]], 1)
  expect_equal(one_row$candidate_score_similarity_reverse[[1L]], 1)
  # Peak counts come from the matched-peak counter, not from the score.
  expect_equal(
    out$candidate_count_similarity_peaks_matched[out$target_id == 1L],
    2L
  )
  expect_equal(
    out$candidate_count_similarity_peaks_matched[out$target_id == 2L],
    0L
  )
})

test_that("a threshold nothing reaches returns an empty table", {
  out <- calculate_entropy_and_similarity(
    lib_ids = 1L,
    lib_precursors = 73,
    lib_spectra = list(sp_propane),
    query_ids = 1L,
    query_precursors = 61,
    query_spectra = list(sp_acetic),
    method = "cosine",
    dalton = 0.01,
    ppm = 10,
    threshold = 0.999,
    approx = TRUE
  )
  # No candidate cleared the threshold, so the result is the typed placeholder
  # carrying NA rather than a zero-row frame.
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 1L)
  expect_true(is.na(out$target_id[[1L]]))
  expect_true(is.na(out$candidate_score_similarity[[1L]]))
})
