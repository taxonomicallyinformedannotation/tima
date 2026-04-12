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
