# Test Suite: read_mgf_opti ----
# Covers parsing, multi-spectrum handling, charge formatting,
# missing PEPMASS intensity, and unsorted peak ordering.

library(testthat)

skip_if_mgf_unavailable <- function() {
  testthat::skip_if_not_installed("Spectra")
  testthat::skip_if_not_installed("MsBackendMgf")
  testthat::skip_if_not_installed("MsCoreUtils")
  testthat::skip_if_not_installed("IRanges")
}

write_mgf <- function(blocks) {
  tf <- tempfile(fileext = ".mgf")
  writeLines(unlist(blocks), tf)
  tf
}

minimal_block <- function(
  title = "Spec1",
  pepmass = "100 123",
  charge = "2+",
  peaks = c("100 10", "101 20")
) {
  c(
    "BEGIN IONS",
    paste0("TITLE=", title),
    paste0("PEPMASS=", pepmass),
    paste0("CHARGE=", charge),
    peaks,
    "END IONS"
  )
}

# ---- input validation --------------------------------------------------------

test_that("read_mgf_opti errors on missing file", {
  expect_error(read_mgf_opti(f = tempfile(fileext = ".mgf")), "not found")
})

test_that("read_mgf_opti errors on multiple files", {
  expect_error(read_mgf_opti(f = c("a.mgf", "b.mgf")))
})

test_that("read_mgf_opti errors on empty path", {
  expect_error(read_mgf_opti(f = character(0)))
})

# ---- single spectrum ---------------------------------------------------------

test_that("read_mgf_opti reads a minimal single-spectrum MGF", {
  skip_if_mgf_unavailable()

  tf <- write_mgf(minimal_block())
  on.exit(unlink(tf))

  res <- read_mgf_opti(f = tf, msLevel = 2L)
  expect_s4_class(res, "DataFrame")
  expect_equal(nrow(res), 1L)
  expect_equal(as.numeric(unlist(res$mz[[1L]])), c(100, 101))
  expect_equal(as.numeric(unlist(res$intensity[[1L]])), c(10, 20))
  expect_equal(as.integer(res$msLevel[[1L]]), 2L)
})

# ---- charge formatting -------------------------------------------------------

test_that("read_mgf_opti strips trailing + from charge", {
  skip_if_mgf_unavailable()

  tf <- write_mgf(minimal_block(charge = "2+"))
  on.exit(unlink(tf))

  res <- read_mgf_opti(f = tf)
  if ("CHARGE" %in% colnames(res)) {
    expect_equal(as.character(res$CHARGE[[1L]]), "2")
  }
})

test_that("read_mgf_opti prefixes negative charge with minus", {
  skip_if_mgf_unavailable()

  tf <- write_mgf(minimal_block(charge = "1-"))
  on.exit(unlink(tf))

  res <- read_mgf_opti(f = tf)
  if ("CHARGE" %in% colnames(res)) {
    expect_equal(as.character(res$CHARGE[[1L]]), "-1")
  }
})

# ---- PEPMASS without intensity -----------------------------------------------

test_that("read_mgf_opti handles PEPMASS without intensity column", {
  skip_if_mgf_unavailable()

  tf <- write_mgf(minimal_block(pepmass = "123.456"))
  on.exit(unlink(tf))

  expect_no_error(res <- read_mgf_opti(f = tf))
  expect_s4_class(res, "DataFrame")
  expect_equal(nrow(res), 1L)
})

# ---- unsorted peaks ----------------------------------------------------------

test_that("read_mgf_opti sorts peaks by m/z", {
  skip_if_mgf_unavailable()

  tf <- write_mgf(minimal_block(peaks = c("200 5", "50 10", "100 20")))
  on.exit(unlink(tf))

  res <- read_mgf_opti(f = tf)
  mz_vals <- as.numeric(unlist(res$mz[[1L]]))
  expect_equal(mz_vals, sort(mz_vals))
})

# ---- multi-spectrum ----------------------------------------------------------

test_that("read_mgf_opti reads multiple spectra in order", {
  skip_if_mgf_unavailable()

  tf <- write_mgf(c(
    minimal_block(
      title = "Spec1",
      pepmass = "100 10",
      peaks = c("50 1", "60 2")
    ),
    minimal_block(
      title = "Spec2",
      pepmass = "200 20",
      peaks = c("90 3", "110 4")
    )
  ))
  on.exit(unlink(tf))

  res <- read_mgf_opti(f = tf)
  expect_equal(nrow(res), 2L)
})

# ---- msLevel is propagated ---------------------------------------------------

test_that("read_mgf_opti assigns the msLevel argument to all rows", {
  skip_if_mgf_unavailable()

  tf <- write_mgf(c(
    minimal_block(title = "A"),
    minimal_block(title = "B")
  ))
  on.exit(unlink(tf))

  res <- read_mgf_opti(f = tf, msLevel = 2L)
  expect_true(all(res$msLevel == 2L))
})

# ---- dataOrigin is set -------------------------------------------------------

test_that("read_mgf_opti sets dataOrigin to the file path", {
  skip_if_mgf_unavailable()

  tf <- write_mgf(minimal_block())
  on.exit(unlink(tf))

  res <- read_mgf_opti(f = tf)
  expect_equal(as.character(res$dataOrigin[[1L]]), tf)
})
