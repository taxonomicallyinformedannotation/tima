# Test Suite: read_mgf_opti ----

library(testthat)


test_that("read_mgf_opti validates file path", {
  expect_error(read_mgf_opti(f = "missing.mgf"), "not found")
})

test_that("read_mgf_opti fails with multiple files", {
  expect_error(
    read_mgf_opti(f = c("foo", "bar"))
  )
})


test_that("read_mgf_opti reads minimal mgf", {
  testthat::skip_if_not_installed(pkg = "Spectra")
  testthat::skip_if_not_installed(pkg = "MsBackendMgf")
  testthat::skip_if_not_installed(pkg = "MsCoreUtils")
  testthat::skip_if_not_installed(pkg = "IRanges")

  mgf <- c(
    "BEGIN IONS",
    "TITLE=Spec1",
    "PEPMASS=100 123",
    "CHARGE=2+",
    "100 10",
    "101 20",
    "END IONS"
  )
  tf <- tempfile(fileext = ".mgf")
  on.exit(unlink(tf), add = TRUE)
  writeLines(mgf, tf)

  res <- read_mgf_opti(f = tf, msLevel = 2L)
  expect_s4_class(res, "DataFrame")
  expect_equal(nrow(res), 1L)
  expect_true("mz" %in% colnames(res))
  expect_true("intensity" %in% colnames(res))
  expect_equal(as.numeric(unlist(res$mz[[1]])), c(100, 101))
  expect_equal(as.numeric(unlist(res$intensity[[1]])), c(10, 20))
  # CHARGE formatting
  if ("CHARGE" %in% colnames(res)) {
    expect_equal(as.character(res$CHARGE[[1]]), "2")
  }
  expect_true(all(res$msLevel == 2L))
})
