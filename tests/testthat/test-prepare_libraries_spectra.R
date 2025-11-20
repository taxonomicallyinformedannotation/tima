# Test Suite: prepare_libraries_spectra ----

library(testthat)

.test_mgf <- function(path, pepmass = 100, charge = "1+", peaks = c(50, 75, 100)) {
  lines <- c(
    "BEGIN IONS",
    paste0("PEPMASS=", pepmass),
    paste0("CHARGE=", charge),
    paste(sapply(peaks, function(m) paste(m, 10)), collapse = "\n"),
    "END IONS"
  )
  writeLines(lines, path)
  path
}

test_that("prepare_libraries_spectra returns empty libs when input missing", {
  local_test_project(copy = TRUE)
  # Force nonexistent input
  out <- prepare_libraries_spectra(input = c("missing1.mgf", "missing2.mgf"), nam_lib = "testlib")
  expect_true(file.exists(out[["pos"]]))
  expect_true(file.exists(out[["neg"]]))
  # SOP TSV
  expect_true(file.exists(out[["sop"]]))
  # Read exported pos RDS and verify single fake spectrum
  sp_pos <- readRDS(out[["pos"]])
  expect_s4_class(sp_pos, "Spectra")
  expect_equal(length(sp_pos), 1)
})

# test_that("prepare_libraries_spectra processes minimal mgf and creates outputs", {
#   paths <- local_test_project(copy = TRUE)
#   mgf1 <- tempfile("lib1", fileext = ".mgf")
#   mgf2 <- tempfile("lib2", fileext = ".mgf")
#   .test_mgf(mgf1, pepmass = 150)
#   .test_mgf(mgf2, pepmass = 250)
#   out <- prepare_libraries_spectra(input = c(mgf1, mgf2), nam_lib = "mini")
#   expect_named(out, c("pos", "neg", "sop"))
#   expect_true(all(file.exists(out)))
#   sp_pos <- readRDS(out[["pos"]])
#   sp_neg <- readRDS(out[["neg"]])
#   expect_s4_class(sp_pos, "Spectra")
#   expect_s4_class(sp_neg, "Spectra")
#   # SOP should have structure columns even if fake (may be empty if inchikey missing)
#   sop <- tidytable::fread(out[["sop"]])
#   expect_true(all(c("structure_inchikey_connectivity_layer", "structure_smiles") %in% names(sop)))
# })

# test_that("prepare_libraries_spectra early-exits when outputs already exist", {
#   paths <- local_test_project(copy = TRUE)
#   mgf <- tempfile("lib_once", fileext = ".mgf")
#   .test_mgf(mgf)
#   out1 <- prepare_libraries_spectra(input = mgf, nam_lib = "once")
#   # Second call should just log and not overwrite substantially (same paths)
#   out2 <- prepare_libraries_spectra(input = mgf, nam_lib = "once")
#   expect_identical(out1, out2)
# })
