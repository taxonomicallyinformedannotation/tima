# Test Suite: prepare_libraries_spectra ----

library(testthat)

test_that("prepare_libraries_spectra returns empty libs when input missing", {
  withr::with_dir(temp_test_dir("prep_lib_spectra_missing"), {
    local_test_project(copy = TRUE)
    out <- prepare_libraries_spectra(
      input = c("missing1.mgf", "missing2.mgf"),
      nam_lib = "testlib"
    )
    expect_true(file.exists(out[["pos"]]))
    expect_true(file.exists(out[["neg"]]))
    expect_true(file.exists(out[["sop"]]))
    sp_pos <- readRDS(out[["pos"]])
    expect_s4_class(sp_pos, "Spectra")
    expect_equal(length(sp_pos), 1)
  })
})

# test_that("prepare_libraries_spectra processes MGF fixtures and creates outputs", {
#   paths <- local_test_project(copy = TRUE)
#
#   # Use fixture MGF files
#   mgf1 <- copy_mgf_fixture("spectra_query.mgf", "lib1.mgf")
#   mgf2 <- copy_mgf_fixture("spectra_library.mgf", "lib2.mgf")
#
#   out <- prepare_libraries_spectra(input = c(mgf1, mgf2), nam_lib = "mini")
#   expect_named(out, c("pos", "neg", "sop"))
#   expect_true(all(file.exists(out)))
#
#   sp_pos <- readRDS(out[["pos"]])
#   sp_neg <- readRDS(out[["neg"]])
#   expect_s4_class(sp_pos, "Spectra")
#   expect_s4_class(sp_neg, "Spectra")
#
#   # SOP should have structure columns
#   sop <- tidytable::fread(out[["sop"]])
#   expect_true(all(
#     c(
#       "structure_inchikey_connectivity_layer",
#       "structure_smiles"
#     ) %in%
#       names(sop)
#   ))
# })

# test_that("prepare_libraries_spectra early-exits when outputs already exist", {
#   paths <- local_test_project(copy = TRUE)
#
#   mgf <- copy_mgf_fixture("spectra_library.mgf")
#
#   out1 <- prepare_libraries_spectra(input = mgf, nam_lib = "once")
#
#   # Second call should just log and not overwrite substantially (same paths)
#   out2 <- prepare_libraries_spectra(input = mgf, nam_lib = "once")
#
#   expect_identical(out1, out2)
# })
