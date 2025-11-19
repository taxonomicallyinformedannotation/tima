# ==============================================================================
# Test Suite: read_mgf_opti
# ==============================================================================

test_that("read_mgf_opti validates file path", {
  expect_error(read_mgf_opti(f = "missing.mgf"), "not found")
})

test_that(
  skip("Not implemented")
)
# test_that("read_mgf_opti reads minimal mgf", {
#   tmp <- withr::local_tempdir()
#   withr::local_dir(tmp)
#   mgf <- c(
#     "BEGIN IONS",
#     "TITLE=Spec1",
#     "PEPMASS=100",
#     "CHARGE=1+",
#     "100 10",
#     "END IONS"
#   )
#   writeLines(mgf, "test.mgf")
#   res <- read_mgf_opti(f = "test.mgf")
#   expect_s3_class(res, "DataFrame")
#   expect_true(nrow(res) >= 1L)
# })
