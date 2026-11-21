library(testthat)

# validate_inputs is an exported wrapper around sanitize_all_inputs

test_that("validate_inputs returns invisibly TRUE with no inputs", {
  result <- validate_inputs()
  expect_true(isTRUE(result))
})

test_that("validate_inputs validates a well-formed features file", {
  feat <- local_test_tsv(data.frame(
    feature_id = c("F1", "F2"),
    mz = c(100.0, 200.0)
  ))

  result <- validate_inputs(features = feat, feature_col = "feature_id")
  expect_true(isTRUE(result))
})

test_that("validate_inputs throws for missing features file", {
  expect_error(
    validate_inputs(features = tempfile("nonexistent_")),
    class = "tima_validation_error"
  )
})

test_that("validate_inputs validates a minimal MGF spectra file", {
  mgf <- local_test_mgf(format_mgf_block(
    pepmass = "200.0",
    charge = NULL,
    peaks = "100.0 1.0"
  ))

  result <- validate_inputs(spectra = mgf)
  expect_true(isTRUE(result))
})

test_that("validate_inputs validates a metadata file", {
  meta <- local_test_tsv(
    data.frame(filename = "s1.mzML", organism = "Homo sapiens")
  )

  result <- validate_inputs(metadata = meta)
  expect_true(isTRUE(result))
})

test_that("validate_inputs validates SIRIUS directory", {
  d <- tempfile(pattern = "tima-sirius-v5-")
  dir.create(d)
  on.exit(unlink(d, recursive = TRUE))

  writeLines(
    "id\tformula\n1\tC6H12O6",
    file.path(d, "formula_identifications.tsv")
  )
  writeLines("id\tclass\n1\tFlavonoids", file.path(d, "canopus_summary.tsv"))
  writeLines(
    "id\tInChIKey\n1\tABCDE",
    file.path(d, "compound_identifications.tsv")
  )

  result <- validate_inputs(sirius = d)
  expect_true(isTRUE(result))
})
