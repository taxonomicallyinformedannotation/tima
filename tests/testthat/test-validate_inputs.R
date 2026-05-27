library(testthat)
library(tima)

ns <- asNamespace("tima")

# validate_inputs is an exported wrapper around sanitize_all_inputs

# Helper: write minimal files
make_tsv <- function(df) {
  f <- tempfile(fileext = ".tsv")
  data.table::fwrite(x = df, file = f, sep = "\t")
  f
}

make_mgf <- function(lines) {
  f <- tempfile(fileext = ".mgf")
  writeLines(lines, f)
  f
}

test_that("validate_inputs returns invisibly TRUE with no inputs", {
  result <- validate_inputs()
  expect_true(isTRUE(result))
})

test_that("validate_inputs validates a well-formed features file", {
  feat <- make_tsv(data.frame(feature_id = c("F1", "F2"), mz = c(100.0, 200.0)))
  on.exit(unlink(feat))

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
  mgf <- make_mgf(c(
    "BEGIN IONS",
    "PEPMASS=200.0",
    "100.0 1.0",
    "END IONS"
  ))
  on.exit(unlink(mgf))

  result <- validate_inputs(spectra = mgf)
  expect_true(isTRUE(result))
})

test_that("validate_inputs validates a metadata file", {
  meta <- tempfile(fileext = ".tsv")
  write.table(
    data.frame(filename = "s1.mzML", organism = "Homo sapiens"),
    meta,
    sep = "\t",
    row.names = FALSE,
    quote = FALSE
  )
  on.exit(unlink(meta))

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
