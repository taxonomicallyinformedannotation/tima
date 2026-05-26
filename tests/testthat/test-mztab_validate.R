library(testthat)

test_that("validate_mztab_tables accepts official mzTab-M file with SML rows", {
  mztab_file <- resolve_fixture_path(
    "mztab/example_study_variable_group.mztab"
  )

  tabs <- read_mztab_tables(mztab_file)
  expect_no_error(validate_mztab_tables(tabs, strict = FALSE))
})

test_that("validate_mztab_tables strict mode accepts official file with SME rows", {
  mztab_file <- resolve_fixture_path(
    "mztab/manual_null_null_lipidomics.mztab"
  )

  tabs <- read_mztab_tables(mztab_file)
  expect_gt(nrow(tabs$sme), 0L)
  expect_no_error(validate_mztab_tables(tabs, strict = TRUE))
})

test_that("validate_mztab_tables strict mode fails when required SME columns are missing", {
  base_file <- resolve_fixture_path(
    "mztab/example_study_variable_group.mztab"
  )
  tabs <- read_mztab_tables(base_file)

  required_sme <- get_mztab_required_columns()$SME
  expect_gt(length(required_sme), 1L)

  # Construct a one-row SME table with all required columns, then drop one.
  good_sme <- as.data.frame(
    as.list(rep("1", length(required_sme))),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  names(good_sme) <- required_sme

  tabs$sme <- good_sme
  expect_no_error(validate_mztab_tables(tabs, strict = TRUE))

  tabs$sme <- good_sme[, -1, drop = FALSE]
  expect_error(
    validate_mztab_tables(tabs, strict = TRUE),
    regexp = "Missing required columns in SME section",
    class = "tima_validation_error"
  )
})

test_that("validate_mztab_tables fails when MTD section is missing", {
  bad_file <- temp_test_path("bad_no_mtd.mztab")
  writeLines(
    c(
      "SMH\tSML_ID\texp_mass_to_charge",
      "SML\t1\t123.45"
    ),
    bad_file
  )

  tabs <- read_mztab_tables(bad_file)

  expect_error(
    validate_mztab_tables(tabs),
    regexp = "metadata section",
    class = "tima_validation_error"
  )
})
