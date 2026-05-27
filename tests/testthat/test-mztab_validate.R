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

test_that("validate_mztab_tables strict mode rejects invalid mzTab-version format", {
  mztab_file <- resolve_fixture_path("mztab/example_study_variable_group.mztab")
  tabs <- read_mztab_tables(mztab_file)

  idx <- which(tabs$metadata$key == "mzTab-version")
  expect_true(length(idx) >= 1L)
  tabs$metadata$value[[idx[[1L]]]] <- "2.1-M"

  expect_error(
    validate_mztab_tables(tabs, strict = TRUE),
    regexp = "Invalid mzTab-version format",
    class = "tima_validation_error"
  )
})

test_that("validate_mztab_tables strict mode rejects incomplete cv registry blocks", {
  mztab_file <- resolve_fixture_path("mztab/example_study_variable_group.mztab")
  tabs <- read_mztab_tables(mztab_file)

  tabs$metadata <- tabs$metadata |>
    tidytable::bind_rows(tidytable::tidytable(
      key = "cv[99]-label",
      value = "TESTCV"
    ))

  expect_error(
    validate_mztab_tables(tabs, strict = TRUE),
    regexp = "Incomplete CV registry entries",
    class = "tima_validation_error"
  )
})

test_that("validate_mztab_tables strict mode rejects unknown cross-section reference IDs", {
  mztab_file <- resolve_fixture_path("mztab/manual_null_null_lipidomics.mztab")
  tabs <- read_mztab_tables(mztab_file)
  expect_gt(nrow(tabs$sml), 0L)
  expect_gt(nrow(tabs$smf), 0L)

  tabs$sml$SMF_ID_REFS[[1L]] <- "999999"

  expect_error(
    validate_mztab_tables(tabs, strict = TRUE),
    regexp = "unknown SMF_ID",
    class = "tima_validation_error"
  )
})

test_that("validate_mztab_tables strict mode enforces SMF ambiguity code formalism", {
  mztab_file <- resolve_fixture_path("mztab/manual_null_null_lipidomics.mztab")
  tabs <- read_mztab_tables(mztab_file)

  if (nrow(tabs$sml) > 0L && "SMF_ID_REFS" %in% names(tabs$sml)) {
    tabs$sml$SMF_ID_REFS <- "1"
  }
  if (nrow(tabs$sml) > 0L && "SME_ID_REFS" %in% names(tabs$sml)) {
    tabs$sml$SME_ID_REFS <- "1|2"
  }

  tabs$smf <- tidytable::tidytable(
    SMF_ID = "1",
    exp_mass_to_charge = "100",
    charge = "1",
    SME_ID_REFS = "1|2",
    SME_ID_REF_ambiguity_code = "null"
  )
  tabs$sme <- tidytable::tidytable(
    SME_ID = c("1", "2"),
    evidence_input_id = c("1", "1"),
    database_identifier = c("A", "B"),
    exp_mass_to_charge = c("100", "100"),
    charge = c("1", "1"),
    theoretical_mass_to_charge = c("100", "100"),
    spectra_ref = c("null", "null"),
    identification_method = c(
      "[, , spectral library matching, ]",
      "[, , spectral library matching, ]"
    ),
    ms_level = c(
      "[MS, MS:1000580, MS2 spectrum, ]",
      "[MS, MS:1000580, MS2 spectrum, ]"
    ),
    rank = c("1", "2")
  )

  expect_error(
    validate_mztab_tables(tabs, strict = TRUE),
    regexp = "SME_ID_REF_ambiguity_code",
    class = "tima_validation_error"
  )
})
