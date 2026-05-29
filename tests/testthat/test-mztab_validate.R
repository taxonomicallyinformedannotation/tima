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

test_that("validate_mztab_tables strict mode enforces SML pipe-aligned ambiguity fields", {
  mztab_file <- resolve_fixture_path("mztab/manual_null_null_lipidomics.mztab")
  tabs <- read_mztab_tables(mztab_file)
  expect_gt(nrow(tabs$sml), 0L)

  tabs$sml$database_identifier[[1L]] <- "CHEBI:1|HMDB:2"
  tabs$sml$chemical_formula[[1L]] <- "C1H2"
  tabs$sml$smiles[[1L]] <- "CC|CO"
  tabs$sml$inchi[[1L]] <- "null|null"
  tabs$sml$chemical_name[[1L]] <- "a|b"
  tabs$sml$uri[[1L]] <- "https://example.org/a|https://example.org/b"
  tabs$sml$adduct_ions[[1L]] <- "[M+H]+|[M+Na]+"

  expect_error(
    validate_mztab_tables(tabs, strict = TRUE),
    regexp = "ambiguity alignment",
    class = "tima_validation_error"
  )
})

test_that("validate_required_cols returns early for empty data frames", {
  fn <- get(".validate_required_cols", envir = asNamespace("tima"))
  expect_invisible(fn(tidytable::tidytable(), c("a", "b"), "SML"))
})

test_that("mztab metadata helpers handle missing and malformed values", {
  get_meta <- get(".mztab_get_metadata_value", envir = asNamespace("tima"))
  parse_ver <- get(".mztab_parse_version", envir = asNamespace("tima"))

  meta <- tidytable::tidytable(
    key = c("mzTab-version", "cv[1]-label"),
    value = c("2.1.0-M", "TEST")
  )

  expect_identical(get_meta(meta, "mzTab-version"), "2.1.0-M")
  expect_true(is.na(get_meta(meta, "missing-key")))
  expect_null(parse_ver(NA_character_))
  expect_null(parse_ver(""))
  expect_null(parse_ver("2.1-M"))
  expect_identical(parse_ver("2.1.0-M"), c(major = 2L, minor = 1L, patch = 0L))
})

test_that("mztab metadata semantics warn on missing recommended fields", {
  fn <- get(".mztab_validate_metadata_semantics", envir = asNamespace("tima"))
  meta <- tidytable::tidytable(
    key = c("mzTab-version", "mzTab-mode"),
    value = c("2.1.0-M", "Summary")
  )

  expect_no_error(fn(meta, strict = FALSE))
})

test_that("mztab cv registry helper is a no-op when no labels exist", {
  fn <- get(".mztab_validate_cv_registry", envir = asNamespace("tima"))
  meta <- tidytable::tidytable(
    key = c("mzTab-version", "mzTab-mode"),
    value = c("2.1.0-M", "Summary")
  )
  expect_no_error(fn(meta, strict = TRUE))
})

test_that("mztab cv registry helper warns in non-strict mode", {
  fn <- get(".mztab_validate_cv_registry", envir = asNamespace("tima"))
  meta <- tidytable::tidytable(
    key = c("cv[1]-label", "cv[1]-full_name"),
    value = c("TEST", "Test Name")
  )

  expect_no_error(fn(meta, strict = FALSE))
})

test_that("mztab reference helpers filter nulls and split pipe values", {
  miss <- get(".mztab_missing_ref_ids", envir = asNamespace("tima"))
  split_pipe <- get(".mztab_split_pipe_values", envir = asNamespace("tima"))

  expect_identical(miss(c("1|2", "null", NA_character_, ""), c("1", "3")), "2")
  expect_identical(split_pipe("a|b| c "), c("a", "b", "c"))
  expect_identical(split_pipe(NA_character_), "null")
  expect_identical(split_pipe(""), "null")
})

test_that("mztab reference integrity helper warns in non-strict mode", {
  fn <- get(".mztab_validate_reference_integrity", envir = asNamespace("tima"))

  tabs <- list(
    sml = tidytable::tidytable(
      SMF_ID_REFS = "999",
      SME_ID_REFS = "888",
      database_identifier = "CHEBI:1",
      chemical_formula = "C1H2",
      smiles = "CC",
      inchi = "null",
      chemical_name = "name",
      uri = "uri",
      adduct_ions = "[M+H]+"
    ),
    smf = tidytable::tidytable(
      SMF_ID = "1",
      SME_ID_REFS = "888"
    ),
    sme = tidytable::tidytable(SME_ID = "1")
  )

  expect_no_error(fn(tabs, strict = FALSE))
})

test_that("validate_mztab_tables rejects missing mzTab-version before deeper checks", {
  tabs <- list(
    metadata = tidytable::tidytable(
      key = "mzTab-type",
      value = "Quantification"
    ),
    sml = tidytable::tidytable(SML_ID = "1"),
    smf = tidytable::tidytable(SMF_ID = "1"),
    sme = tidytable::tidytable()
  )

  expect_error(
    validate_mztab_tables(tabs),
    regexp = "Missing required mzTab metadata field: mzTab-version",
    class = "tima_validation_error"
  )
})

test_that("validate_mztab_tables rejects empty SML and SMF sections", {
  tabs <- list(
    metadata = tidytable::tidytable(
      key = c("mzTab-version", "mzTab-type"),
      value = c("2.1.0-M", "Quantification")
    ),
    sml = tidytable::tidytable(),
    smf = tidytable::tidytable(),
    sme = tidytable::tidytable()
  )

  expect_error(
    validate_mztab_tables(tabs),
    regexp = "must contain SML and/or SMF rows",
    class = "tima_validation_error"
  )
})

test_that("validate_mztab_tables non-strict mode warns on missing SME columns", {
  tabs <- list(
    metadata = tidytable::tidytable(
      key = c("mzTab-version", "mzTab-type"),
      value = c("2.1.0-M", "Quantification")
    ),
    sml = tidytable::tidytable(SML_ID = "1"),
    smf = tidytable::tidytable(SMF_ID = "1", exp_mass_to_charge = "100"),
    sme = tidytable::tidytable(SME_ID = "1")
  )

  expect_no_error(validate_mztab_tables(tabs, strict = FALSE))
})
