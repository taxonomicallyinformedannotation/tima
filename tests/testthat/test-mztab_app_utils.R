library(testthat)

test_that("plan_mztab_app_outputs prefers cached mzTab-derived files", {
  tmpdir <- tempfile("mztab_app_utils_cached_")
  dir.create(tmpdir)
  data_source <- file.path(tmpdir, "data", "source")
  dir.create(data_source, recursive = TRUE, showWarnings = FALSE)

  mztab <- file.path(data_source, "study.mztab")
  file.create(mztab)

  features <- file.path(data_source, "study_features.tsv")
  spectra <- file.path(data_source, "study_spectra.mgf")
  metadata <- file.path(data_source, "study_metadata.tsv")
  file.create(features)
  file.create(spectra)
  file.create(metadata)

  plan <- plan_mztab_app_outputs(
    mztab_path = mztab,
    data_source_path = data_source
  )

  expect_false(plan$needs_read)
  expect_identical(plan$resolved$features, features)
  expect_identical(plan$resolved$spectra, spectra)
  expect_identical(plan$resolved$metadata, metadata)
  expect_null(plan$write$features)
  expect_null(plan$write$spectra)
  expect_null(plan$write$metadata)
})

test_that("plan_mztab_app_outputs requests only missing mzTab-derived files", {
  tmpdir <- tempfile("mztab_app_utils_partial_")
  dir.create(tmpdir)
  data_source <- file.path(tmpdir, "data", "source")
  dir.create(data_source, recursive = TRUE, showWarnings = FALSE)

  mztab <- file.path(data_source, "study.mztab")
  file.create(mztab)

  features <- file.path(data_source, "study_features.tsv")
  file.create(features)

  plan <- plan_mztab_app_outputs(
    mztab_path = mztab,
    data_source_path = data_source
  )

  expect_true(plan$needs_read)
  expect_identical(plan$resolved$features, features)
  expect_null(plan$resolved$spectra)
  expect_null(plan$resolved$metadata)
  expect_null(plan$write$features)
  expect_identical(
    plan$write$spectra,
    file.path(data_source, "study_spectra.mgf")
  )
  expect_identical(
    plan$write$metadata,
    file.path(data_source, "study_metadata.tsv")
  )
})

test_that("plan_mztab_app_outputs preserves explicit core files", {
  tmpdir <- tempfile("mztab_app_utils_explicit_")
  dir.create(tmpdir)
  data_source <- file.path(tmpdir, "data", "source")
  dir.create(data_source, recursive = TRUE, showWarnings = FALSE)

  mztab <- file.path(data_source, "study.mztab")
  file.create(mztab)

  explicit_features <- file.path(data_source, "explicit_features.tsv")
  explicit_spectra <- file.path(data_source, "explicit_spectra.mgf")
  file.create(explicit_features)
  file.create(explicit_spectra)

  plan <- plan_mztab_app_outputs(
    mztab_path = mztab,
    data_source_path = data_source,
    features_path = explicit_features,
    spectra_path = explicit_spectra
  )

  expect_true(plan$needs_read)
  expect_identical(plan$resolved$features, explicit_features)
  expect_identical(plan$resolved$spectra, explicit_spectra)
  expect_null(plan$resolved$metadata)
  expect_null(plan$write$features)
  expect_null(plan$write$spectra)
  expect_identical(
    plan$write$metadata,
    file.path(data_source, "study_metadata.tsv")
  )
})

test_that("plan_mztab_app_outputs returns existing files unchanged when mztab is missing", {
  plan <- plan_mztab_app_outputs(
    mztab_path = "",
    data_source_path = tempdir(),
    features_path = "features.tsv",
    spectra_path = "spectra.mgf",
    metadata_path = "metadata.tsv"
  )

  expect_false(plan$needs_read)
  expect_false(plan$all_cached)
  expect_identical(plan$resolved$features, "features.tsv")
  expect_identical(plan$resolved$spectra, "spectra.mgf")
  expect_identical(plan$resolved$metadata, "metadata.tsv")
  expect_null(plan$write$features)
  expect_null(plan$write$spectra)
  expect_null(plan$write$metadata)
  expect_null(plan$outputs$features)
  expect_null(plan$outputs$spectra)
  expect_null(plan$outputs$metadata)
})

test_that("plan_mztab_app_outputs requests all outputs when nothing is cached", {
  tmpdir <- tempfile("mztab_app_utils_missing_")
  dir.create(tmpdir)
  data_source <- file.path(tmpdir, "data", "source")
  dir.create(data_source, recursive = TRUE, showWarnings = FALSE)

  mztab <- file.path(data_source, "study.mztab")
  file.create(mztab)

  plan <- plan_mztab_app_outputs(
    mztab_path = mztab,
    data_source_path = data_source
  )

  expect_true(plan$needs_read)
  expect_false(plan$all_cached)
  expect_null(plan$resolved$features)
  expect_null(plan$resolved$spectra)
  expect_null(plan$resolved$metadata)
  expect_identical(
    plan$write$features,
    file.path(data_source, "study_features.tsv")
  )
  expect_identical(
    plan$write$spectra,
    file.path(data_source, "study_spectra.mgf")
  )
  expect_identical(
    plan$write$metadata,
    file.path(data_source, "study_metadata.tsv")
  )
})
