# Test Suite: import_spectra ----

library(testthat)

test_that("import_spectra handles MGF files correctly", {
  local_test_project(copy = TRUE)

  # Create test MGF in a temp file
  temp_mgf <- withr::local_tempfile(fileext = ".mgf")

  data.frame(
    FEATURE_ID = "FT001",
    mz = I(list(c(100.0, 200.0, 300.0))),
    intensity = I(list(c(10.0, 50.0, 30.0))),
    precursorCharge = 1L,
    MS_LEVEL = 2L,
    PRECURSOR_MZ = 200.0,
    Spectrum_type = "MS2"
  ) |>
    Spectra::Spectra() |>
    MsBackendMgf::export(
      backend = MsBackendMgf::MsBackendMgf(),
      file = temp_mgf
    )

  result <- import_spectra(file = temp_mgf)
  expect_s4_class(result, "Spectra")
})

test_that("import_spectra handles MSP files with failures", {
  # This tests spectrum 1 failure case
  msp_file <- dir(
    system.file("extdata", package = "MsBackendMsp"),
    full.names = TRUE,
    pattern = "msp$"
  )[8L]

  if (file.exists(msp_file)) {
    result <- import_spectra(file = msp_file)
    expect_s4_class(result, "Spectra")
  } else {
    skip("MSP test file not found")
  }
})

test_that("import_spectra validates file and polarity inputs", {
  expect_error(
    import_spectra(file = 123),
    "single character string",
    class = "tima_validation_error"
  )

  expect_error(
    import_spectra(file = tempfile(fileext = ".mgf"), polarity = "bad"),
    "polarity",
    class = "tima_validation_error"
  )
})

test_that("import_spectra validates tolerances and missing files", {
  expect_error(
    import_spectra(file = "missing_file.mgf", dalton = 0),
    "positive",
    class = "tima_validation_error"
  )

  expect_error(
    import_spectra(file = "missing_file.mgf"),
    "spectra file not found",
    class = "tima_validation_error"
  )
})

make_test_spectra <- function(df) {
  Spectra::Spectra(df)
}

test_that("import_spectra rejects unsupported file extensions", {
  bad_file <- withr::local_tempfile(fileext = ".txt")
  writeLines("not a spectra file", con = bad_file)

  expect_error(
    import_spectra(file = bad_file),
    "unsupported file format",
    class = "tima_validation_error"
  )
})

test_that("import_spectra wraps malformed RDS import errors", {
  bad_rds <- withr::local_tempfile(fileext = ".rds")
  writeLines("this is not an RDS payload", con = bad_rds)

  expect_error(
    import_spectra(file = bad_rds),
    "failed to import spectra",
    class = "tima_runtime_error"
  )
})

test_that("import_spectra returns early for empty spectra files", {
  empty_rds <- withr::local_tempfile(fileext = ".rds")
  base::saveRDS(make_test_spectra(data.frame()), file = empty_rds)

  result <- import_spectra(file = empty_rds, sanitize = FALSE, combine = FALSE)
  expect_s4_class(result, "Spectra")
  expect_equal(length(result), 0L)
})

test_that("import_spectra parses FEATURE_ID from TITLE when missing explicit IDs", {
  spec_rds <- withr::local_tempfile(fileext = ".rds")
  spectra <- make_test_spectra(data.frame(
    TITLE = "id:42, rt:12.3, mz:123.45",
    precursorMz = 123.45,
    precursorCharge = 1L,
    mz = I(list(c(50, 75, 100))),
    intensity = I(list(c(10, 20, 30)))
  ))
  base::saveRDS(spectra, file = spec_rds)

  result <- import_spectra(file = spec_rds, sanitize = FALSE, combine = FALSE)
  expect_equal(length(result), 1L)
  expect_true("FEATURE_ID" %in% colnames(result@backend@spectraData))
  expect_identical(result$FEATURE_ID[[1]], "42")
})

test_that("import_spectra harmonizes precursor_mz metadata", {
  spec_rds <- withr::local_tempfile(fileext = ".rds")
  spectra <- make_test_spectra(data.frame(
    precursor_mz = 123.45,
    precursorCharge = 1L,
    mz = I(list(c(50, 75, 100))),
    intensity = I(list(c(10, 20, 30)))
  ))
  base::saveRDS(spectra, file = spec_rds)

  result <- import_spectra(file = spec_rds, sanitize = FALSE, combine = FALSE)
  expect_equal(length(result), 1L)
  expect_identical(as.numeric(result$precursorMz[[1]]), 123.45)
})

test_that("import_spectra normalizes precursor_mz metadata to precursorMz", {
  spec_rds <- withr::local_tempfile(fileext = ".rds")
  spectra <- make_test_spectra(data.frame(
    precursor_mz = 111.1,
    precursorCharge = 1L,
    mz = I(list(c(10, 20, 30))),
    intensity = I(list(c(100, 80, 60)))
  ))
  base::saveRDS(spectra, file = spec_rds)

  result <- import_spectra(file = spec_rds, sanitize = FALSE, combine = FALSE)
  expect_s4_class(result, "Spectra")
  expect_true("precursorMz" %in% colnames(result@backend@spectraData))
  expect_identical(result$precursorMz[[1]], 111.1)
})

test_that("import_spectra filters by polarity when requested", {
  spec_rds <- withr::local_tempfile(fileext = ".rds")
  spectra <- make_test_spectra(data.frame(
    precursorMz = c(111.1, 222.2),
    precursorCharge = c(1L, -1L),
    mz = I(list(c(10, 20), c(30, 40))),
    intensity = I(list(c(100, 200), c(150, 250)))
  ))
  base::saveRDS(spectra, file = spec_rds)

  pos <- import_spectra(
    file = spec_rds,
    polarity = "pos",
    sanitize = FALSE,
    combine = FALSE
  )
  neg <- import_spectra(
    file = spec_rds,
    polarity = "neg",
    sanitize = FALSE,
    combine = FALSE
  )

  expect_equal(length(pos), 1L)
  expect_equal(length(neg), 1L)
  expect_identical(pos$precursorCharge[[1]], 1L)
  expect_identical(neg$precursorCharge[[1]], -1L)
})

test_that("import_spectra validates non-negative cutoff", {
  tmp <- withr::local_tempfile(fileext = ".rds")
  base::saveRDS(make_test_spectra(data.frame()), file = tmp)

  expect_error(
    import_spectra(file = tmp, cutoff = -1),
    "cutoff intensity must be non-negative",
    class = "tima_validation_error"
  )
})

test_that("import_spectra skips sanitize work for RDS files when disabled", {
  spec_rds <- withr::local_tempfile(fileext = ".rds")
  spectra <- make_test_spectra(data.frame(
    precursorMz = 111.1,
    precursorCharge = 1L,
    mz = I(list(c(10, 20, 30))),
    intensity = I(list(c(100, 80, 60)))
  ))
  base::saveRDS(spectra, file = spec_rds)

  state <- new.env(parent = emptyenv())
  state$called <- FALSE
  testthat::with_mocked_bindings(
    sanitize_spectra = function(spectra, cutoff, dalton, min_fragments, ppm) {
      state$called <- TRUE
      spectra
    },
    {
      out <- import_spectra(
        file = spec_rds,
        sanitize = FALSE,
        combine = FALSE
      )
      expect_s4_class(out, "Spectra")
      expect_false(state$called)
    },
    .package = "tima"
  )
})

test_that("import_spectra calls sanitize_spectra when sanitize is TRUE", {
  spec_rds <- withr::local_tempfile(fileext = ".rds")
  spectra <- make_test_spectra(data.frame(
    precursorMz = 111.1,
    precursorCharge = 1L,
    mz = I(list(c(10, 20, 30))),
    intensity = I(list(c(100, 80, 60)))
  ))
  base::saveRDS(spectra, file = spec_rds)

  state <- new.env(parent = emptyenv())
  state$called <- FALSE
  state$observed_min_fragments <- NULL

  testthat::with_mocked_bindings(
    sanitize_spectra = function(spectra, cutoff, dalton, min_fragments, ppm) {
      state$called <- TRUE
      state$observed_min_fragments <- min_fragments
      spectra
    },
    {
      out <- import_spectra(
        file = spec_rds,
        sanitize = TRUE,
        combine = FALSE,
        min_fragments = 3L
      )
      expect_s4_class(out, "Spectra")
      expect_true(state$called)
      expect_identical(state$observed_min_fragments, 3L)
    },
    .package = "tima"
  )
})

test_that("import_spectra covers precursorCharge=0, TITLE id extraction, msLevel filter, neg polarity, SLAW_ID combine, and RDS fast path", {
  # 1. Non-Spectra RDS errors in .prepare_rds_spectra_fast
  bad_rds <- withr::local_tempfile(fileext = ".rds")
  base::saveRDS(list(a = 1), file = bad_rds)
  expect_error(
    .prepare_rds_spectra_fast(list(a = 1), polarity = NA_character_),
    "RDS file did not contain a Spectra object",
    class = "tima_validation_error"
  )

  # 2. Fast RDS path with MSLEVEL, PRECURSOR_MZ, spectrum_id, and TITLE id extraction
  spec_fast <- make_test_spectra(data.frame(
    MSLEVEL = 2L,
    PRECURSOR_MZ = 150.1,
    precursorCharge = -1L,
    spectrum_id = 101L,
    TITLE = "id:42, rt:10.0",
    mz = I(list(c(50, 100))),
    intensity = I(list(c(100, 200)))
  ))
  fast_rds <- withr::local_tempfile(fileext = ".rds")
  base::saveRDS(spec_fast, file = fast_rds)
  out_fast <- import_spectra(
    file = fast_rds,
    polarity = "neg",
    sanitize = FALSE,
    combine = FALSE
  )
  expect_identical(out_fast$FEATURE_ID, "42")
  expect_identical(out_fast$spectrum_id, "101")

  # 3. Full path (sanitize = TRUE or combine = TRUE) with precursorCharge = 0, msLevel = c(1L, 2L),
  # TITLE id extraction, and negative polarity
  spec_full <- make_test_spectra(data.frame(
    MSLEVEL = c(1L, 2L, 2L),
    PRECURSOR_MZ = c(150.1, 150.1, 150.1),
    precursorCharge = c(0L, 0L, -1L),
    spectrum_id = c(1L, 2L, 3L),
    TITLE = c("id:10", "id:20", "id:20"),
    mz = I(list(c(50, 100), c(50, 100), c(50, 100))),
    intensity = I(list(c(100, 200), c(100, 200), c(100, 200)))
  ))
  full_rds <- withr::local_tempfile(fileext = ".rds")
  base::saveRDS(spec_full, file = full_rds)
  out_full <- import_spectra(
    file = full_rds,
    polarity = "neg",
    sanitize = FALSE,
    combine = TRUE
  )
  expect_equal(attr(out_full, "tima_ms1_skipped"), 1L)

  # 4. Combine by SLAW_ID
  spec_slaw <- make_test_spectra(data.frame(
    msLevel = c(2L, 2L),
    precursorMz = c(150.1, 150.1),
    precursorCharge = c(1L, 1L),
    SLAW_ID = c("S1", "S1"),
    mz = I(list(c(50, 100), c(50, 100))),
    intensity = I(list(c(100, 200), c(100, 200)))
  ))
  slaw_rds <- withr::local_tempfile(fileext = ".rds")
  base::saveRDS(spec_slaw, file = slaw_rds)
  out_slaw <- import_spectra(
    file = slaw_rds,
    polarity = "pos",
    sanitize = FALSE,
    combine = TRUE
  )
  expect_equal(length(out_slaw), 1L)
})
