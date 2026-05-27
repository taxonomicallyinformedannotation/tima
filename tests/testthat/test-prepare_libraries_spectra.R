# Test Suite: prepare_libraries_spectra ----

library(testthat)

test_that("prepare_libraries_spectra() validates nam_lib parameter", {
  skip_if_not_installed("Spectra")

  tmpfile <- tempfile(fileext = ".mgf")
  # Create a minimal valid MGF file
  writeLines(
    c(
      "BEGIN IONS",
      "TITLE=Spectrum1",
      "PEPMASS=100.0",
      "50.0 100",
      "60.0 200",
      "END IONS"
    ),
    tmpfile
  )

  expect_error(
    prepare_libraries_spectra(
      input = tmpfile,
      nam_lib = c("lib1", "lib2") # Must be single string
    )
  )
})

test_that("prepare_libraries_spectra() validates column name parameters", {
  skip_if_not_installed("Spectra")

  tmpfile <- tempfile(fileext = ".mgf")
  writeLines(
    c(
      "BEGIN IONS",
      "TITLE=Spectrum1",
      "PEPMASS=100.0",
      "50.0 100",
      "100.0 100",
      "150.0 100",
      "END IONS"
    ),
    tmpfile
  )

  expect_error(
    prepare_libraries_spectra(
      input = tmpfile,
      col_ad = c("col1", "col2") # Must be single string
    )
  )
})

test_that("prepare_libraries_spectra() prepares a single library MGF successfully", {
  skip_if_not_installed("Spectra")
  withr::local_dir(new = temp_test_dir("prepare_libraries_spectra_1"))
  mgf <- tempfile(fileext = ".mgf")
  writeLines(
    c(
      "BEGIN IONS",
      "TITLE=Spec1",
      "PEPMASS=150.05",
      "CHARGE=1+",
      "IONMODE=pos",
      "EXACTMASS=149.05",
      "SPECTRUMID=CCMSLIB0123456789",
      "FILENAME=Compound1",
      "NAME=Compound1",
      "INCHIKEY=AAAAAA-BBBBBB-N",
      "SMILES=CCC",
      "50 200",
      "150 200",
      "250 200",
      "END IONS"
    ),
    mgf
  )
  res <- prepare_libraries_spectra(input = mgf, nam_lib = "TEST_LIB")
  expect_type(res, "character")
})

test_that("prepare_libraries_spectra() handles multiple input files", {
  skip_if_not_installed("Spectra")
  withr::local_dir(new = temp_test_dir("prepare_libraries_spectra_2"))
  mgf1 <- tempfile(fileext = ".mgf")
  mgf2 <- tempfile(fileext = ".mgf")
  writeLines(
    c(
      "BEGIN IONS",
      "TITLE=Spec1",
      "PEPMASS=150.05",
      "CHARGE=1+",
      "IONMODE=pos",
      "EXACTMASS=149.05",
      "SPECTRUMID=CCMSLIB0123456789",
      "FILENAME=Compound1",
      "NAME=Compound1",
      "INCHIKEY=AAAAAA-BBBBBB-N",
      "SMILES=CCC",
      "50 200",
      "150 200",
      "250 200",
      "END IONS"
    ),
    mgf1
  )
  writeLines(
    c(
      "BEGIN IONS",
      "TITLE=Spec2",
      "PEPMASS=150.05",
      "CHARGE=1+",
      "IONMODE=pos",
      "EXACTMASS=149.05",
      "SPECTRUMID=CCMSLIB0123456789",
      "FILENAME=Compound1",
      "NAME=Compound1",
      "INCHIKEY=AAAAAA-BBBBBB-N",
      "SMILES=CCC",
      "51 200",
      "151 200",
      "251 200",
      "END IONS"
    ),
    mgf2
  )
  res <- prepare_libraries_spectra(input = c(mgf1, mgf2), nam_lib = "MULTI")
  expect_type(res, "character")
})

test_that("prepare_libraries_spectra() runs when optional columns are absent", {
  skip_if_not_installed("Spectra")
  withr::local_dir(new = temp_test_dir("prepare_libraries_spectra_3"))
  mgf <- tempfile(fileext = ".mgf")
  writeLines(
    c(
      "BEGIN IONS",
      "TITLE=Spec1",
      "PEPMASS=150.05",
      "CHARGE=1+",
      "IONMODE=pos",
      "EXACTMASS=149.05",
      "SPECTRUMID=CCMSLIB0123456789",
      "FILENAME=Compound1",
      "NAME=Compound1",
      "INCHIKEY=AAAAAA-BBBBBB-N",
      "SMILES=CCC",
      "50 200",
      "150 200",
      "250 200",
      "END IONS"
    ),
    mgf
  )
  res <- prepare_libraries_spectra(input = mgf, nam_lib = "NO_META")
  expect_type(res, "character")
})

test_that("create_empty_spectral_library returns minimal Spectra object", {
  skip_if_not_installed("Spectra")
  sp <- create_empty_spectral_library()
  expect_s4_class(sp, "Spectra")
  expect_true(length(sp) >= 1L)
})

test_that("create_empty_sop_library returns expected columns", {
  sop <- create_empty_sop_library()
  expect_true(all(
    c(
      "structure_smiles",
      "structure_smiles_no_stereo",
      "organism_name"
    ) %in%
      names(sop)
  ))
})

test_that("prepare_libraries_spectra returns cached outputs when files already exist", {
  skip_if_not_installed("Spectra")
  withr::local_dir(new = temp_test_dir("prepare_libraries_cached"))

  out_pos <- file.path(
    "data",
    "interim",
    "libraries",
    "spectra",
    "exp",
    "LIB_pos.rds"
  )
  out_neg <- file.path(
    "data",
    "interim",
    "libraries",
    "spectra",
    "exp",
    "LIB_neg.rds"
  )
  out_sop <- file.path(
    "data",
    "interim",
    "libraries",
    "sop",
    "LIB_prepared.tsv.gz"
  )
  dir.create(dirname(out_pos), recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(out_sop), recursive = TRUE, showWarnings = FALSE)
  file.create(out_pos)
  file.create(out_neg)
  file.create(out_sop)

  local_mocked_bindings(
    get_params = function(step) {
      force(step)
      list(
        files = list(libraries = list(spectral = list(raw = character(0L)))),
        names = list(mgf = list())
      )
    },
    export_params = function(parameters, step) {
      force(parameters)
      force(step)
      invisible(NULL)
    },
    .package = "tima"
  )

  out <- prepare_libraries_spectra(input = "dummy.mgf", nam_lib = "LIB")
  expect_equal(unname(out), c(out_pos, out_neg, out_sop))
})

test_that("prepare_libraries_spectra creates empty templates when input is missing", {
  skip_if_not_installed("Spectra")
  withr::local_dir(new = temp_test_dir("prepare_libraries_empty"))
  local_test_project(copy = TRUE)

  out <- prepare_libraries_spectra(
    input = NULL,
    nam_lib = "EMPTYLIB"
  )

  expect_true(all(file.exists(unname(out))))

  spectra_pos <- readRDS(out[["pos"]])
  spectra_neg <- readRDS(out[["neg"]])
  sop <- tidytable::fread(out[["sop"]])

  expect_s4_class(spectra_pos, "Spectra")
  expect_s4_class(spectra_neg, "Spectra")
  expect_true(
    is.data.frame(sop) || tidytable::is_tidytable(sop)
  )
  expect_true(all(names(create_empty_sop_library()) %in% names(sop)))
})
