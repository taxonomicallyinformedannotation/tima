# Test Suite: annotate_spectra ----

library(testthat)
library(tidytable)

# Helper to create minimal mgf with adjustable precursor masses & polarity
write_minimal_mgf <- function(path, precursors, charge = "1+") {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  lines <- c()
  for (i in seq_along(precursors)) {
    mz <- precursors[i]
    lines <- c(
      lines,
      "BEGIN IONS",
      paste0("TITLE=Spectrum_", i),
      paste0("PEPMASS=", mz),
      paste0("CHARGE=", charge),
      paste0("NAME=Name_", i),
      paste0("INCHIKEY=AAAAAAAAAAAAAA-BBBBBBBBBB-C"),
      paste0("SMILES=CCC"),
      "MSLEVEL=2",
      # 3 dummy peaks
      paste0(round(mz * 0.5), " 100"),
      paste0(round(mz * 0.75), " 200"),
      paste0(round(mz), " 300"),
      "END IONS",
      ""
    )
  }
  writeLines(lines, path)
  invisible(path)
}

# ---------------- Validation tests ----------------

test_that("annotate_spectra validates parameters", {
  withr::local_dir(temp_test_dir("ann_spe_validate"))
  local_test_project(copy = TRUE)
  paths <- get_default_paths()
  get_file(
    url = paths$urls$examples$spectra_mini,
    export = paths$data$source$spectra
  )
  lib_path <- file.path(
    "data",
    "interim",
    "libraries",
    "spectra",
    "lib_pos.mgf"
  )
  write_minimal_mgf(lib_path, precursors = c(150))
  expect_error(
    annotate_spectra(libraries = list(pos = lib_path), polarity = "invalid"),
    "pos.*neg"
  )
  expect_error(
    annotate_spectra(libraries = list(pos = lib_path), threshold = 1.2),
    "between 0 and 1"
  )
  expect_error(
    annotate_spectra(libraries = list(pos = lib_path), ppm = -2),
    "must be between"
  )
  expect_error(
    annotate_spectra(libraries = list(pos = lib_path), dalton = -0.01),
    "must be between"
  )
  expect_error(
    annotate_spectra(libraries = list(pos = lib_path), qutoff = -10),
    "must be between"
  )
  expect_error(
    annotate_spectra(
      input = "nope.mgf",
      libraries = list(pos = lib_path),
      polarity = "pos"
    ),
    "Input file"
  )
  expect_error(
    annotate_spectra(libraries = list(pos = "nope.mgf"), polarity = "pos"),
    "Library file"
  )
})

# ---------------- Basic run & output ----------------

test_that("annotate_spectra produces output file and columns", {
  withr::local_dir(temp_test_dir("ann_spe_basic"))
  local_test_project(copy = TRUE)
  paths <- get_default_paths()
  get_file(
    url = paths$urls$examples$spectra_mini,
    export = paths$data$source$spectra
  )
  lib_path <- file.path(
    "data",
    "interim",
    "libraries",
    "spectra",
    "lib_pos.mgf"
  )
  write_minimal_mgf(lib_path, precursors = c(200, 250))
  out <- annotate_spectra(
    libraries = list(pos = lib_path),
    polarity = "pos",
    ppm = 20,
    dalton = 0.05
  )
  expect_true(file.exists(out))
  df <- fread(out)
  expect_true(all(
    c(
      "feature_id",
      "candidate_structure_name",
      "candidate_score_similarity"
    ) %in%
      names(df)
  ))
})

# ---------------- Threshold filtering ----------------

test_that("threshold filtering removes low similarity candidates", {
  withr::local_dir(temp_test_dir("ann_spe_threshold"))
  local_test_project(copy = TRUE)
  paths <- get_default_paths()
  get_file(
    url = paths$urls$examples$spectra_mini,
    export = paths$data$source$spectra
  )
  lib_path <- file.path(
    "data",
    "interim",
    "libraries",
    "spectra",
    "lib_pos.mgf"
  )
  write_minimal_mgf(lib_path, precursors = c(300))
  out_low <- annotate_spectra(
    libraries = list(pos = lib_path),
    polarity = "pos",
    threshold = 0.0
  )
  out_high <- annotate_spectra(
    libraries = list(pos = lib_path),
    polarity = "pos",
    threshold = 0.9
  )
  df_low <- fread(out_low)
  df_high <- fread(out_high)
  expect_true(nrow(df_low) >= nrow(df_high))
})

# ---------------- Approx vs strict precursor reduction ----------------

test_that("approx mode keeps more library spectra than strict mode", {
  withr::local_dir(temp_test_dir("ann_spe_approx"))
  local_test_project(copy = TRUE)
  # Build query with specific precursor values
  query_path <- get_params(step = "annotate_spectra")$files$spectral$raw[1]
  dir.create(dirname(query_path), recursive = TRUE, showWarnings = FALSE)
  write_minimal_mgf(query_path, precursors = c(150, 500))
  # Library with broad precursor range; only some overlap
  lib_path <- file.path(
    "data",
    "interim",
    "libraries",
    "spectra",
    "lib_pos.mgf"
  )
  write_minimal_mgf(lib_path, precursors = c(100, 150, 200, 450, 500, 700))
  out_strict <- annotate_spectra(
    input = query_path,
    libraries = list(pos = lib_path),
    polarity = "pos",
    approx = FALSE,
    ppm = 5,
    dalton = 0.01,
    threshold = 0
  )
  out_approx <- annotate_spectra(
    input = query_path,
    libraries = list(pos = lib_path),
    polarity = "pos",
    approx = TRUE,
    ppm = 5,
    dalton = 0.01,
    threshold = 0
  )
  df_strict <- fread(out_strict)
  df_approx <- fread(out_approx)
  # Approximated search should not have fewer distinct library candidates when any matches occur
  expect_true(nrow(df_approx) >= nrow(df_strict) || nrow(df_approx) == 0)
})

# ---------------- Empty template behavior ----------------

test_that("empty result exports template", {
  withr::local_dir(temp_test_dir("ann_spe_empty"))
  local_test_project(copy = TRUE)
  # Query precursors far from library (strict reduction eliminates all)
  query_path <- get_params(step = "annotate_spectra")$files$spectral$raw[1]
  dir.create(dirname(query_path), recursive = TRUE, showWarnings = FALSE)
  write_minimal_mgf(query_path, precursors = c(50))
  lib_path <- file.path(
    "data",
    "interim",
    "libraries",
    "spectra",
    "lib_pos.mgf"
  )
  write_minimal_mgf(lib_path, precursors = c(5000))
  out <- annotate_spectra(
    input = query_path,
    libraries = list(pos = lib_path),
    polarity = "pos",
    approx = FALSE,
    dalton = 0.01,
    ppm = 5,
    threshold = 0.5
  )
  df <- fread(out)
  expect_true(all(fake_annotations_columns() |> names() %in% names(df)))
  expect_equal(nrow(df), 1) # template has single row
})
