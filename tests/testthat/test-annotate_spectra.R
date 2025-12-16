# Test Suite: annotate_spectra ----

library(testthat)

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

# Validation tests ----

test_that("annotate_spectra validates parameters", {
  withr::local_dir(new = temp_test_dir("ann_spe_validate"))
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
    "qutoff must be non-negative or NULL, got: -10",
    fixed = TRUE
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

# Basic run & output ----

test_that("annotate_spectra produces output file and columns", {
  withr::local_dir(new = temp_test_dir("ann_spe_basic"))
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
  df <- tidytable::fread(out)
  expect_true(all(
    c(
      "feature_id",
      "candidate_structure_name",
      "candidate_score_similarity"
    ) %in%
      names(df)
  ))
})

# Threshold filtering ----

test_that("threshold filtering removes low similarity candidates", {
  withr::local_dir(new = temp_test_dir("ann_spe_threshold"))
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
  df_low <- tidytable::fread(out_low)
  df_high <- tidytable::fread(out_high)
  expect_true(nrow(df_low) >= nrow(df_high))
})

# Approx vs strict precursor reduction ----

test_that("approx mode keeps more library spectra than strict mode", {
  withr::local_dir(new = temp_test_dir("ann_spe_approx"))
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
  df_strict <- tidytable::fread(out_strict)
  df_approx <- tidytable::fread(out_approx)
  # Approximated search should not have fewer distinct library candidates when any matches occur
  expect_true(nrow(df_approx) >= nrow(df_strict) || nrow(df_approx) == 0)
})

# Empty template behavior ----

test_that("empty result exports template", {
  withr::local_dir(new = temp_test_dir("ann_spe_empty"))
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
  df <- tidytable::fread(out)
  expect_true(all(fake_annotations_columns() |> names() %in% names(df)))
  expect_equal(nrow(df), 1) # template has single row
})

# Polarity-based library filtering ----

test_that("polarity filtering drops non-matching libraries and handles none left", {
  withr::local_dir(new = temp_test_dir("ann_spe_polarity_filter"))
  local_test_project(copy = TRUE)
  paths <- get_default_paths()
  get_file(
    url = paths$urls$examples$spectra_mini,
    export = paths$data$source$spectra
  )
  # Create two libs: one pos, one neg
  lib_pos <- file.path("data", "interim", "libraries", "spectra", "lib_pos.mgf")
  lib_neg <- file.path("data", "interim", "libraries", "spectra", "lib_neg.mgf")
  write_minimal_mgf(lib_pos, precursors = c(150))
  write_minimal_mgf(lib_neg, precursors = c(150), charge = "1-")
  # When polarity is pos, neg library path should be filtered out
  out <- annotate_spectra(
    libraries = c(lib_pos, lib_neg),
    polarity = "pos",
    threshold = 0
  )
  expect_true(file.exists(out))
  # When only neg libraries provided for pos polarity, expect empty template
  out2 <- annotate_spectra(
    libraries = c(lib_neg),
    polarity = "pos",
    threshold = 0
  )
  df2 <- tidytable::fread(out2)
  expect_equal(nrow(df2), 1)
})

# Empty peaks in library cleaning ----

test_that("library spectra with empty peaks are removed before concatenation", {
  withr::local_dir(new = temp_test_dir("ann_spe_empty_peaks"))
  local_test_project(copy = TRUE)
  paths <- get_default_paths()
  get_file(
    url = paths$urls$examples$spectra_mini,
    export = paths$data$source$spectra
  )
  # Create a normal library and an 'empty' one with MSLEVEL but no peaks
  lib_ok <- file.path(
    "data",
    "interim",
    "libraries",
    "spectra",
    "lib_ok_pos.mgf"
  )
  lib_empty <- file.path(
    "data",
    "interim",
    "libraries",
    "spectra",
    "lib_empty_pos.mgf"
  )
  write_minimal_mgf(lib_ok, precursors = c(200))
  dir.create(dirname(lib_empty), recursive = TRUE, showWarnings = FALSE)
  writeLines(
    c(
      "BEGIN IONS",
      "TITLE=EmptySpectrum",
      "PEPMASS=300",
      "CHARGE=1+",
      "MSLEVEL=2",
      "END IONS",
      ""
    ),
    lib_empty
  )
  out <- annotate_spectra(
    libraries = c(lib_ok, lib_empty),
    polarity = "pos",
    threshold = 0
  )
  expect_true(file.exists(out))
  df <- tidytable::fread(out)
  expect_true(nrow(df) >= 1)
})

# High intensity cutoff yields no query spectra ----

test_that("high qutoff removes all query peaks leading to empty template", {
  withr::local_dir(new = temp_test_dir("ann_spe_high_cutoff"))
  local_test_project(copy = TRUE)
  paths <- get_default_paths()
  # Place query in expected path
  query_path <- get_params(step = "annotate_spectra")$files$spectral$raw[1]
  dir.create(dirname(query_path), recursive = TRUE, showWarnings = FALSE)
  write_minimal_mgf(query_path, precursors = c(250))
  lib_path <- file.path(
    "data",
    "interim",
    "libraries",
    "spectra",
    "lib_pos.mgf"
  )
  write_minimal_mgf(lib_path, precursors = c(250))
  # Use an extremely high cutoff to drop all peaks
  out <- annotate_spectra(
    input = query_path,
    libraries = list(pos = lib_path),
    polarity = "pos",
    qutoff = 1e9,
    threshold = 0
  )
  df <- tidytable::fread(out)
  expect_equal(nrow(df), 1)
})

# Duplicate candidate collapse by connectivity layer ----

test_that("duplicate candidates collapsed by connectivity layer are unique", {
  withr::local_dir(new = temp_test_dir("ann_spe_dedupe"))
  local_test_project(copy = TRUE)
  paths <- get_default_paths()
  get_file(
    url = paths$urls$examples$spectra_mini,
    export = paths$data$source$spectra
  )
  # Build two libraries with same connectivity layer but different spectrum IDs
  lib1 <- file.path("data", "interim", "libraries", "spectra", "lib1_pos.mgf")
  lib2 <- file.path("data", "interim", "libraries", "spectra", "lib2_pos.mgf")
  write_minimal_mgf(lib1, precursors = c(300))
  # For lib2, reuse same precursor; connectivity is fixed by write_minimal_mgf
  write_minimal_mgf(lib2, precursors = c(300))
  out <- annotate_spectra(
    libraries = c(lib1, lib2),
    polarity = "pos",
    threshold = 0
  )
  df <- tidytable::fread(out)
  uniq <- tidytable::distinct(
    df,
    feature_id,
    candidate_structure_inchikey_connectivity_layer
  )
  expect_equal(nrow(uniq), nrow(df))
})

# NA metadata fallback handling ----

test_that("NA fallback for smiles and inchikey connectivity is applied", {
  withr::local_dir(new = temp_test_dir("ann_spe_na_fallback"))
  local_test_project(copy = TRUE)
  paths <- get_default_paths()
  get_file(
    url = paths$urls$examples$spectra_mini,
    export = paths$data$source$spectra
  )
  # Create a library entry missing SMILES_2D and INCHIKEY_2D to trigger fallbacks
  lib_path <- file.path(
    "data",
    "interim",
    "libraries",
    "spectra",
    "lib_pos_missing_meta.mgf"
  )
  dir.create(dirname(lib_path), recursive = TRUE, showWarnings = FALSE)
  writeLines(
    c(
      "BEGIN IONS",
      "TITLE=MetaMissing",
      "PEPMASS=320",
      "CHARGE=1+",
      "NAME=Example",
      "INCHIKEY=QQQQQQQQQQQQQQ-RRRRRRRRRR-S",
      "SMILES=CCO",
      "MSLEVEL=2",
      "160 100",
      "240 200",
      "320 300",
      "END IONS",
      ""
    ),
    lib_path
  )
  out <- annotate_spectra(
    libraries = list(pos = lib_path),
    polarity = "pos",
    threshold = 0
  )
  df <- tidytable::fread(out)
  expect_true("candidate_structure_smiles_no_stereo" %in% names(df))
  expect_true("candidate_structure_inchikey_connectivity_layer" %in% names(df))
  # Fallback should derive connectivity layer from INCHIKEY if missing 2D field
  expect_true(all(
    grepl("-", df$candidate_structure_inchikey_connectivity_layer) == FALSE
  ))
})

# Internal helper function tests ----

test_that("normalize_input_files handles different input types", {
  # Character vector
  result <- normalize_input_files(c("file1.mgf", "file2.mgf"), "Test")
  expect_equal(result, c("file1.mgf", "file2.mgf"))

  # List
  result <- normalize_input_files(list("file1.mgf", "file2.mgf"), "Test")
  expect_equal(result, c("file1.mgf", "file2.mgf"))

  # Named list
  result <- normalize_input_files(
    list(pos = "file1.mgf", neg = "file2.mgf"),
    "Test"
  )
  expect_equal(result, c("file1.mgf", "file2.mgf"))

  # Error on invalid type
  expect_error(
    normalize_input_files(123, "Test"),
    "Test elements must be character strings"
  )
})

test_that("resolve_annotation_output handles paths correctly", {
  result <- resolve_annotation_output("path1.tsv")
  expect_equal(result, "path1.tsv")

  result <- resolve_annotation_output(c("path1.tsv", "path2.tsv"))
  expect_equal(result, "path1.tsv")

  expect_error(
    resolve_annotation_output(123),
    "Output path must be a character string"
  )

  expect_error(
    resolve_annotation_output(character(0)),
    "Output must contain at least one file path"
  )
})

test_that("filter_library_paths_by_polarity filters correctly", {
  paths <- c(
    "lib_pos.mgf",
    "lib_neg.mgf",
    "lib_pos_2.mgf",
    "neutral.mgf"
  )

  result_pos <- filter_library_paths_by_polarity(paths, "pos")
  expect_true(all(grepl("pos", result_pos)))
  expect_equal(length(result_pos), 2)

  result_neg <- filter_library_paths_by_polarity(paths, "neg")
  expect_true(all(grepl("neg", result_neg)))
  expect_equal(length(result_neg), 1)

  # Single path unchanged
  result <- filter_library_paths_by_polarity("lib.mgf", "pos")
  expect_equal(result, "lib.mgf")

  # Empty list
  result <- filter_library_paths_by_polarity(character(0), "pos")
  expect_equal(length(result), 0)
})

# test_that("annotate_spectra handles multiple input files", {
#   skip_on_cran()
#   withr::local_dir(new = temp_test_dir("ann_spe_multi"))
#   local_test_project(copy = TRUE)
#
#   # Create multiple query files
#   query_dir <- dirname(get_params(step = "annotate_spectra")$files$spectral$raw[
#     1
#   ])
#   dir.create(query_dir, recursive = TRUE, showWarnings = FALSE)
#   query1 <- file.path(query_dir, "query1.mgf")
#   query2 <- file.path(query_dir, "query2.mgf")
#   write_minimal_mgf(query1, precursors = c(150))
#   write_minimal_mgf(query2, precursors = c(200))
#
#   lib_path <- file.path(
#     "data",
#     "interim",
#     "libraries",
#     "spectra",
#     "lib_pos.mgf"
#   )
#   write_minimal_mgf(lib_path, precursors = c(150, 200))
#
#   out <- annotate_spectra(
#     input = c(query1, query2),
#     libraries = list(pos = lib_path),
#     polarity = "pos",
#     threshold = 0
#   )
#
#   expect_true(file.exists(out))
#   df <- tidytable::fread(out)
#   expect_true(nrow(df) >= 0)
# })

test_that("annotate_spectra handles empty library error", {
  skip_on_cran()
  withr::local_dir(new = temp_test_dir("ann_spe_empty_lib"))
  local_test_project(copy = TRUE)

  paths <- get_default_paths()
  get_file(
    url = paths$urls$examples$spectra_mini,
    export = paths$data$source$spectra
  )

  expect_error(
    annotate_spectra(libraries = character(0), polarity = "pos"),
    "At least one library must be provided"
  )

  expect_error(
    annotate_spectra(libraries = list(), polarity = "pos"),
    "Library elements must be character strings"
  )
})

test_that("annotate_spectra applies intensity cutoff", {
  skip_on_cran()
  withr::local_dir(new = temp_test_dir("ann_spe_cutoff"))
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
  write_minimal_mgf(lib_path, precursors = c(200))

  # High cutoff
  out <- annotate_spectra(
    libraries = list(pos = lib_path),
    polarity = "pos",
    qutoff = 50,
    threshold = 0
  )

  expect_true(file.exists(out))
})

test_that("annotate_spectra handles different similarity methods", {
  skip_on_cran()
  withr::local_dir(new = temp_test_dir("ann_spe_methods"))
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
  write_minimal_mgf(lib_path, precursors = c(200))

  # Test entropy method
  out <- annotate_spectra(
    libraries = list(pos = lib_path),
    polarity = "pos",
    method = "entropy",
    threshold = 0
  )

  expect_true(file.exists(out))
})

test_that("annotate_spectra validates method parameter", {
  skip_on_cran()
  withr::local_dir(new = temp_test_dir("ann_spe_method_invalid"))
  local_test_project(copy = TRUE)

  lib_path <- file.path(
    "data",
    "interim",
    "libraries",
    "spectra",
    "lib_pos.mgf"
  )
  write_minimal_mgf(lib_path, precursors = c(150))

  expect_error(
    annotate_spectra(
      libraries = list(pos = lib_path),
      polarity = "pos",
      method = "invalid_method"
    ),
    "method"
  )
})

test_that("annotate_spectra validates approx parameter", {
  skip_on_cran()
  withr::local_dir(new = temp_test_dir("ann_spe_approx_invalid"))
  local_test_project(copy = TRUE)

  lib_path <- file.path(
    "data",
    "interim",
    "libraries",
    "spectra",
    "lib_pos.mgf"
  )
  write_minimal_mgf(lib_path, precursors = c(150))

  expect_error(
    annotate_spectra(
      libraries = list(pos = lib_path),
      polarity = "pos",
      approx = "yes"
    ),
    "approx"
  )
})
