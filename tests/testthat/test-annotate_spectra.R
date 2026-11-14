# Test Suite: annotate_spectra ----

library(testthat)

# Helper to create minimal mgf with adjustable precursor masses & polarity
write_minimal_mgf <- function(path, precursors, charge = "1+") {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  lines <- unlist(
    lapply(seq_along(precursors), function(i) {
      mz <- precursors[[i]]
      c(
        "BEGIN IONS",
        paste0("TITLE=Spectrum_", i),
        paste0("PEPMASS=", mz),
        paste0("CHARGE=", charge),
        paste0("NAME=Name_", i),
        paste0("INCHIKEY=AAAAAAAAAAAAAA-BBBBBBBBBB-C"),
        paste0("SMILES=CCC"),
        "MSLEVEL=2",
        paste0(round(mz * 0.5), " 100"),
        paste0(round(mz * 0.75), " 200"),
        paste0(round(mz), " 300"),
        "END IONS",
        ""
      )
    }),
    use.names = FALSE
  )
  writeLines(lines, path)
  invisible(path)
}

# Validation tests ----

test_that("annotate_spectra validates parameters", {
  withr::local_dir(new = temp_test_dir("ann_spe_validate"))
  local_test_project(copy = TRUE)
  paths <- get_default_paths()
  write_minimal_mgf(paths$data$source$spectra, precursors = c(150))
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
    annotate_spectra(libraries = list(pos = lib_path), cutoff = -10),
    "cutoff"
  )
  expect_error(
    annotate_spectra(
      input = "nope.mgf",
      libraries = list(pos = lib_path),
      polarity = "pos"
    ),
    "Input data validation failed"
  )
  expect_error(
    annotate_spectra(libraries = list(pos = "nope.mgf"), polarity = "pos"),
    "library file"
  )
})

# Basic run & output ----

test_that("annotate_spectra produces output file and columns", {
  skip_if_no_network("https://github.com")
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
  expect_warning(
    out <- annotate_spectra(
      libraries = list(pos = lib_path),
      polarity = "pos",
      ppm = 20,
      dalton = 0.05
    ),
    "Skipped.*MS1.*spectrum|cannot be annotated by spectral matching"
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
  skip_if_no_network("https://github.com")
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
  expect_warning(
    out_low <- annotate_spectra(
      libraries = list(pos = lib_path),
      polarity = "pos",
      threshold = 0.0
    ),
    "Skipped.*MS1.*spectrum|cannot be annotated by spectral matching"
  )
  expect_warning(
    out_high <- annotate_spectra(
      libraries = list(pos = lib_path),
      polarity = "pos",
      threshold = 0.9
    ),
    "Skipped.*MS1.*spectrum|cannot be annotated by spectral matching"
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
  skip_if_no_network("https://github.com")
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
  expect_warning(
    out <- annotate_spectra(
      libraries = c(lib_pos, lib_neg),
      polarity = "pos",
      threshold = 0
    ),
    "Skipped.*MS1.*spectrum|cannot be annotated by spectral matching"
  )
  expect_true(file.exists(out))
  # When only neg libraries provided for pos polarity, expect empty template
  expect_warning(
    out2 <- annotate_spectra(
      libraries = c(lib_neg),
      polarity = "pos",
      threshold = 0
    ),
    "Skipped.*MS1.*spectrum|cannot be annotated by spectral matching"
  )
  df2 <- tidytable::fread(out2)
  expect_equal(nrow(df2), 1)
})

# Empty peaks in library cleaning ----

test_that("library spectra with empty peaks are removed before concatenation", {
  skip_if_no_network("https://github.com")
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
  expect_warning(
    out <- annotate_spectra(
      libraries = c(lib_ok, lib_empty),
      polarity = "pos",
      threshold = 0
    ),
    "Skipped.*MS1.*spectrum|cannot be annotated by spectral matching"
  )
  expect_true(file.exists(out))
  df <- tidytable::fread(out)
  expect_true(nrow(df) >= 1)
})

# High intensity cutoff yields no query spectra ----

test_that("high cutoff removes all query peaks leading to empty template", {
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
    cutoff = 1e9,
    threshold = 0
  )
  df <- tidytable::fread(out)
  expect_equal(nrow(df), 1)
})

# Duplicate candidate collapse by connectivity layer ----

test_that("duplicate candidates collapsed by SMILES are unique", {
  skip_if_no_network("https://github.com")
  withr::local_dir(new = temp_test_dir("ann_spe_dedupe"))
  local_test_project(copy = TRUE)
  paths <- get_default_paths()
  get_file(
    url = paths$urls$examples$spectra_mini,
    export = paths$data$source$spectra
  )
  # Build two libraries with same SMILES but different spectrum IDs
  lib1 <- file.path("data", "interim", "libraries", "spectra", "lib1_pos.mgf")
  lib2 <- file.path("data", "interim", "libraries", "spectra", "lib2_pos.mgf")
  write_minimal_mgf(lib1, precursors = c(300))
  # For lib2, reuse same precursor; SMILES is fixed by write_minimal_mgf
  write_minimal_mgf(lib2, precursors = c(300))
  expect_warning(
    out <- annotate_spectra(
      libraries = c(lib1, lib2),
      polarity = "pos",
      threshold = 0
    ),
    "Skipped.*MS1.*spectrum|cannot be annotated by spectral matching"
  )
  df <- tidytable::fread(out)
  uniq <- tidytable::distinct(
    df,
    feature_id,
    candidate_structure_smiles_no_stereo
  )
  expect_equal(nrow(uniq), nrow(df))
})

# NA metadata fallback handling ----

test_that("NA fallback for smiles is applied", {
  skip_if_no_network("https://github.com")
  withr::local_dir(new = temp_test_dir("ann_spe_na_fallback"))
  local_test_project(copy = TRUE)
  paths <- get_default_paths()
  get_file(
    url = paths$urls$examples$spectra_mini,
    export = paths$data$source$spectra
  )
  # Create a library entry missing SMILES_2D to trigger fallback to SMILES
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
      "PEPMASS=200",
      "CHARGE=1+",
      "NAME=Example",
      "SMILES=CCO",
      "MSLEVEL=2",
      "100 100",
      "150 200",
      "200 300",
      "END IONS",
      ""
    ),
    lib_path
  )
  expect_warning(
    out <- annotate_spectra(
      libraries = list(pos = lib_path),
      polarity = "pos",
      threshold = 0
    ),
    "Skipped.*MS1.*spectrum|cannot be annotated by spectral matching"
  )
  df <- tidytable::fread(out)
  expect_true("candidate_structure_smiles_no_stereo" %in% names(df))
  # Filter out placeholder/template rows (no similarity score)
  actual <- df[!is.na(df$candidate_score_similarity), ]
  # SMILES fallback should use target_smiles when target_smiles_no_stereo is NA
  if (nrow(actual) > 0L) {
    expect_true(!anyNA(actual$candidate_structure_smiles_no_stereo))
  }
})

# Internal helper function tests ----

test_that("compute_similarity_safe uses precursor_mz fallback metadata", {
  temp_dir <- tempdir()
  query_path <- file.path(temp_dir, "query_precursor_mz.mgf")
  lib_path <- file.path(temp_dir, "lib_precursor_mz.mgf")
  dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)
  write_minimal_mgf(query_path, precursors = c(200))
  write_minimal_mgf(lib_path, precursors = c(200))

  query_sp <- import_spectra(
    query_path,
    cutoff = 0,
    dalton = 0.01,
    min_fragments = 1L,
    polarity = "pos",
    ppm = 10
  )
  lib_sp <- import_spectra(
    lib_path,
    cutoff = 0,
    dalton = 0.01,
    min_fragments = 1L,
    polarity = "pos",
    ppm = 10
  )

  query_sp@backend@spectraData$precursor_mz <-
    query_sp@backend@spectraData$precursorMz
  query_sp@backend@spectraData$precursorMz <- NULL
  query_sp@backend@spectraData$spectrum_id <- "Q1"
  lib_sp@backend@spectraData$precursor_mz <- lib_sp@backend@spectraData$precursorMz
  lib_sp@backend@spectraData$precursorMz <- NULL
  lib_sp@backend@spectraData$spectrum_id <- "L1"

  out <- compute_similarity_safe(
    lib_sp = lib_sp,
    query_sp = query_sp,
    query_meta = tidytable::tidytable(
      feature_id = "F1",
      query_adduct = NA_character_
    ),
    method = "cosine",
    dalton = 0.01,
    ppm = 10,
    threshold = 0,
    approx = TRUE
  )

  expect_s3_class(out, "data.frame")
  expect_true(nrow(out) >= 1L)
})

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
    "output path must be a character string"
  )

  expect_error(
    resolve_annotation_output(character(0)),
    "output must contain at least one file path"
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
  skip_if_no_network("https://github.com")
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
    "at least one library must be provided",
    class = "tima_validation_error"
  )

  expect_error(
    annotate_spectra(libraries = list(), polarity = "pos"),
    "Library elements must be character strings"
  )
})

test_that("annotate_spectra applies intensity cutoff", {
  skip_if_no_network("https://github.com")
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
  expect_warning(
    out <- annotate_spectra(
      libraries = list(pos = lib_path),
      polarity = "pos",
      cutoff = 50,
      threshold = 0
    ),
    "Skipped.*MS1.*spectrum|cannot be annotated by spectral matching"
  )

  expect_true(file.exists(out))
})

test_that("annotate_spectra handles different similarity methods", {
  skip_if_no_network("https://github.com")
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
  expect_warning(
    out <- annotate_spectra(
      libraries = list(pos = lib_path),
      polarity = "pos",
      method = "entropy",
      threshold = 0
    ),
    "Skipped.*MS1.*spectrum|cannot be annotated by spectral matching"
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

test_that("annotate_spectra maps deprecated qutoff to cutoff", {
  withr::local_options(lifecycle_verbosity = "warning")
  withr::local_dir(new = temp_test_dir("ann_spe_mock_qutoff"))
  query_file <- tempfile(fileext = ".mgf")
  lib_file <- tempfile(fileext = ".mgf")
  write_minimal_mgf(query_file, precursors = c(100))
  write_minimal_mgf(lib_file, precursors = c(101))
  output_file <- temp_test_path("out.tsv")

  expect_warning(
    out <- annotate_spectra(
      input = query_file,
      libraries = lib_file,
      polarity = "pos",
      output = output_file,
      qutoff = 42,
      approx = TRUE
    ),
    "deprecated"
  )

  expect_identical(out, output_file)
  expect_true(file.exists(out))

  # Also works when evaluated in a non-namespace package environment (e.g. covr)
  fn_pkg_env <- annotate_spectra
  ns_list <- as.list(asNamespace("tima"), all.names = TRUE)
  ns_list[[".__NAMESPACE__."]] <- NULL
  environment(fn_pkg_env) <- list2env(ns_list, parent = globalenv())
  caller <- new.env(parent = globalenv())
  caller$fn_pkg_env <- fn_pkg_env
  caller$query_file <- query_file
  caller$lib_file <- lib_file
  caller$output_file <- output_file
  expect_warning(
    evalq(
      fn_pkg_env(
        input = query_file,
        libraries = lib_file,
        polarity = "pos",
        output = output_file,
        qutoff = 42,
        approx = TRUE
      ),
      caller
    ),
    "deprecated"
  )
})

test_that("annotate_spectra reports missing input files after pre-flight checks", {
  withr::local_dir(new = temp_test_dir("ann_spe_mock_missing_input"))

  expect_error(
    annotate_spectra(
      input = "missing_query.mgf",
      libraries = "lib_pos.mgf",
      polarity = "pos",
      output = temp_test_path("out.tsv"),
      approx = TRUE
    ),
    "Input data validation failed",
    class = "tima_validation_error"
  )
})

test_that("annotate_spectra reports missing library files after pre-flight checks", {
  withr::local_dir(new = temp_test_dir("ann_spe_mock_missing_library"))
  query_file <- tempfile(fileext = ".mgf")
  writeLines(
    c("BEGIN IONS", "PEPMASS=100", "CHARGE=1+", "100 10", "END IONS"),
    query_file
  )

  expect_error(
    annotate_spectra(
      input = query_file,
      libraries = "missing_library.mgf",
      polarity = "pos",
      output = temp_test_path("out.tsv"),
      approx = TRUE
    ),
    "library file\\(s\\) not found",
    class = "tima_validation_error"
  )
})

test_that("annotate_spectra returns empty template when polarity filtering removes all libraries", {
  withr::local_dir(new = temp_test_dir("ann_spe_mock_empty_libs"))
  query_file <- tempfile(fileext = ".mgf")
  lib_a <- tempfile(fileext = ".mgf")
  lib_b <- tempfile(fileext = ".mgf")
  write_minimal_mgf(query_file, precursors = c(200))
  write_minimal_mgf(lib_a, precursors = c(210))
  write_minimal_mgf(lib_b, precursors = c(220))
  output_file <- temp_test_path("out.tsv")

  local_mocked_bindings(
    get_params = function(step) list(),
    export_params = function(...) invisible(NULL),
    resolve_annotation_output = function(output) output,
    assert_choice = function(...) invisible(NULL),
    assert_flag = function(...) invisible(NULL),
    assert_scalar_numeric = function(...) invisible(NULL),
    normalize_input_files = function(x, label) x,
    sanitize_all_inputs = function(mgf_file) invisible(NULL),
    import_spectra = function(
      input,
      cutoff,
      dalton,
      min_fragments,
      polarity,
      ppm
    ) {
      list("query")
    },
    filter_library_paths_by_polarity = function(paths, polarity) character(0),
    annotate_spectra_handle_empty_result = function(
      path,
      params,
      message = NULL
    ) {
      expect_identical(message, "No libraries remain after polarity filtering")
      path
    }
  )

  out <- annotate_spectra(
    input = query_file,
    libraries = c(lib_a, lib_b),
    polarity = "pos",
    output = output_file,
    approx = TRUE
  )

  expect_identical(out, output_file)
})

test_that("annotate_spectra returns empty template when cleaned library is empty", {
  withr::local_dir(new = temp_test_dir("ann_spe_mock_clean_empty"))
  query_file <- tempfile(fileext = ".mgf")
  lib_file <- tempfile(fileext = ".mgf")
  write_minimal_mgf(query_file, precursors = c(300))
  write_minimal_mgf(lib_file, precursors = c(301))
  output_file <- temp_test_path("out.tsv")

  local_mocked_bindings(
    get_params = function(step) list(),
    export_params = function(...) invisible(NULL),
    resolve_annotation_output = function(output) output,
    assert_choice = function(...) invisible(NULL),
    assert_flag = function(...) invisible(NULL),
    assert_scalar_numeric = function(...) invisible(NULL),
    normalize_input_files = function(x, label) x,
    sanitize_all_inputs = function(mgf_file) invisible(NULL),
    import_spectra = function(
      input,
      cutoff,
      dalton,
      min_fragments,
      polarity,
      ppm
    ) {
      list("query")
    },
    filter_library_paths_by_polarity = function(paths, polarity) paths,
    import_and_clean_library_collection = function(
      libs_vec,
      dalton,
      polarity,
      ppm,
      approx = TRUE,
      query_precursors = NULL,
      query_adducts = NULL
    ) {
      invisible(approx)
      invisible(query_precursors)
      invisible(query_adducts)
      list()
    },
    annotate_spectra_handle_empty_result = function(
      path,
      params,
      message = NULL
    ) {
      expect_identical(message, "No spectra left in library after cleaning")
      path
    }
  )

  out <- annotate_spectra(
    input = query_file,
    libraries = lib_file,
    polarity = "pos",
    output = output_file,
    approx = TRUE
  )

  expect_identical(out, output_file)
})

test_that("annotate_spectra returns empty template when precursor reduction removes all library spectra", {
  withr::local_dir(new = temp_test_dir("ann_spe_mock_precursor_empty"))
  query_file <- tempfile(fileext = ".mgf")
  lib_file <- tempfile(fileext = ".mgf")
  write_minimal_mgf(query_file, precursors = c(400))
  write_minimal_mgf(lib_file, precursors = c(401))
  output_file <- temp_test_path("out.tsv")

  local_mocked_bindings(
    get_params = function(step) list(),
    export_params = function(...) invisible(NULL),
    resolve_annotation_output = function(output) output,
    assert_choice = function(...) invisible(NULL),
    assert_flag = function(...) invisible(NULL),
    assert_scalar_numeric = function(...) invisible(NULL),
    normalize_input_files = function(x, label) x,
    sanitize_all_inputs = function(mgf_file) invisible(NULL),
    import_spectra = function(
      input,
      cutoff,
      dalton,
      min_fragments,
      polarity,
      ppm
    ) {
      list("query")
    },
    filter_library_paths_by_polarity = function(paths, polarity) paths,
    import_and_clean_library_collection = function(
      libs_vec,
      dalton,
      polarity,
      ppm,
      approx = TRUE,
      query_precursors = NULL,
      query_adducts = NULL
    ) {
      invisible(libs_vec)
      invisible(dalton)
      invisible(polarity)
      invisible(ppm)
      invisible(query_precursors)
      invisible(query_adducts)
      if (isTRUE(approx)) {
        list("lib")
      } else {
        list()
      }
    },
    log_library_stats = function(spectral_library) invisible(NULL),
    get_precursors = function(x) 100,
    annotate_spectra_handle_empty_result = function(
      path,
      params,
      message = NULL
    ) {
      expect_identical(
        message,
        "No spectra remain after precursor-based reduction"
      )
      path
    }
  )

  out <- annotate_spectra(
    input = query_file,
    libraries = lib_file,
    polarity = "pos",
    output = output_file,
    approx = FALSE
  )

  expect_identical(out, output_file)
})

test_that("resolve_annotation_output validates type and length", {
  expect_error(
    resolve_annotation_output(1),
    "output path must be a character string",
    class = "tima_validation_error"
  )

  expect_error(
    resolve_annotation_output(character(0)),
    "output must contain at least one file path",
    class = "tima_validation_error"
  )

  expect_identical(resolve_annotation_output(c("a.tsv", "b.tsv")), "a.tsv")
})

test_that("normalize_input_files flattens lists and validates element types", {
  expect_equal(
    normalize_input_files(list("a.mgf", "b.mgf"), "Input"),
    c("a.mgf", "b.mgf")
  )

  expect_error(
    normalize_input_files(1:3, "Input"),
    "Input elements must be character strings",
    class = "tima_validation_error"
  )

  expect_equal(
    normalize_input_files(list("a.mgf", 2), "Input"),
    c("a.mgf", "2")
  )
})

test_that("filter_library_paths_by_polarity keeps single path and filters vectors", {
  expect_identical(
    filter_library_paths_by_polarity("lib_pos.mgf", "pos"),
    "lib_pos.mgf"
  )

  expect_equal(
    filter_library_paths_by_polarity(
      c("lib_pos_a.mgf", "lib_neg_b.mgf", "lib_pos_c.mgf"),
      "pos"
    ),
    c("lib_pos_a.mgf", "lib_pos_c.mgf")
  )
})

test_that("finalize_results uses SMILES for deduplication", {
  df_sim <- tidytable::tidytable(
    feature_id = "F1",
    target_id = 1L,
    precursorMz = 100,
    candidate_spectrum_entropy = "0.8",
    candidate_score_similarity = "0.7",
    candidate_count_similarity_peaks_matched = "3"
  )

  meta <- tidytable::tidytable(
    target_id = 1L,
    target_adduct = "[M+H]+",
    target_library = "lib",
    target_spectrum_id = "sp1",
    target_smiles = "CCO",
    target_smiles_no_stereo = NA_character_,
    target_name = "ethanol",
    target_precursorMz = 101,
    target_neutral_mass = 100
  )
  query_meta <- tidytable::tidytable(
    feature_id = "F1",
    query_adduct = "[M+H]+",
    query_precursorMz = 100,
    query_neutral_mass = 99
  )

  out <- finalize_results(df_sim = df_sim, meta = meta, query_meta = query_meta)

  expect_equal(out$candidate_structure_smiles_no_stereo, "CCO")
  expect_false(
    "candidate_structure_inchikey_connectivity_layer" %in% names(out)
  )
})

test_that("finalize_results returns empty tidytable for empty similarity input", {
  out <- finalize_results(
    df_sim = tidytable::tidytable(),
    meta = tidytable::tidytable()
  )

  expect_equal(nrow(out), 0L)
})

test_that("compute_similarity_safe returns empty table on calculation error", {
  withr::local_dir(new = temp_test_dir("ann_spe_compute_safe"))
  local_test_project(copy = TRUE)

  query_path <- tempfile(fileext = ".mgf")
  lib_path <- tempfile(fileext = ".mgf")
  write_minimal_mgf(query_path, precursors = c(200))
  write_minimal_mgf(lib_path, precursors = c(200))

  query_sp <- import_spectra(
    query_path,
    cutoff = 0,
    dalton = 0.01,
    polarity = "pos",
    ppm = 10
  )
  lib_sp <- import_spectra(
    lib_path,
    cutoff = 0,
    dalton = 0.01,
    polarity = "pos",
    ppm = 10
  )

  local_mocked_bindings(
    calculate_entropy_and_similarity = function(...) {
      stop("forced similarity failure")
    }
  )

  out <- compute_similarity_safe(
    lib_sp = lib_sp,
    query_sp = query_sp,
    method = "gnps",
    dalton = 0.01,
    ppm = 10,
    threshold = 0,
    approx = TRUE
  )

  expect_equal(nrow(out), 0L)
})

test_that("convert_precursor_for_matching converts known adducts to neutral mass", {
  expected_1 <- calculate_mass_of_m(mz = 123.4567, adduct_string = "[M+H]+")
  expected_2 <- calculate_mass_of_m(mz = 145.4421, adduct_string = "[M+Na]+")

  out <- convert_precursor_for_matching(
    precursors = c(123.4567, 145.4421),
    adducts = c("[M+H]+", "[M+Na]+")
  )

  expect_equal(out[[1L]], expected_1, tolerance = 1e-5)
  expect_equal(out[[2L]], expected_2, tolerance = 1e-5)
  expect_true(all(is.finite(out)))
})

test_that("convert_precursor_for_matching falls back to precursor m/z when adduct is missing", {
  prec <- c(100.1, 200.2)
  out <- convert_precursor_for_matching(
    precursors = prec,
    adducts = c(NA_character_, "")
  )

  expect_equal(out, prec)
})

test_that("annotate_spectra_handle_empty_result returns path and writes empty template", {
  withr::local_dir(new = temp_test_dir("ann_spe_mock_empty_result"))
  output_file <- temp_test_path("out.tsv")

  out <- annotate_spectra_handle_empty_result(
    path = output_file,
    params = list(a = 1L),
    message = NULL
  )

  expect_equal(out, output_file)
  expect_true(file.exists(output_file))
})

test_that("extract_vector uses fill when field is missing", {
  obj <- Spectra::Spectra(data.frame(
    precursorMz = 123.4,
    mz = I(list(c(50, 100))),
    intensity = I(list(c(1, 2)))
  ))

  out <- extract_vector(
    obj = obj,
    field = "nonexistent_field",
    len = 3L,
    fill = "missing"
  )

  expect_equal(out, rep("missing", 3L))
})

test_that("get_precursors extracts precursorMz when available", {
  sp <- Spectra::Spectra(data.frame(
    precursorMz = c(123.4, 234.5),
    mz = I(list(c(50, 100), c(60, 120))),
    intensity = I(list(c(1, 2), c(3, 4)))
  ))

  out <- get_precursors(sp)
  expect_equal(out, c(123.4, 234.5))
})

test_that("get_precursors falls back to precursor_mz alias", {
  sp <- Spectra::Spectra(data.frame(
    precursor_mz = c(111.1, 222.2),
    mz = I(list(c(40, 80), c(55, 110))),
    intensity = I(list(c(2, 4), c(3, 6)))
  ))

  out <- get_precursors(sp)
  expect_equal(out, c(111.1, 222.2))
})

test_that("extract_vector resolves alias names with exact and case-insensitive matching", {
  obj <- Spectra::Spectra(data.frame(
    precursorMz = c(123.4, 234.5),
    smiles_no_stereo = c("CCO", "CCC"),
    FORMULA = c("C2H6O", "C3H8"),
    mz = I(list(c(50, 100), c(60, 120))),
    intensity = I(list(c(1, 2), c(3, 4)))
  ))

  out_smiles <- extract_vector(
    obj = obj,
    field = c("smiles_2D", "smiles_no_stereo"),
    len = 2L,
    fill = NA_character_
  )
  out_formula <- extract_vector(
    obj = obj,
    field = c("formula", "FORMULA"),
    len = 2L,
    fill = NA_character_
  )

  expect_equal(out_smiles, c("CCO", "CCC"))
  expect_equal(out_formula, c("C2H6O", "C3H8"))
})

test_that("build_library_metadata extracts SMILES-based metadata aliases", {
  lib_sp <- Spectra::Spectra(data.frame(
    precursorMz = c(100, 200),
    adduct = c("[M+H]+", "[M+Na]+"),
    structure_smiles = c("CCO", "CCC"),
    structure_smiles_no_stereo = c("CCO", "CCC"),
    structure_name = c("ethanol", "propane"),
    structure_tag = c("well_formed", "trusted"),
    library = c("GNPS", "ISDB - Wikidata"),
    mz = I(list(c(50, 100), c(80, 120))),
    intensity = I(list(c(10, 20), c(30, 40)))
  ))

  meta <- build_library_metadata(lib_sp = lib_sp, lib_precursors = c(100, 200))

  expect_equal(meta$target_name, c("ethanol", "propane"))
  expect_equal(meta$target_smiles_no_stereo, c("CCO", "CCC"))
  expect_equal(meta$target_tag, c("well_formed", "trusted"))
  ## InChIKey, formula, mass, xlogp are no longer extracted;
  ## they are recomputed from SMILES downstream via process_smiles().
  expect_false("target_inchikey" %in% names(meta))
  expect_false("target_formula" %in% names(meta))
})

test_that("resolve_ms1_query_adduct_map selects the best adduct per feature", {
  ms1_tbl <- data.frame(
    feature_id = c("A", "A", "B", "C", "C"),
    candidate_adduct = c("[M+H]+", "[M+Na]+", "[M+H]+", "[M+H]+", "[M+H]+"),
    candidate_annotation_level = c(
      "secondary",
      "primary",
      "primary",
      "secondary",
      "secondary"
    ),
    candidate_evidence_tier = c(
      "baseline",
      "supported_strong",
      "supported_weak",
      "baseline",
      "supported_weak"
    ),
    candidate_structure_error_mz = c(0.2, 0.0, 0.5, 0.3, 0.1)
  )

  out <- resolve_ms1_query_adduct_map(ms1_tbl)

  expect_equal(out$feature_id, c("A", "B", "C"))
  expect_equal(out$ms1_query_adduct, c("[M+Na]+", "[M+H]+", "[M+H]+"))
})

test_that("finalize_results outputs SMILES-based structure identification only", {
  df_sim <- tidytable::tidytable(
    feature_id = "F1",
    target_id = 1L,
    precursorMz = 100,
    candidate_spectrum_entropy = "0.8",
    candidate_score_similarity = "0.7",
    candidate_count_similarity_peaks_matched = "3"
  )

  meta <- tidytable::tidytable(
    target_id = 1L,
    target_adduct = "[M+H]+",
    target_library = "lib",
    target_spectrum_id = "sp1",
    target_smiles = "CCO",
    target_smiles_no_stereo = "CCO",
    target_name = "ethanol",
    target_precursorMz = 101,
    target_neutral_mass = 100
  )
  query_meta <- tidytable::tidytable(
    feature_id = "F1",
    query_adduct = "[M+H]+",
    query_precursorMz = 100,
    query_neutral_mass = 99
  )

  out <- finalize_results(df_sim = df_sim, meta = meta, query_meta = query_meta)

  expect_equal(out$candidate_structure_smiles_no_stereo, "CCO")
  ## InChIKey columns are no longer derived in finalize_results;

  ## they are recomputed from SMILES downstream via process_smiles().
  expect_false("candidate_structure_inchikey_no_stereo" %in% names(out))
  expect_false(
    "candidate_structure_inchikey_connectivity_layer" %in% names(out)
  )
})

test_that("finalize_results keeps the highest-similarity structure when deduplicating", {
  df_sim <- tidytable::tidytable(
    feature_id = c("F1", "F1"),
    target_id = c(1L, 2L),
    precursorMz = c(100, 100),
    candidate_spectrum_entropy = c("0.6", "0.6"),
    candidate_score_similarity = c("0.40", "0.90"),
    candidate_score_similarity_forward = c("0.40", "0.90"),
    candidate_score_similarity_reverse = c("0.40", "0.90"),
    candidate_count_similarity_peaks_matched = c("3", "3")
  )

  meta <- tidytable::tidytable(
    target_id = c(1L, 2L),
    target_adduct = c("[M+H]+", "[M+Na]+"),
    target_library = c("lib", "lib"),
    target_spectrum_id = c("sp1", "sp2"),
    target_smiles = c("CCO", "CCO"),
    target_smiles_no_stereo = c("CCO", "CCO"),
    target_name = c("ethanol", "ethanol"),
    target_precursorMz = c(101, 123),
    target_neutral_mass = c(100, 100)
  )
  query_meta <- tidytable::tidytable(
    feature_id = "F1",
    query_adduct = "[M+H]+",
    query_precursorMz = 100,
    query_neutral_mass = 99
  )

  out <- finalize_results(df_sim = df_sim, meta = meta, query_meta = query_meta)

  expect_identical(out$candidate_spectrum_id[[1L]], "sp2")
  expect_identical(out$candidate_score_similarity[[1L]], 0.90)
})


test_that("extract_vector resolves alias names with exact and case-insensitive matching", {
  obj <- Spectra::Spectra(data.frame(
    precursorMz = c(123.4, 234.5),
    smiles_no_stereo = c("CCO", "CCC"),
    FORMULA = c("C2H6O", "C3H8"),
    mz = I(list(c(50, 100), c(60, 120))),
    intensity = I(list(c(1, 2), c(3, 4)))
  ))

  out_smiles <- extract_vector(
    obj = obj,
    field = c("smiles_2D", "smiles_no_stereo"),
    len = 2L,
    fill = NA_character_
  )
  out_formula <- extract_vector(
    obj = obj,
    field = c("formula", "FORMULA"),
    len = 2L,
    fill = NA_character_
  )

  expect_equal(out_smiles, c("CCO", "CCC"))
  expect_equal(out_formula, c("C2H6O", "C3H8"))
})

test_that("build_library_metadata extracts SMILES-based metadata aliases (2)", {
  lib_sp <- Spectra::Spectra(data.frame(
    precursorMz = c(100, 200),
    adduct = c("[M+H]+", "[M+Na]+"),
    structure_smiles = c("CCO", "CCC"),
    structure_smiles_no_stereo = c("CCO", "CCC"),
    structure_name = c("ethanol", "propane"),
    structure_tag = c("well_formed", "trusted"),
    library = c("GNPS", "ISDB - Wikidata"),
    mz = I(list(c(50, 100), c(80, 120))),
    intensity = I(list(c(10, 20), c(30, 40)))
  ))

  meta <- build_library_metadata(lib_sp = lib_sp, lib_precursors = c(100, 200))

  expect_equal(meta$target_name, c("ethanol", "propane"))
  expect_equal(meta$target_smiles_no_stereo, c("CCO", "CCC"))
  expect_equal(meta$target_tag, c("well_formed", "trusted"))
  expect_false("target_inchikey" %in% names(meta))
  expect_false("target_formula" %in% names(meta))
})

test_that("extract_vector handles empty fields and length reconciliation branches", {
  obj_one <- Spectra::Spectra(data.frame(
    precursorMz = 123.4,
    name = "n1",
    mz = I(list(c(50, 100))),
    intensity = I(list(c(1, 2)))
  ))

  expect_equal(
    extract_vector(obj_one, field = character(0), len = 3L, fill = "x"),
    c("x", "x", "x")
  )

  expect_equal(
    extract_vector(obj_one, field = "name", len = 3L, fill = NA_character_),
    c("n1", "n1", "n1")
  )

  obj_two <- Spectra::Spectra(data.frame(
    precursorMz = c(100, 200),
    name = c("A", "B"),
    mz = I(list(c(50, 80), c(60, 90))),
    intensity = I(list(c(1, 2), c(3, 4)))
  ))

  expect_equal(
    extract_vector(obj_two, field = "name", len = 3L, fill = "pad"),
    c("A", "B", "pad")
  )

  expect_equal(
    extract_vector(obj_two, field = "name", len = 1L, fill = "pad"),
    "A"
  )
})

test_that("get_precursors returns NA vector when precursor columns are missing", {
  sp <- Spectra::Spectra(data.frame(
    spectrum_id = c("s1", "s2"),
    mz = I(list(c(40, 80), c(60, 90))),
    intensity = I(list(c(2, 4), c(3, 6)))
  ))

  out <- get_precursors(sp)
  expect_equal(length(out), 2L)
  expect_true(all(is.na(out)))
})

test_that("convert_precursor_for_matching handles guard and invalid-conversion branches", {
  expect_equal(
    convert_precursor_for_matching(numeric(0), character(0)),
    numeric(0)
  )

  expect_equal(
    convert_precursor_for_matching(c(100, 200), c("[M+H]+")),
    c(100, 200)
  )

  expect_equal(
    convert_precursor_for_matching(c(100, 200), c("", NA_character_)),
    c(100, 200)
  )

  local_mocked_bindings(
    parse_adduct = function(x) {
      force(x)
      list(n_charges = 0L, n_mer = 1L, n_iso = 0L, los_add_clu = 0)
    }
  )
  expect_equal(
    convert_precursor_for_matching(c(100), c("[M+H]+")),
    100
  )

  local_mocked_bindings(
    parse_adduct = function(x) {
      force(x)
      list(n_charges = 1L, n_mer = 1L, n_iso = 0L, los_add_clu = 200)
    }
  )
  out <- convert_precursor_for_matching(c(100), c("[M+H]+"))
  expect_equal(out[[1L]], 100)
})

test_that("reduce_library_by_precursor keeps only overlapping precursor windows", {
  lib_sp <- Spectra::Spectra(data.frame(
    precursorMz = c(100, 200, 300),
    mz = I(list(c(50, 90), c(60, 120), c(80, 140))),
    intensity = I(list(c(10, 20), c(30, 40), c(50, 60)))
  ))

  kept <- reduce_library_by_precursor(
    lib_sp = lib_sp,
    query_precursors = 200.01,
    dalton = 0.05,
    ppm = 5
  )
  expect_equal(length(kept), 1L)
  expect_equal(get_precursors(kept), 200)

  none <- reduce_library_by_precursor(
    lib_sp = lib_sp,
    query_precursors = 500,
    dalton = 0.01,
    ppm = 1
  )
  expect_equal(length(none), 0L)
})

test_that("reduce_library_by_precursor keeps adduct-shifted library spectra via neutral mass", {
  query_m <- 300
  query_prec <- calculate_mz_from_mass(query_m, "[M+H4N]+")
  lib_prec <- calculate_mz_from_mass(query_m, "[M+H]+")

  lib_sp <- Spectra::Spectra(data.frame(
    precursorMz = c(lib_prec, 500),
    adduct = c("[M+H]+", "[M+H]+"),
    mz = I(list(c(50, 80), c(60, 100))),
    intensity = I(list(c(10, 20), c(30, 40)))
  ))

  kept <- reduce_library_by_precursor(
    lib_sp = lib_sp,
    query_precursors = query_prec,
    query_adducts = "[M+H4N]+",
    dalton = 0.01,
    ppm = 5
  )

  expect_equal(length(kept), 1L)
  expect_equal(get_precursors(kept)[[1L]], lib_prec, tolerance = 1e-8)
})

test_that("compute_similarity_safe returns calculation output on success", {
  query_sp <- Spectra::Spectra(data.frame(
    precursorMz = 100,
    spectrum_id = "q1",
    mz = I(list(c(50, 80))),
    intensity = I(list(c(10, 20)))
  ))
  lib_sp <- Spectra::Spectra(data.frame(
    precursorMz = 100,
    mz = I(list(c(50, 80))),
    intensity = I(list(c(10, 20)))
  ))

  local_mocked_bindings(
    calculate_entropy_and_similarity = function(...) {
      tidytable::tidytable(target_id = 1L, feature_id = "q1")
    }
  )

  out <- compute_similarity_safe(
    lib_sp = lib_sp,
    query_sp = query_sp,
    method = "entropy",
    dalton = 0.01,
    ppm = 10,
    threshold = 0,
    approx = TRUE
  )

  expect_equal(nrow(out), 1L)
  expect_true(all(c("target_id", "feature_id") %in% names(out)))
})

test_that("finalize_results flags exact adduct and neutral-mass rescued matches", {
  df_sim <- tidytable::tidytable(
    feature_id = c("F1", "F2"),
    target_id = c(1L, 2L),
    precursorMz = c(100, 200),
    candidate_spectrum_entropy = c("0.8", "0.8"),
    candidate_score_similarity = c("0.9", "0.7"),
    candidate_score_similarity_forward = c("0.9", "0.7"),
    candidate_score_similarity_reverse = c("0.9", "0.7"),
    candidate_count_similarity_peaks_matched = c("3", "3"),
    .similarity_space = c("precursor_mz", "neutral_M")
  )
  meta <- tidytable::tidytable(
    target_id = c(1L, 2L),
    target_adduct = c("[M+H]+", "[M+H]+"),
    target_library = c("lib", "lib"),
    target_spectrum_id = c("sp1", "sp2"),
    target_smiles = c("CCO", "CCC"),
    target_smiles_no_stereo = c("CCO", "CCC"),
    target_name = c("ethanol", "propane"),
    target_precursorMz = c(101, 201),
    target_neutral_mass = c(100, 200)
  )
  query_meta <- tidytable::tidytable(
    feature_id = c("F1", "F2"),
    query_adduct = c("[M+H]+", "[M+H4N]+"),
    query_precursorMz = c(100, 200),
    query_neutral_mass = c(99, 199)
  )

  out <- finalize_results(df_sim = df_sim, meta = meta, query_meta = query_meta)

  expect_identical(
    out$candidate_adduct_match_mode[out$feature_id == "F1"],
    "exact_adduct"
  )
  expect_identical(
    out$candidate_adduct_match_mode[out$feature_id == "F2"],
    "m_delta_rescued"
  )
  expect_identical(
    out$candidate_query_adduct[out$feature_id == "F2"],
    "[M+H4N]+"
  )
})

test_that("build_query_metadata prefers annotate_masses adduct assignments over MGF metadata", {
  query_sp <- Spectra::Spectra(data.frame(
    FEATURE_ID = "F10",
    precursorMz = calculate_mz_from_mass(300, "[M+H4N]+"),
    adduct = "[M+H]+",
    mz = I(list(c(50, 80))),
    intensity = I(list(c(10, 20)))
  ))
  ms1_tbl <- tidytable::tidytable(
    feature_id = "F10",
    candidate_adduct = "[M+H4N]+",
    candidate_annotation_level = "primary",
    candidate_evidence_tier = "supported_strong"
  )

  out <- build_query_metadata(
    query_sp = query_sp,
    ms1_annotations = ms1_tbl
  )

  expect_identical(
    adduct_to_state_key(out$query_adduct[[1L]]),
    adduct_to_state_key("[M+H4N]+")
  )
})

test_that("log_library_stats handles spectra without library metadata", {
  lib_sp <- Spectra::Spectra(data.frame(
    precursorMz = c(100, 200),
    mz = I(list(c(50, 80), c(60, 90))),
    intensity = I(list(c(10, 20), c(30, 40)))
  ))

  expect_null(log_library_stats(lib_sp))
})

test_that("annotate_spectra_handle_empty_result logs warnings when message is provided", {
  withr::local_dir(new = temp_test_dir("ann_spe_empty_with_msg"))
  output_file <- temp_test_path("out_with_message.tsv")
  saw_warn <- new.env(parent = emptyenv())
  saw_warn$called <- FALSE

  local_mocked_bindings(
    log_warn = function(message) {
      if (identical(message, "nothing left")) {
        saw_warn$called <- TRUE
      }
      invisible(NULL)
    }
  )

  out <- annotate_spectra_handle_empty_result(
    path = output_file,
    params = list(a = 1L),
    message = "nothing left"
  )

  expect_true(saw_warn$called)
  expect_identical(out, output_file)
  expect_true(file.exists(output_file))
})

test_that(".match_precursor_windows matches closed Da and ppm intervals accurately", {
  lib_prec <- c(100.0, 200.0, 500.0, 1000.0, NA_real_, -10.0)
  qry_prec <- c(100.01, 1000.01, NA_real_)

  # At m/z 100, dalton = 0.01 covers 100.01 (exact boundary);
  # at m/z 1000, ppm = 10 covers 1000.01 (10 ppm = 0.01 Da).
  keep <- .match_precursor_windows(
    library_precursors = lib_prec,
    query_precursors = qry_prec,
    dalton = 0.01,
    ppm = 10
  )
  expect_equal(keep, c(TRUE, FALSE, FALSE, TRUE, FALSE, FALSE))

  # Empty or non-finite inputs return all FALSE
  expect_equal(
    .match_precursor_windows(c(NA_real_, -1), c(100), dalton = 0.01, ppm = 10),
    c(FALSE, FALSE)
  )
})

test_that("finalize_results breaks score ties using adduct match mode rank", {
  # Same feature, library, structure, and similarity score (0.85),
  # but target 1 is neutral_M rescued while target 2 is exact_adduct.
  df_sim <- tidytable::tidytable(
    feature_id = c("F1", "F1"),
    target_id = c(1L, 2L),
    precursorMz = c(100, 100),
    candidate_spectrum_entropy = c(0.5, 0.5),
    candidate_score_similarity = c(0.85, 0.85),
    candidate_score_similarity_forward = c(0.85, 0.85),
    candidate_score_similarity_reverse = c(0.85, 0.85),
    candidate_count_similarity_peaks_matched = c(4L, 4L),
    .similarity_space = c("neutral_M", "precursor_mz")
  )
  meta <- tidytable::tidytable(
    target_id = c(1L, 2L),
    target_adduct = c("[M+Na]+", "[M+H]+"),
    target_library = c("GNPS", "GNPS"),
    target_spectrum_id = c("sp_rescued", "sp_exact"),
    target_smiles = c("CCO", "CCO"),
    target_smiles_no_stereo = c("CCO", "CCO"),
    target_name = c("ethanol", "ethanol"),
    target_precursorMz = c(123, 101),
    target_neutral_mass = c(100, 100)
  )
  query_meta <- tidytable::tidytable(
    feature_id = "F1",
    query_adduct = "[M+H]+",
    query_precursorMz = 101,
    query_neutral_mass = 100
  )

  out <- finalize_results(df_sim = df_sim, meta = meta, query_meta = query_meta)
  expect_equal(nrow(out), 1L)
  expect_identical(out$candidate_spectrum_id[[1L]], "sp_exact")
  expect_identical(out$candidate_adduct_match_mode[[1L]], "exact_adduct")
  expect_true(is.na(out$annotation_note[[1L]]))
})

test_that("resolve_ms1_query_adduct_map filters to requested feature_ids and breaks ties by m/z error", {
  ms1_tbl <- tidytable::tidytable(
    feature_id = c("F1", "F1", "F2"),
    candidate_adduct = c("[M+H]+", "[M+Na]+", "[M+K]+"),
    candidate_annotation_level = c("primary", "primary", "primary"),
    candidate_evidence_tier = c(
      "supported_strong",
      "supported_strong",
      "supported_strong"
    ),
    candidate_structure_error_mz = c("-0.005", "0.001", "0.000")
  )

  out <- resolve_ms1_query_adduct_map(ms1_tbl, feature_ids = "F1")
  expect_equal(nrow(out), 1L)
  expect_identical(out$feature_id[[1L]], "F1")
  expect_identical(out$ms1_query_adduct[[1L]], "[M+Na]+")
})

test_that("log_library_stats handles missing/empty library column and computes exact per-library structure counts", {
  # 1. Missing 'library' column returns NULL silently
  sp_no_lib <- Spectra::Spectra(data.frame(
    precursorMz = 100.0,
    mz = I(list(50.0)),
    intensity = I(list(10.0))
  ))
  expect_null(log_library_stats(sp_no_lib))

  # 2. Empty Spectra with 'library' column returns NULL
  sp_empty <- Spectra::Spectra(data.frame(
    precursorMz = numeric(0),
    library = character(0),
    mz = I(list()),
    intensity = I(list())
  ))
  expect_null(log_library_stats(sp_empty))

  # 3. All-NA 'library' column returns NULL
  sp_na_lib <- Spectra::Spectra(data.frame(
    precursorMz = c(100.0, 200.0),
    library = c(NA_character_, NA_character_),
    mz = I(list(50.0, 60.0)),
    intensity = I(list(10.0, 20.0))
  ))
  expect_null(log_library_stats(sp_na_lib))

  # 4. Multi-library with partial NA library and fallback across smiles_no_stereo -> smiles -> inchikey
  sp_multi <- Spectra::Spectra(data.frame(
    precursorMz = c(100, 101, 102, 103, 104),
    library = c("LibB", "LibA", NA_character_, "LibA", "LibA"),
    smiles_no_stereo = c("CCO", NA_character_, "CCC", "CCO", NA_character_),
    smiles = c(NA_character_, "CCN", "CCC", NA_character_, NA_character_),
    inchikey = c(
      NA_character_,
      NA_character_,
      NA_character_,
      NA_character_,
      "IK_3"
    ),
    mz = I(list(50, 50, 50, 50, 50)),
    intensity = I(list(10, 10, 10, 10, 10))
  ))

  rec <- new.env(parent = emptyenv())
  rec$msg <- NULL
  local_mocked_bindings(
    log_info = function(fmt, ...) {
      rec$msg <- sprintf(fmt, ...)
      invisible(NULL)
    }
  )

  expect_null(log_library_stats(sp_multi))
  expect_type(rec$msg, "character")
  # LibA has 3 spectra and 3 unique structures (CCN, CCO, IK_3); LibB has 1 spectrum and 1 unique structure (CCO)
  expect_match(rec$msg, "LibA\\s+3\\s+3\\s+75\\.00%")
  expect_match(rec$msg, "LibB\\s+1\\s+1\\s+25\\.00%")
})

test_that("resolve_ms1_query_adduct_map, compute_similarity_safe, and finalize_results cover file, neutral_M, and fallback branches", {
  tmp_dir <- withr::local_tempdir()

  # 1. resolve_ms1_query_adduct_map: file path with and without required cols, non-df input, missing cols, NA feature_ids, unmatched feature_ids
  ms1_valid_file <- file.path(tmp_dir, "ms1_valid.tsv")
  tidytable::fwrite(
    tidytable::tidytable(
      feature_id = c("1", "2"),
      candidate_adduct = c("[M+H]+", "[M+Na]+"),
      candidate_annotation_level = c("primary", "secondary"),
      candidate_evidence_tier = c("supported_strong", "supported_weak"),
      candidate_structure_error_mz = c("0.001", "0.005")
    ),
    ms1_valid_file,
    sep = "\t"
  )
  res_file <- resolve_ms1_query_adduct_map(ms1_valid_file, feature_ids = "1")
  expect_identical(res_file$ms1_query_adduct, "[M+H]+")

  ms1_bad_file <- file.path(tmp_dir, "ms1_bad.tsv")
  tidytable::fwrite(
    tidytable::tidytable(other_col = "x"),
    ms1_bad_file,
    sep = "\t"
  )
  expect_identical(nrow(resolve_ms1_query_adduct_map(ms1_bad_file)), 0L)
  expect_identical(nrow(resolve_ms1_query_adduct_map(123L)), 0L)
  expect_identical(
    nrow(resolve_ms1_query_adduct_map(data.frame(feature_id = "1"))),
    0L
  )
  expect_identical(
    nrow(resolve_ms1_query_adduct_map(
      ms1_valid_file,
      feature_ids = NA_character_
    )),
    0L
  )
  expect_identical(
    nrow(resolve_ms1_query_adduct_map(ms1_valid_file, feature_ids = "999")),
    0L
  )

  # 2. compute_similarity_safe: non-Spectra guard and neutral_M space rescue
  expect_identical(nrow(compute_similarity_safe(list(), list())), 0L)

  # Query at [M+Na]+ (mz = 122.989221) and library at [M+H]+ (mz = 101.007276) share neutral mass ~ 100.0
  qry_mgf <- file.path(tmp_dir, "qry_neutral.mgf")
  lib_mgf <- file.path(tmp_dir, "lib_neutral.mgf")
  write_minimal_mgf(qry_mgf, precursors = c(122.989221))
  write_minimal_mgf(lib_mgf, precursors = c(101.007276))
  sp_qry <- import_spectra(
    qry_mgf,
    cutoff = 0,
    dalton = 0.05,
    min_fragments = 1L,
    polarity = "pos",
    ppm = 20
  )
  sp_lib <- import_spectra(
    lib_mgf,
    cutoff = 0,
    dalton = 0.05,
    min_fragments = 1L,
    polarity = "pos",
    ppm = 20
  )
  sp_qry@backend@spectraData$spectrum_id <- "Q1"
  sp_lib@backend@spectraData$spectrum_id <- "L1"
  sp_lib@backend@spectraData$adduct <- "[M+H]+"
  sp_qry@backend@peaksData[[1L]][, 1L] <- sp_lib@backend@peaksData[[1L]][, 1L]
  expect_identical(length(remove_empty_peak_spectra(sp_lib)), 1L)
  sim_res <- compute_similarity_safe(
    lib_sp = sp_lib,
    query_sp = sp_qry,
    query_meta = tidytable::tidytable(
      feature_id = "Q1",
      query_adduct = "[M+Na]+",
      query_neutral_mass = 100.0
    ),
    method = "gnps",
    dalton = 0.05,
    ppm = 20,
    threshold = 0.1,
    approx = FALSE
  )
  expect_true("neutral_M" %in% sim_res$.similarity_space)

  # Zero-similarity / zero-matched-peak pairs are rejected even when threshold = 0
  sp_qry_disjoint <- sp_qry
  sp_qry_disjoint@backend@peaksData[[1L]][, 1L] <- c(25.0, 35.0)
  sim_zero <- compute_similarity_safe(
    lib_sp = sp_lib,
    query_sp = sp_qry_disjoint,
    query_meta = tidytable::tidytable(
      feature_id = "Q1",
      query_adduct = "[M+Na]+",
      query_neutral_mass = 100.0
    ),
    method = "gnps",
    dalton = 0.05,
    ppm = 20,
    threshold = 0,
    approx = FALSE
  )
  expect_identical(nrow(sim_zero), 0L)

  # 3. finalize_results with query_meta = NULL (extracting query_adduct / query_neutral_mass from df_sim or defaulting to NA)
  meta_tbl <- tidytable::tidytable(
    target_id = 1L,
    target_adduct = "[M+H]+",
    target_library = "GNPS",
    target_smiles = "CCO",
    target_smiles_no_stereo = "CCO",
    target_inchikey = "IK1",
    target_inchikey_no_stereo = "IK1",
    target_inchikey_connectivity_layer = "IK1",
    target_formula = "C2H6O",
    target_exact_mass = 46.04186,
    target_name = "Ethanol",
    target_spectrum_id = "SP1",
    target_spl = NA_character_,
    target_precursor = 101.0073
  )
  fin_no_qmeta <- finalize_results(
    df_sim = tidytable::tidytable(
      feature_id = "Q1",
      target_id = 1L,
      candidate_score_similarity = 0.95,
      candidate_spectrum_entropy = 1.2,
      candidate_count_similarity_peaks_matched = 2L,
      query_adduct = "[M+Na]+",
      query_neutral_mass = 100.0,
      .similarity_space = "neutral_M"
    ),
    meta = meta_tbl,
    query_meta = NULL
  )
  expect_identical(nrow(fin_no_qmeta), 1L)
  expect_identical(fin_no_qmeta$candidate_adduct_match_mode, "m_delta_rescued")

  # 4. MS1 zero-similarity tracker records 0.00 only for MS1 candidates whose
  # structure had a precursor-matching reference spectrum and no positive match
  sp_lib_zero <- sp_lib
  sp_lib_zero@backend@spectraData$smiles_no_stereo <- "CCO"
  sp_lib_zero@backend@spectraData$library <- "ISDB"
  sp_lib_zero@backend@spectraData$spectrum_id <- "SP_ZERO_1"
  sp_lib_zero@backend@spectraData$adduct <- "[M+H]+"
  sp_lib_zero@backend@spectraData$precursorMz <- 101.0073

  qmeta_zero <- tidytable::tidytable(
    feature_id = c("Q_ZERO", "Q_UNMEASURED"),
    query_adduct = c("[M+H]+", "[M+H]+"),
    query_precursorMz = c(101.0073, 250.0000),
    query_neutral_mass = c(100.0, 249.0)
  )
  ms1_zero_df <- tidytable::tidytable(
    feature_id = c("Q_ZERO", "Q_ZERO", "Q_UNMEASURED"),
    candidate_structure_smiles_no_stereo = c("CCO", "CCC", "CCO"),
    candidate_adduct = c("[M+H]+", "[M+H]+", "[M+H]+"),
    candidate_structure_error_mz = c("0.0001", "0.0002", "0.0001"),
    candidate_structure_name = c("Ethanol", "Propane", "Ethanol")
  )
  trk <- init_ms1_zero_tracker(
    ms1_annotations = ms1_zero_df,
    query_meta = qmeta_zero,
    dalton = 0.01,
    ppm = 10
  )
  update_ms1_zero_tracker(trk, sp_lib_zero)
  zero_rows <- finalize_ms1_zero_tracker(
    tracker = trk,
    df_final = tidytable::tidytable()
  )
  expect_identical(nrow(zero_rows), 1L)
  expect_identical(zero_rows$feature_id, "Q_ZERO")
  expect_identical(zero_rows$candidate_structure_smiles_no_stereo, "CCO")
  expect_identical(zero_rows$candidate_score_similarity, 0)
  expect_identical(zero_rows$candidate_count_similarity_peaks_matched, 0L)
  expect_identical(zero_rows$candidate_adduct_match_mode, "exact_adduct")

  # When Q_ZERO + CCO already has a positive match in df_final, no 0.00 row is emitted
  zero_rows_suppressed <- finalize_ms1_zero_tracker(
    tracker = trk,
    df_final = tidytable::tidytable(
      feature_id = "Q_ZERO",
      candidate_structure_smiles_no_stereo = "CCO",
      candidate_score_similarity = 0.85
    )
  )
  expect_identical(nrow(zero_rows_suppressed), 0L)
})

test_that("annotate_spectra and helpers cover validation, preflight, RDS libraries, and edge cases", {
  td <- withr::local_tempdir()
  q_mgf <- file.path(td, "query.mgf")
  lib_mgf <- file.path(td, "lib_pos.mgf")
  out_tsv <- file.path(td, "out.tsv")

  writeLines(
    c(
      "BEGIN IONS",
      "FEATURE_ID=Q1",
      "PEPMASS=101.0073",
      "CHARGE=1+",
      "ADDUCT=[M+H]+",
      "MSLEVEL=2",
      "40.0 100",
      "50.0 200",
      "60.0 300",
      "101.0073 1000",
      "END IONS",
      ""
    ),
    q_mgf
  )
  writeLines(
    c(
      "BEGIN IONS",
      "SPECTRUM_ID=LIB1",
      "PEPMASS=101.0073",
      "CHARGE=1+",
      "ADDUCT=[M+H]+",
      "MSLEVEL=2",
      "SMILES=CCO",
      "NAME=Ethanol",
      "LIBRARY=GNPS",
      "40.0 100",
      "50.0 200",
      "60.0 300",
      "101.0073 1000",
      "END IONS",
      ""
    ),
    lib_mgf
  )

  # 1. Invalid cutoff (< 0) and empty libraries error
  expect_error(
    annotate_spectra(
      input = q_mgf,
      libraries = lib_mgf,
      polarity = "pos",
      output = out_tsv,
      cutoff = -1
    ),
    "must be non-negative or NULL"
  )
  expect_error(
    annotate_spectra(
      input = q_mgf,
      libraries = character(0),
      polarity = "pos",
      output = out_tsv
    ),
    "at least one library must be provided"
  )

  # 2. Missing input file errors via .preflight_mgf_fatal_issues
  expect_error(
    annotate_spectra(
      input = file.path(td, "missing.mgf"),
      libraries = lib_mgf,
      polarity = "pos",
      output = out_tsv
    ),
    "Input data validation failed"
  )

  # 3. Recoverable empty MGF & MS1-only MGF return template output
  empty_mgf <- file.path(td, "empty.mgf")
  writeLines(character(0), empty_mgf)
  expect_warning(
    res_empty <- annotate_spectra(
      input = empty_mgf,
      libraries = lib_mgf,
      polarity = "pos",
      output = out_tsv
    ),
    "No MS2 spectra"
  )
  expect_true(file.exists(res_empty))

  ms1_only_mgf <- file.path(td, "ms1_only.mgf")
  writeLines(
    c(
      "BEGIN IONS",
      "FEATURE_ID=Q_MS1",
      "PEPMASS=101.0073",
      "MSLEVEL=1",
      "101.0073 1000",
      "END IONS",
      ""
    ),
    ms1_only_mgf
  )
  expect_warning(
    res_ms1 <- annotate_spectra(
      input = ms1_only_mgf,
      libraries = lib_mgf,
      polarity = "pos",
      output = out_tsv
    ),
    "MS1"
  )
  expect_true(file.exists(res_ms1))

  # 4. Mixed MS1+MS2 query MGF + real match + ms1_annotations file (producing both positive match and 0.00 row)
  mixed_mgf <- file.path(td, "mixed.mgf")
  writeLines(
    c(
      "BEGIN IONS",
      "FEATURE_ID=Q_MS1",
      "PEPMASS=101.0073",
      "MSLEVEL=1",
      "101.0073 1000",
      "END IONS",
      "",
      "BEGIN IONS",
      "FEATURE_ID=Q1",
      "PEPMASS=101.0073",
      "CHARGE=1+",
      "ADDUCT=[M+H]+",
      "MSLEVEL=2",
      "40.0 100",
      "50.0 200",
      "60.0 300",
      "101.0073 1000",
      "END IONS",
      "",
      "BEGIN IONS",
      "FEATURE_ID=Q2",
      "PEPMASS=101.0073",
      "CHARGE=1+",
      "ADDUCT=[M+H]+",
      "MSLEVEL=2",
      "70.0 100",
      "75.0 200",
      "80.0 300",
      "END IONS",
      ""
    ),
    mixed_mgf
  )
  ms1_file <- file.path(td, "ms1_ann.tsv")
  tidytable::fwrite(
    tidytable::tidytable(
      feature_id = c("Q1", "Q2"),
      candidate_structure_smiles_no_stereo = c("CCO", "CCO"),
      candidate_adduct = c("[M+H]+", "[M+H]+"),
      candidate_structure_error_mz = c("0.0001", "0.0001"),
      candidate_structure_name = c("Ethanol", "Ethanol")
    ),
    ms1_file,
    sep = "\t"
  )
  expect_warning(
    res_mixed <- annotate_spectra(
      input = mixed_mgf,
      libraries = lib_mgf,
      polarity = "pos",
      output = out_tsv,
      threshold = 0,
      ms1_annotations = ms1_file
    ),
    "MS1"
  )
  df_mixed <- tidytable::fread(res_mixed)
  expect_true(all(c("Q1", "Q2") %in% df_mixed$feature_id))
  expect_true(any(df_mixed$candidate_score_similarity == 0))

  # 5. Also cover annotate_spectra when ONLY ms1_zero_rows are produced (no positive hits above threshold)
  zero_only_mgf <- file.path(td, "zero_only.mgf")
  writeLines(
    c(
      "BEGIN IONS",
      "FEATURE_ID=Q_MS1",
      "PEPMASS=101.0073",
      "MSLEVEL=1",
      "101.0073 1000",
      "END IONS",
      "",
      "BEGIN IONS",
      "FEATURE_ID=Q2",
      "PEPMASS=101.0073",
      "CHARGE=1+",
      "ADDUCT=[M+H]+",
      "MSLEVEL=2",
      "70.0 100",
      "75.0 200",
      "80.0 300",
      "END IONS",
      ""
    ),
    zero_only_mgf
  )
  expect_warning(
    res_zero_only <- annotate_spectra(
      input = zero_only_mgf,
      libraries = lib_mgf,
      polarity = "pos",
      output = out_tsv,
      threshold = 0,
      ms1_annotations = ms1_file
    ),
    "MS1"
  )
  expect_true(file.exists(res_zero_only))

  # 6. .preflight_mgf_fatal_issues with non-character issues and .annotate_spectra_warn_no_ms2 mixed
  expect_identical(
    .preflight_mgf_fatal_issues(list(valid = FALSE, issues = 123L)),
    character(0L)
  )
  expect_warning(
    .annotate_spectra_warn_no_ms2(
      c("No spectra found", "No MS2 spectra found"),
      c("a.mgf", "b.mgf")
    ),
    "One or more input files are empty"
  )

  # 7. import_and_clean_library_collection with .rds (valid Spectra, non-Spectra RDS, and multiple libraries)
  sp_lib_obj <- import_spectra(lib_mgf)
  rds_valid <- file.path(td, "lib_valid.rds")
  rds_invalid <- file.path(td, "lib_invalid.rds")
  saveRDS(sp_lib_obj, rds_valid)
  saveRDS(list(not_spectra = 1), rds_invalid)

  expect_identical(
    import_and_clean_library_collection(
      character(0),
      dalton = 0.01,
      polarity = "pos",
      ppm = 10
    ),
    list()
  )
  expect_error(
    import_and_clean_library_collection(
      rds_invalid,
      dalton = 0.01,
      polarity = "pos",
      ppm = 10
    ),
    "library source did not produce a Spectra object",
    class = "tima_validation_error"
  )
  sp_multi <- suppressWarnings(import_and_clean_library_collection(
    c(rds_valid, lib_mgf),
    dalton = 0.01,
    polarity = "pos",
    ppm = 10
  ))
  expect_s4_class(sp_multi, "Spectra")
  expect_identical(length(sp_multi), 2L)

  testthat::with_mocked_bindings(
    reduce_library_by_precursor = function(...) stop("mock_reduce_err"),
    {
      sp_err_red <- suppressWarnings(import_and_clean_library_collection(
        rds_valid,
        dalton = 0.01,
        polarity = "pos",
        ppm = 10,
        approx = FALSE,
        query_precursors = 101.0073
      ))
      expect_s4_class(sp_err_red, "Spectra")
    }
  )
  expect_identical(
    nrow(compute_similarity_safe(
      lib_sp = sp_lib_obj,
      query_sp = sp_lib_obj,
      method = "gnps",
      dalton = 0.01,
      ppm = 10,
      threshold = 0.99,
      approx = FALSE,
      query_precursors = 999.0
    )),
    0L
  )

  # 8. extract_vector edge cases: case-insensitive match, length == 1, length < len, length > len
  sp_ev <- sp_lib_obj
  sp_ev@backend@spectraData$CUSTOM_CI <- "val_ci"
  expect_identical(
    extract_vector(sp_ev, "custom_ci", length(sp_ev), NA_character_),
    "val_ci"
  )
  # Multiple spectra to test length == 1, length < len, length > len
  sp_ev2 <- suppressWarnings(Spectra::concatenateSpectra(
    sp_lib_obj,
    sp_lib_obj
  ))
  expect_identical(
    extract_vector(sp_ev, "CUSTOM_CI", 2L, NA_character_),
    c("val_ci", "val_ci")
  )
  expect_identical(
    extract_vector(sp_ev2, "SPECTRUM_ID", 3L, "FILL"),
    c("LIB1", "LIB1", "FILL")
  )
  expect_identical(
    extract_vector(sp_ev2, "SPECTRUM_ID", 1L, "FILL", subset_idx = 1L),
    "LIB1"
  )

  # 9. build_library_metadata with target_adduct_raw (subset and full length)
  meta_sub <- build_library_metadata(
    sp_ev2,
    c(101.0073, 101.0073),
    target_ids = 1L,
    target_adduct_raw = "[M+H]+"
  )
  expect_identical(nrow(meta_sub), 1L)
  meta_full <- build_library_metadata(
    sp_ev2,
    c(101.0073, 101.0073),
    target_ids = 1:2,
    target_adduct_raw = c("[M+H]+", "[M+Na]+")
  )
  expect_identical(nrow(meta_full), 2L)

  # 10. init_ms1_zero_tracker / update_ms1_zero_tracker / finalize_ms1_zero_tracker edge cases
  qmeta_1 <- tidytable::tidytable(
    feature_id = "Q1",
    query_precursorMz = 101.0073
  )
  expect_null(init_ms1_zero_tracker(NULL, qmeta_1, 0.01, 10))
  expect_null(init_ms1_zero_tracker(123L, qmeta_1, 0.01, 10))
  bad_ms1_file <- file.path(td, "bad_ms1.tsv")
  tidytable::fwrite(tidytable::tidytable(other_col = "x"), bad_ms1_file)
  expect_null(init_ms1_zero_tracker(bad_ms1_file, qmeta_1, 0.01, 10))
  expect_null(init_ms1_zero_tracker(
    tidytable::tidytable(other_col = "x"),
    qmeta_1,
    0.01,
    10
  ))
  expect_null(init_ms1_zero_tracker(
    tidytable::tidytable(
      feature_id = "NO_MATCH",
      candidate_structure_smiles_no_stereo = "CCO"
    ),
    qmeta_1,
    0.01,
    10
  ))

  # Duplicate (feature_id, smiles) and filtered rows, plus missing error_mz / structure_name / query_adduct
  trk_dup <- init_ms1_zero_tracker(
    tidytable::tidytable(
      feature_id = c("Q1", "Q1", "OTHER"),
      candidate_structure_smiles_no_stereo = c("CCO", "CCO", "CCC")
    ),
    qmeta_1,
    0.01,
    10
  )
  expect_identical(length(trk_dup$feature_id), 1L)
  expect_true(is.na(trk_dup$query_adduct))
  expect_true(is.na(trk_dup$error_mz))
  expect_true(is.na(trk_dup$structure_name))

  trk_cadd <- init_ms1_zero_tracker(
    tidytable::tidytable(
      feature_id = "Q1",
      candidate_structure_smiles_no_stereo = "CCO",
      candidate_adduct = "[M+H]+"
    ),
    qmeta_1,
    0.01,
    10
  )
  expect_identical(trk_cadd$query_adduct, "[M+H]+")

  # update_ms1_zero_tracker with empty spectra, non-matching smiles, non-finite precursors, partial valid precursors, and duplicate library smiles
  expect_null(update_ms1_zero_tracker(NULL, sp_ev2))
  expect_null(update_ms1_zero_tracker(trk_dup, sp_ev2[integer(0)]))
  sp_nomatch <- sp_ev2
  sp_nomatch@backend@spectraData$SMILES <- c("C1CCCCC1", "C1CCCCC1")
  expect_null(update_ms1_zero_tracker(trk_dup, sp_nomatch))

  sp_naprec <- sp_ev2
  sp_naprec@backend@spectraData$precursorMz <- c(NA_real_, -1)
  expect_null(update_ms1_zero_tracker(trk_dup, sp_naprec))

  # Partial valid precursors (1 NA, 1 out-of-window) -> hits !all(valid_lp) and length(matched_unm) == 0L
  sp_partial <- sp_ev2
  sp_partial@backend@spectraData$precursorMz <- c(NA_real_, 500.0)
  sp_partial@backend@spectraData$SMILES <- c("CCO", "CCO")
  expect_null(update_ms1_zero_tracker(trk_dup, sp_partial))
  expect_false(trk_dup$measured[[1L]])

  # Duplicate SMILES in library where first spectrum precursor is far (200.0) and second matches (101.0073)
  sp_dup_sm <- sp_ev2
  sp_dup_sm@backend@spectraData$precursorMz <- c(200.0, 101.0073)
  sp_dup_sm@backend@spectraData$SMILES <- c("CCO", "CCO")
  update_ms1_zero_tracker(trk_dup, sp_dup_sm)
  expect_true(trk_dup$measured[[1L]])
  # Calling update again when all measured returns NULL early
  expect_null(update_ms1_zero_tracker(trk_dup, sp_dup_sm))
  expect_identical(
    nrow(finalize_ms1_zero_tracker(NULL, tidytable::tidytable())),
    0L
  )

  # 11. convert_precursor_for_matching with unparseable adduct and convert_precursor_to_neutral_if_possible empty
  expect_identical(
    convert_precursor_for_matching(c(100, 200), c("NOT_AN_ADDUCT", "[0M+H]+")),
    c(100, 200)
  )
  expect_identical(
    convert_precursor_to_neutral_if_possible(numeric(0), NULL),
    numeric(0)
  )

  # 12. finalize_results missing query_adduct / query_neutral_mass in both query_meta and df_sim
  fin_min <- finalize_results(
    df_sim = tidytable::tidytable(
      feature_id = "Q1",
      target_id = 1L,
      candidate_score_similarity = 0.9,
      candidate_spectrum_entropy = 1.0,
      candidate_count_similarity_peaks_matched = 2L
    ),
    meta = meta_sub,
    query_meta = tidytable::tidytable(feature_id = "Q1")
  )
  expect_identical(nrow(fin_min), 1L)
  fin_min2 <- finalize_results(
    df_sim = tidytable::tidytable(
      feature_id = "Q1",
      target_id = 1L,
      candidate_score_similarity = 0.9,
      candidate_spectrum_entropy = 1.0,
      candidate_count_similarity_peaks_matched = 2L
    ),
    meta = meta_sub,
    query_meta = NULL
  )
  expect_identical(nrow(fin_min2), 1L)

  # 13. log_library_stats with multiple ID columns where first has NAs and second fills them
  sp_stats <- sp_ev2
  sp_stats@backend@spectraData$library <- c("GNPS", "ISDB")
  sp_stats@backend@spectraData$smiles_no_stereo <- c(NA_character_, "CCO")
  sp_stats@backend@spectraData$smiles <- c("CCC", "CCO")
  sp_stats@backend@spectraData$inchikey <- c("IK1", "IK2")
  expect_null(log_library_stats(sp_stats))
})
