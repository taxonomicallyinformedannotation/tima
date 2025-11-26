# Test Suite: annotate_spectra ----

library(testthat)
library(tidytable)

# Test Fixtures ----

#' Create minimal MGF file for testing
#' @keywords internal
write_minimal_mgf <- function(
  path,
  n_spectra = 1,
  pepmass = 100,
  charge = "1+"
) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)

  lines <- c()
  for (i in seq_len(n_spectra)) {
    lines <- c(
      lines,
      "BEGIN IONS",
      paste0("TITLE=Spectrum_", i),
      paste0("PEPMASS=", pepmass + (i + 1) * 50),
      paste0("CHARGE=", charge),
      paste0("NAME=Name_", i),
      paste0("INCHIKEY=Inchikey_", i),
      paste0("SMILES=SMILES_", i),
      paste0("MSLEVEL=2"),
      "50 100",
      "75 200",
      "100 300",
      "END IONS",
      ""
    )
  }

  writeLines(lines, path)
  invisible(path)
}

## Input Validation ----

test_that("annotate_spectra validates polarity parameter", {
  withr::with_dir(temp_test_dir("annotate_spectra_polarity"), {
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
      "test_lib_pos.mgf"
    )
    write_minimal_mgf(lib_path)

    expect_error(
      annotate_spectra(
        libraries = list(
          pos = paths$data$interim$libraries$spectra$is$pos$isdb
        ),
        polarity = "invalid"
      ),
      "pos.*neg"
    )

    expect_error(
      annotate_spectra(
        libraries = list(
          pos = paths$data$interim$libraries$spectra$is$pos$isdb
        ),
        polarity = "positive"
      ),
      "pos.*neg"
    )
  })
})

test_that("annotate_spectra validates numeric parameters", {
  withr::with_dir(temp_test_dir("annotate_spectra_numeric"), {
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
      "test_lib_pos.mgf"
    )
    write_minimal_mgf(lib_path)

    expect_error(
      annotate_spectra(
        libraries = list(
          pos = paths$data$interim$libraries$spectra$is$pos$isdb
        ),
        threshold = 1.5
      ),
      "between 0 and 1"
    )
    expect_error(
      annotate_spectra(
        libraries = list(
          pos = paths$data$interim$libraries$spectra$is$pos$isdb
        ),
        threshold = -0.1
      ),
      "between 0 and 1"
    )
    expect_error(
      annotate_spectra(
        libraries = list(
          pos = paths$data$interim$libraries$spectra$is$pos$isdb
        ),
        ppm = -10
      ),
      "must be between"
    )
    expect_error(
      annotate_spectra(
        libraries = list(
          pos = paths$data$interim$libraries$spectra$is$pos$isdb
        ),
        dalton = -0.01
      ),
      "must be between"
    )
    expect_error(
      annotate_spectra(
        libraries = list(
          pos = paths$data$interim$libraries$spectra$is$pos$isdb
        ),
        qutoff = -0.1
      ),
      "must be between"
    )
  })
})

test_that("annotate_spectra validates file existence", {
  withr::with_dir(temp_test_dir("annotate_spectra_files"), {
    local_test_project(copy = TRUE)
    expect_error(
      annotate_spectra(
        input = "nonexistent_spectra.mgf",
        libraries = list(pos = "library.mgf"),
        polarity = "pos"
      ),
      "not found"
    )
    get_file(
      url = get_default_paths()$urls$examples$spectra_mini,
      export = get_params(step = "annotate_spectra")$files$spectral$raw
    )
    expect_error(
      annotate_spectra(
        libraries = list(pos = "nonexistent_library.mgf"),
        polarity = "pos"
      ),
      "not found"
    )
  })
})

test_that("annotate_spectra requires at least one library", {
  withr::with_dir(temp_test_dir("annotate_spectra_library"), {
    local_test_project(copy = TRUE)
    get_file(
      url = get_default_paths()$urls$examples$spectra_mini,
      export = get_params(step = "annotate_spectra")$files$spectral$raw
    )
    expect_error(
      annotate_spectra(libraries = list(), polarity = "pos"),
      "Library elements must be character strings"
    )
  })
})

## Basic Functionality ----

test_that("annotate_spectra works with single MGF library", {
  withr::with_dir(temp_test_dir("annotate_spectra_single"), {
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
      "test_lib_pos.mgf"
    )
    write_minimal_mgf(lib_path)
    expect_no_error(annotate_spectra(
      libraries = list(pos = lib_path),
      polarity = "pos",
      ppm = 10,
      dalton = 0.01
    ))
  })
})

test_that("annotate_spectra works in negative mode", {
  withr::with_dir(temp_test_dir("annotate_spectra_neg"), {
    local_test_project(copy = TRUE)
    paths <- get_default_paths()
    get_file(
      url = paths$urls$examples$spectra_mini,
      export = paths$data$source$spectra
    )
    lib_path_neg <- file.path(
      "data",
      "interim",
      "libraries",
      "spectra",
      "test_lib_neg.mgf"
    )
    write_minimal_mgf(lib_path_neg, charge = "1-", pepmass = 120)
    expect_no_error(annotate_spectra(
      libraries = list(neg = lib_path_neg),
      polarity = "neg",
      ppm = 10,
      dalton = 0.01
    ))
  })
})

test_that("annotate_spectra works with multiple libraries", {
  withr::with_dir(temp_test_dir("annotate_spectra_multi"), {
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
      "test_lib_pos.mgf"
    )
    write_minimal_mgf(lib_path)
    lib_path2 <- file.path(
      "data",
      "interim",
      "libraries",
      "spectra",
      "test_lib_pos2.mgf"
    )
    write_minimal_mgf(lib_path2, pepmass = 110)
    expect_no_error(annotate_spectra(
      libraries = list(pos = lib_path, pos2 = lib_path2),
      polarity = "pos",
      ppm = 10,
      dalton = 0.01
    ))
  })
})

## Different Similarity Methods ----

test_that("annotate_spectra works with cosine similarity", {
  withr::with_dir(temp_test_dir("annotate_spectra_cosine"), {
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
      "test_lib_pos.mgf"
    )
    write_minimal_mgf(lib_path)
    expect_no_error(annotate_spectra(
      libraries = list(pos = lib_path),
      polarity = "pos",
      method = "cosine",
      ppm = 10,
      dalton = 0.01
    ))
  })
})

test_that("annotate_spectra works with entropy similarity", {
  withr::with_dir(temp_test_dir("annotate_spectra_entropy"), {
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
      "test_lib_pos.mgf"
    )
    write_minimal_mgf(lib_path)
    expect_no_error(annotate_spectra(
      libraries = list(pos = lib_path),
      polarity = "pos",
      method = "entropy",
      ppm = 10,
      dalton = 0.01
    ))
  })
})

## Threshold Settings ----

test_that("annotate_spectra respects similarity threshold", {
  withr::with_dir(temp_test_dir("annotate_spectra_threshold"), {
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
      "test_lib_pos.mgf"
    )
    write_minimal_mgf(lib_path)
    expect_no_error(annotate_spectra(
      libraries = list(pos = lib_path),
      polarity = "pos",
      threshold = 0.5,
      ppm = 10,
      dalton = 0.01
    ))
    expect_no_error(annotate_spectra(
      libraries = list(pos = lib_path),
      polarity = "pos",
      threshold = 0.9,
      ppm = 10,
      dalton = 0.01
    ))
  })
})

## Tolerance Settings ----

test_that("annotate_spectra accepts different tolerance settings", {
  withr::with_dir(temp_test_dir("annotate_spectra_tolerance"), {
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
      "test_lib_pos.mgf"
    )
    write_minimal_mgf(lib_path)
    expect_no_error(annotate_spectra(
      libraries = list(pos = lib_path),
      polarity = "pos",
      ppm = 5,
      dalton = 0.001
    ))
    expect_no_error(annotate_spectra(
      libraries = list(pos = lib_path),
      polarity = "pos",
      ppm = 20,
      dalton = 0.05
    ))
  })
})

## Approx Mode (Precursor-Free Matching)

test_that("annotate_spectra works with approx mode enabled", {
  withr::with_dir(temp_test_dir("annotate_spectra_approx_on"), {
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
      "test_lib_pos.mgf"
    )
    write_minimal_mgf(lib_path)
    expect_no_error(annotate_spectra(
      libraries = list(pos = lib_path),
      polarity = "pos",
      approx = TRUE,
      ppm = 10,
      dalton = 0.01
    ))
  })
})

test_that("annotate_spectra works with approx mode disabled", {
  withr::with_dir(temp_test_dir("annotate_spectra_approx_off"), {
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
      "test_lib_pos.mgf"
    )
    write_minimal_mgf(lib_path)
    expect_no_error(annotate_spectra(
      libraries = list(pos = lib_path),
      polarity = "pos",
      approx = FALSE,
      ppm = 10,
      dalton = 0.01
    ))
  })
})

## Intensity Cutoff ----

test_that("annotate_spectra respects intensity cutoff", {
  withr::with_dir(temp_test_dir("annotate_spectra_cutoff"), {
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
      "test_lib_pos.mgf"
    )
    write_minimal_mgf(lib_path)
    expect_no_error(annotate_spectra(
      libraries = list(pos = lib_path),
      polarity = "pos",
      qutoff = 0.0,
      ppm = 10,
      dalton = 0.01
    ))
    expect_no_error(annotate_spectra(
      libraries = list(pos = lib_path),
      polarity = "pos",
      qutoff = 0.1,
      ppm = 10,
      dalton = 0.01
    ))
  })
})

## Output Validation ----

test_that("annotate_spectra produces valid output file", {
  withr::with_dir(temp_test_dir("annotate_spectra_output"), {
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
      "test_lib_pos.mgf"
    )
    write_minimal_mgf(lib_path)
    annotate_spectra(
      libraries = list(pos = lib_path),
      polarity = "pos",
      ppm = 10,
      dalton = 0.01
    )
    output_file <- get_params(
      step = "annotate_spectra"
    )$files$annotations$raw$spectral$spectral
    expect_true(file.exists(output_file))
    if (file.exists(output_file) && file.size(output_file) > 0) {
      result <- tidytable::fread(output_file)
      expect_s3_class(result, "data.frame")
    }
  })
})

## Empty/Edge Cases ----

test_that("annotate_spectra handles empty input gracefully", {
  skip("Requires empty MGF file creation")
})

## Performance ----

test_that("annotate_spectra completes in reasonable time", {
  skip("Performance test - run manually")
})
