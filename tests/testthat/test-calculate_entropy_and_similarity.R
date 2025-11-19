# Test Suite: calculate_entropy_and_similarity ----

library(testthat)
pkgload::load_all(quiet = TRUE) |>
  suppressMessages()

test_that("calculate_entropy_and_similarity works with small spectra", {
  # Small test spectra
  x_small <- cbind(
    mz = c(10, 36, 63, 91, 93),
    intensity = c(14, 15, 999, 650, 1)
  ) |>
    list()

  y_small <- cbind(
    mz = c(10, 12, 50, 63, 105),
    intensity = c(35, 5, 16, 999, 450)
  ) |>
    list()

  pmz_x <- 91.0
  pmz_y <- 105.0

  # Cosine similarity with approximation
  result1 <- calculate_entropy_and_similarity(
    lib_ids = 1L,
    lib_precursors = pmz_y,
    lib_spectra = y_small,
    query_ids = 1L,
    query_precursors = pmz_x,
    query_spectra = x_small,
    dalton = 0.01,
    ppm = 1.0,
    threshold = 0.0,
    method = "cosine",
    approx = TRUE
  )
  expect_type(result1, "list")

  # GNPS method without approximation
  result2 <- calculate_entropy_and_similarity(
    lib_ids = 1L,
    lib_precursors = pmz_y,
    lib_spectra = y_small,
    query_ids = 1L,
    query_precursors = pmz_x,
    query_spectra = x_small,
    dalton = 0.01,
    ppm = 1.0,
    threshold = 0.0,
    method = "gnps",
    approx = FALSE
  )
  expect_type(result2, "list")
})

test_that("calculate_entropy_and_similarity works with large spectra", {
  set.seed(123)
  n_query_large <- 1000
  n_lib_large <- 1000

  x_large <- cbind(
    mz = sort(runif(n_query_large, 100, 1000)),
    intensity = runif(n_query_large, 1, 1000)
  ) |>
    list()

  y_large <- cbind(
    mz = sort(runif(n_lib_large, 100, 1000)),
    intensity = runif(n_lib_large, 1, 1000)
  ) |>
    list()

  pmz_x <- 91.0
  pmz_y <- 105.0

  # Entropy method with approximation
  result <- calculate_entropy_and_similarity(
    lib_ids = 1L,
    lib_precursors = pmz_y,
    lib_spectra = y_large,
    query_ids = 1L,
    query_precursors = pmz_x,
    query_spectra = x_large,
    dalton = 0.01,
    ppm = 1.0,
    threshold = 0.0,
    method = "entropy",
    approx = TRUE
  )
  expect_type(result, "list")
})

test_that("calculate_entropy_and_similarity fails with invalid method", {
  x_small <- cbind(mz = c(10, 36, 63), intensity = c(14, 15, 999)) |> list()
  y_small <- cbind(mz = c(10, 12, 50), intensity = c(35, 5, 16)) |> list()

  expect_error(
    calculate_entropy_and_similarity(
      lib_ids = 1L,
      lib_precursors = 105.0,
      lib_spectra = y_small,
      query_ids = 1L,
      query_precursors = 91.0,
      query_spectra = x_small,
      dalton = 0.01,
      ppm = 1.0,
      threshold = 0.0,
      method = "foo",
      approx = TRUE
    )
  )
})

test_that("annotate_spectra works in negative mode", {
  skip_on_cran()
  local_test_project(copy = TRUE)
  paths <- get_default_paths()

  # Setup data
  get_file(
    url = paths$urls$examples$spectra_mini,
    export = paths$data$source$spectra
  )
  get_file(
    url = paths$urls$examples$spectral_lib_mini$with_rt,
    export = paths$data$source$libraries$spectra$exp$with_rt
  )

  prepare_libraries_spectra()

  expect_no_error(
    annotate_spectra(
      libraries = list(
        neg = paths$data$source$libraries$spectra$exp$with_rt,
        pos = paths$data$source$libraries$spectra$exp$with_rt
      ),
      ppm = 1.0,
      dalton = 0.001,
      polarity = "neg"
    )
  )
})

# test_that("annotate_spectra handles empty library", {
#   local_test_project(copy = TRUE)
#
#   expect_no_error(
#     annotate_spectra(
#       libraries = list("data/interim/libraries/spectra/exp/nope_pos.rds")
#     )
#   )
#
#
# })

test_that("annotate_spectra works with approximation", {
  skip_on_cran()
  local_test_project(copy = TRUE)
  paths <- get_default_paths()

  get_file(
    url = paths$urls$examples$spectra_mini,
    export = paths$data$source$spectra
  )
  get_file(
    url = paths$urls$examples$spectral_lib_mini$pos,
    export = paths$data$interim$libraries$spectra$is$pos$isdb
  )

  prepare_libraries_spectra()

  expect_no_error(
    annotate_spectra(
      libraries = list(
        pos = "data/interim/libraries/spectra/exp/internal_pos.rds"
      ),
      ppm = 1.0,
      dalton = 0.001,
      approx = TRUE,
      threshold = 1.0
    )
  )
})

test_that("create_edges works with edge cases", {
  frag_matrix <- matrix(
    c(123.456, 234.567),
    nrow = 1L,
    dimnames = list(NULL, c("mz", "int"))
  )

  result <- create_edges(
    frags = rep(list(frag_matrix), 3L),
    nspecs = 3L,
    precs = c(123.456, 234.567, 345.678),
    method = "gnps",
    ms2_tolerance = 0.01,
    ppm_tolerance = 10.0,
    threshold = 0.1,
    matched_peaks = 0L
  )

  expect_s3_class(result, "data.frame")
})

test_that("create_edges_spectra works with entropy method", {
  skip_on_cran()
  local_test_project(copy = TRUE)
  paths <- get_default_paths()

  get_file(
    url = paths$urls$examples$spectra_mini,
    export = paths$data$source$spectra
  )

  expect_no_error(
    create_edges_spectra(
      ppm = 1.0,
      dalton = 0.001,
      method = "entropy"
    )
  )
})

test_that("create_edges_spectra handles MS1 only data", {
  local_test_project(copy = TRUE)
  paths <- get_default_paths()

  get_file(
    url = paths$urls$examples$spectra_ms1,
    export = paths$data$source$spectra |>
      gsub(
        pattern = "example_spectra",
        replacement = "example_spectra_ms1",
        fixed = TRUE
      )
  )

  expect_no_error(
    create_edges_spectra(
      input = "data/source/example_spectra_ms1.mgf",
      ppm = 1.0,
      dalton = 0.001
    )
  )
})
