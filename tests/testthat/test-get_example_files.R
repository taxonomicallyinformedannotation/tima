# Test Suite: get_example_files ----

library(testthat)

test_that("get_example_files validates inputs", {
  expect_error(
    get_example_files(example = numeric()),
    "example.*non-empty character vector"
  )

  expect_error(
    get_example_files(example = character()),
    "example.*non-empty character vector"
  )

  expect_error(
    get_example_files(example = "unknown_example"),
    "Valid options"
  )

  expect_error(
    get_example_files(in_cache = "yes"),
    "in_cache.*single logical value"
  )
})

test_that("get_example_files dispatches requested downloads without cache", {
  calls <- new.env(parent = emptyenv())
  calls$get_file <- list()
  calls$get_example_sirius <- 0L
  calls$go_to_cache <- 0L

  with_mocked_bindings(
    {
      expect_invisible(
        get_example_files(
          example = c(
            "features",
            "metadata",
            "sirius",
            "spectra",
            "spectral_lib_with_rt"
          ),
          in_cache = FALSE
        )
      )
    },
    get_default_paths = function() {
      list(
        urls = list(
          examples = list(
            features = "https://example.test/features.csv",
            metadata = "https://example.test/metadata.tsv",
            spectra = "https://example.test/spectra.mgf",
            spectral_lib_mini = list(
              with_rt = "https://example.test/library.tsv"
            )
          )
        ),
        data = list(
          source = list(
            features = "data/source/features.csv",
            metadata = "data/source/metadata.tsv",
            spectra = "data/source/spectra.mgf",
            libraries = list(
              spectra = list(
                exp = list(with_rt = "data/source/library.tsv")
              )
            )
          )
        )
      )
    },
    get_file = function(url, export) {
      calls$get_file[[length(calls$get_file) + 1L]] <<- list(
        url = url,
        export = export
      )
      export
    },
    get_example_sirius = function() {
      calls$get_example_sirius <- calls$get_example_sirius + 1L
      invisible(NULL)
    },
    go_to_cache = function(dir = ".tima") {
      force(dir)
      calls$go_to_cache <- calls$go_to_cache + 1L
      invisible("~/.tima")
    },
    .package = "tima"
  )

  expect_equal(calls$go_to_cache, 0L)
  expect_equal(calls$get_example_sirius, 1L)
  expect_length(calls$get_file, 4L)
  expect_identical(
    vapply(calls$get_file, `[[`, character(1), "export"),
    c(
      "data/source/features.csv",
      "data/source/metadata.tsv",
      "data/source/spectra.mgf",
      "data/source/library.tsv"
    )
  )
})

test_that("get_example_files enters cache mode before downloading", {
  calls <- new.env(parent = emptyenv())
  calls$get_file <- 0L
  calls$go_to_cache <- 0L

  with_mocked_bindings(
    {
      expect_invisible(get_example_files(example = "features", in_cache = TRUE))
    },
    get_default_paths = function() {
      list(
        urls = list(examples = list(features = "https://example.test/features.csv")),
        data = list(source = list(features = "data/source/features.csv"))
      )
    },
    get_file = function(url, export) {
      force(url)
      force(export)
      calls$get_file <- calls$get_file + 1L
      invisible("data/source/features.csv")
    },
    go_to_cache = function(dir = ".tima") {
      force(dir)
      calls$go_to_cache <- calls$go_to_cache + 1L
      invisible("~/.tima")
    },
    .package = "tima"
  )

  expect_equal(calls$go_to_cache, 1L)
  expect_equal(calls$get_file, 1L)
})

test_that("get_example_files default examples exclude spectral RT library", {
  calls <- new.env(parent = emptyenv())
  calls$get_file <- character()
  calls$get_example_sirius <- 0L
  calls$go_to_cache <- 0L

  with_mocked_bindings(
    {
      expect_invisible(get_example_files())
    },
    get_default_paths = function() {
      list(
        urls = list(
          examples = list(
            features = "https://example.test/features.csv",
            metadata = "https://example.test/metadata.tsv",
            spectra = "https://example.test/spectra.mgf",
            spectral_lib_mini = list(
              with_rt = "https://example.test/library.tsv"
            )
          )
        ),
        data = list(
          source = list(
            features = "data/source/features.csv",
            metadata = "data/source/metadata.tsv",
            spectra = "data/source/spectra.mgf",
            libraries = list(
              spectra = list(
                exp = list(with_rt = "data/source/library.tsv")
              )
            )
          )
        )
      )
    },
    get_file = function(url, export) {
      force(url)
      calls$get_file <- c(calls$get_file, export)
      invisible(export)
    },
    get_example_sirius = function() {
      calls$get_example_sirius <- calls$get_example_sirius + 1L
      invisible(NULL)
    },
    go_to_cache = function(dir = ".tima") {
      force(dir)
      calls$go_to_cache <- calls$go_to_cache + 1L
      invisible("~/.tima")
    },
    .package = "tima"
  )

  expect_equal(calls$go_to_cache, 1L)
  expect_equal(calls$get_example_sirius, 1L)
  expect_identical(
    calls$get_file,
    c(
      "data/source/features.csv",
      "data/source/metadata.tsv",
      "data/source/spectra.mgf"
    )
  )
  expect_false(any(grepl("library\\.tsv$", calls$get_file)))
})

test_that("get_example_files handles a single example file", {
  calls <- new.env(parent = emptyenv())
  calls$get_file <- list()

  with_mocked_bindings(
    {
      expect_invisible(get_example_files(example = "features", in_cache = FALSE))
    },
    get_default_paths = function() {
      list(
        urls = list(examples = list(features = "https://example.test/features.csv")),
        data = list(source = list(features = "data/source/features.csv"))
      )
    },
    get_file = function(url, export) {
      calls$get_file[[length(calls$get_file) + 1L]] <<- c(url = url, export = export)
      invisible(export)
    },
    .package = "tima"
  )

  expect_length(calls$get_file, 1L)
  expect_identical(unname(calls$get_file[[1L]][["export"]]), "data/source/features.csv")
})
