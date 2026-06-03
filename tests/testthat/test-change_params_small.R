# Test Suite: change_params_small ----

library(testthat)


## Input Validation Tests ----

test_that("change_params_small validates ms_pol parameter", {
  expect_error(
    change_params_small(ms_pol = "invalid"),
    "must be either 'pos' or 'neg'"
  )

  expect_error(
    change_params_small(ms_pol = "positive"),
    "must be either 'pos' or 'neg'"
  )

  expect_error(
    change_params_small(ms_pol = 123),
    "must be either 'pos' or 'neg'"
  )
})

test_that("change_params_small handles NULL ms_pol", {
  skip_on_ci()
  # Should not error with NULL
  expect_no_error(
    change_params_small(ms_pol = NULL)
  )
})

## File Validation Tests ----

test_that("change_params_small errors on missing features file", {
  skip_on_ci()
  expect_error(
    change_params_small(fil_fea_raw = "nonexistent_features.csv"),
    "Features file does not exist"
  )
})

test_that("change_params_small errors on missing metadata file", {
  skip_on_ci()
  expect_error(
    change_params_small(fil_met_raw = "nonexistent_metadata.tsv"),
    "Metadata file does not exist"
  )
})

test_that("change_params_small errors on missing SIRIUS file", {
  skip_on_ci()
  expect_error(
    change_params_small(fil_sir_raw = "nonexistent_sirius.zip"),
    "SIRIUS annotations file does not exist"
  )
})

test_that("change_params_small errors on missing spectra file", {
  skip_on_ci()
  expect_error(
    change_params_small(fil_spe_raw = "nonexistent_spectra.mgf"),
    "Spectra file does not exist"
  )
})

test_that("change_params_small errors on missing mzTab file", {
  skip_on_ci()
  expect_error(
    change_params_small(fil_mzt_raw = "nonexistent_annotations.mztab"),
    "mzTab file does not exist"
  )
})

## Successful Update Tests ----

test_that("change_params_small runs with valid polarity", {
  skip_on_ci()
  skip_if_not_installed("yaml")

  # Create minimal environment
  tmpdir <- tempfile()
  dir.create(tmpdir, recursive = TRUE)
  dir.create(file.path(tmpdir, "params"), recursive = TRUE)
  on.exit(unlink(tmpdir, recursive = TRUE))

  # Create minimal params files
  minimal_params <- list(
    ms = list(polarity = "pos"),
    files = list(
      pattern = "test",
      features = list(raw = "features.csv"),
      metadata = list(raw = "metadata.tsv"),
      annotations = list(raw = list(sirius = "sirius.zip")),
      spectral = list(raw = "spectra.mgf")
    )
  )

  yaml::write_yaml(
    minimal_params,
    file.path(tmpdir, "params/prepare_params.yaml")
  )
  yaml::write_yaml(
    minimal_params,
    file.path(tmpdir, "params/prepare_params_advanced.yaml")
  )

  # Create dummy files in source directory
  source_dir <- file.path(tmpdir, "data/source")
  dir.create(source_dir, recursive = TRUE)

  write.csv(
    data.frame(a = 1),
    file.path(source_dir, "features.csv"),
    row.names = FALSE
  )
  write.table(
    data.frame(b = 1),
    file.path(source_dir, "metadata.tsv"),
    sep = "\t",
    row.names = FALSE
  )
  writeLines("dummy", file.path(source_dir, "sirius.zip"))
  writeLines("dummy", file.path(source_dir, "spectra.mgf"))

  # Should run without error using cache_dir parameter
  result <- change_params_small(ms_pol = "neg", cache_dir = tmpdir)

  expect_null(result) # Returns invisible NULL

  # Check that params were updated
  params <- yaml::read_yaml(
    file = file.path(tmpdir, "params/prepare_params.yaml")
  )
  expect_equal(params$ms$polarity, "neg")
})

test_that("change_params_small updates all file parameters", {
  skip_if_not_installed("yaml")
  skip_on_ci()

  tmpdir <- tempfile()
  dir.create(tmpdir, recursive = TRUE)
  dir.create(file.path(tmpdir, "params"), recursive = TRUE)
  on.exit(unlink(tmpdir, recursive = TRUE))

  # Create params
  minimal_params <- list(
    ms = list(polarity = "pos"),
    files = list(
      pattern = "old",
      features = list(raw = "old_features.csv"),
      metadata = list(raw = "old_metadata.tsv"),
      annotations = list(raw = list(sirius = "old_sirius.zip")),
      spectral = list(raw = "old_spectra.mgf")
    )
  )

  yaml::write_yaml(
    minimal_params,
    file.path(tmpdir, "params/prepare_params.yaml")
  )
  yaml::write_yaml(
    minimal_params,
    file.path(tmpdir, "params/prepare_params_advanced.yaml")
  )

  # Create source directory for test files
  source_dir <- file.path(tmpdir, "test_source")
  dir.create(source_dir, recursive = TRUE)

  # Create new files in source directory
  new_files <- list(
    features = file.path(source_dir, "new_features.csv"),
    metadata = file.path(source_dir, "new_metadata.tsv"),
    sirius = file.path(source_dir, "new_sirius.zip"),
    spectra = file.path(source_dir, "new_spectra.mgf"),
    mztab = file.path(source_dir, "new_annotations.mztab")
  )

  invisible(vapply(
    X = new_files,
    FUN = function(f) {
      writeLines("dummy", f)
      TRUE
    },
    FUN.VALUE = logical(1L)
  ))

  # Update all parameters with cache_dir
  result <- change_params_small(
    fil_pat = "new_pattern",
    fil_fea_raw = new_files$features,
    fil_met_raw = new_files$metadata,
    fil_sir_raw = new_files$sirius,
    fil_spe_raw = new_files$spectra,
    fil_mzt_raw = new_files$mztab,
    ms_pol = "neg",
    cache_dir = tmpdir
  )

  # Verify updates
  params <- yaml::read_yaml(
    file = file.path(tmpdir, "params/prepare_params.yaml")
  )
  expect_equal(params$files$pattern, "new_pattern")
  expect_equal(params$ms$polarity, "neg")
  # Files are copied to data/source
  expect_true(grepl("new_features.csv", params$files$features$raw))
  expect_true(grepl("new_annotations.mztab", params$files$mztab$raw))
})

test_that("change_params_small supports mzTab-only update while keeping existing core files", {
  skip_if_not_installed("yaml")
  skip_on_ci()

  tmpdir <- tempfile()
  dir.create(tmpdir, recursive = TRUE)
  dir.create(file.path(tmpdir, "params"), recursive = TRUE)
  on.exit(unlink(tmpdir, recursive = TRUE))

  minimal_params <- list(
    ms = list(polarity = "pos"),
    files = list(
      pattern = "old",
      mztab = list(raw = "null"),
      features = list(raw = "data/source/old_features.csv"),
      metadata = list(raw = "data/source/old_metadata.tsv"),
      annotations = list(
        raw = list(sirius = "data/interim/annotations/old_sirius.zip")
      ),
      spectral = list(raw = "data/source/old_spectra.mgf")
    )
  )

  yaml::write_yaml(
    minimal_params,
    file.path(tmpdir, "params/prepare_params.yaml")
  )
  yaml::write_yaml(
    minimal_params,
    file.path(tmpdir, "params/prepare_params_advanced.yaml")
  )

  source_dir <- file.path(tmpdir, "test_source")
  dir.create(source_dir, recursive = TRUE)
  mztab_path <- file.path(source_dir, "input_only.mztab")
  writeLines("MTD\tmzTab-version\t2.0.0-M", mztab_path)

  expect_no_error(change_params_small(
    fil_mzt_raw = mztab_path,
    cache_dir = tmpdir
  ))

  params <- yaml::read_yaml(file.path(tmpdir, "params/prepare_params.yaml"))
  expect_true(grepl("input_only.mztab", params$files$mztab$raw))
  expect_identical(params$files$features$raw, "data/source/old_features.csv")
  expect_identical(params$files$spectral$raw, "data/source/old_spectra.mgf")
  expect_identical(params$files$metadata$raw, "data/source/old_metadata.tsv")
})

test_that("change_params_small accepts mzTab with explicit overlays for features/metadata/spectra", {
  skip_if_not_installed("yaml")
  skip_on_ci()

  tmpdir <- tempfile()
  dir.create(tmpdir, recursive = TRUE)
  dir.create(file.path(tmpdir, "params"), recursive = TRUE)
  on.exit(unlink(tmpdir, recursive = TRUE))

  minimal_params <- list(
    ms = list(polarity = "pos"),
    files = list(
      pattern = "old",
      mztab = list(raw = "null"),
      features = list(raw = "data/source/old_features.csv"),
      metadata = list(raw = "data/source/old_metadata.tsv"),
      annotations = list(
        raw = list(sirius = "data/interim/annotations/old_sirius.zip")
      ),
      spectral = list(raw = "data/source/old_spectra.mgf")
    )
  )

  yaml::write_yaml(
    minimal_params,
    file.path(tmpdir, "params/prepare_params.yaml")
  )
  yaml::write_yaml(
    minimal_params,
    file.path(tmpdir, "params/prepare_params_advanced.yaml")
  )

  source_dir <- file.path(tmpdir, "test_source")
  dir.create(source_dir, recursive = TRUE)
  mztab_path <- file.path(source_dir, "input_overlay.mztab")
  fea_path <- file.path(source_dir, "overlay_features.csv")
  met_path <- file.path(source_dir, "overlay_metadata.tsv")
  spe_path <- file.path(source_dir, "overlay_spectra.mgf")

  writeLines("MTD\tmzTab-version\t2.0.0-M", mztab_path)
  write.csv(data.frame(a = 1), fea_path, row.names = FALSE)
  write.table(data.frame(b = 1), met_path, sep = "\t", row.names = FALSE)
  writeLines("BEGIN IONS\nEND IONS", spe_path)

  expect_no_error(change_params_small(
    fil_mzt_raw = mztab_path,
    fil_fea_raw = fea_path,
    fil_met_raw = met_path,
    fil_spe_raw = spe_path,
    cache_dir = tmpdir
  ))

  params <- yaml::read_yaml(file.path(tmpdir, "params/prepare_params.yaml"))
  expect_true(grepl("input_overlay.mztab", params$files$mztab$raw))
  expect_true(grepl("overlay_features.csv", params$files$features$raw))
  expect_true(grepl("overlay_metadata.tsv", params$files$metadata$raw))
  expect_true(grepl("overlay_spectra.mgf", params$files$spectral$raw))
})

## Internal Helper Tests ----

test_that("validate_params_small_inputs accepts valid polarity", {
  expect_silent(validate_params_small_inputs(ms_pol = "pos"))
  expect_silent(validate_params_small_inputs(ms_pol = "neg"))
  expect_silent(validate_params_small_inputs(ms_pol = NULL))
})

test_that("validate_params_small_inputs errors on invalid polarity", {
  expect_error(
    validate_params_small_inputs(ms_pol = "invalid"),
    "must be either 'pos' or 'neg'"
  )
  expect_error(
    validate_params_small_inputs(ms_pol = "positive"),
    "must be either 'pos' or 'neg'"
  )
  expect_error(
    validate_params_small_inputs(ms_pol = 1),
    "must be either 'pos' or 'neg'"
  )
})

test_that("copy_file_to_target copies file successfully", {
  skip_if_not_installed("fs")

  tmpdir <- tempfile()
  dir.create(tmpdir, recursive = TRUE)
  on.exit(unlink(tmpdir, recursive = TRUE))

  # Create source file
  source_file <- file.path(tmpdir, "source.txt")
  writeLines("test content", source_file)

  # Create target directory
  target_dir <- file.path(tmpdir, "target")
  dir.create(target_dir, recursive = TRUE)

  # Copy file
  result <- copy_file_to_target(
    file_path = source_file,
    target_dir = target_dir,
    file_description = "Test file"
  )

  expect_true(file.exists(result))
  expect_equal(basename(result), "source.txt")
  expect_true(file.exists(file.path(target_dir, "source.txt")))
})

test_that("copy_file_to_target errors on missing file", {
  tmpdir <- tempfile()
  dir.create(tmpdir, recursive = TRUE)
  on.exit(unlink(tmpdir, recursive = TRUE))

  expect_error(
    copy_file_to_target(
      file_path = file.path(tmpdir, "nonexistent.txt"),
      target_dir = tmpdir,
      file_description = "Missing file"
    ),
    "Missing file.*does not exist"
  )
})

test_that("copy_file_to_target overwrites existing file", {
  skip_if_not_installed("fs")

  tmpdir <- tempfile()
  dir.create(tmpdir, recursive = TRUE)
  on.exit(unlink(tmpdir, recursive = TRUE))

  # Create source file
  source_file <- file.path(tmpdir, "source.txt")
  writeLines("new content", source_file)

  # Create target directory with existing file
  target_dir <- file.path(tmpdir, "target")
  dir.create(target_dir, recursive = TRUE)
  target_file <- file.path(target_dir, "source.txt")
  writeLines("old content", target_file)

  # Copy file (should overwrite)
  result <- copy_file_to_target(
    file_path = source_file,
    target_dir = target_dir,
    file_description = "Test file"
  )

  expect_true(file.exists(result))
  content <- readLines(result)
  expect_equal(content, "new content")
})

test_that("create_yaml_null_handler creates proper function", {
  handler <- create_yaml_null_handler()
  expect_true(is.function(handler))

  # Test NA conversion
  result_na <- handler(NA)
  expect_equal(result_na |> names(), NULL)
  expect_s3_class(result_na, "verbatim")

  # Test non-NA passthrough
  result_string <- handler("test")
  expect_equal(result_string, "test")
  expect_false(inherits(result_string, "verbatim"))

  result_number <- handler(42)
  expect_equal(result_number, 42)

  result_logical <- handler(TRUE)
  expect_equal(result_logical, TRUE)
})

test_that("change_params_small handles NULL parameters gracefully", {
  skip_if_not_installed("yaml")
  skip_on_ci()

  tmpdir <- tempfile()
  dir.create(tmpdir, recursive = TRUE)
  dir.create(file.path(tmpdir, "params"), recursive = TRUE)
  on.exit(unlink(tmpdir, recursive = TRUE))

  # Create minimal params
  minimal_params <- list(
    ms = list(polarity = "pos"),
    files = list(
      pattern = "original",
      features = list(raw = "features.csv"),
      metadata = list(raw = "metadata.tsv"),
      annotations = list(raw = list(sirius = "sirius.zip")),
      spectral = list(raw = "spectra.mgf")
    )
  )

  yaml::write_yaml(
    minimal_params,
    file.path(tmpdir, "params/prepare_params.yaml")
  )
  yaml::write_yaml(
    minimal_params,
    file.path(tmpdir, "params/prepare_params_advanced.yaml")
  )

  # Create source directory
  source_dir <- file.path(tmpdir, "data/source")
  dir.create(source_dir, recursive = TRUE)

  # Create dummy files
  write.csv(
    data.frame(a = 1),
    file.path(source_dir, "features.csv"),
    row.names = FALSE
  )
  write.table(
    data.frame(b = 1),
    file.path(source_dir, "metadata.tsv"),
    sep = "\t",
    row.names = FALSE
  )
  writeLines("dummy", file.path(source_dir, "sirius.zip"))
  writeLines("dummy", file.path(source_dir, "spectra.mgf"))

  # Run with all NULL parameters - should not error
  expect_no_error(
    change_params_small(cache_dir = tmpdir)
  )

  # Verify params unchanged
  params <- yaml::read_yaml(file.path(tmpdir, "params/prepare_params.yaml"))
  expect_equal(params$files$pattern, "original")
  expect_equal(params$ms$polarity, "pos")
})

test_that("change_params_small handles partial updates", {
  skip_if_not_installed("yaml")
  skip_on_ci()

  tmpdir <- tempfile()
  dir.create(tmpdir, recursive = TRUE)
  dir.create(file.path(tmpdir, "params"), recursive = TRUE)
  on.exit(unlink(tmpdir, recursive = TRUE))

  # Create params
  minimal_params <- list(
    ms = list(polarity = "pos"),
    files = list(
      pattern = "original",
      features = list(raw = "features.csv"),
      metadata = list(raw = "metadata.tsv"),
      annotations = list(raw = list(sirius = "sirius.zip")),
      spectral = list(raw = "spectra.mgf")
    )
  )

  yaml::write_yaml(
    minimal_params,
    file.path(tmpdir, "params/prepare_params.yaml")
  )
  yaml::write_yaml(
    minimal_params,
    file.path(tmpdir, "params/prepare_params_advanced.yaml")
  )

  # Create source directory
  source_dir <- file.path(tmpdir, "test_files")
  dir.create(source_dir, recursive = TRUE)

  # Create files
  writeLines("dummy", file.path(source_dir, "features.csv"))
  writeLines("dummy", file.path(source_dir, "new_features.csv"))

  # Update only features file with cache_dir
  result <- change_params_small(
    fil_fea_raw = file.path(source_dir, "new_features.csv"),
    cache_dir = tmpdir
  )

  # Verify only features changed
  params <- yaml::read_yaml(
    file = file.path(tmpdir, "params/prepare_params.yaml")
  )
  expect_true(grepl("new_features.csv", params$files$features$raw))
  expect_equal(params$files$pattern, "original") # Unchanged
  expect_equal(params$ms$polarity, "pos") # Unchanged
})

test_that("change_params_small updates both yaml files", {
  skip_if_not_installed("yaml")
  skip_on_ci()

  tmpdir <- tempfile()
  dir.create(tmpdir, recursive = TRUE)
  dir.create(file.path(tmpdir, "params"), recursive = TRUE)
  on.exit(unlink(tmpdir, recursive = TRUE))

  withr::local_dir(new = tmpdir)

  # Create params
  minimal_params <- list(
    ms = list(polarity = "pos"),
    files = list(
      pattern = "test",
      features = list(raw = "features.csv"),
      metadata = list(raw = "metadata.tsv"),
      annotations = list(raw = list(sirius = "sirius.zip")),
      spectral = list(raw = "spectra.mgf")
    )
  )

  yaml::write_yaml(
    minimal_params,
    file.path(tmpdir, "params/prepare_params.yaml")
  )
  yaml::write_yaml(
    minimal_params,
    file.path(tmpdir, "params/prepare_params_advanced.yaml")
  )

  # Create files
  writeLines("dummy", "features.csv")
  writeLines("dummy", "metadata.tsv")
  writeLines("dummy", "sirius.zip")
  writeLines("dummy", "spectra.mgf")

  # Update pattern
  result <- change_params_small(fil_pat = "updated_pattern", cache_dir = ".")

  # Verify both files updated
  params1 <- yaml::read_yaml(
    file = file.path(tmpdir, "params/prepare_params.yaml")
  )
  params2 <- yaml::read_yaml(
    file = file.path(
      tmpdir,
      "params/prepare_params_advanced.yaml"
    )
  )

  expect_equal(params1$files$pattern, "updated_pattern")
  expect_equal(params2$files$pattern, "test")
})

test_that("change_params_small uses go_to_cache when cache_dir is NULL", {
  skip_if_not_installed("withr")

  tmp <- withr::local_tempdir()
  captured_yaml <- NULL

  local_mocked_bindings(
    go_to_cache = function() {
      old_wd <- getwd()
      on.exit(setwd(old_wd), add = TRUE)
      setwd(tmp)
      invisible(NULL)
    },
    get_default_paths = function() {
      list(
        data = list(
          source = list(path = file.path(tmp, "data", "source")),
          interim = list(
            annotations = list(
              path = file.path(tmp, "data", "interim", "annotations")
            )
          )
        ),
        params = list(
          prepare_params = file.path(tmp, "params", "prepare_params.yaml")
        )
      )
    },
    create_dir = function(path) {
      dir.create(path, recursive = TRUE, showWarnings = FALSE)
      invisible(NULL)
    },
    load_yaml_files = function() {
      list(
        yamls_params = list(
          prepare_params = list(
            files = list(
              pattern = "old",
              features = list(raw = "old_features.csv"),
              metadata = list(raw = "old_metadata.tsv"),
              annotations = list(
                raw = list(sirius = "old_sirius.zip", mzmine = "old_mzmine.csv")
              ),
              spectral = list(raw = "old_spectra.mgf")
            ),
            ms = list(polarity = "pos"),
            organisms = list(taxon = "Old taxon"),
            options = list(high_evidence = FALSE, summarize = FALSE)
          )
        )
      )
    },
    copy_file_to_target = function(file_path, target_dir, file_description) {
      force(file_path)
      force(target_dir)
      force(file_description)
      file.path(target_dir, basename(file_path))
    },
    .package = "tima"
  )

  local_mocked_bindings(
    write_yaml = function(x, file, ...) {
      captured_yaml <<- x
      invisible(NULL)
    },
    .package = "yaml"
  )

  expect_no_error(change_params_small(
    fil_pat = "new_pattern",
    fil_sir_raw = NULL,
    org_tax = NULL,
    cache_dir = NULL
  ))

  expect_equal(captured_yaml$files$pattern, "new_pattern")
  expect_true(is.na(captured_yaml$files$annotations$raw$sirius))
  expect_true(is.na(captured_yaml$organisms$taxon))
})

# ---- Internal function tests ----

test_that("validate_params_small_inputs validates polarity", {
  # Valid values
  expect_silent(validate_params_small_inputs(ms_pol = "pos"))
  expect_silent(validate_params_small_inputs(ms_pol = "neg"))
  expect_silent(validate_params_small_inputs(ms_pol = NULL))

  # Invalid values
  expect_error(
    validate_params_small_inputs(ms_pol = "invalid"),
    "must be either 'pos' or 'neg'"
  )

  expect_error(
    validate_params_small_inputs(ms_pol = "positive"),
    "must be either 'pos' or 'neg'"
  )

  expect_error(
    validate_params_small_inputs(ms_pol = 1),
    "must be either 'pos' or 'neg'"
  )
})

test_that("copy_file_to_target copies files correctly", {
  skip_if_not_installed("fs")

  tmpdir <- tempfile()
  dir.create(tmpdir, recursive = TRUE)
  on.exit(unlink(tmpdir, recursive = TRUE))

  # Create source file
  source_file <- file.path(tmpdir, "source.txt")
  writeLines("test content", source_file)

  # Create target directory
  target_dir <- file.path(tmpdir, "target")
  dir.create(target_dir, recursive = TRUE)

  # Copy file
  result <- copy_file_to_target(
    file_path = source_file,
    target_dir = target_dir,
    file_description = "Test"
  )

  expect_true(file.exists(result))
  expect_equal(dirname(result), target_dir)
  expect_equal(basename(result), "source.txt")
})

test_that("copy_file_to_target errors on missing file", {
  tmpdir <- tempfile()
  dir.create(tmpdir, recursive = TRUE)
  on.exit(unlink(tmpdir, recursive = TRUE))

  expect_error(
    copy_file_to_target(
      file_path = file.path(tmpdir, "nonexistent.txt"),
      target_dir = tmpdir,
      file_description = "Missing"
    ),
    "Missing file does not exist"
  )
})

test_that("create_yaml_null_handler creates proper function", {
  handler <- create_yaml_null_handler()

  expect_true(is.function(handler))

  # Test NA conversion
  result_na <- handler(NA)
  expect_equal(result_na |> names(), NULL)
  expect_s3_class(result_na, "verbatim")

  # Test non-NA passthrough
  expect_equal(handler("test"), "test")
  expect_equal(handler(42), 42)
  expect_equal(handler(TRUE), TRUE)
})

test_that("prepare_params propagates mzTab paths into mzTab workflow step YAMLs", {
  skip_if_not_installed("yaml")

  params_root <- system.file("params", package = "tima")
  skip_if(
    !nzchar(params_root),
    "Could not locate installed tima params directory"
  )
  default_dir <- file.path(params_root, "default")
  default_files <- list.files(
    default_dir,
    pattern = "\\.yaml$",
    full.names = TRUE
  )

  yamls <- lapply(default_files, yaml::read_yaml)
  names(yamls) <- sub("\\.yaml$", "", basename(default_files))
  prepare_params_path <- file.path(params_root, "prepare_params.yaml")
  prepare_params_advanced_path <- file.path(
    params_root,
    "prepare_params_advanced.yaml"
  )
  yamls$prepare_params <- yaml::read_yaml(prepare_params_path)
  yamls$prepare_params_advanced <- yaml::read_yaml(prepare_params_advanced_path)

  fake_yaml_files <- c(
    default_files,
    prepare_params_path,
    prepare_params_advanced_path
  )
  fake_yaml_names <- c(
    sub("\\.yaml$", "", basename(default_files)),
    "prepare_params",
    "prepare_params_advanced"
  )

  captured <- new.env(parent = emptyenv())

  testthat::local_mocked_bindings(
    load_yaml_files = function() {
      list(
        yamls_params = yamls,
        yaml_files = fake_yaml_files,
        yaml_names = fake_yaml_names
      )
    },
    create_dir = function(...) invisible(NULL),
    .package = "tima"
  )

  testthat::local_mocked_bindings(
    write_yaml = function(x, file, ...) {
      assign(basename(file), x, envir = captured)
      invisible(NULL)
    },
    .package = "yaml"
  )

  params_small <- yaml::read_yaml(prepare_params_path)
  params_advanced <- yaml::read_yaml(prepare_params_advanced_path)
  params_small$files$mztab$raw <- "data/source/example_input.mztab"
  params_advanced$files$mztab$raw <- "data/source/example_input.mztab"

  prepare_params(
    params_small = params_small,
    params_advanced = params_advanced,
    step = "prepare_annotations_mztab"
  )
  mztab_step <- get("prepare_annotations_mztab.yaml", envir = captured)
  expect_identical(
    mztab_step$files$mztab$raw,
    "data/source/example_input.mztab"
  )
  expect_true(
    "mztab" %in% names(mztab_step$files$annotations$prepared$structural)
  )

  prepare_params(
    params_small = params_small,
    params_advanced = params_advanced,
    step = "write_mztab"
  )
  write_step <- get("write_mztab.yaml", envir = captured)
  expect_identical(
    write_step$files$mztab$raw,
    "data/source/example_input.mztab"
  )
  expect_true(nzchar(write_step$files$output$mztab))
  expect_match(write_step$files$output$mztab, "\\.mztab$")
})

test_that("change_params_small initializes missing mztab entry and updates optional fields", {
  tmp <- withr::local_tempdir()
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(tmp)

  dir.create(file.path(tmp, "params"), recursive = TRUE, showWarnings = FALSE)
  dir.create(
    file.path(tmp, "data", "source"),
    recursive = TRUE,
    showWarnings = FALSE
  )
  dir.create(
    file.path(tmp, "data", "interim", "annotations"),
    recursive = TRUE,
    showWarnings = FALSE
  )
  writeLines("MTD\tmzTab-version\t2.0.0-M", file.path(tmp, "input.mztab"))

  captured <- NULL

  local_mocked_bindings(
    go_to_cache = function() {
      invisible(NULL)
    },
    get_default_paths = function() {
      list(
        data = list(
          source = list(path = file.path(tmp, "data", "source")),
          interim = list(
            annotations = list(
              path = file.path(tmp, "data", "interim", "annotations")
            )
          )
        ),
        params = list(
          prepare_params = file.path(tmp, "params", "prepare_params.yaml")
        )
      )
    },
    create_dir = function(path) {
      dir.create(path, recursive = TRUE, showWarnings = FALSE)
      invisible(NULL)
    },
    load_yaml_files = function() {
      list(
        yamls_params = list(
          prepare_params = list(
            files = list(
              pattern = "keep",
              features = list(raw = "features.csv"),
              metadata = list(raw = "metadata.tsv"),
              annotations = list(raw = list(sirius = "sirius.zip")),
              spectral = list(raw = "spectra.mgf")
            ),
            ms = list(polarity = "pos"),
            organisms = list(taxon = "existing taxon"),
            options = list(high_evidence = FALSE, summarize = FALSE)
          )
        )
      )
    },
    copy_file_to_target = function(file_path, target_dir, file_description) {
      force(file_path)
      force(target_dir)
      force(file_description)
      file.path(target_dir, basename(file_path))
    },
    .package = "tima"
  )

  local_mocked_bindings(
    write_yaml = function(x, file, ...) {
      captured <<- x
      invisible(NULL)
    },
    .package = "yaml"
  )

  expect_no_error(change_params_small(
    fil_mzt_raw = file.path(tmp, "input.mztab"),
    hig_evi = TRUE,
    summarize = TRUE,
    org_tax = NULL,
    cache_dir = NULL
  ))

  expect_true(is.list(captured$files$mztab))
  expect_identical(
    captured$files$mztab$raw,
    file.path(tmp, "data", "source", "input.mztab")
  )
  expect_identical(captured$organisms$taxon, NA)
  expect_identical(captured$options$high_evidence, TRUE)
  expect_identical(captured$options$summarize, TRUE)
})
