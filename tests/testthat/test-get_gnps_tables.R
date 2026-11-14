# Test Suite: get_gnps_tables ----

library(testthat)

test_that("get_gnps_tables works with valid GNPS job ID", {
  skip_if_no_network("https://github.com")
  skip_if_no_network("https://gnps.ucsd.edu")
  withr::local_dir(new = temp_test_dir("get_gnps_tables_valid"))
  local_test_project(copy = TRUE)
  paths <- get_default_paths()
  params <- get_params(step = "prepare_params_advanced")

  get_file(
    url = paths$urls$examples$features,
    export = paths$data$source$features
  )
  get_file(
    url = paths$urls$examples$metadata,
    export = paths$data$source$metadata
  )

  expect_no_error(
    get_gnps_tables(
      filename = "example",
      path_features = paths$data$source$features,
      path_metadata = paths$data$source$metadata,
      path_spectra = paths$data$source$spectra,
      gnps_job_id = params$gnps$id
    )
  )
})

test_that("get_gnps_tables handles missing metadata", {
  skip_if_no_network("https://github.com")
  skip_if_no_network("https://gnps.ucsd.edu")
  withr::local_dir(new = temp_test_dir("get_gnps_tables_nometa"))
  local_test_project(copy = TRUE)
  paths <- get_default_paths()

  get_file(
    url = paths$urls$examples$features,
    export = paths$data$source$features
  )

  expect_no_error(
    get_gnps_tables(
      filename = "other",
      path_features = paths$data$source$features,
      path_metadata = paths$data$source$metadata,
      path_spectra = paths$data$source$spectra,
      gnps_job_id = paths$gnps$example2
    )
  )
})

test_that("get_gnps_tables works with example GNPS ID", {
  skip_if_no_network("https://github.com")
  skip_if_no_network("https://gnps.ucsd.edu")
  withr::local_dir(new = temp_test_dir("get_gnps_tables_example"))
  local_test_project(copy = TRUE)
  paths <- get_default_paths()

  get_file(
    url = paths$urls$examples$features,
    export = paths$data$source$features
  )
  get_file(
    url = paths$urls$examples$metadata,
    export = paths$data$source$metadata
  )

  expect_no_error(
    get_gnps_tables(
      path_features = paths$data$source$features,
      path_metadata = paths$data$source$metadata,
      path_spectra = paths$data$source$spectra,
      gnps_job_id = get_default_paths()$gnps$example
    )
  )
})

test_that("get_gnps_tables handles NULL GNPS job ID", {
  withr::local_dir(new = temp_test_dir("get_gnps_tables_null"))
  local_test_project(copy = TRUE)
  paths <- get_default_paths()

  dir.create(
    dirname(paths$data$source$features),
    recursive = TRUE,
    showWarnings = FALSE
  )
  writeLines(
    "row ID\trow m/z\trow retention time\n1\t150.0558\t1.5",
    paths$data$source$features
  )

  expect_no_error(
    get_gnps_tables(
      filename = "noGNPS",
      path_features = paths$data$source$features,
      path_metadata = list(),
      path_spectra = paths$data$source$spectra,
      gnps_job_id = NULL
    )
  )
})

test_that("get_gnps_tables handles empty GNPS job ID", {
  withr::local_dir(new = temp_test_dir("get_gnps_tables_empty"))
  local_test_project(copy = TRUE)
  paths <- get_default_paths()

  dir.create(
    dirname(paths$data$source$features),
    recursive = TRUE,
    showWarnings = FALSE
  )
  writeLines(
    "row ID\trow m/z\trow retention time\n1\t150.0558\t1.5",
    paths$data$source$features
  )

  expect_no_error(
    get_gnps_tables(
      filename = "noNoGNPS",
      path_features = paths$data$source$features,
      path_metadata = NULL,
      path_spectra = paths$data$source$spectra,
      gnps_job_id = ""
    )
  )
})

test_that("validate_gnps_job_id enforces 32-character boundary or example match", {
  ex_id <- "example_custom_id_short"

  # Exact match to gnps_job_example passes even when nchar != 32
  expect_no_warning(
    res_ex <- validate_gnps_job_id(ex_id, gnps_job_example = ex_id)
  )
  expect_null(res_ex)

  # Exact 32-character ID passes when different from gnps_job_example
  valid_32 <- strrep("a", 32L)
  expect_no_warning(
    res_32 <- validate_gnps_job_id(valid_32, gnps_job_example = ex_id)
  )
  expect_null(res_32)

  # Boundary n - 1 (31 chars) fails with classed validation error
  invalid_31 <- strrep("a", 31L)
  expect_error(
    validate_gnps_job_id(invalid_31, gnps_job_example = ex_id),
    regexp = "invalid GNPS job ID",
    class = "tima_validation_error"
  )

  # Boundary n + 1 (33 chars) fails with classed validation error
  invalid_33 <- strrep("a", 33L)
  expect_error(
    validate_gnps_job_id(invalid_33, gnps_job_example = ex_id),
    regexp = "invalid GNPS job ID",
    class = "tima_validation_error"
  )

  # Empty string (0 chars) fails when gnps_job_example is non-empty
  expect_error(
    validate_gnps_job_id("", gnps_job_example = ex_id),
    regexp = "invalid GNPS job ID",
    class = "tima_validation_error"
  )
})

test_that("download_gnps_file skips existing files, downloads missing files, and catches errors", {
  tmp_dir <- withr::local_tempdir()
  existing_file <- file.path(tmp_dir, "existing.tsv")
  writeLines("original_content", existing_file)

  rec <- new.env(parent = emptyenv())
  rec$urls <- character(0)
  rec$exports <- character(0)

  local_mocked_bindings(
    get_file = function(url, export, ...) {
      rec$urls <- c(rec$urls, url)
      rec$exports <- c(rec$exports, export)
      writeLines("downloaded_content", export)
      export
    }
  )

  # 1. Existing file: get_file must NOT be called and content must remain untouched
  expect_no_warning(
    res1 <- download_gnps_file(
      "https://example.org/1",
      existing_file,
      "features"
    )
  )
  expect_null(res1)
  expect_identical(rec$urls, character(0))
  expect_identical(readLines(existing_file), "original_content")

  # 2. Missing file: get_file IS called with exact url and export path
  missing_file <- file.path(tmp_dir, "missing.tsv")
  expect_no_warning(
    res2 <- download_gnps_file(
      "https://example.org/2",
      missing_file,
      "metadata"
    )
  )
  expect_null(res2)
  expect_identical(rec$urls, "https://example.org/2")
  expect_identical(rec$exports, missing_file)
  expect_identical(readLines(missing_file), "downloaded_content")

  # 3. Error during download: caught and logged without aborting
  local_mocked_bindings(
    get_file = function(url, export, ...) {
      stop("simulated network failure")
    }
  )
  failing_file <- file.path(tmp_dir, "failing.tsv")
  expect_no_error(
    res3 <- download_gnps_file("https://example.org/3", failing_file, "spectra")
  )
  expect_null(res3)
  expect_false(file.exists(failing_file))
})

test_that("create_fake_gnps_files writes exact schemas, handles metadata normalization, and falls back on write error", {
  tmp_dir <- withr::local_tempdir()
  withr::local_dir(tmp_dir)

  dir_a <- file.path(tmp_dir, "interim_a")
  dir_f <- file.path(tmp_dir, "interim_f")
  dir.create(dir_a, recursive = TRUE)
  dir.create(dir_f, recursive = TRUE)

  feat_path <- file.path(tmp_dir, "sub_f", "features.tsv")
  spec_path <- file.path(tmp_dir, "sub_s", "spectra.mgf")
  meta_path <- file.path(tmp_dir, "sub_m", "meta.tsv")
  dir.create(dirname(meta_path), recursive = TRUE)

  # List metadata path gets unlisted
  res <- create_fake_gnps_files(
    filename = "test_run",
    path_features = feat_path,
    path_metadata = list(meta_path),
    path_spectra = spec_path,
    path_interim_a = dir_a,
    path_interim_f = dir_f
  )

  expect_identical(
    res,
    c(
      features = feat_path,
      metadata = meta_path,
      spectra = spec_path,
      annotations = file.path(dir_a, "test_run_gnps.tsv"),
      components = file.path(dir_f, "test_run_components.tsv"),
      edges = file.path(dir_f, "test_run_edges_spectra.tsv")
    )
  )

  # Verify exact file contents and schemas
  expect_identical(readLines(feat_path), c("feature_id", ""))
  expect_identical(
    readLines(spec_path),
    c("BEGIN IONS", "TITLE=placeholder", "END IONS")
  )

  ann_df <- utils::read.delim(res[["annotations"]], check.names = FALSE)
  expect_identical(nrow(ann_df), 0L)
  expect_identical(
    colnames(ann_df),
    c(
      "#Scan#",
      "MassDiff",
      "MZErrorPPM",
      "Precursor_MZ",
      "LibraryName",
      "Compound_Name",
      "INCHI",
      "InChIKey",
      "InChIKey-Planar",
      "ExactMass",
      "MQScore",
      "SharedPeaks",
      "npclassifier_pathway",
      "npclassifier_superclass",
      "npclassifier_class",
      "superclass",
      "class",
      "subclass"
    )
  )

  comp_df <- utils::read.delim(res[["components"]], check.names = FALSE)
  expect_identical(nrow(comp_df), 0L)
  expect_identical(colnames(comp_df), c("cluster index", "componentindex"))

  edges_df <- utils::read.delim(res[["edges"]], check.names = FALSE)
  expect_identical(nrow(edges_df), 0L)
  expect_identical(colnames(edges_df), c("CLUSTERID1", "CLUSTERID2"))

  meta_df <- utils::read.delim(res[["metadata"]], check.names = FALSE)
  expect_identical(nrow(meta_df), 0L)
  expect_identical(colnames(meta_df), c("filename", "ATTRIBUTE_species"))

  # Empty / NULL metadata path defaults to data/source/metadata.tsv
  dir.create(file.path("data", "source"), recursive = TRUE)
  for (empty_meta in list(NULL, character(0), list(), "")) {
    res_def <- create_fake_gnps_files(
      filename = "def",
      path_features = feat_path,
      path_metadata = empty_meta,
      path_spectra = spec_path,
      path_interim_a = dir_a,
      path_interim_f = dir_f
    )
    expect_identical(
      res_def[["metadata"]],
      file.path("data", "source", "metadata.tsv")
    )
    expect_true(file.exists(file.path("data", "source", "metadata.tsv")))
  }

  # Fallback write.table when export_output throws an error
  local_mocked_bindings(
    export_output = function(...) stop("simulated export_output error")
  )
  res_fb <- create_fake_gnps_files(
    filename = "fallback",
    path_features = feat_path,
    path_metadata = meta_path,
    path_spectra = spec_path,
    path_interim_a = dir_a,
    path_interim_f = dir_f
  )
  expect_true(file.exists(res_fb[["annotations"]]))
  ann_fb <- utils::read.delim(res_fb[["annotations"]], check.names = FALSE)
  expect_identical(nrow(ann_fb), 0L)
  expect_identical(colnames(ann_fb)[1], "#Scan#")

  # Empty path_features and path_spectra skip placeholder creation without warning
  expect_no_warning(
    res_no_fs <- create_fake_gnps_files(
      filename = "no_fs",
      path_features = "",
      path_metadata = meta_path,
      path_spectra = "",
      path_interim_a = dir_a,
      path_interim_f = dir_f
    )
  )
  expect_identical(res_no_fs[["features"]], "")
  expect_identical(res_no_fs[["spectra"]], "")
})

test_that("get_gnps_tables constructs exact URLs and paths for fbmn and classical workflows offline", {
  tmp_dir <- withr::local_tempdir()
  withr::local_dir(tmp_dir)
  dir_src <- file.path(tmp_dir, "src")
  dir_a <- file.path(tmp_dir, "int_a")
  dir_f <- file.path(tmp_dir, "int_f")
  feat_p <- file.path(dir_src, "feat.tsv")
  spec_p <- file.path(dir_src, "spec.mgf")
  meta_p <- file.path(dir_src, "meta.tsv")

  job_32 <- "0123456789abcdef0123456789abcdef"
  ex_32 <- "fedcba9876543210fedcba9876543210"

  local_mocked_bindings(
    get_file = function(url, export, ...) export
  )

  # 1. Invalid workflow is rejected with tima_validation_error
  expect_error(
    get_gnps_tables(
      gnps_job_id = job_32,
      gnps_job_example = ex_32,
      workflow = "invalid_wf",
      path_features = feat_p,
      path_metadata = meta_p,
      path_spectra = spec_p,
      path_source = dir_src,
      path_interim_a = dir_a,
      path_interim_f = dir_f
    ),
    regexp = "unsupported workflow",
    class = "tima_validation_error"
  )

  # 2. Invalid job ID is rejected before downloading
  expect_error(
    get_gnps_tables(
      gnps_job_id = "short_id",
      gnps_job_example = ex_32,
      workflow = "fbmn",
      path_features = feat_p,
      path_metadata = meta_p,
      path_spectra = spec_p,
      path_source = dir_src,
      path_interim_a = dir_a,
      path_interim_f = dir_f
    ),
    regexp = "invalid GNPS job ID",
    class = "tima_validation_error"
  )

  # 3. FBMN workflow with custom 32-char job ID and NULL metadata
  rec <- new.env(parent = emptyenv())
  rec$urls <- character(0)
  rec$paths <- character(0)
  local_mocked_bindings(
    get_file = function(url, export, ...) {
      rec$urls <- c(rec$urls, url)
      rec$paths <- c(rec$paths, export)
      export
    }
  )

  res_fbmn <- get_gnps_tables(
    gnps_job_id = job_32,
    gnps_job_example = ex_32,
    workflow = "fbmn",
    path_features = feat_p,
    path_metadata = NULL,
    path_spectra = spec_p,
    path_source = dir_src,
    path_interim_a = dir_a,
    path_interim_f = dir_f
  )

  base_u <- paste0(
    "https://gnps.ucsd.edu/ProteoSAFe/DownloadResultFile?task=",
    job_32,
    "&block=main&file="
  )
  expected_fbmn_paths <- c(
    features = feat_p,
    metadata = file.path(dir_src, paste0(job_32, "_metadata.tsv")),
    spectra = spec_p,
    annotations = file.path(dir_a, paste0(job_32, "_gnps.tsv")),
    components = file.path(dir_f, paste0(job_32, "_components.tsv")),
    edges = file.path(dir_f, paste0(job_32, "_edges_spectra.tsv"))
  )
  expect_identical(res_fbmn, expected_fbmn_paths)
  expect_identical(unname(expected_fbmn_paths), rec$paths)
  expect_identical(
    rec$urls,
    c(
      paste0(base_u, "quantification_table_reformatted/"),
      paste0(base_u, "metadata_table/"),
      paste0(base_u, "spectra/"),
      paste0(base_u, "DB_result/"),
      paste0(base_u, "clusterinfo_summary/"),
      paste0(base_u, "networkedges_selfloop/")
    )
  )

  # 3b. FBMN workflow with empty character(0) path_metadata
  res_fbmn_empty_meta <- get_gnps_tables(
    gnps_job_id = job_32,
    gnps_job_example = ex_32,
    workflow = "fbmn",
    path_features = feat_p,
    path_metadata = character(0),
    path_spectra = spec_p,
    path_source = dir_src,
    path_interim_a = dir_a,
    path_interim_f = dir_f
  )
  expect_identical(
    res_fbmn_empty_meta[["metadata"]],
    file.path(dir_src, paste0(job_32, "_metadata.tsv"))
  )

  # 4. Classical workflow with example job ID and list metadata path
  rec$cl_urls <- character(0)
  rec$cl_paths <- character(0)
  local_mocked_bindings(
    get_file = function(url, export, ...) {
      rec$cl_urls <- c(rec$cl_urls, url)
      rec$cl_paths <- c(rec$cl_paths, export)
      export
    }
  )

  res_cl <- get_gnps_tables(
    gnps_job_id = ex_32,
    gnps_job_example = ex_32,
    workflow = "classical",
    path_features = feat_p,
    path_metadata = list(meta_p),
    path_spectra = spec_p,
    path_source = dir_src,
    path_interim_a = dir_a,
    path_interim_f = dir_f
  )

  base_ex <- paste0(
    "https://gnps.ucsd.edu/ProteoSAFe/DownloadResultFile?task=",
    ex_32,
    "&block=main&file="
  )
  expected_cl_paths <- c(
    features = feat_p,
    metadata = meta_p,
    spectra = spec_p,
    annotations = file.path(dir_a, "example_gnps.tsv"),
    components = file.path(dir_f, "example_components.tsv"),
    edges = file.path(dir_f, "example_edges_spectra.tsv")
  )
  expect_identical(res_cl, expected_cl_paths)
  expect_identical(unname(expected_cl_paths), rec$cl_paths)
  expect_identical(
    rec$cl_urls,
    c(
      paste0(base_ex, "quantification_table_reformatted/"),
      paste0(base_ex, "metadata_table/"),
      paste0(base_ex, "spectra/"),
      paste0(base_ex, "result_specnets_DB/"),
      paste0(
        base_ex,
        "clusterinfosummarygroup_attributes_withIDs_withcomponentID/"
      ),
      paste0(base_ex, "networkedges_selfloop/")
    )
  )

  # 4b. Classical workflow with scalar character path_metadata
  rec$sc_paths <- character(0)
  local_mocked_bindings(
    get_file = function(url, export, ...) {
      rec$sc_paths <- c(rec$sc_paths, export)
      export
    }
  )
  res_sc <- get_gnps_tables(
    gnps_job_id = ex_32,
    gnps_job_example = ex_32,
    workflow = "classical",
    path_features = feat_p,
    path_metadata = meta_p,
    path_spectra = spec_p,
    path_source = dir_src,
    path_interim_a = dir_a,
    path_interim_f = dir_f
  )
  expect_identical(res_sc[["metadata"]], meta_p)
  expect_identical(rec$sc_paths[[2]], meta_p)

  # 5. NULL and "" gnps_job_id dispatch to create_fake_gnps_files offline
  dir.create(dir_a, recursive = TRUE)
  dir.create(dir_f, recursive = TRUE)
  res_null <- get_gnps_tables(
    gnps_job_id = NULL,
    filename = "offline_null",
    path_features = feat_p,
    path_metadata = meta_p,
    path_spectra = spec_p,
    path_source = dir_src,
    path_interim_a = dir_a,
    path_interim_f = dir_f
  )
  expect_identical(
    res_null[["annotations"]],
    file.path(dir_a, "offline_null_gnps.tsv")
  )
  expect_true(file.exists(res_null[["annotations"]]))

  res_empty <- get_gnps_tables(
    gnps_job_id = "",
    filename = "offline_empty",
    path_features = feat_p,
    path_metadata = meta_p,
    path_spectra = spec_p,
    path_source = dir_src,
    path_interim_a = dir_a,
    path_interim_f = dir_f
  )
  expect_identical(
    res_empty[["annotations"]],
    file.path(dir_a, "offline_empty_gnps.tsv")
  )
  expect_true(file.exists(res_empty[["annotations"]]))
})
