# Test Suite: create_edges_spectra ----

library(testthat)

test_that("create_edges_spectra validates input parameter", {
  expect_error(
    create_edges_spectra(input = NULL),
    "must be"
  )

  expect_error(
    create_edges_spectra(input = 123),
    "must be"
  )
})

test_that("create_edges_spectra uses the full similarity graph by default", {
  mgf_file <- tempfile(fileext = ".mgf")
  mgf <- c(
    "BEGIN IONS",
    "TITLE=Spectrum1",
    "FEATURE_ID=1",
    "PEPMASS=100",
    "CHARGE=1+",
    "50 10",
    "75 20",
    "100 30",
    "END IONS",
    "",
    "BEGIN IONS",
    "TITLE=Spectrum2",
    "FEATURE_ID=2",
    "PEPMASS=101",
    "CHARGE=1+",
    "50 15",
    "75 25",
    "100 35",
    "END IONS"
  )
  writeLines(mgf, mgf_file)

  output <- create_edges_spectra(input = mgf_file)
  expect_true(file.exists(output))
})

test_that("create_edges_spectra validates ppm parameter", {
  mgf_file <- tempfile(fileext = ".mgf")
  mgf <- c("BEGIN IONS", "PEPMASS=100", "CHARGE=1+", "100 10", "END IONS")
  writeLines(mgf, mgf_file)

  expect_error(
    create_edges_spectra(input = mgf_file, ppm = -10),
    "ppm"
  )
})

test_that("create_edges_spectra validates dalton parameter", {
  mgf_file <- tempfile(fileext = ".mgf")
  mgf <- c("BEGIN IONS", "PEPMASS=100", "CHARGE=1+", "100 10", "END IONS")
  writeLines(mgf, mgf_file)

  expect_error(
    create_edges_spectra(input = mgf_file, dalton = -0.01),
    "dalton"
  )
})

test_that("create_edges_spectra validates method parameter", {
  mgf_file <- tempfile(fileext = ".mgf")
  writeLines(
    c("BEGIN IONS", "PEPMASS=100", "CHARGE=1+", "100 10", "END IONS"),
    mgf_file
  )

  expect_error(
    create_edges_spectra(input = mgf_file, method = "unknown_method"),
    "method"
  )
})

test_that("create_edges_spectra validates cutoff parameter", {
  mgf_file <- tempfile(fileext = ".mgf")
  writeLines(
    c("BEGIN IONS", "PEPMASS=100", "CHARGE=1+", "100 10", "END IONS"),
    mgf_file
  )

  expect_error(
    create_edges_spectra(input = mgf_file, cutoff = -0.5),
    "cutoff"
  )
})

test_that("create_edges_spectra validates output parameter", {
  mgf_file <- tempfile(fileext = ".mgf")
  writeLines(
    c("BEGIN IONS", "PEPMASS=100", "CHARGE=1+", "100 10", "END IONS"),
    mgf_file
  )

  expect_error(
    create_edges_spectra(input = mgf_file, output = 123),
    "output must be a single character string",
    class = "tima_validation_error"
  )
})

test_that("create_edges_spectra validates list input file entries", {
  expect_error(
    create_edges_spectra(input = list(1, 2)),
    "all input elements must be character strings",
    class = "tima_validation_error"
  )

  expect_error(
    create_edges_spectra(input = list("missing_a.mgf", "missing_b.mgf")),
    "input file\\(s\\) not found",
    class = "tima_validation_error"
  )
})

test_that("create_edges_spectra checks input file exists", {
  expect_error(
    create_edges_spectra(input = "nonexistent.mgf"),
    "not found|does not exist"
  )
})

test_that("create_edges_spectra handles single spectrum", {
  withr::local_dir(new = temp_test_dir("create_edges_spectra_single"))
  mgf_file <- tempfile(fileext = ".mgf")
  mgf <- c(
    "BEGIN IONS",
    "TITLE=Spectrum1",
    "PEPMASS=100.5",
    "CHARGE=1+",
    "100 10",
    "101 20",
    "END IONS"
  )
  writeLines(mgf, mgf_file)

  output <- create_edges_spectra(
    input = mgf_file,
    ppm = 10,
    dalton = 0.01,
    min_fragments = 1L
  )

  expect_true(file.exists(output))

  df <- tidytable::fread(output)
  # Single spectrum should produce empty edges or single NA row
  expect_true(nrow(df) <= 1)
})

test_that("create_edges_spectra creates output file", {
  withr::local_dir(new = temp_test_dir("create_edges_spectra_output"))
  mgf_file <- tempfile(fileext = ".mgf")
  mgf <- c(
    "BEGIN IONS",
    "TITLE=Spectrum1",
    "FEATURE_ID=1",
    "PEPMASS=100",
    "CHARGE=1+",
    "50 10",
    "75 20",
    "100 30",
    "END IONS",
    "",
    "BEGIN IONS",
    "TITLE=Spectrum2",
    "FEATURE_ID=2",
    "PEPMASS=200",
    "CHARGE=1+",
    "50 15",
    "75 25",
    "200 35",
    "END IONS"
  )
  writeLines(mgf, mgf_file)

  output <- create_edges_spectra(
    input = mgf_file
  )

  expect_true(file.exists(output))
  expect_match(output, "\\.tsv$")

  df <- tidytable::fread(output)
  expect_false("componentindex" %in% colnames(df))
  expect_false("score" %in% colnames(df))
  expect_false("matched_peaks" %in% colnames(df))
  expect_true("candidate_score_similarity" %in% colnames(df))
  expect_true("candidate_count_similarity_peaks_matched" %in% colnames(df))
})

test_that("create_edges_spectra ignores thresholds and uses the full graph", {
  withr::local_dir(new = temp_test_dir("create_edges_spectra_no_threshold"))
  mgf_file <- tempfile(fileext = ".mgf")
  writeLines(
    c(
      "BEGIN IONS",
      "TITLE=Spectrum1",
      "FEATURE_ID=1",
      "PEPMASS=100",
      "CHARGE=1+",
      "50 10",
      "75 20",
      "100 30",
      "END IONS",
      "",
      "BEGIN IONS",
      "TITLE=Spectrum2",
      "FEATURE_ID=2",
      "PEPMASS=101",
      "CHARGE=1+",
      "50 15",
      "75 25",
      "100 35",
      "END IONS"
    ),
    mgf_file
  )

  out <- create_edges_spectra(
    input = mgf_file
  )

  exported <- tidytable::fread(out)
  expect_true(nrow(exported) >= 1)
})

test_that("create_edges_spectra attributes edges by Louvain community", {
  edges <- tidytable::tidytable(
    feature_source = c("A", "B", "C", "D", "E", "F", "A"),
    feature_target = c("B", "C", "A", "E", "F", "D", "D"),
    weight = c(0.95, 0.95, 0.95, 0.95, 0.95, 0.95, 0.05)
  )

  result <- build_components_from_edges(
    edges = edges,
    name_source = "feature_source",
    name_target = "feature_target",
    resolution = 0.1,
    n_iterations = 50L,
    seed = 42,
    label_column = "componentindex",
    cut_to_communities = TRUE
  )

  expect_equal(length(unique(result$components_table$componentindex)), 2)
  expect_equal(nrow(result$edges), 6)
  expect_false(any(
    result$edges$feature_source == "A" & result$edges$feature_target == "D"
  ))
  expect_false(any(
    result$edges$feature_source == "D" & result$edges$feature_target == "A"
  ))
  expect_false("componentindex" %in% names(result$edges))
})

test_that("create_edges_spectra keeps feature IDs from FEATURE_ID (mzTab proxy MGF style)", {
  withr::local_dir(new = temp_test_dir("create_edges_spectra_feature_ids"))
  mgf_file <- tempfile(fileext = ".mgf")
  writeLines(
    c(
      "BEGIN IONS",
      "TITLE=1",
      "FEATURE_ID=1",
      "PEPMASS=100",
      "CHARGE=1+",
      "50 10",
      "75 20",
      "END IONS",
      "",
      "BEGIN IONS",
      "TITLE=2",
      "FEATURE_ID=2",
      "PEPMASS=101",
      "CHARGE=1+",
      "50 12",
      "75 18",
      "END IONS"
    ),
    mgf_file
  )

  output <- create_edges_spectra(
    input = mgf_file
  )

  df <- tidytable::fread(output)
  expect_true(all(c("CLUSTERID1", "CLUSTERID2") %in% colnames(df)))
  expect_true(any(df$CLUSTERID1 == "1" | df$CLUSTERID2 == "2"))
})

test_that("create_edges_spectra() creates edges from two spectra with entropy", {
  skip_if_not_installed("Spectra")
  withr::local_dir(new = temp_test_dir("create_edges_spectra_entropy"))
  mgf_file <- tempfile(fileext = ".mgf")
  writeLines(
    c(
      "BEGIN IONS",
      "TITLE=Spectrum1",
      "FEATURE_ID=1",
      "PEPMASS=100",
      "CHARGE=1+",
      "50 10",
      "75 20",
      "100 30",
      "END IONS",
      "",
      "BEGIN IONS",
      "TITLE=Spectrum2",
      "FEATURE_ID=2",
      "PEPMASS=200",
      "CHARGE=1+",
      "50 15",
      "75 25",
      "200 35",
      "END IONS"
    ),
    mgf_file
  )
  out <- create_edges_spectra(
    input = mgf_file,
    method = "entropy"
  )
  expect_true(file.exists(out))
  df <- tidytable::fread(out)
  expect_true(
    nrow(df) <= 1 || all(c("CLUSTERID1", "CLUSTERID2") %in% names(df))
  )
})

test_that("create_edges_spectra() runs with cosine method", {
  skip_if_not_installed("Spectra")
  withr::local_dir(new = temp_test_dir("create_edges_spectra_cosine"))
  mgf_file <- tempfile(fileext = ".mgf")
  writeLines(
    c(
      "BEGIN IONS",
      "TITLE=Spectrum1",
      "FEATURE_ID=1",
      "PEPMASS=100",
      "CHARGE=1+",
      "50 10",
      "75 20",
      "100 30",
      "END IONS",
      "",
      "BEGIN IONS",
      "TITLE=Spectrum2",
      "FEATURE_ID=2",
      "PEPMASS=200",
      "CHARGE=1+",
      "50 15",
      "75 25",
      "200 35",
      "END IONS"
    ),
    mgf_file
  )
  out <- create_edges_spectra(
    input = mgf_file,
    method = "cosine"
  )
  expect_true(file.exists(out))
})

test_that("create_edges_spectra handles empty MGF file", {
  withr::local_dir(new = temp_test_dir("create_edges_empty"))
  mgf_file <- tempfile(fileext = ".mgf")
  writeLines("", mgf_file)

  # Should handle gracefully
  expect_error(
    create_edges_spectra(input = mgf_file),
    "No usable spectra found in input"
  )
})

test_that("create_edges_spectra handles invalid MGF format", {
  withr::local_dir(new = temp_test_dir("create_edges_invalid"))
  mgf_file <- tempfile(fileext = ".mgf")
  writeLines("INVALID MGF CONTENT", mgf_file)

  expect_error(
    create_edges_spectra(input = mgf_file),
    "No usable spectra found in input"
  )
})

test_that("create_edges_spectra completes downstream edge-building from parsed spectra", {
  withr::local_dir(new = temp_test_dir("create_edges_spectra_downstream"))

  input_file <- tempfile(fileext = ".mgf")
  writeLines(
    c(
      "BEGIN IONS",
      "TITLE=Spectrum1",
      "FEATURE_ID=F1",
      "PEPMASS=100",
      "CHARGE=1+",
      "50 10",
      "75 20",
      "100 30",
      "END IONS",
      "",
      "BEGIN IONS",
      "TITLE=Spectrum2",
      "FEATURE_ID=F2",
      "PEPMASS=101",
      "CHARGE=1+",
      "50 15",
      "75 25",
      "100 35",
      "END IONS"
    ),
    input_file
  )

  output_file <- tempfile(fileext = ".tsv")

  out <- create_edges_spectra(
    input = input_file,
    output = output_file,
    name_source = "CLUSTERID1",
    name_target = "CLUSTERID2",
    method = "gnps",
    ppm = 10,
    dalton = 0.01,
    cutoff = 0
  )

  expect_identical(out, output_file)

  exported <- tidytable::fread(out)
  expect_true(all(c("CLUSTERID1", "CLUSTERID2") %in% names(exported)))
  expect_true(all(
    c("feature_spectrum_entropy", "feature_spectrum_peaks") %in%
      names(exported)
  ))
  expect_true(any(exported$CLUSTERID1 == "F1" | exported$CLUSTERID2 == "F2"))
})
