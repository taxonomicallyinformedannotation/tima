library(testthat)

validate_weight_annotations_inputs <- validate_weight_annotations_inputs
load_annotation_tables <- load_annotation_tables
load_edges_table <- load_edges_table
load_structure_organism_pairs <- load_structure_organism_pairs
log_annotation_stats <- log_annotation_stats

# Helper: write a minimal TSV file and return its path
make_tsv <- function(df, ...) {
  f <- tempfile(...)
  tidytable::fwrite(x = df, file = f, sep = "\t")
  f
}

# ── validate_weight_annotations_inputs ────────────────────────────────────────

test_that("validate_weight_annotations_inputs passes with valid inputs", {
  lib <- make_tsv(data.frame(a = 1))
  com <- make_tsv(data.frame(a = 1))
  edg <- make_tsv(data.frame(a = 1))
  tax <- make_tsv(data.frame(a = 1))
  ann <- make_tsv(data.frame(a = 1))
  on.exit(unlink(c(lib, com, edg, tax, ann)))

  expect_invisible(validate_weight_annotations_inputs(
    library = lib,
    components = com,
    edges = edg,
    taxa = tax,
    annotations = ann,
    minimal_ms1_condition = "OR",
    weight_spectral = 1 / 3,
    weight_chemical = 1 / 3,
    weight_biological = 1 / 3,
    minimal_consistency = 0.2,
    minimal_ms1_bio = 0.5,
    minimal_ms1_chemo = 0.5,
    ms1_only = FALSE,
    compounds_names = TRUE,
    high_confidence = FALSE,
    remove_ties = FALSE,
    summarize = TRUE,
    force = FALSE,
    candidates_neighbors = 5L,
    candidates_final = 1L
  ))
})

test_that("validate_weight_annotations_inputs errors on missing required file", {
  on.exit(unlink(c(lib, com, edg, tax, ann)))
  lib <- make_tsv(data.frame(a = 1))
  com <- make_tsv(data.frame(a = 1))
  edg <- make_tsv(data.frame(a = 1))
  tax <- make_tsv(data.frame(a = 1))
  ann <- make_tsv(data.frame(a = 1))

  expect_error(
    validate_weight_annotations_inputs(
      library = "/nonexistent/library.tsv",
      components = com,
      edges = edg,
      taxa = tax,
      annotations = ann,
      minimal_ms1_condition = "OR",
      weight_spectral = 1 / 3,
      weight_chemical = 1 / 3,
      weight_biological = 1 / 3,
      minimal_consistency = 0.2,
      minimal_ms1_bio = 0.5,
      minimal_ms1_chemo = 0.5,
      ms1_only = FALSE,
      compounds_names = TRUE,
      high_confidence = FALSE,
      remove_ties = FALSE,
      summarize = TRUE,
      force = FALSE,
      candidates_neighbors = 5L,
      candidates_final = 1L
    )
  )
})

test_that("validate_weight_annotations_inputs errors on bad ms1_condition", {
  lib <- make_tsv(data.frame(a = 1))
  com <- make_tsv(data.frame(a = 1))
  edg <- make_tsv(data.frame(a = 1))
  tax <- make_tsv(data.frame(a = 1))
  ann <- make_tsv(data.frame(a = 1))
  on.exit(unlink(c(lib, com, edg, tax, ann)))

  expect_error(validate_weight_annotations_inputs(
    library = lib,
    components = com,
    edges = edg,
    taxa = tax,
    annotations = ann,
    minimal_ms1_condition = "BAD",
    weight_spectral = 1 / 3,
    weight_chemical = 1 / 3,
    weight_biological = 1 / 3,
    minimal_consistency = 0.2,
    minimal_ms1_bio = 0.5,
    minimal_ms1_chemo = 0.5,
    ms1_only = FALSE,
    compounds_names = TRUE,
    high_confidence = FALSE,
    remove_ties = FALSE,
    summarize = TRUE,
    force = FALSE,
    candidates_neighbors = 5L,
    candidates_final = 1L
  ))
})

test_that("validate_weight_annotations_inputs errors when weights do not sum to 1", {
  lib <- make_tsv(data.frame(a = 1))
  com <- make_tsv(data.frame(a = 1))
  edg <- make_tsv(data.frame(a = 1))
  tax <- make_tsv(data.frame(a = 1))
  ann <- make_tsv(data.frame(a = 1))
  on.exit(unlink(c(lib, com, edg, tax, ann)))

  expect_error(validate_weight_annotations_inputs(
    library = lib,
    components = com,
    edges = edg,
    taxa = tax,
    annotations = ann,
    minimal_ms1_condition = "OR",
    weight_spectral = 0.5,
    weight_chemical = 0.5,
    weight_biological = 0.5, # sum = 1.5
    minimal_consistency = 0.2,
    minimal_ms1_bio = 0.5,
    minimal_ms1_chemo = 0.5,
    ms1_only = FALSE,
    compounds_names = TRUE,
    high_confidence = FALSE,
    remove_ties = FALSE,
    summarize = TRUE,
    force = FALSE,
    candidates_neighbors = 5L,
    candidates_final = 1L
  ))
})

test_that("validate_weight_annotations_inputs errors on negative weight", {
  lib <- make_tsv(data.frame(a = 1))
  com <- make_tsv(data.frame(a = 1))
  edg <- make_tsv(data.frame(a = 1))
  tax <- make_tsv(data.frame(a = 1))
  ann <- make_tsv(data.frame(a = 1))
  on.exit(unlink(c(lib, com, edg, tax, ann)))

  expect_error(validate_weight_annotations_inputs(
    library = lib,
    components = com,
    edges = edg,
    taxa = tax,
    annotations = ann,
    minimal_ms1_condition = "OR",
    weight_spectral = -0.1,
    weight_chemical = 0.6,
    weight_biological = 0.5,
    minimal_consistency = 0.2,
    minimal_ms1_bio = 0.5,
    minimal_ms1_chemo = 0.5,
    ms1_only = FALSE,
    compounds_names = TRUE,
    high_confidence = FALSE,
    remove_ties = FALSE,
    summarize = TRUE,
    force = FALSE,
    candidates_neighbors = 5L,
    candidates_final = 1L
  ))
})

# ── load_annotation_tables ────────────────────────────────────────────────────

test_that("load_annotation_tables loads and binds multiple files", {
  df1 <- data.frame(feature_id = "F1", candidate_score_similarity = NA_real_)
  df2 <- data.frame(feature_id = "F2", candidate_score_similarity = NA_real_)
  f1 <- make_tsv(df1)
  f2 <- make_tsv(df2)
  on.exit(unlink(c(f1, f2)))

  result <- load_annotation_tables(c(f1, f2), ms1_only = FALSE)
  expect_equal(nrow(result), 2L)
  expect_true("feature_id" %in% names(result))
})

test_that("load_annotation_tables filters MS1-only when requested", {
  df <- data.frame(
    feature_id = c("F1", "F2", "F3"),
    candidate_score_similarity = c(NA_real_, 0.9, NA_real_),
    candidate_score_sirius_csi = c(NA_real_, NA_real_, 0.8)
  )
  f <- make_tsv(df)
  on.exit(unlink(f))

  result_all <- load_annotation_tables(f, ms1_only = FALSE)
  result_ms1 <- load_annotation_tables(f, ms1_only = TRUE)

  expect_equal(nrow(result_all), 3L)
  # ms1_only keeps only rows where both similarity AND sirius_csi are NA
  expect_equal(nrow(result_ms1), 1L)
  expect_equal(as.character(result_ms1$feature_id[[1]]), "F1")
})

# ── load_edges_table ──────────────────────────────────────────────────────────

test_that("load_edges_table loads and filters to top N neighbors", {
  df <- data.frame(
    feature_source = c("F1", "F1", "F1", "F2"),
    feature_target = c("F2", "F3", "F4", "F1"),
    candidate_score_similarity = c(0.9, 0.7, 0.5, 0.8)
  )
  f <- make_tsv(df)
  on.exit(unlink(f))

  result <- load_edges_table(f, candidates_neighbors = 2L)

  # F1 should only have its top 2 neighbors (0.9 and 0.7)
  f1_rows <- result[result$feature_source == "F1", ]
  expect_equal(nrow(f1_rows), 2L)
  expect_true(all(as.numeric(f1_rows$candidate_score_similarity) >= 0.7))
})

# ── load_structure_organism_pairs ─────────────────────────────────────────────

test_that("load_structure_organism_pairs loads library and joins optional files", {
  lib_df <- data.frame(
    candidate_structure_inchikey_connectivity_layer = c(
      "AAAAAAAAAAAAA",
      "BBBBBBBBBBBBB"
    ),
    candidate_structure_exact_mass = c(180.1, 200.2)
  )
  lib <- make_tsv(lib_df)
  on.exit(unlink(lib))

  result <- load_structure_organism_pairs(
    lib,
    str_stereo = NULL,
    org_tax_ott = NULL
  )
  expect_equal(nrow(result), 2L)
  expect_true(
    "candidate_structure_inchikey_connectivity_layer" %in% names(result)
  )
})

# ── log_annotation_stats ──────────────────────────────────────────────────────

test_that("log_annotation_stats returns invisibly NULL without error", {
  df <- tidytable::tidytable(
    feature_id = c("F1", "F1", "F2"),
    candidate_library = c("spectral", "spectral", "GNPS"),
    candidate_structure_inchikey_connectivity_layer = c(
      "AAAAAAAAAAAAA",
      "BBBBBBBBBBBBB",
      "CCCCCCCCCCCCC"
    )
  )
  expect_invisible(log_annotation_stats(df))
})
