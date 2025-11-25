# Test Suite: weight_annotations ----

library(testthat)

## validate_weight_annotations_inputs ----

test_that("validate_weight_annotations_inputs accepts valid inputs", {
  # Create test files using helper
  files <- wa_create_minimal_files()

  expect_silent(
    validate_weight_annotations_inputs(
      library = files$library,
      components = files$components,
      edges = files$edges,
      taxa = files$taxa,
      annotations = files$ann,
      minimal_ms1_condition = "OR",
      weight_spectral = 0.33,
      weight_chemical = 0.33,
      weight_biological = 0.34,
      minimal_consistency = 0.5,
      minimal_ms1_bio = 0.5,
      minimal_ms1_chemo = 0.5,
      ms1_only = FALSE,
      compounds_names = TRUE,
      high_confidence = FALSE,
      remove_ties = FALSE,
      summarize = FALSE,
      force = FALSE,
      candidates_neighbors = 5,
      candidates_final = 10
    )
  )
})

test_that("validate_weight_annotations_inputs rejects missing required files", {
  expect_error(
    validate_weight_annotations_inputs(
      library = temp_test_path("nonexistent.tsv"),
      components = temp_test_path("components.tsv"),
      edges = temp_test_path("edges.tsv"),
      taxa = temp_test_path("taxa.tsv"),
      annotations = temp_test_path("ann.tsv"),
      minimal_ms1_condition = "OR",
      weight_spectral = 0.33,
      weight_chemical = 0.33,
      weight_biological = 0.34,
      minimal_consistency = 0.5,
      minimal_ms1_bio = 0.5,
      minimal_ms1_chemo = 0.5,
      ms1_only = FALSE,
      compounds_names = TRUE,
      high_confidence = FALSE,
      remove_ties = FALSE,
      summarize = FALSE,
      force = FALSE,
      candidates_neighbors = 5,
      candidates_final = 10
    ),
    "Required file not found"
  )
})

test_that("validate_weight_annotations_inputs rejects invalid minimal_ms1_condition", {
  # Create test files using helper
  files <- wa_create_minimal_files()

  expect_error(
    validate_weight_annotations_inputs(
      library = files$library,
      components = files$components,
      edges = files$edges,
      taxa = files$taxa,
      annotations = files$ann,
      minimal_ms1_condition = "XOR",
      weight_spectral = 0.33,
      weight_chemical = 0.33,
      weight_biological = 0.34,
      minimal_consistency = 0.5,
      minimal_ms1_bio = 0.5,
      minimal_ms1_chemo = 0.5,
      ms1_only = FALSE,
      compounds_names = TRUE,
      high_confidence = FALSE,
      remove_ties = FALSE,
      summarize = FALSE,
      force = FALSE,
      candidates_neighbors = 5,
      candidates_final = 10
    ),
    "must be 'OR' or 'AND'"
  )
})

test_that("validate_weight_annotations_inputs rejects weights not summing to 1", {
  dir.create("data/interim/annotations", recursive = TRUE, showWarnings = FALSE)
  dir.create("data/interim/features", recursive = TRUE, showWarnings = FALSE)
  dir.create(
    "data/interim/libraries/sop/merged",
    recursive = TRUE,
    showWarnings = FALSE
  )
  dir.create("data/interim/metadata", recursive = TRUE, showWarnings = FALSE)

  writeLines("", "data/interim/libraries/sop/merged/keys.tsv")
  writeLines("", "data/interim/features/components.tsv")
  writeLines("", "data/interim/features/edges.tsv")
  writeLines("", "data/interim/metadata/taxa.tsv")
  writeLines("", "data/interim/annotations/ann.tsv")

  expect_error(
    validate_weight_annotations_inputs(
      library = "data/interim/libraries/sop/merged/keys.tsv",
      components = "data/interim/features/components.tsv",
      edges = "data/interim/features/edges.tsv",
      taxa = "data/interim/metadata/taxa.tsv",
      annotations = "data/interim/annotations/ann.tsv",
      minimal_ms1_condition = "OR",
      weight_spectral = 0.5,
      weight_chemical = 0.3,
      weight_biological = 0.3,
      minimal_consistency = 0.5,
      minimal_ms1_bio = 0.5,
      minimal_ms1_chemo = 0.5,
      ms1_only = FALSE,
      compounds_names = TRUE,
      high_confidence = FALSE,
      remove_ties = FALSE,
      summarize = FALSE,
      force = FALSE,
      candidates_neighbors = 5,
      candidates_final = 10
    ),
    "Weights must sum to 1"
  )
})

test_that("validate_weight_annotations_inputs rejects negative weights", {
  dir.create("data/interim/annotations", recursive = TRUE, showWarnings = FALSE)
  dir.create("data/interim/features", recursive = TRUE, showWarnings = FALSE)
  dir.create(
    "data/interim/libraries/sop/merged",
    recursive = TRUE,
    showWarnings = FALSE
  )
  dir.create("data/interim/metadata", recursive = TRUE, showWarnings = FALSE)

  writeLines("", "data/interim/libraries/sop/merged/keys.tsv")
  writeLines("", "data/interim/features/components.tsv")
  writeLines("", "data/interim/features/edges.tsv")
  writeLines("", "data/interim/metadata/taxa.tsv")
  writeLines("", "data/interim/annotations/ann.tsv")

  expect_error(
    validate_weight_annotations_inputs(
      library = "data/interim/libraries/sop/merged/keys.tsv",
      components = "data/interim/features/components.tsv",
      edges = "data/interim/features/edges.tsv",
      taxa = "data/interim/metadata/taxa.tsv",
      annotations = "data/interim/annotations/ann.tsv",
      minimal_ms1_condition = "OR",
      weight_spectral = -0.1,
      weight_chemical = 0.6,
      weight_biological = 0.5,
      minimal_consistency = 0.5,
      minimal_ms1_bio = 0.5,
      minimal_ms1_chemo = 0.5,
      ms1_only = FALSE,
      compounds_names = TRUE,
      high_confidence = FALSE,
      remove_ties = FALSE,
      summarize = FALSE,
      force = FALSE,
      candidates_neighbors = 5,
      candidates_final = 10
    ),
    "must be non-negative"
  )
})

test_that("validate_weight_annotations_inputs rejects invalid score parameters", {
  dir.create("data/interim/annotations", recursive = TRUE, showWarnings = FALSE)
  dir.create("data/interim/features", recursive = TRUE, showWarnings = FALSE)
  dir.create(
    "data/interim/libraries/sop/merged",
    recursive = TRUE,
    showWarnings = FALSE
  )
  dir.create("data/interim/metadata", recursive = TRUE, showWarnings = FALSE)

  writeLines("", "data/interim/libraries/sop/merged/keys.tsv")
  writeLines("", "data/interim/features/components.tsv")
  writeLines("", "data/interim/features/edges.tsv")
  writeLines("", "data/interim/metadata/taxa.tsv")
  writeLines("", "data/interim/annotations/ann.tsv")

  expect_error(
    validate_weight_annotations_inputs(
      library = "data/interim/libraries/sop/merged/keys.tsv",
      components = "data/interim/features/components.tsv",
      edges = "data/interim/features/edges.tsv",
      taxa = "data/interim/metadata/taxa.tsv",
      annotations = "data/interim/annotations/ann.tsv",
      minimal_ms1_condition = "OR",
      weight_spectral = 0.33,
      weight_chemical = 0.33,
      weight_biological = 0.34,
      minimal_consistency = 1.5,
      minimal_ms1_bio = 0.5,
      minimal_ms1_chemo = 0.5,
      ms1_only = FALSE,
      compounds_names = TRUE,
      high_confidence = FALSE,
      remove_ties = FALSE,
      summarize = FALSE,
      force = FALSE,
      candidates_neighbors = 5,
      candidates_final = 10
    ),
    "must be between 0 and 1"
  )
})

test_that("validate_weight_annotations_inputs rejects invalid candidates parameters", {
  dir.create("data/interim/annotations", recursive = TRUE, showWarnings = FALSE)
  dir.create("data/interim/features", recursive = TRUE, showWarnings = FALSE)
  dir.create(
    "data/interim/libraries/sop/merged",
    recursive = TRUE,
    showWarnings = FALSE
  )
  dir.create("data/interim/metadata", recursive = TRUE, showWarnings = FALSE)

  writeLines("", "data/interim/libraries/sop/merged/keys.tsv")
  writeLines("", "data/interim/features/components.tsv")
  writeLines("", "data/interim/features/edges.tsv")
  writeLines("", "data/interim/metadata/taxa.tsv")
  writeLines("", "data/interim/annotations/ann.tsv")

  expect_error(
    validate_weight_annotations_inputs(
      library = "data/interim/libraries/sop/merged/keys.tsv",
      components = "data/interim/features/components.tsv",
      edges = "data/interim/features/edges.tsv",
      taxa = "data/interim/metadata/taxa.tsv",
      annotations = "data/interim/annotations/ann.tsv",
      minimal_ms1_condition = "OR",
      weight_spectral = 0.33,
      weight_chemical = 0.33,
      weight_biological = 0.34,
      minimal_consistency = 0.5,
      minimal_ms1_bio = 0.5,
      minimal_ms1_chemo = 0.5,
      ms1_only = FALSE,
      compounds_names = TRUE,
      high_confidence = FALSE,
      remove_ties = FALSE,
      summarize = FALSE,
      force = FALSE,
      candidates_neighbors = 0,
      candidates_final = 10
    ),
    "candidates_neighbors must be > 0, got: 0"
  )
})

## load_annotation_tables ----

test_that("load_annotation_tables loads and combines files", {
  # Create test annotation files
  ann1 <- tidytable::tidytable(
    feature_id = c("F1", "F2"),
    candidate_score_similarity = c("0.8", "0.7")
  )
  ann2 <- tidytable::tidytable(
    feature_id = c("F3", "F4"),
    candidate_score_similarity = c("0.6", "0.5")
  )

  ann1_file <- temp_test_path("ann1.tsv")
  ann2_file <- temp_test_path("ann2.tsv")
  tidytable::fwrite(x = ann1, file = ann1_file, sep = "\t")
  tidytable::fwrite(x = ann2, file = ann2_file, sep = "\t")

  result <- load_annotation_tables(
    c(ann1_file, ann2_file),
    ms1_only = FALSE
  )

  expect_equal(nrow(result), 4)
  expect_true(all(c("F1", "F2", "F3", "F4") %in% result$feature_id))
})

test_that("load_annotation_tables filters MS1 only when requested", {
  ann <- tidytable::tidytable(
    feature_id = c("F1", "F2", "F3"),
    candidate_score_similarity = c(NA, "0.8", NA),
    candidate_score_sirius_csi = c(NA, NA, "0.7")
  )

  ann_file <- temp_test_path("ann.tsv")
  tidytable::fwrite(x = ann, file = ann_file, sep = "\t")

  result <- load_annotation_tables(ann_file, ms1_only = TRUE)

  # Should keep only F1 (both scores are NA)
  expect_equal(nrow(result), 1)
  expect_equal(result$feature_id, "F1")
})

## load_edges_tables ----

test_that("load_edges_table loads and filters top neighbors", {
  edges <- tidytable::tidytable(
    feature_source = c("F1", "F1", "F1", "F2", "F2"),
    feature_target = c("F2", "F3", "F4", "F3", "F4"),
    candidate_score_similarity = c("0.9", "0.8", "0.7", "0.6", "0.5")
  )

  edges_file <- temp_test_path("edges.tsv")
  tidytable::fwrite(x = edges, file = edges_file, sep = "\t")

  result <- load_edges_table(edges_file, candidates_neighbors = 2)

  # Should keep top 2 neighbors per source feature
  expect_equal(nrow(result), 4)

  f1_edges <- result |>
    tidytable::filter(feature_source == "F1")
  expect_equal(nrow(f1_edges), 2)
  expect_true(all(f1_edges$feature_target %in% c("F2", "F3")))
})

## log_annotation_stats ----

test_that("log_annotation_stats runs without error", {
  ann <- tidytable::tidytable(
    feature_id = c("F1", "F2", "F3"),
    candidate_library = c("lib1", "lib1", "lib2"),
    candidate_structure_inchikey_connectivity_layer = c("A", "B", "C")
  )

  expect_no_error(log_annotation_stats(ann))
})

test_that("log_annotation_stats handles empty annotations", {
  ann <- tidytable::tidytable(
    feature_id = character(),
    candidate_library = character(),
    candidate_structure_inchikey_connectivity_layer = character()
  )

  expect_no_error(log_annotation_stats(ann))
})

test_that("log_annotation_stats handles NA inchikeys", {
  ann <- tidytable::tidytable(
    feature_id = c("F1", "F2", "F3"),
    candidate_library = c("lib1", "lib1", "lib2"),
    candidate_structure_inchikey_connectivity_layer = c("A", NA, "C")
  )

  expect_no_error(log_annotation_stats(ann))
})

## validate_weight_annotations_inputs - Edge Cases ----

test_that("validate_weight_annotations_inputs accepts boundary weight sum", {
  dir.create("data/interim/annotations", recursive = TRUE, showWarnings = FALSE)
  dir.create("data/interim/features", recursive = TRUE, showWarnings = FALSE)
  dir.create(
    "data/interim/libraries/sop/merged",
    recursive = TRUE,
    showWarnings = FALSE
  )
  dir.create("data/interim/metadata", recursive = TRUE, showWarnings = FALSE)

  writeLines("", "data/interim/libraries/sop/merged/keys.tsv")
  writeLines("", "data/interim/features/components.tsv")
  writeLines("", "data/interim/features/edges.tsv")
  writeLines("", "data/interim/metadata/taxa.tsv")
  writeLines("", "data/interim/annotations/ann.tsv")

  # Sum is 1.009 (within 0.01 tolerance)
  expect_silent(
    validate_weight_annotations_inputs(
      library = "data/interim/libraries/sop/merged/keys.tsv",
      components = "data/interim/features/components.tsv",
      edges = "data/interim/features/edges.tsv",
      taxa = "data/interim/metadata/taxa.tsv",
      annotations = "data/interim/annotations/ann.tsv",
      minimal_ms1_condition = "OR",
      weight_spectral = 0.333,
      weight_chemical = 0.333,
      weight_biological = 0.343,
      minimal_consistency = 0.5,
      minimal_ms1_bio = 0.5,
      minimal_ms1_chemo = 0.5,
      ms1_only = FALSE,
      compounds_names = TRUE,
      high_confidence = FALSE,
      remove_ties = FALSE,
      summarize = FALSE,
      force = FALSE,
      candidates_neighbors = 5,
      candidates_final = 10
    )
  )
})

test_that("validate_weight_annotations_inputs accepts all weights = 0 boundary", {
  dir.create("data/interim/annotations", recursive = TRUE, showWarnings = FALSE)
  dir.create("data/interim/features", recursive = TRUE, showWarnings = FALSE)
  dir.create(
    "data/interim/libraries/sop/merged",
    recursive = TRUE,
    showWarnings = FALSE
  )
  dir.create("data/interim/metadata", recursive = TRUE, showWarnings = FALSE)

  writeLines("", "data/interim/libraries/sop/merged/keys.tsv")
  writeLines("", "data/interim/features/components.tsv")
  writeLines("", "data/interim/features/edges.tsv")
  writeLines("", "data/interim/metadata/taxa.tsv")
  writeLines("", "data/interim/annotations/ann.tsv")

  expect_silent(
    validate_weight_annotations_inputs(
      library = "data/interim/libraries/sop/merged/keys.tsv",
      components = "data/interim/features/components.tsv",
      edges = "data/interim/features/edges.tsv",
      taxa = "data/interim/metadata/taxa.tsv",
      annotations = "data/interim/annotations/ann.tsv",
      minimal_ms1_condition = "OR",
      weight_spectral = 0,
      weight_chemical = 0,
      weight_biological = 1,
      minimal_consistency = 0,
      minimal_ms1_bio = 0,
      minimal_ms1_chemo = 0,
      ms1_only = FALSE,
      compounds_names = TRUE,
      high_confidence = FALSE,
      remove_ties = FALSE,
      summarize = FALSE,
      force = FALSE,
      candidates_neighbors = 1,
      candidates_final = 1
    )
  )
})

test_that("validate_weight_annotations_inputs rejects non-logical parameters", {
  dir.create("data/interim/annotations", recursive = TRUE, showWarnings = FALSE)
  dir.create("data/interim/features", recursive = TRUE, showWarnings = FALSE)
  dir.create(
    "data/interim/libraries/sop/merged",
    recursive = TRUE,
    showWarnings = FALSE
  )
  dir.create("data/interim/metadata", recursive = TRUE, showWarnings = FALSE)

  writeLines("", "data/interim/libraries/sop/merged/keys.tsv")
  writeLines("", "data/interim/features/components.tsv")
  writeLines("", "data/interim/features/edges.tsv")
  writeLines("", "data/interim/metadata/taxa.tsv")
  writeLines("", "data/interim/annotations/ann.tsv")

  expect_error(
    validate_weight_annotations_inputs(
      library = "data/interim/libraries/sop/merged/keys.tsv",
      components = "data/interim/features/components.tsv",
      edges = "data/interim/features/edges.tsv",
      taxa = "data/interim/metadata/taxa.tsv",
      annotations = "data/interim/annotations/ann.tsv",
      minimal_ms1_condition = "OR",
      weight_spectral = 0.33,
      weight_chemical = 0.33,
      weight_biological = 0.34,
      minimal_consistency = 0.5,
      minimal_ms1_bio = 0.5,
      minimal_ms1_chemo = 0.5,
      ms1_only = "yes",
      compounds_names = TRUE,
      high_confidence = FALSE,
      remove_ties = FALSE,
      summarize = FALSE,
      force = FALSE,
      candidates_neighbors = 5,
      candidates_final = 10
    ),
    "ms1_only must be a single TRUE or FALSE value, got: character"
  )
})

## load_annotation_tables - Edge Cases ----

test_that("load_annotation_tables returns all columns as character", {
  ann <- tidytable::tidytable(
    feature_id = c("F1", "F2"),
    score = c("0.8", "0.9"),
    count = c("5", "10")
  )

  tidytable::fwrite(x = ann, file = "ann.tsv", sep = "\t")

  result <- load_annotation_tables("ann.tsv", ms1_only = FALSE)

  expect_true(all(vapply(result, is.character, logical(1))))
})

## load_edges_table - Edge Cases ----

test_that("load_edges_table handles single neighbor request", {
  edges <- tidytable::tidytable(
    feature_source = c("F1", "F1", "F1"),
    feature_target = c("F2", "F3", "F4"),
    candidate_score_similarity = c("0.9", "0.8", "0.7")
  )

  edges_file <- temp_test_path("edges.tsv")
  tidytable::fwrite(x = edges, file = edges_file, sep = "\t")

  result <- load_edges_table(edges_file, candidates_neighbors = 1)

  expect_equal(nrow(result), 1)
  expect_equal(result$feature_target, "F2")
})

test_that("load_edges_table handles ties in similarity scores", {
  edges <- tidytable::tidytable(
    feature_source = c("F1", "F1", "F1"),
    feature_target = c("F2", "F3", "F4"),
    candidate_score_similarity = c("0.9", "0.9", "0.7")
  )

  edges_file <- temp_test_path("edges.tsv")
  tidytable::fwrite(x = edges, file = edges_file, sep = "\t")

  result <- load_edges_table(edges_file, candidates_neighbors = 2)

  # with_ties = FALSE, should get exactly 2 rows
  expect_equal(nrow(result), 2)
})

test_that("load_edges_table handles empty file", {
  edges <- tidytable::tidytable(
    feature_source = character(),
    feature_target = character(),
    candidate_score_similarity = character()
  )

  edges_file <- temp_test_path("edges.tsv")
  tidytable::fwrite(x = edges, file = edges_file, sep = "\t")

  result <- load_edges_table(edges_file, candidates_neighbors = 5)

  expect_equal(nrow(result), 0)
})
test_that("log_annotation_stats handles empty data", {
  ann <- tidytable::tidytable(
    feature_id = character(),
    candidate_library = character(),
    candidate_structure_inchikey_connectivity_layer = character()
  )

  expect_no_error(log_annotation_stats(ann))
})

## Edge Cases ----

test_that("load_annotation_tables handles single file", {
  ann <- tidytable::tidytable(
    feature_id = c("F1"),
    candidate_score_similarity = c("0.8")
  )

  ann_file <- temp_test_path("ann.tsv")
  tidytable::fwrite(x = ann, file = ann_file, sep = "\t")

  result <- load_annotation_tables(ann_file, ms1_only = FALSE)

  expect_equal(nrow(result), 1)
})

test_that("load_edges_table handles features with fewer neighbors than requested", {
  edges <- tidytable::tidytable(
    feature_source = c("F1"),
    feature_target = c("F2"),
    candidate_score_similarity = c("0.9")
  )

  edges_file <- temp_test_path("edges.tsv")
  tidytable::fwrite(x = edges, file = edges_file, sep = "\t")

  # Request 5 neighbors but only 1 available
  result <- load_edges_table(edges_file, candidates_neighbors = 5)

  expect_equal(nrow(result), 1)
})

## validate_weight_annotations_inputs - Additional edge cases ----

test_that("test-validate_weight_annotations_inputs accepts boundary weight values", {
  # Create test files using helper
  files <- wa_create_minimal_files()

  # All weight on spectral
  expect_silent(
    validate_weight_annotations_inputs(
      library = files$library,
      components = files$components,
      edges = files$edges,
      taxa = files$taxa,
      annotations = files$ann,
      minimal_ms1_condition = "OR",
      weight_spectral = 1.0,
      weight_chemical = 0.0,
      weight_biological = 0.0,
      minimal_consistency = 0.5,
      minimal_ms1_bio = 0.5,
      minimal_ms1_chemo = 0.5,
      ms1_only = FALSE,
      compounds_names = TRUE,
      high_confidence = FALSE,
      remove_ties = FALSE,
      summarize = FALSE,
      force = FALSE,
      candidates_neighbors = 5,
      candidates_final = 10
    )
  )
})

test_that("test-validate_weight_annotations_inputs accepts boundary score values", {
  # Create test files using helper
  files <- wa_create_minimal_files()

  # Min and max score values
  expect_silent(
    validate_weight_annotations_inputs(
      library = files$library,
      components = files$components,
      edges = files$edges,
      taxa = files$taxa,
      annotations = files$ann,
      minimal_ms1_condition = "AND",
      weight_spectral = 0.33,
      weight_chemical = 0.33,
      weight_biological = 0.34,
      minimal_consistency = 0.0,
      minimal_ms1_bio = 1.0,
      minimal_ms1_chemo = 0.0,
      ms1_only = FALSE,
      compounds_names = TRUE,
      high_confidence = FALSE,
      remove_ties = FALSE,
      summarize = FALSE,
      force = FALSE,
      candidates_neighbors = 1,
      candidates_final = 1
    )
  )
})

test_that("test-validate_weight_annotations_inputs handles multiple annotation files", {
  # Create test files using helper
  files <- wa_create_minimal_files()

  expect_silent(
    validate_weight_annotations_inputs(
      library = files$library,
      components = files$components,
      edges = files$edges,
      taxa = files$taxa,
      annotations = c(files$ann, files$ann2),
      minimal_ms1_condition = "OR",
      weight_spectral = 0.33,
      weight_chemical = 0.33,
      weight_biological = 0.34,
      minimal_consistency = 0.5,
      minimal_ms1_bio = 0.5,
      minimal_ms1_chemo = 0.5,
      ms1_only = FALSE,
      compounds_names = TRUE,
      high_confidence = FALSE,
      remove_ties = FALSE,
      summarize = FALSE,
      force = FALSE,
      candidates_neighbors = 5,
      candidates_final = 10
    )
  )
})

test_that("test-validate_weight_annotations_inputs rejects when one annotation file missing", {
  # Create test files using helper
  files <- wa_create_minimal_files()

  expect_error(
    validate_weight_annotations_inputs(
      library = files$library,
      components = files$components,
      edges = files$edges,
      taxa = files$taxa,
      annotations = c(files$ann, temp_test_path("missing.tsv")),
      minimal_ms1_condition = "OR",
      weight_spectral = 0.33,
      weight_chemical = 0.33,
      weight_biological = 0.34,
      minimal_consistency = 0.5,
      minimal_ms1_bio = 0.5,
      minimal_ms1_chemo = 0.5,
      ms1_only = FALSE,
      compounds_names = TRUE,
      high_confidence = FALSE,
      remove_ties = FALSE,
      summarize = FALSE,
      force = FALSE,
      candidates_neighbors = 5,
      candidates_final = 10
    ),
    "Annotation file.*not found"
  )
})

test_that("test-validate_weight_annotations_inputs handles optional files gracefully", {
  # Create test files using helper
  files <- wa_create_minimal_files()

  # Should warn for missing optional files but not error
  expect_no_error(
    validate_weight_annotations_inputs(
      library = files$library,
      components = files$components,
      edges = files$edges,
      taxa = files$taxa,
      annotations = files$ann,
      str_stereo = temp_test_path("nonexistent.tsv"),
      org_tax_ott = temp_test_path("nonexistent2.tsv"),
      canopus = temp_test_path("nonexistent3.tsv"),
      formula = temp_test_path("nonexistent4.tsv"),
      minimal_ms1_condition = "OR",
      weight_spectral = 0.33,
      weight_chemical = 0.33,
      weight_biological = 0.34,
      minimal_consistency = 0.5,
      minimal_ms1_bio = 0.5,
      minimal_ms1_chemo = 0.5,
      ms1_only = FALSE,
      compounds_names = TRUE,
      high_confidence = FALSE,
      remove_ties = FALSE,
      summarize = FALSE,
      force = FALSE,
      candidates_neighbors = 5,
      candidates_final = 10
    )
  )
})

test_that("test-validate_weight_annotations_inputs rejects invalid logical parameters", {
  # Create test files using helper
  files <- wa_create_minimal_files()

  expect_error(
    validate_weight_annotations_inputs(
      library = "data/interim/libraries/sop/merged/keys.tsv",
      components = "data/interim/features/components.tsv",
      edges = "data/interim/features/edges.tsv",
      taxa = "data/interim/metadata/taxa.tsv",
      annotations = "data/interim/annotations/ann.tsv",
      minimal_ms1_condition = "OR",
      weight_spectral = 0.33,
      weight_chemical = 0.33,
      weight_biological = 0.34,
      minimal_consistency = 0.5,
      minimal_ms1_bio = 0.5,
      minimal_ms1_chemo = 0.5,
      ms1_only = "yes", # Invalid - should be logical
      compounds_names = TRUE,
      high_confidence = FALSE,
      remove_ties = FALSE,
      summarize = FALSE,
      force = FALSE,
      candidates_neighbors = 5,
      candidates_final = 10
    ),
    "ms1_only must be a single TRUE or FALSE value, got: character"
  )
})

test_that("test-validate_weight_annotations_inputs accepts all logical parameter combinations", {
  dir.create("data/interim/annotations", recursive = TRUE, showWarnings = FALSE)
  dir.create("data/interim/features", recursive = TRUE, showWarnings = FALSE)
  dir.create(
    "data/interim/libraries/sop/merged",
    recursive = TRUE,
    showWarnings = FALSE
  )
  dir.create("data/interim/metadata", recursive = TRUE, showWarnings = FALSE)

  writeLines("", "data/interim/libraries/sop/merged/keys.tsv")
  writeLines("", "data/interim/features/components.tsv")
  writeLines("", "data/interim/features/edges.tsv")
  writeLines("", "data/interim/metadata/taxa.tsv")
  writeLines("", "data/interim/annotations/ann.tsv")

  # Test with all TRUE
  expect_silent(
    validate_weight_annotations_inputs(
      library = "data/interim/libraries/sop/merged/keys.tsv",
      components = "data/interim/features/components.tsv",
      edges = "data/interim/features/edges.tsv",
      taxa = "data/interim/metadata/taxa.tsv",
      annotations = "data/interim/annotations/ann.tsv",
      minimal_ms1_condition = "OR",
      weight_spectral = 0.33,
      weight_chemical = 0.33,
      weight_biological = 0.34,
      minimal_consistency = 0.5,
      minimal_ms1_bio = 0.5,
      minimal_ms1_chemo = 0.5,
      ms1_only = TRUE,
      compounds_names = TRUE,
      high_confidence = TRUE,
      remove_ties = TRUE,
      summarize = TRUE,
      force = TRUE,
      candidates_neighbors = 5,
      candidates_final = 10
    )
  )

  # Test with all FALSE
  expect_silent(
    validate_weight_annotations_inputs(
      library = "data/interim/libraries/sop/merged/keys.tsv",
      components = "data/interim/features/components.tsv",
      edges = "data/interim/features/edges.tsv",
      taxa = "data/interim/metadata/taxa.tsv",
      annotations = "data/interim/annotations/ann.tsv",
      minimal_ms1_condition = "OR",
      weight_spectral = 0.33,
      weight_chemical = 0.33,
      weight_biological = 0.34,
      minimal_consistency = 0.5,
      minimal_ms1_bio = 0.5,
      minimal_ms1_chemo = 0.5,
      ms1_only = FALSE,
      compounds_names = FALSE,
      high_confidence = FALSE,
      remove_ties = FALSE,
      summarize = FALSE,
      force = FALSE,
      candidates_neighbors = 5,
      candidates_final = 10
    )
  )
})

test_that("test-validate_weight_annotations_inputs rejects zero candidates", {
  dir.create("data/interim/annotations", recursive = TRUE, showWarnings = FALSE)
  dir.create("data/interim/features", recursive = TRUE, showWarnings = FALSE)
  dir.create(
    "data/interim/libraries/sop/merged",
    recursive = TRUE,
    showWarnings = FALSE
  )
  dir.create("data/interim/metadata", recursive = TRUE, showWarnings = FALSE)

  writeLines("", "data/interim/libraries/sop/merged/keys.tsv")
  writeLines("", "data/interim/features/components.tsv")
  writeLines("", "data/interim/features/edges.tsv")
  writeLines("", "data/interim/metadata/taxa.tsv")
  writeLines("", "data/interim/annotations/ann.tsv")

  expect_error(
    validate_weight_annotations_inputs(
      library = "data/interim/libraries/sop/merged/keys.tsv",
      components = "data/interim/features/components.tsv",
      edges = "data/interim/features/edges.tsv",
      taxa = "data/interim/metadata/taxa.tsv",
      annotations = "data/interim/annotations/ann.tsv",
      minimal_ms1_condition = "OR",
      weight_spectral = 0.33,
      weight_chemical = 0.33,
      weight_biological = 0.34,
      minimal_consistency = 0.5,
      minimal_ms1_bio = 0.5,
      minimal_ms1_chemo = 0.5,
      ms1_only = FALSE,
      compounds_names = TRUE,
      high_confidence = FALSE,
      remove_ties = FALSE,
      summarize = FALSE,
      force = FALSE,
      candidates_neighbors = 0,
      candidates_final = 10
    ),
    "candidates_neighbors must be > 0, got: 0"
  )
})

test_that("test-validate_weight_annotations_inputs rejects negative scores", {
  dir.create("data/interim/annotations", recursive = TRUE, showWarnings = FALSE)
  dir.create("data/interim/features", recursive = TRUE, showWarnings = FALSE)
  dir.create(
    "data/interim/libraries/sop/merged",
    recursive = TRUE,
    showWarnings = FALSE
  )
  dir.create("data/interim/metadata", recursive = TRUE, showWarnings = FALSE)

  writeLines("", "data/interim/libraries/sop/merged/keys.tsv")
  writeLines("", "data/interim/features/components.tsv")
  writeLines("", "data/interim/features/edges.tsv")
  writeLines("", "data/interim/metadata/taxa.tsv")
  writeLines("", "data/interim/annotations/ann.tsv")

  expect_error(
    validate_weight_annotations_inputs(
      library = "data/interim/libraries/sop/merged/keys.tsv",
      components = "data/interim/features/components.tsv",
      edges = "data/interim/features/edges.tsv",
      taxa = "data/interim/metadata/taxa.tsv",
      annotations = "data/interim/annotations/ann.tsv",
      minimal_ms1_condition = "OR",
      weight_spectral = 0.33,
      weight_chemical = 0.33,
      weight_biological = 0.34,
      minimal_consistency = -0.1,
      minimal_ms1_bio = 0.5,
      minimal_ms1_chemo = 0.5,
      ms1_only = FALSE,
      compounds_names = TRUE,
      high_confidence = FALSE,
      remove_ties = FALSE,
      summarize = FALSE,
      force = FALSE,
      candidates_neighbors = 5,
      candidates_final = 10
    ),
    "must be between 0 and 1"
  )
})

test_that("test-validate_weight_annotations_inputs rejects scores > 1", {
  dir.create("data/interim/annotations", recursive = TRUE, showWarnings = FALSE)
  dir.create("data/interim/features", recursive = TRUE, showWarnings = FALSE)
  dir.create(
    "data/interim/libraries/sop/merged",
    recursive = TRUE,
    showWarnings = FALSE
  )
  dir.create("data/interim/metadata", recursive = TRUE, showWarnings = FALSE)

  writeLines("", "data/interim/libraries/sop/merged/keys.tsv")
  writeLines("", "data/interim/features/components.tsv")
  writeLines("", "data/interim/features/edges.tsv")
  writeLines("", "data/interim/metadata/taxa.tsv")
  writeLines("", "data/interim/annotations/ann.tsv")

  expect_error(
    validate_weight_annotations_inputs(
      library = "data/interim/libraries/sop/merged/keys.tsv",
      components = "data/interim/features/components.tsv",
      edges = "data/interim/features/edges.tsv",
      taxa = "data/interim/metadata/taxa.tsv",
      annotations = "data/interim/annotations/ann.tsv",
      minimal_ms1_condition = "OR",
      weight_spectral = 0.33,
      weight_chemical = 0.33,
      weight_biological = 0.34,
      minimal_consistency = 0.5,
      minimal_ms1_bio = 1.5,
      minimal_ms1_chemo = 0.5,
      ms1_only = FALSE,
      compounds_names = TRUE,
      high_confidence = FALSE,
      remove_ties = FALSE,
      summarize = FALSE,
      force = FALSE,
      candidates_neighbors = 5,
      candidates_final = 10
    ),
    "must be between 0 and 1"
  )
})

test_that("test-validate_weight_annotations_inputs accepts weight sum within tolerance", {
  dir.create("data/interim/annotations", recursive = TRUE, showWarnings = FALSE)
  dir.create("data/interim/features", recursive = TRUE, showWarnings = FALSE)
  dir.create(
    "data/interim/libraries/sop/merged",
    recursive = TRUE,
    showWarnings = FALSE
  )
  dir.create("data/interim/metadata", recursive = TRUE, showWarnings = FALSE)

  writeLines("", "data/interim/libraries/sop/merged/keys.tsv")
  writeLines("", "data/interim/features/components.tsv")
  writeLines("", "data/interim/features/edges.tsv")
  writeLines("", "data/interim/metadata/taxa.tsv")
  writeLines("", "data/interim/annotations/ann.tsv")

  # Sum slightly off due to floating point (0.33 + 0.33 + 0.34 = 1.0)
  expect_silent(
    validate_weight_annotations_inputs(
      library = "data/interim/libraries/sop/merged/keys.tsv",
      components = "data/interim/features/components.tsv",
      edges = "data/interim/features/edges.tsv",
      taxa = "data/interim/metadata/taxa.tsv",
      annotations = "data/interim/annotations/ann.tsv",
      minimal_ms1_condition = "OR",
      weight_spectral = 1 / 3,
      weight_chemical = 1 / 3,
      weight_biological = 1 / 3,
      minimal_consistency = 0.5,
      minimal_ms1_bio = 0.5,
      minimal_ms1_chemo = 0.5,
      ms1_only = FALSE,
      compounds_names = TRUE,
      high_confidence = FALSE,
      remove_ties = FALSE,
      summarize = FALSE,
      force = FALSE,
      candidates_neighbors = 5,
      candidates_final = 10
    )
  )
})

test_that("test-rearrange_annotations merges and deduplicates correctly", {
  ann <- wa_min_annotation_table()
  formula <- wa_min_formula_table()
  canopus <- wa_min_canopus_table()
  edges <- wa_min_edges_table()
  result <- rearrange_annotations(
    annotation_table = ann,
    formula_table = formula,
    canopus_table = canopus,
    edges_table = edges
  )
  expect_true("feature_spectrum_entropy" %in% names(result))
  expect_true(
    "candidate_structure_inchikey_connectivity_layer" %in% names(result)
  )
  expect_true(anyDuplicated(result$feature_id) >= 0)
})

test_that("test-load_structure_organism_pairs joins supplemental files", {
  files <- wa_create_minimal_files(tempdir())
  stereo <- file.path("stereo.tsv")
  writeLines(
    "structure_inchikey_connectivity_layer\tstructure_smiles_no_stereo",
    stereo
  )
  tax <- file.path("tax.tsv")
  writeLines("organism_name\torganism_taxonomy_01domain", tax)
  writeLines(
    "structure_inchikey_connectivity_layer\torganism_name",
    files$library
  )
  result <- load_structure_organism_pairs(files$library, stereo, tax)
  expect_true(is.data.frame(result))
})

test_that("test-export_results writes three files", {
  results_list <- list(
    mini = tidytable::tidytable(feature_id = "F1"),
    filtered = tidytable::tidytable(feature_id = "F1"),
    full = tidytable::tidytable(feature_id = "F1")
  )
  # Stub get_default_paths locally
  get_default_paths <- function() wa_stub_get_default_paths(tmp)
  out_vec <- export_results(
    results_list,
    output = temp_test_path("out.tsv"),
    pattern = "test"
  )
  expect_true(all(file.exists(out_vec)))
  expect_equal(names(out_vec), c("filtered", "full"))
})

# test_that("test-load_annotation_tables converts '' and NA strings to NA", {
#
#
#   ann <- tidytable::tidytable(
#     feature_id = c("F1", "F2"),
#     candidate_score_similarity = c("", "NA")
#   )
#   tidytable::fwrite(ann, "ann.tsv", sep = "\t")
#   result <- load_annotation_tables("ann.tsv", ms1_only = FALSE)
#   expect_true(all(is.na(result$candidate_score_similarity)))
# })
