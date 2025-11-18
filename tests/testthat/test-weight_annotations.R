# ==============================================================================
# Test Suite: weight_annotations
# ==============================================================================

# ==============================================================================
# Test Suite: validate_weight_annotations_inputs
# ==============================================================================

test_that("validate_weight_annotations_inputs accepts valid inputs", {
  tmp <- withr::local_tempdir()
  withr::local_dir(tmp)

  # Create test files
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
    tima:::validate_weight_annotations_inputs(
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
      candidates_neighbors = 5,
      candidates_final = 10
    )
  )
})

test_that("validate_weight_annotations_inputs rejects missing required files", {
  expect_error(
    tima:::validate_weight_annotations_inputs(
      library = "nonexistent.tsv",
      components = "components.tsv",
      edges = "edges.tsv",
      taxa = "taxa.tsv",
      annotations = "ann.tsv",
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
  tmp <- withr::local_tempdir()
  withr::local_dir(tmp)

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
    tima:::validate_weight_annotations_inputs(
      library = "data/interim/libraries/sop/merged/keys.tsv",
      components = "data/interim/features/components.tsv",
      edges = "data/interim/features/edges.tsv",
      taxa = "data/interim/metadata/taxa.tsv",
      annotations = "data/interim/annotations/ann.tsv",
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
  tmp <- withr::local_tempdir()
  withr::local_dir(tmp)

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
    tima:::validate_weight_annotations_inputs(
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
    "Weights must sum to 1.0"
  )
})

test_that("validate_weight_annotations_inputs rejects negative weights", {
  tmp <- withr::local_tempdir()
  withr::local_dir(tmp)

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
    tima:::validate_weight_annotations_inputs(
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
  tmp <- withr::local_tempdir()
  withr::local_dir(tmp)

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
    tima:::validate_weight_annotations_inputs(
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
  tmp <- withr::local_tempdir()
  withr::local_dir(tmp)

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
    tima:::validate_weight_annotations_inputs(
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
    "candidates_neighbors must be a positive integer"
  )
})

# ==============================================================================
# Test Suite: load_annotation_tables
# ==============================================================================

test_that("load_annotation_tables loads and combines files", {
  tmp <- withr::local_tempdir()
  withr::local_dir(tmp)

  # Create test annotation files
  ann1 <- tidytable::tidytable(
    feature_id = c("F1", "F2"),
    candidate_score_similarity = c("0.8", "0.7")
  )
  ann2 <- tidytable::tidytable(
    feature_id = c("F3", "F4"),
    candidate_score_similarity = c("0.6", "0.5")
  )

  tidytable::fwrite(ann1, "ann1.tsv", sep = "\t")
  tidytable::fwrite(ann2, "ann2.tsv", sep = "\t")

  result <- tima:::load_annotation_tables(
    c("ann1.tsv", "ann2.tsv"),
    ms1_only = FALSE
  )

  expect_equal(nrow(result), 4)
  expect_true(all(c("F1", "F2", "F3", "F4") %in% result$feature_id))
})

test_that("load_annotation_tables filters MS1 only when requested", {
  tmp <- withr::local_tempdir()
  withr::local_dir(tmp)

  ann <- tidytable::tidytable(
    feature_id = c("F1", "F2", "F3"),
    candidate_score_similarity = c(NA, "0.8", NA),
    candidate_score_sirius_csi = c(NA, NA, "0.7")
  )

  tidytable::fwrite(ann, "ann.tsv", sep = "\t")

  result <- tima:::load_annotation_tables("ann.tsv", ms1_only = TRUE)

  # Should keep only F1 (both scores are NA)
  expect_equal(nrow(result), 1)
  expect_equal(result$feature_id, "F1")
})

# ==============================================================================
# Test Suite: load_edges_table
# ==============================================================================

test_that("load_edges_table loads and filters top neighbors", {
  tmp <- withr::local_tempdir()
  withr::local_dir(tmp)

  edges <- tidytable::tidytable(
    feature_source = c("F1", "F1", "F1", "F2", "F2"),
    feature_target = c("F2", "F3", "F4", "F3", "F4"),
    candidate_score_similarity = c("0.9", "0.8", "0.7", "0.6", "0.5")
  )

  tidytable::fwrite(edges, "edges.tsv", sep = "\t")

  result <- tima:::load_edges_table("edges.tsv", candidates_neighbors = 2)

  # Should keep top 2 neighbors per source feature
  expect_equal(nrow(result), 4)

  f1_edges <- result |> tidytable::filter(feature_source == "F1")
  expect_equal(nrow(f1_edges), 2)
  expect_true(all(f1_edges$feature_target %in% c("F2", "F3")))
})

# ==============================================================================
# Test Suite: log_annotation_stats
# ==============================================================================

test_that("log_annotation_stats runs without error", {
  ann <- tidytable::tidytable(
    feature_id = c("F1", "F2", "F3"),
    candidate_library = c("lib1", "lib1", "lib2"),
    candidate_structure_inchikey_connectivity_layer = c("A", "B", "C")
  )

  expect_no_error(tima:::log_annotation_stats(ann))
})

test_that("log_annotation_stats handles empty annotations", {
  ann <- tidytable::tidytable(
    feature_id = character(),
    candidate_library = character(),
    candidate_structure_inchikey_connectivity_layer = character()
  )

  expect_no_error(tima:::log_annotation_stats(ann))
})

test_that("log_annotation_stats handles NA inchikeys", {
  ann <- tidytable::tidytable(
    feature_id = c("F1", "F2", "F3"),
    candidate_library = c("lib1", "lib1", "lib2"),
    candidate_structure_inchikey_connectivity_layer = c("A", NA, "C")
  )

  expect_no_error(tima:::log_annotation_stats(ann))
})

# ==============================================================================
# Additional Test Suite: validate_weight_annotations_inputs - Edge Cases
# ==============================================================================

test_that("validate_weight_annotations_inputs accepts boundary weight sum", {
  tmp <- withr::local_tempdir()
  withr::local_dir(tmp)

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
    tima:::validate_weight_annotations_inputs(
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
  tmp <- withr::local_tempdir()
  withr::local_dir(tmp)

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
    tima:::validate_weight_annotations_inputs(
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
  tmp <- withr::local_tempdir()
  withr::local_dir(tmp)

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
    tima:::validate_weight_annotations_inputs(
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
    "ms1_only must be logical"
  )
})

# ==============================================================================
# Additional Test Suite: load_annotation_tables - Edge Cases
# ==============================================================================

# test_that("load_annotation_tables handles NA values correctly", {
#   tmp <- withr::local_tempdir()
#   withr::local_dir(tmp)
#
#   ann <- tidytable::tidytable(
#     feature_id = c("F1", "F2"),
#     candidate_score_similarity = c("", "NA")
#   )
#
#   tidytable::fwrite(ann, "ann.tsv", sep = "\t")
#
#   result <- tima:::load_annotation_tables("ann.tsv", ms1_only = FALSE)
#
#   expect_true(is.na(result$candidate_score_similarity[1]))
#   expect_true(is.na(result$candidate_score_similarity[2]))
# })

test_that("load_annotation_tables returns all columns as character", {
  tmp <- withr::local_tempdir()
  withr::local_dir(tmp)

  ann <- tidytable::tidytable(
    feature_id = c("F1", "F2"),
    score = c("0.8", "0.9"),
    count = c("5", "10")
  )

  tidytable::fwrite(ann, "ann.tsv", sep = "\t")

  result <- tima:::load_annotation_tables("ann.tsv", ms1_only = FALSE)

  expect_true(all(vapply(result, is.character, logical(1))))
})

# ==============================================================================
# Additional Test Suite: load_edges_table - Edge Cases
# ==============================================================================

test_that("load_edges_table handles single neighbor request", {
  tmp <- withr::local_tempdir()
  withr::local_dir(tmp)

  edges <- tidytable::tidytable(
    feature_source = c("F1", "F1", "F1"),
    feature_target = c("F2", "F3", "F4"),
    candidate_score_similarity = c("0.9", "0.8", "0.7")
  )

  tidytable::fwrite(edges, "edges.tsv", sep = "\t")

  result <- tima:::load_edges_table("edges.tsv", candidates_neighbors = 1)

  expect_equal(nrow(result), 1)
  expect_equal(result$feature_target, "F2")
})

test_that("load_edges_table handles ties in similarity scores", {
  tmp <- withr::local_tempdir()
  withr::local_dir(tmp)

  edges <- tidytable::tidytable(
    feature_source = c("F1", "F1", "F1"),
    feature_target = c("F2", "F3", "F4"),
    candidate_score_similarity = c("0.9", "0.9", "0.7")
  )

  tidytable::fwrite(edges, "edges.tsv", sep = "\t")

  result <- tima:::load_edges_table("edges.tsv", candidates_neighbors = 2)

  # with_ties = FALSE, should get exactly 2 rows
  expect_equal(nrow(result), 2)
})

test_that("load_edges_table handles empty file", {
  tmp <- withr::local_tempdir()
  withr::local_dir(tmp)

  edges <- tidytable::tidytable(
    feature_source = character(),
    feature_target = character(),
    candidate_score_similarity = character()
  )

  tidytable::fwrite(edges, "edges.tsv", sep = "\t")

  result <- tima:::load_edges_table("edges.tsv", candidates_neighbors = 5)

  expect_equal(nrow(result), 0)
})
test_that("log_annotation_stats handles empty data", {
  ann <- tidytable::tidytable(
    feature_id = character(),
    candidate_library = character(),
    candidate_structure_inchikey_connectivity_layer = character()
  )

  expect_no_error(tima:::log_annotation_stats(ann))
})

# ==============================================================================
# Test Suite: Edge Cases
# ==============================================================================

test_that("load_annotation_tables handles single file", {
  tmp <- withr::local_tempdir()
  withr::local_dir(tmp)

  ann <- tidytable::tidytable(
    feature_id = c("F1"),
    candidate_score_similarity = c("0.8")
  )

  tidytable::fwrite(ann, "ann.tsv", sep = "\t")

  result <- tima:::load_annotation_tables("ann.tsv", ms1_only = FALSE)

  expect_equal(nrow(result), 1)
})

test_that("load_edges_table handles features with fewer neighbors than requested", {
  tmp <- withr::local_tempdir()
  withr::local_dir(tmp)

  edges <- tidytable::tidytable(
    feature_source = c("F1"),
    feature_target = c("F2"),
    candidate_score_similarity = c("0.9")
  )

  tidytable::fwrite(edges, "edges.tsv", sep = "\t")

  # Request 5 neighbors but only 1 available
  result <- tima:::load_edges_table("edges.tsv", candidates_neighbors = 5)

  expect_equal(nrow(result), 1)
})
