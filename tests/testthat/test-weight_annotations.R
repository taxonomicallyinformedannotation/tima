# Test: Weight Annotations - Full Integration Function
library(testthat)
library(tima)

# =============================================================================
# Fast validation tests for weight_annotations() - avoid heavy I/O
# =============================================================================

test_that("weight_annotations validates minimal_ms1_condition", {
  # Use withr to create temp dir that auto-cleans
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
    weight_annotations(
      library = "data/interim/libraries/sop/merged/keys.tsv",
      components = "data/interim/features/components.tsv",
      edges = "data/interim/features/edges.tsv",
      taxa = "data/interim/metadata/taxa.tsv",
      annotations = "data/interim/annotations/ann.tsv",
      minimal_ms1_condition = "XOR"
    ),
    "must be 'OR' or 'AND'"
  )
})

test_that("weight_annotations validates weights sum to 1 and non-negative", {
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

  # Sum not equal to 1
  expect_error(
    weight_annotations(
      library = "data/interim/libraries/sop/merged/keys.tsv",
      components = "data/interim/features/components.tsv",
      edges = "data/interim/features/edges.tsv",
      taxa = "data/interim/metadata/taxa.tsv",
      annotations = "data/interim/annotations/ann.tsv",
      weight_spectral = 0.2,
      weight_chemical = 0.2,
      weight_biological = 0.2
    ),
    "Weights must sum to 1.0"
  )
})

test_that("weight_annotations validates score thresholds in [0,1]", {
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
    weight_annotations(
      library = "data/interim/libraries/sop/merged/keys.tsv",
      components = "data/interim/features/components.tsv",
      edges = "data/interim/features/edges.tsv",
      taxa = "data/interim/metadata/taxa.tsv",
      annotations = "data/interim/annotations/ann.tsv",
      minimal_consistency = 1.1
    ),
    "between 0 and 1"
  )
})

test_that("weight_annotations validates logical flags", {
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
    weight_annotations(
      library = "data/interim/libraries/sop/merged/keys.tsv",
      components = "data/interim/features/components.tsv",
      edges = "data/interim/features/edges.tsv",
      taxa = "data/interim/metadata/taxa.tsv",
      annotations = "data/interim/annotations/ann.tsv",
      ms1_only = "yes"
    ),
    "logical"
  )
})

test_that("weight_annotations validates candidate counts", {
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
    weight_annotations(
      library = "data/interim/libraries/sop/merged/keys.tsv",
      components = "data/interim/features/components.tsv",
      edges = "data/interim/features/edges.tsv",
      taxa = "data/interim/metadata/taxa.tsv",
      annotations = "data/interim/annotations/ann.tsv",
      candidates_neighbors = 0
    ),
    "positive"
  )
})

# Keep integration tests skipped for heavy I/O

test_that("weight_annotations integration placeholder", {
  skip("Integration test - requires full file structure setup")
})
