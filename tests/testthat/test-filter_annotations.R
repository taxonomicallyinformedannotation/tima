# Test Suite: filter_annotations (mother-function only) ----

library(testthat)

# Minimal fixtures for mother-function tests only

.make_ann <- function(path) {
  write.table(
    data.frame(
      feature_id = c("F1", "F2"),
      candidate_library = c("ms1", "spectral"),
      candidate_structure_inchikey_connectivity_layer = c("A", "B"),
      rt = c("1.0", "2.0"),
      candidate_score_similarity = c(NA, 0.8)
    ),
    path,
    sep = "\t",
    row.names = FALSE
  )
  path
}

.make_feat <- function(path) {
  write.table(
    data.frame(
      feature_id = c("F1", "F2"),
      rt = c(1.0, 2.0),
      mz = c(100.05, 150.08)
    ),
    path,
    sep = "\t",
    row.names = FALSE
  )
  path
}

.make_rt <- function(path) {
  write.table(
    data.frame(
      candidate_structure_inchikey_connectivity_layer = c("A", "B"),
      rt_target = c("1.02", "2.05")
    ),
    path,
    sep = "\t",
    row.names = FALSE
  )
  path
}

# Validation through mother function

test_that("filter_annotations() errors on missing annotations file", {
  feat <- tempfile(fileext = ".tsv")
  out <- tempfile(fileext = ".tsv")
  .make_feat(feat)
  expect_error(
    filter_annotations(
      annotations = "nope.tsv",
      features = feat,
      rts = character(0),
      output = out
    ),
    "Annotation file"
  )
})

test_that("filter_annotations() errors on missing features file", {
  ann <- tempfile(fileext = ".tsv")
  .make_ann(ann)
  out <- tempfile(fileext = ".tsv")
  expect_error(
    filter_annotations(
      annotations = ann,
      features = "nope.tsv",
      rts = character(0),
      output = out
    ),
    "Features file"
  )
})

test_that("filter_annotations() runs end-to-end without RT library", {
  ann <- tempfile(fileext = ".tsv")
  .make_ann(ann)
  feat <- tempfile(fileext = ".tsv")
  .make_feat(feat)
  out <- tempfile(fileext = ".tsv")
  res <- filter_annotations(
    annotations = ann,
    features = feat,
    rts = character(0),
    output = out,
    tolerance_rt = 0.5
  )
  expect_true(file.exists(res))
})

# test_that("filter_annotations() runs with RT library and multiple annotation files", {
#   ann1 <- tempfile(fileext = ".tsv")
#   .make_ann(ann1)
#   ann2 <- tempfile(fileext = ".tsv")
#   .make_ann(ann2)
#   feat <- tempfile(fileext = ".tsv")
#   .make_feat(feat)
#   rts <- tempfile(fileext = ".tsv")
#   .make_rt(rts)
#   out <- tempfile(fileext = ".tsv")
#   res <- filter_annotations(
#     annotations = c(ann1, ann2),
#     features = feat,
#     rts = rts,
#     output = out,
#     tolerance_rt = 0.1
#   )
#   expect_true(file.exists(res))
# })

test_that("filter_annotations() errors on invalid tolerance_rt", {
  ann <- tempfile(fileext = ".tsv")
  .make_ann(ann)
  feat <- tempfile(fileext = ".tsv")
  .make_feat(feat)
  out <- tempfile(fileext = ".tsv")
  expect_error(
    filter_annotations(
      annotations = ann,
      features = feat,
      rts = character(0),
      output = out,
      tolerance_rt = -1
    ),
    "tolerance_rt must be"
  )
})
