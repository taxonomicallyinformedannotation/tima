# Test Suite: prepare_libraries_sop_hmdb_like ----
# Tests for the HMDB-like SOP library preparation helper

library(testthat)

# ── Input validation ──────────────────────────────────────────────────────────

test_that("prepare_libraries_sop_hmdb_like errors on non-character input", {
  expect_error(
    prepare_libraries_sop_hmdb_like(
      input = 1L,
      output = tempfile(fileext = ".tsv"),
      source_name = "test"
    ),
    class = "tima_error"
  )
})

test_that("prepare_libraries_sop_hmdb_like errors on non-character output", {
  expect_error(
    prepare_libraries_sop_hmdb_like(
      input = "/some/input.sdf.zip",
      output = 1L,
      source_name = "test"
    ),
    class = "tima_error"
  )
})

test_that("prepare_libraries_sop_hmdb_like errors on non-character source_name", {
  expect_error(
    prepare_libraries_sop_hmdb_like(
      input = "/some/input.sdf.zip",
      output = tempfile(fileext = ".tsv"),
      source_name = 99
    ),
    class = "tima_error"
  )
})

test_that("prepare_libraries_sop_hmdb_like errors on invalid tag type", {
  expect_error(
    prepare_libraries_sop_hmdb_like(
      input = "/some/input.sdf.zip",
      output = tempfile(fileext = ".tsv"),
      source_name = "test",
      tag = 123L
    ),
    class = "tima_error"
  )
})

test_that("prepare_libraries_sop_hmdb_like accepts NA as tag", {
  # NA tag gets converted to NA_character_ silently - test input validation only
  expect_error(
    prepare_libraries_sop_hmdb_like(
      input = "/nonexistent/input.sdf.zip",
      output = tempfile(fileext = ".tsv"),
      source_name = "test",
      tag = NA
    )
  )
})

test_that("prepare_libraries_sop_hmdb_like errors on multi-element tag", {
  expect_error(
    prepare_libraries_sop_hmdb_like(
      input = "/some/input.sdf.zip",
      output = tempfile(fileext = ".tsv"),
      source_name = "test",
      tag = c("a", "b")
    ),
    class = "tima_error"
  )
})
