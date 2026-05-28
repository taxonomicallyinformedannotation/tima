# Test Suite: write_mztab_tables ----
# Tests for internal mzTab table serialization helpers in write_mztab_tables.R

library(testthat)

ns <- asNamespace("tima")
mztab_pluck <- get(".mztab_pluck", envir = ns)
mztab_pluck_na <- get(".mztab_pluck_na", envir = ns)
mztab_expand_summarized <- get(".mztab_expand_summarized", envir = ns)

# ── .mztab_pluck ─────────────────────────────────────────────────────────────

test_that(".mztab_pluck returns column as character when present", {
  df <- data.frame(a = c(1, 2), b = c("x", "y"), stringsAsFactors = FALSE)
  result <- mztab_pluck(df, "a")
  expect_equal(result, c("1", "2"))

  result_b <- mztab_pluck(df, "b")
  expect_equal(result_b, c("x", "y"))
})

test_that(".mztab_pluck returns 'null' vector when column absent", {
  df <- data.frame(a = c(1, 2), stringsAsFactors = FALSE)
  result <- mztab_pluck(df, "missing_col")
  expect_equal(result, c("null", "null"))
  expect_length(result, 2L)
})

# ── .mztab_pluck_na ───────────────────────────────────────────────────────────

test_that(".mztab_pluck_na replaces NA and empty with 'null'", {
  df <- data.frame(
    a = c("hello", NA_character_, "", "null", "world"),
    stringsAsFactors = FALSE
  )
  result <- mztab_pluck_na(df, "a")
  expect_equal(result, c("hello", "null", "null", "null", "world"))
})

test_that(".mztab_pluck_na returns all 'null' for absent column", {
  df <- data.frame(a = 1:3)
  result <- mztab_pluck_na(df, "b")
  expect_equal(result, rep("null", 3L))
})

# ── .mztab_expand_summarized ──────────────────────────────────────────────────

test_that(".mztab_expand_summarized returns unchanged df when no pipe-sep columns", {
  df <- data.frame(
    feature_id = c("f1", "f2"),
    candidate_score = c("0.8", "0.7"),
    stringsAsFactors = FALSE
  )
  result <- mztab_expand_summarized(df)
  expect_equal(nrow(result), 2L)
  expect_equal(result$feature_id, c("f1", "f2"))
})

test_that(".mztab_expand_summarized expands pipe-separated candidate columns", {
  df <- data.frame(
    feature_id = c("f1"),
    candidate_smiles = c("CC|CCC"),
    rank_1 = c("1|2"),
    stringsAsFactors = FALSE
  )
  result <- mztab_expand_summarized(df)
  expect_equal(nrow(result), 2L)
  expect_equal(result$feature_id, c("f1", "f1"))
  expect_equal(trimws(result$candidate_smiles), c("CC", "CCC"))
})

test_that(".mztab_expand_summarized handles rows with and without pipe", {
  df <- data.frame(
    feature_id = c("f1", "f2"),
    candidate_smiles = c("CC|CCC", "CCC"),
    stringsAsFactors = FALSE
  )
  result <- mztab_expand_summarized(df)
  expect_equal(nrow(result), 3L)
})

test_that(".mztab_expand_summarized returns df unchanged when no candidate/rank/score cols", {
  df <- data.frame(
    feature_id = c("f1"),
    mz = c(100.0),
    stringsAsFactors = FALSE
  )
  result <- mztab_expand_summarized(df)
  expect_equal(nrow(result), 1L)
  expect_equal(result$feature_id, "f1")
})
