# Test Suite: write_mztab_merge ----
# Tests for internal mzTab merge helpers in write_mztab_merge.R

library(testthat)

ns <- asNamespace("tima")
mztab_split_ref_ids <- get(".mztab_split_ref_ids", envir = ns)
mztab_union_ref_ids <- get(".mztab_union_ref_ids", envir = ns)
mztab_remap_ref_ids <- get(".mztab_remap_ref_ids", envir = ns)
mztab_next_numeric_id <- get(".mztab_next_numeric_id", envir = ns)
mztab_score_to_reliability <- get(".mztab_score_to_reliability", envir = ns)
mztab_ensure_cols <- get(".mztab_ensure_cols", envir = ns)

# ── .mztab_split_ref_ids ──────────────────────────────────────────────────────

test_that(".mztab_split_ref_ids returns empty vector for null/NA/empty", {
  expect_equal(mztab_split_ref_ids(NA), character(0))
  expect_equal(mztab_split_ref_ids(""), character(0))
  expect_equal(mztab_split_ref_ids("null"), character(0))
})

test_that(".mztab_split_ref_ids splits pipe-separated IDs", {
  result <- mztab_split_ref_ids("1|2|3")
  expect_equal(result, c("1", "2", "3"))
})

test_that(".mztab_split_ref_ids returns unique values", {
  result <- mztab_split_ref_ids("1|2|1|3")
  expect_equal(sort(result), c("1", "2", "3"))
})

test_that(".mztab_split_ref_ids handles single ID", {
  result <- mztab_split_ref_ids("42")
  expect_equal(result, "42")
})

# ── .mztab_union_ref_ids ──────────────────────────────────────────────────────

test_that(".mztab_union_ref_ids returns 'null' when both inputs are null/NA", {
  expect_equal(mztab_union_ref_ids("null", "null"), "null")
  expect_equal(mztab_union_ref_ids(NA, NA), "null")
  expect_equal(mztab_union_ref_ids("", ""), "null")
})

test_that(".mztab_union_ref_ids unions non-empty ref IDs", {
  result <- mztab_union_ref_ids("1|2", "3|4")
  parts <- sort(strsplit(result, "|", fixed = TRUE)[[1L]])
  expect_equal(parts, c("1", "2", "3", "4"))
})

test_that(".mztab_union_ref_ids deduplicates overlapping IDs", {
  result <- mztab_union_ref_ids("1|2", "2|3")
  parts <- sort(strsplit(result, "|", fixed = TRUE)[[1L]])
  expect_equal(parts, c("1", "2", "3"))
})

test_that(".mztab_union_ref_ids handles one null side", {
  result <- mztab_union_ref_ids("null", "1|2")
  parts <- sort(strsplit(result, "|", fixed = TRUE)[[1L]])
  expect_equal(parts, c("1", "2"))
})

# ── .mztab_remap_ref_ids ──────────────────────────────────────────────────────

test_that(".mztab_remap_ref_ids remaps IDs using the id_map", {
  id_map <- list("a" = "1", "b" = "2")
  result <- mztab_remap_ref_ids(c("a|b"), id_map)
  parts <- sort(strsplit(result, "|", fixed = TRUE)[[1L]])
  expect_equal(parts, c("1", "2"))
})

test_that(".mztab_remap_ref_ids returns 'null' for null input", {
  id_map <- list("a" = "1")
  result <- mztab_remap_ref_ids(c("null"), id_map)
  expect_equal(unname(result), "null")
})

test_that(".mztab_remap_ref_ids keeps unmapped IDs unchanged", {
  id_map <- list("a" = "1")
  result <- mztab_remap_ref_ids(c("b"), id_map)
  expect_equal(unname(result), "b")
})

test_that(".mztab_remap_ref_ids handles vectorized input", {
  id_map <- list("x" = "10", "y" = "20")
  result <- mztab_remap_ref_ids(c("x", "y", "null"), id_map)
  expect_equal(unname(result), c("10", "20", "null"))
})

# ── .mztab_next_numeric_id ────────────────────────────────────────────────────

test_that(".mztab_next_numeric_id returns 1 for empty input", {
  expect_equal(mztab_next_numeric_id(character(0)), 1L)
  expect_equal(mztab_next_numeric_id(c()), 1L)
})

test_that(".mztab_next_numeric_id returns max+1", {
  expect_equal(mztab_next_numeric_id(c("1", "5", "3")), 6L)
  expect_equal(mztab_next_numeric_id(c("10")), 11L)
})

test_that(".mztab_next_numeric_id ignores non-numeric IDs", {
  expect_equal(mztab_next_numeric_id(c("a", "b", "3")), 4L)
})

# ── .mztab_score_to_reliability ───────────────────────────────────────────────

test_that(".mztab_score_to_reliability returns 1 for spectral + high score", {
  result <- mztab_score_to_reliability(0.9, "spectral library")
  expect_equal(result, 1L)
})

test_that(".mztab_score_to_reliability returns 2 for spectral + moderate score", {
  result <- mztab_score_to_reliability(0.5, "gnps")
  expect_equal(result, 2L)
})

test_that(".mztab_score_to_reliability returns 2 for non-spectral + score >= 0.5", {
  result <- mztab_score_to_reliability(0.6, "database_match")
  expect_equal(result, 2L)
})

test_that(".mztab_score_to_reliability returns 3 for score >= 0.2 non-spectral", {
  result <- mztab_score_to_reliability(0.3, "database")
  expect_equal(result, 3L)
})

test_that(".mztab_score_to_reliability returns 4 for low/NA score non-spectral", {
  result <- mztab_score_to_reliability(0.1, "database")
  expect_equal(result, 4L)
  result_na <- mztab_score_to_reliability(NA_real_, "database")
  expect_equal(result_na, 4L)
})

test_that(".mztab_score_to_reliability handles vectorized input", {
  scores <- c(0.9, 0.5, 0.3, 0.1)
  libs <- c("spectral", "spectral", "database", "database")
  result <- mztab_score_to_reliability(scores, libs)
  expect_equal(result, c(1L, 2L, 3L, 4L))
})

# ── .mztab_ensure_cols ────────────────────────────────────────────────────────

test_that(".mztab_ensure_cols adds missing columns filled with 'null'", {
  df <- tidytable::tidytable(a = c(1, 2))
  result <- mztab_ensure_cols(df, c("a", "b", "c"))
  expect_true("b" %in% colnames(result))
  expect_true("c" %in% colnames(result))
  # Missing columns are filled with empty string, then converted to "null"
  expect_true(all(result$b == "null"))
  expect_true(all(result$c == "null"))
})

test_that(".mztab_ensure_cols does not overwrite existing columns", {
  df <- tidytable::tidytable(a = c("x", "y"))
  result <- mztab_ensure_cols(df, c("a", "b"))
  expect_equal(result$a, c("x", "y"))
})

test_that(".mztab_ensure_cols orders columns as requested", {
  df <- tidytable::tidytable(z = c("1"), a = c("2"))
  result <- mztab_ensure_cols(df, c("a", "b", "z"))
  expect_equal(colnames(result)[1L], "a")
  expect_equal(colnames(result)[2L], "b")
  expect_equal(colnames(result)[3L], "z")
})
