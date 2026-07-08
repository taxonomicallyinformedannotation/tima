# Test Suite: mztab_parser ----

library(testthat)

test_that("normalize_mztab_value trims text and maps canonical nulls to NA", {
  expect_identical(.normalize_mztab_value(1L), 1L)
  expect_identical(
    .normalize_mztab_value(c("  keep  ", "", "null", "NULL", "NA")),
    c("keep", NA_character_, NA_character_, NA_character_, "NA")
  )
})

test_that("split mzTab param fields respects escaped commas and empty input", {
  expect_identical(.mztab_split_param_fields(NA_character_), character(0))
  expect_identical(.mztab_split_param_fields(""), character(0))
  expect_identical(
    .mztab_split_param_fields("[MS, MS:1001, name, a\\,b], c"),
    c("[MS", "MS:1001", "name", "a,b]", "c")
  )
})

test_that("normalize mzTab prefix separators rewrites plain-space prefixes", {
  lines <- c("MTD key value", "SML another value", "already\tseparated")
  out <- .normalize_mztab_prefix_sep(lines)
  expect_true(grepl("^MTD\t", out[[1L]]))
  expect_true(grepl("^SML\t", out[[2L]]))
  expect_identical(out[[3L]], "already\tseparated")
})

test_that("parse mzTab metadata returns empty or normalized key/value tables", {
  empty <- .parse_mztab_metadata(c("SML\tid\tvalue"))
  expect_equal(nrow(empty), 0L)

  parsed <- .parse_mztab_metadata(c(
    "MTD\tmzTab-version\t1.0.0",
    "MTD\tmzTab-mode\tnull",
    "MTD\tcomment\tkept"
  ))
  expect_identical(parsed$key, c("mzTab-version", "mzTab-mode", "comment"))
  expect_true(is.na(parsed$value[[2L]]))
  expect_identical(parsed$value[[1L]], "1.0.0")
})

test_that("parse mzTab section table handles empty and ragged tables", {
  empty <- .parse_mztab_section_table(
    lines = c("MTD\ta\tb"),
    header_prefix = "SMH",
    row_prefix = "SML"
  )
  expect_equal(nrow(empty), 0L)

  parsed <- .parse_mztab_section_table(
    lines = c(
      "SMH\tid\tname",
      "SML\t1\tAlpha",
      "SML\t2\tBeta\tExtra"
    ),
    header_prefix = "SMH",
    row_prefix = "SML"
  )
  expect_equal(nrow(parsed), 2L)
  expect_true("opt_global_extra_col_1" %in% names(parsed))
  expect_identical(parsed$opt_global_extra_col_1[[2L]], "Extra")
})

test_that("mzTab json helpers pick, scalarize and tableify common shapes", {
  expect_identical(.mztab_json_pick(list(a = 1, b = 2), c("b", "a")), 2)
  expect_null(.mztab_json_pick(list(a = 1), c("b", "c")))

  expect_identical(
    .mztab_json_param_to_string(list(
      cvLabel = "MS",
      cvAccession = "MS:1",
      name = "x",
      value = "y"
    )),
    "[MS, MS:1, x, y]"
  )
  expect_true(is.na(.mztab_json_param_to_string(list())))

  expect_identical(.mztab_json_scalar(1L), "1")
  expect_identical(.mztab_json_scalar(c("a", "b")), "a|b")
  expect_identical(.mztab_json_scalar(list(list(a = 1), list(b = 2))), "1|2")
  expect_true(is.na(.mztab_json_scalar(NULL)))

  rows <- .mztab_json_rows_to_table(list(
    list(sml_id = 1, prefix = "drop", value = "x"),
    list(sml_id = 2, comment = "drop", name = list("A", "B"))
  ))
  expect_true(all(c("SML_ID", "value", "name") %in% names(rows)))
  expect_identical(rows$SML_ID, c("1", "2"))
  expect_identical(rows$name[[2L]], "A|B")

  meta <- .mztab_json_metadata_to_table(list(
    mz_tab_version = "1.0.0",
    mz_tab_mode = "Summary",
    custom = list("X", "Y"),
    empty = NULL
  ))
  expect_identical(meta$key, c("mzTab-version", "mzTab-mode", "custom"))
  expect_identical(meta$value[[3L]], "X|Y")
})

test_that("read_mztab_tables reads json and plain-text files", {
  json_file <- tempfile(fileext = ".json")
  writeLines(
    jsonlite::toJSON(
      list(
        metadata = list(mz_tab_version = "1.0.0"),
        small_molecule_summary = list(list(sml_id = 1, name = "Alpha"))
      ),
      auto_unbox = TRUE
    ),
    json_file
  )
  on.exit(unlink(json_file))

  json_out <- read_mztab_tables(json_file)
  expect_identical(json_out$metadata$key, "mzTab-version")
  expect_identical(json_out$sml$SML_ID[[1L]], "1")

  txt_file <- tempfile(fileext = ".mztab")
  writeLines(
    c(
      "MTD\tmzTab-version\t1.0.0",
      "SMH\tid\tname",
      "SML\t1\tAlpha"
    ),
    txt_file
  )
  on.exit(unlink(txt_file), add = TRUE)

  txt_out <- read_mztab_tables(txt_file)
  expect_identical(txt_out$metadata$key, "mzTab-version")
  expect_true(nrow(txt_out$sml) >= 1L)
})

test_that("read_mztab_tables handles malformed and empty input defensively", {
  empty_file <- tempfile(fileext = ".mztab")
  writeLines(character(0), empty_file)
  on.exit(unlink(empty_file), add = TRUE)

  empty_out <- read_mztab_tables(empty_file)
  expect_equal(nrow(empty_out$metadata), 0L)
  expect_equal(nrow(empty_out$sml), 0L)
  expect_equal(nrow(empty_out$smf), 0L)
  expect_equal(nrow(empty_out$sme), 0L)

  malformed_file <- tempfile(fileext = ".mztab")
  writeLines(
    c(
      "MTD\tmzTab-version\t2.1.0-M",
      "MTD\tcomment",
      "SMH\tSML_ID\tname",
      "SML\t1\tAlpha",
      "SML\t2"
    ),
    malformed_file
  )
  on.exit(unlink(malformed_file), add = TRUE)

  malformed_out <- read_mztab_tables(malformed_file)
  expect_equal(nrow(malformed_out$metadata), 2L)
  expect_identical(malformed_out$metadata$value[[2L]], NA_character_)
  expect_equal(nrow(malformed_out$sml), 2L)
})
