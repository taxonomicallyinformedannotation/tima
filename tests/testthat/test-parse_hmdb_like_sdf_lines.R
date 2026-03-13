# Test Suite: parse_hmdb_like_sdf_lines ----
# Tests the core SDF parser that handles HMDB, SMPDB, T3DB, YMDB, MIMEDB, etc.

library(testthat)

# ---- helpers -----------------------------------------------------------------

#' Build a minimal well-formed SDF block for testing.
#' @param fields Named character vector of field->value pairs.
make_sdf_block <- function(..., id = "HMDB0000001") {
  fields <- c(list(...), list())
  header <- c(
    paste0("  ", id), # molecule name line
    "  Mrv2211", # program/timestamp line
    "", # comment line
    "  0  0  0  0  0  0  0  0  0  0999 V2000", # counts line
    "M  END" # mol block terminator
  )
  data_lines <- unlist(lapply(names(fields), function(nm) {
    c(paste0("> <", nm, ">"), fields[[nm]], "")
  }))
  c(header, data_lines, "$$$$")
}

single_block <- function() {
  make_sdf_block(
    HMDB_ID = "HMDB0000001",
    SMILES = "CC(=O)O",
    INCHI_KEY = "QTBSBXVTEAMEQO-UHFFFAOYSA-N",
    FORMULA = "C2H4O2",
    EXACT_MASS = "60.02113",
    JCHEM_LOGP = "-0.17",
    GENERIC_NAME = "Acetic acid"
  )
}

# ---- empty input -------------------------------------------------------------

test_that("parse_hmdb_like_sdf_lines returns empty tibble on zero lines", {
  out <- parse_hmdb_like_sdf_lines(character(0))
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 0L)
  expect_true("inchikey" %in% names(out))
})

# ---- single record -----------------------------------------------------------

test_that("parse_hmdb_like_sdf_lines extracts all fields from one record", {
  out <- parse_hmdb_like_sdf_lines(single_block())
  expect_equal(nrow(out), 1L)
  expect_equal(out$inchikey, "QTBSBXVTEAMEQO-UHFFFAOYSA-N")
  expect_equal(out$smiles, "CC(=O)O")
  expect_equal(out$formula, "C2H4O2")
  expect_equal(out$mass, "60.02113")
  expect_equal(out$logp, "-0.17")
  expect_equal(out$name, "Acetic acid")
  expect_equal(out$id, "HMDB0000001")
})

# ---- field aliasing ----------------------------------------------------------

test_that("parse_hmdb_like_sdf_lines maps all known ID aliases to 'id'", {
  aliases <- c(
    "DATABASE_ID",
    "HMDB_ID",
    "SMPDB_ID",
    "YMDB_ID",
    "T3DB_ID",
    "MIMEDB_ID"
  )
  invisible(vapply(
    X = aliases,
    FUN = function(alias) {
      block <- c(
        "  mol",
        "  prog",
        "",
        "  0  0  0  0  0  0  0  0  0  0999 V2000",
        "M  END",
        paste0("> <", alias, ">"),
        "DB00001",
        "",
        "> <INCHI_KEY>",
        "QTBSBXVTEAMEQO-UHFFFAOYSA-N",
        "",
        "$$$$"
      )
      out <- parse_hmdb_like_sdf_lines(block)
      expect_equal(out$id, "DB00001", label = paste("alias:", alias))
      TRUE
    },
    FUN.VALUE = logical(1L)
  ))
})

test_that("parse_hmdb_like_sdf_lines maps logP aliases correctly", {
  invisible(vapply(
    X = c("JCHEM_LOGP", "ALOGPS_LOGP", "XLOGP3"),
    FUN = function(field) {
      block <- c(
        "  mol",
        "  prog",
        "",
        "  0  0  0  0  0  0  0  0  0  0999 V2000",
        "M  END",
        paste0("> <", field, ">"),
        "2.5",
        "",
        "$$$$"
      )
      out <- parse_hmdb_like_sdf_lines(block)
      expect_equal(out$logp, "2.5", label = paste("logp alias:", field))
      TRUE
    },
    FUN.VALUE = logical(1L)
  ))
})

test_that("parse_hmdb_like_sdf_lines maps name aliases correctly", {
  invisible(vapply(
    X = c("GENERIC_NAME"),
    FUN = function(field) {
      block <- c(
        "  mol",
        "  prog",
        "",
        "  0  0  0  0  0  0  0  0  0  0999 V2000",
        "M  END",
        paste0("> <", field, ">"),
        "TestName",
        "",
        "$$$$"
      )
      out <- parse_hmdb_like_sdf_lines(block)
      expect_equal(out$name, "TestName", label = paste("name alias:", field))
      TRUE
    },
    FUN.VALUE = logical(1L)
  ))
})

# ---- multi-record ------------------------------------------------------------

test_that("parse_hmdb_like_sdf_lines parses multiple records independently", {
  block1 <- make_sdf_block(
    HMDB_ID = "HMDB0000001",
    INCHI_KEY = "AAAAAAAAAAAAAAA-UHFFFAOYSA-N",
    SMILES = "CC",
    FORMULA = "C2H6",
    EXACT_MASS = "30.047",
    JCHEM_LOGP = "1.0",
    GENERIC_NAME = "Ethane"
  )
  block2 <- make_sdf_block(
    HMDB_ID = "HMDB0000002",
    INCHI_KEY = "BBBBBBBBBBBBBBB-UHFFFAOYSA-N",
    SMILES = "CCC",
    FORMULA = "C3H8",
    EXACT_MASS = "44.063",
    JCHEM_LOGP = "1.5",
    GENERIC_NAME = "Propane"
  )
  out <- parse_hmdb_like_sdf_lines(c(block1, block2))
  expect_equal(nrow(out), 2L)
  expect_equal(out$id, c("HMDB0000001", "HMDB0000002"))
  expect_equal(out$formula, c("C2H6", "C3H8"))
  expect_equal(out$name, c("Ethane", "Propane"))
})

test_that("parse_hmdb_like_sdf_lines scales to 1000 records without error", {
  # Generate 1000 minimal records (no mol block needed for field parser)
  blocks <- lapply(seq_len(1000L), function(i) {
    c(
      paste0("  mol_", i),
      "  prog",
      "",
      "  0  0 V2000",
      "M  END",
      "> <HMDB_ID>",
      paste0("HMDB", sprintf("%07d", i)),
      "",
      "> <INCHI_KEY>",
      paste0(strrep("A", 14L), "-UHFFFAOYSA-N"),
      "",
      "> <SMILES>",
      "CC",
      "",
      "> <FORMULA>",
      "C2H6",
      "",
      "> <EXACT_MASS>",
      "30.047",
      "",
      "$$$$"
    )
  })
  lines <- unlist(blocks)
  out <- parse_hmdb_like_sdf_lines(lines)
  expect_equal(nrow(out), 1000L)
  expect_equal(out$formula[1], "C2H6")
  expect_equal(out$formula[1000], "C2H6")
})

# ---- missing fields ----------------------------------------------------------

test_that("parse_hmdb_like_sdf_lines returns NA for absent fields", {
  block <- c(
    "  mol",
    "  prog",
    "",
    "  0  0 V2000",
    "M  END",
    "> <INCHI_KEY>",
    "QTBSBXVTEAMEQO-UHFFFAOYSA-N",
    "",
    "$$$$"
  )
  out <- parse_hmdb_like_sdf_lines(block)
  expect_true(is.na(out$id))
  expect_true(is.na(out$smiles))
  expect_true(is.na(out$logp))
  expect_true(is.na(out$name))
})

# ---- unknown fields are silently ignored ------------------------------------

test_that("parse_hmdb_like_sdf_lines ignores unrecognised field tags", {
  block <- c(
    "  mol",
    "  prog",
    "",
    "  0  0 V2000",
    "M  END",
    "> <UNKNOWN_FIELD>",
    "should_be_ignored",
    "",
    "> <INCHI_KEY>",
    "QTBSBXVTEAMEQO-UHFFFAOYSA-N",
    "",
    "$$$$"
  )
  expect_no_error({
    out <- parse_hmdb_like_sdf_lines(block)
  })
  expect_equal(out$inchikey, "QTBSBXVTEAMEQO-UHFFFAOYSA-N")
})

# ---- first-wins for duplicate fields in one record --------------------------

test_that("parse_hmdb_like_sdf_lines keeps first occurrence of duplicate field", {
  block <- c(
    "  mol",
    "  prog",
    "",
    "  0  0 V2000",
    "M  END",
    "> <GENERIC_NAME>",
    "First",
    "",
    "> <NAME>",
    "Second",
    "", # should be ignored since name already set
    "$$$$"
  )
  out <- parse_hmdb_like_sdf_lines(block)
  # Both GENERIC_NAME and NAME map to "name"; whichever appears first wins
  expect_equal(out$name, "First")
})

# ---- field header with extra whitespace -------------------------------------

test_that("parse_hmdb_like_sdf_lines handles extra whitespace in header tags", {
  block <- c(
    "  mol",
    "  prog",
    "",
    "  0  0 V2000",
    "M  END",
    ">  < INCHI_KEY >",
    "QTBSBXVTEAMEQO-UHFFFAOYSA-N",
    "",
    "$$$$"
  )
  out <- parse_hmdb_like_sdf_lines(block)
  expect_equal(out$inchikey, "QTBSBXVTEAMEQO-UHFFFAOYSA-N")
})

# ---- output schema ----------------------------------------------------------

test_that("parse_hmdb_like_sdf_lines always returns expected column names", {
  expected_cols <- c(
    "id",
    "smiles",
    "inchikey",
    "formula",
    "mass",
    "logp",
    "name"
  )
  out_empty <- parse_hmdb_like_sdf_lines(character(0))
  expect_named(out_empty, expected_cols)

  out_single <- parse_hmdb_like_sdf_lines(single_block())
  expect_named(out_single, expected_cols)
})
