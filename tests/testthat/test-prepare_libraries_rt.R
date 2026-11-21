# Test Suite: prepare_libraries_rt ----

library(testthat)

make_rt_df <- function(
  rt = c(5.2, 7.8, 10.3),
  inchikey = c(
    "LFQSCWFLJHTTHZ-UHFFFAOYSA-N",
    "RYYVLZVUVIJVGH-UHFFFAOYSA-N",
    "GHVNFZFCNZKVNT-UHFFFAOYSA-N"
  ),
  smiles = c("CC(C)C", "CCO", "CC(=O)O")
) {
  out <- tidytable::tidytable(rt = rt)
  if (!is.null(inchikey)) {
    out$inchikey <- inchikey
  }
  if (!is.null(smiles)) {
    out$smiles <- smiles
  }
  out
}

call_prepare_libraries_rt <- function(
  ...,
  dir = withr::local_tempdir(.local_envir = .local_envir),
  .local_envir = parent.frame()
) {
  args <- list(
    mgf_exp = NULL,
    mgf_is = NULL,
    temp_exp = NULL,
    temp_is = NULL,
    unit_rt = "minutes",
    name_rt = "rt",
    name_inchikey = "inchikey",
    name_smiles = "smiles",
    name_name = "structure_name",
    output_rt = file.path(dir, "rt_library.tsv"),
    output_sop = file.path(dir, "sop.tsv")
  )
  overrides <- list(...)
  if (length(overrides) > 0L) {
    args[names(overrides)] <- overrides
  }
  do.call(prepare_libraries_rt, args)
}

## Input Validation ----

test_that("prepare_libraries_rt validates unit_rt parameter", {
  # Invalid unit should error
  expect_error(
    call_prepare_libraries_rt(unit_rt = "hours"),
    "unit_rt",
    class = "tima_validation_error"
  )

  # Empty string should error
  expect_error(
    call_prepare_libraries_rt(unit_rt = ""),
    "unit_rt",
    class = "tima_validation_error"
  )
})

test_that("prepare_libraries_rt validates output_rt parameter", {
  # Non-character should error
  expect_error(
    call_prepare_libraries_rt(output_rt = 123),
    "output_rt",
    class = "tima_validation_error"
  )

  # Vector should error
  expect_error(
    call_prepare_libraries_rt(output_rt = c("file1.tsv", "file2.tsv")),
    "output_rt",
    class = "tima_validation_error"
  )
})

test_that("prepare_libraries_rt validates output_sop parameter", {
  # Non-character should error
  expect_error(
    call_prepare_libraries_rt(output_sop = NULL),
    "output_sop",
    class = "tima_validation_error"
  )
})

## Edge Cases and Empty Input ----

test_that("prepare_libraries_rt handles all NULL inputs", {
  # Should handle all NULL inputs gracefully
  result <- call_prepare_libraries_rt()

  # Should return paths
  expect_type(result, "character")
  expect_named(result, c("rt", "sop"))

  # Files should exist
  expect_true(file.exists(result["rt"]))
  expect_true(file.exists(result["sop"]))

  # Should contain empty/minimal structure
  rt_table <- tidytable::fread(result["rt"])
  sop_table <- tidytable::fread(result["sop"])

  expect_s3_class(rt_table, "data.frame")
  expect_s3_class(sop_table, "data.frame")

  # Check expected columns exist
  expect_true("rt" %in% colnames(rt_table))
  expect_true("structure_inchikey" %in% colnames(sop_table))
})

test_that("prepare_libraries_rt handles empty character vectors", {
  # Empty character vectors should be treated as NULL
  result <- call_prepare_libraries_rt(
    mgf_exp = character(0),
    mgf_is = character(0),
    temp_exp = character(0),
    temp_is = character(0)
  )

  expect_type(result, "character")
  expect_true(file.exists(result["rt"]))
  expect_true(file.exists(result["sop"]))
})

test_that("prepare_libraries_rt handles NA in input vectors", {
  # NA values should be filtered out
  result <- call_prepare_libraries_rt(
    mgf_exp = c(NA_character_, NA_character_),
    mgf_is = NA_character_,
    temp_exp = c(NA, "")
  )

  expect_type(result, "character")
  expect_true(file.exists(result["rt"]))
})

# Functional Tests - CSV Input ----

test_that("prepare_libraries_rt processes CSV with experimental RT", {
  dir <- withr::local_tempdir()
  csv_exp <- file.path(dir, "exp_rt.csv")
  export_output(x = make_rt_df(), file = csv_exp)

  result <- call_prepare_libraries_rt(temp_exp = csv_exp, dir = dir)

  expect_type(result, "character")
  expect_named(result, c("rt", "sop"))

  # Check RT library
  rt_table <- tidytable::fread(result["rt"], colClasses = "character")
  expect_true(nrow(rt_table) > 0)
  expect_true("rt" %in% colnames(rt_table))
  expect_true(
    "candidate_structure_inchikey_connectivity_layer" %in% colnames(rt_table)
  )
  expect_true("type" %in% colnames(rt_table))

  # Check all entries are marked as experimental
  expect_true(all(rt_table$type == "experimental"))

  # Check SOP table
  sop_table <- tidytable::fread(result["sop"], colClasses = "character")
  expect_true(nrow(sop_table) > 0)
  expect_true("structure_inchikey" %in% colnames(sop_table))
  expect_true("structure_smiles" %in% colnames(sop_table))
  expect_true("structure_inchikey_connectivity_layer" %in% colnames(sop_table))
})

test_that("prepare_libraries_rt processes CSV with predicted RT", {
  dir <- withr::local_tempdir()
  csv_is <- file.path(dir, "predicted_rt.csv")
  export_output(x = make_rt_df(rt = c(4.5, 6.2, 9.8)), file = csv_is)

  result <- call_prepare_libraries_rt(temp_is = csv_is, dir = dir)

  # Check RT library
  rt_table <- tidytable::fread(result["rt"], colClasses = "character")
  expect_true(nrow(rt_table) > 0)

  # Check all entries are marked as predicted
  expect_true(all(rt_table$type == "predicted"))
})

test_that("prepare_libraries_rt converts seconds to minutes", {
  dir <- withr::local_tempdir()
  csv_exp <- file.path(dir, "exp_rt_seconds.csv")
  export_output(x = make_rt_df(rt = c(312, 468, 618)), file = csv_exp)

  result <- call_prepare_libraries_rt(
    temp_exp = csv_exp,
    unit_rt = "seconds", # Input in seconds
    dir = dir
  )

  # Check RT library
  rt_table <- tidytable::fread(result["rt"], colClasses = "character")
  expect_true(nrow(rt_table) > 0)

  # RTs should be converted to minutes (divided by 60)
  rt_values <- as.numeric(rt_table$rt)
  expect_true(all(rt_values < 20)) # Should be in minutes now
  expect_true(all(rt_values > 1))

  # Check approximate conversion
  expect_true(any(abs(rt_values - 5.2) < 0.1))
})

test_that("prepare_libraries_rt filters out invalid entries", {
  dir <- withr::local_tempdir()
  csv_exp <- file.path(dir, "exp_mixed.csv")
  exp_data <- make_rt_df(
    rt = c(5.2, NA, 7.8, -1, 10.3),
    inchikey = c(
      "LFQSCWFLJHTTHZ-UHFFFAOYSA-N",
      "RYYVLZVUVIJVGH-UHFFFAOYSA-N",
      NA,
      "GHVNFZFCNZKVNT-UHFFFAOYSA-N",
      "ABCDEFGHIJKLMN-OPQRSTUVWX-Y"
    ),
    smiles = c("CC(C)C", "CCO", "CC(=O)O", "CCC", "CCCC")
  )
  export_output(x = exp_data, file = csv_exp)

  result <- call_prepare_libraries_rt(temp_exp = csv_exp, dir = dir)

  # Check RT library - should filter out NA rt and NA inchikey
  rt_table <- tidytable::fread(result["rt"], colClasses = "character")

  # Should have fewer entries than input (filtered out NAs)
  expect_true(nrow(rt_table) < 5)

  # No NA values in rt or inchikey columns
  expect_true(!anyNA(rt_table$rt))
  expect_true(!anyNA(rt_table$candidate_structure_inchikey_connectivity_layer))
})

# Functional Tests - Combined Sources ----

test_that("prepare_libraries_rt combines experimental and predicted data", {
  dir <- withr::local_tempdir()
  csv_exp <- file.path(dir, "exp_rt.csv")
  export_output(
    x = make_rt_df(
      rt = c(5.2, 7.8),
      inchikey = c(
        "LFQSCWFLJHTTHZ-UHFFFAOYSA-N",
        "RYYVLZVUVIJVGH-UHFFFAOYSA-N"
      ),
      smiles = c("CC(C)C", "CCO")
    ),
    file = csv_exp
  )

  csv_is <- file.path(dir, "pred_rt.csv")
  export_output(
    x = make_rt_df(
      rt = c(10.3, 12.5),
      inchikey = c(
        "GHVNFZFCNZKVNT-UHFFFAOYSA-N",
        "XLYOFNOQVPJJNP-UHFFFAOYSA-M"
      ),
      smiles = c("CC(=O)O", "CCCC")
    ),
    file = csv_is
  )

  result <- call_prepare_libraries_rt(
    temp_exp = csv_exp,
    temp_is = csv_is,
    dir = dir
  )

  # Check RT library combines both sources
  rt_table <- tidytable::fread(result["rt"], colClasses = "character")
  expect_true(nrow(rt_table) >= 4) # At least 2 exp + 2 pred

  # Should have both experimental and predicted types
  expect_true("experimental" %in% rt_table$type)
  expect_true("predicted" %in% rt_table$type)

  # Check SOP table has all structures
  sop_table <- tidytable::fread(result["sop"], colClasses = "character")
  expect_true(nrow(sop_table) >= 4)
})

test_that("prepare_libraries_rt handles duplicate InChIKeys", {
  dir <- withr::local_tempdir()
  csv_exp <- file.path(dir, "duplicates.csv")
  export_output(
    x = make_rt_df(
      rt = c(5.2, 5.3, 7.8), # Same structure with slightly different RTs
      inchikey = c(
        "LFQSCWFLJHTTHZ-UHFFFAOYSA-N",
        "LFQSCWFLJHTTHZ-UHFFFAOYSA-N",
        "RYYVLZVUVIJVGH-UHFFFAOYSA-N"
      ),
      smiles = c("CC(C)C", "CC(C)C", "CCO")
    ),
    file = csv_exp
  )

  result <- call_prepare_libraries_rt(temp_exp = csv_exp, dir = dir)

  # RT library can have multiple RTs per structure
  rt_table <- tidytable::fread(result["rt"], colClasses = "character")
  expect_true(nrow(rt_table) >= 2)

  # SOP table should have unique structures
  sop_table <- tidytable::fread(result["sop"], colClasses = "character")
  unique_structures <- unique(sop_table$structure_inchikey_connectivity_layer)
  expect_equal(length(unique_structures), nrow(sop_table))
})

# Output Structure ----

test_that("prepare_libraries_rt creates correct output structure", {
  dir <- withr::local_tempdir()
  csv_exp <- file.path(dir, "simple_rt.csv")
  export_output(
    x = make_rt_df(
      rt = c(5.2),
      inchikey = c("LFQSCWFLJHTTHZ-UHFFFAOYSA-N"),
      smiles = c("CC(C)C")
    ),
    file = csv_exp
  )

  result <- call_prepare_libraries_rt(temp_exp = csv_exp, dir = dir)

  # Check RT library columns
  rt_table <- tidytable::fread(result["rt"], colClasses = "character")
  required_rt_cols <- c(
    "rt",
    "candidate_structure_inchikey_connectivity_layer",
    "type"
  )
  expect_required_columns(rt_table, required_rt_cols)

  # Check SOP table columns
  sop_table <- tidytable::fread(result["sop"], colClasses = "character")
  required_sop_cols <- c(
    "structure_smiles",
    "structure_inchikey",
    "structure_inchikey_connectivity_layer",
    "organism_name"
  )
  expect_required_columns(sop_table, required_sop_cols)

  # organism_name should be NA (pseudo SOP)
  expect_true(all(is.na(sop_table$organism_name)))
})

# Performance ----

test_that("prepare_libraries_rt handles moderate-scale data efficiently", {
  dir <- withr::local_tempdir()
  csv_exp <- file.path(dir, "large_rt.csv")

  n_entries <- 500
  exp_data <- make_rt_df(
    rt = runif(n_entries, 1, 30),
    inchikey = generate_fake_inchikey(n_entries, seed = 777),
    smiles = replicate(
      n_entries,
      paste(
        sample(c("C", "O", "N", "=", "-"), 10, TRUE),
        collapse = ""
      )
    )
  )
  export_output(x = exp_data, file = csv_exp)

  # Should complete in reasonable time (<3 seconds)
  start_time <- Sys.time()

  result <- call_prepare_libraries_rt(temp_exp = csv_exp, dir = dir)

  elapsed_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

  expect_true(
    elapsed_time < 3,
    info = sprintf(
      "Processing took %.2f seconds (expected < 3s)",
      elapsed_time
    )
  )

  # Verify output
  rt_table <- tidytable::fread(result["rt"])
  expect_true(nrow(rt_table) > 0)
  expect_true(nrow(rt_table) <= n_entries)
})

test_that("prepare_libraries_rt validates RT unit and outputs", {
  withr::local_dir(new = temp_test_dir("prep_lib_rt_validate"))
  # Invalid unit
  expect_error(
    prepare_libraries_rt(unit_rt = "hours"),
    "must be 'seconds' or 'minutes'"
  )

  # Output paths must be single strings
  expect_error(prepare_libraries_rt(output_rt = c("a", "b")), "single")
  expect_error(prepare_libraries_rt(output_sop = c("a", "b")), "single")
})

test_that("prepare_libraries_rt handles missing InChIKeys", {
  dir <- withr::local_tempdir()
  csv_exp <- file.path(dir, "exp_no_ik.csv")
  export_output(
    x = make_rt_df(
      rt = c(5.2, 7.8),
      inchikey = NULL,
      smiles = c("CC(C)C", "CCO")
    ),
    file = csv_exp
  )

  # Should warn about missing InChIKeys
  # TODO the WARN is as log, not as real warning
  expect_no_error({
    result <- call_prepare_libraries_rt(temp_exp = csv_exp, dir = dir)
  })

  expect_type(result, "character")
})

test_that("prepare_libraries_rt keeps MGF entries without peaks or precursor", {
  dir <- withr::local_tempdir()
  mgf_exp <- file.path(dir, "exp_rt.mgf")
  write_test_mgf(
    c(
      format_mgf_block(
        pepmass = "47.05",
        peaks = character(0),
        charge = NULL,
        extra_headers = c(
          "RTINSECONDS=312",
          "SMILES=CCO",
          "INCHIKEY=LFQSCWFLJHTTHZ-UHFFFAOYSA-N"
        )
      ),
      format_mgf_block(
        peaks = "43.0 100",
        charge = NULL,
        extra_headers = c(
          "RTINSECONDS=468",
          "SMILES=CC(=O)O",
          "INCHIKEY=QTBSBXVTEAMEQO-UHFFFAOYSA-N"
        )
      )
    ),
    mgf_exp
  )

  # `unit_rt` is ignored for RTINSECONDS, which is always in seconds
  result <- call_prepare_libraries_rt(
    mgf_exp = mgf_exp,
    col_ik = "INCHIKEY",
    col_na = "NAME",
    col_rt = "RTINSECONDS",
    col_sm = "SMILES",
    dir = dir
  )

  rt_table <- tidytable::fread(result["rt"], colClasses = "character")
  expect_equal(sort(as.numeric(rt_table$rt)), c(5.2, 7.8))
})

test_that("read_mgf_metadata reads every block's fields", {
  dir <- withr::local_tempdir()
  mgf <- file.path(dir, "meta.mgf")
  write_test_mgf(
    c(
      format_mgf_block(
        title = "a",
        peaks = character(0),
        charge = NULL,
        extra_headers = "rtinseconds=1"
      ),
      format_mgf_block(title = "b", peaks = "10.0 5", charge = NULL)
    ),
    mgf
  )
  meta <- read_mgf_metadata(mgf)
  expect_equal(meta$TITLE, c("a", "b"))
  expect_equal(meta$RTINSECONDS, c("1", NA))

  # Predicted MGF as list, missing rt/smiles columns, and unparseable SMILES without InChIKey
  mgf_is_file <- file.path(dir, "is_rt.mgf")
  write_test_mgf(
    format_mgf_block(
      peaks = character(0),
      charge = NULL,
      extra_headers = c("RTINSECONDS=120", "SMILES=invalid_smiles_xyz")
    ),
    mgf_is_file
  )
  csv_no_rt <- file.path(dir, "no_rt_col.csv")
  writeLines("other_col\n1", csv_no_rt)
  out_rt_edge <- file.path(dir, "rt_edge.tsv")
  out_sop_edge <- file.path(dir, "sop_edge.tsv")
  res_edge <- call_prepare_libraries_rt(
    mgf_is = list(mgf_is_file),
    temp_exp = list(csv_no_rt),
    col_ik = "INCHIKEY",
    col_na = "NAME",
    col_rt = "rtime",
    col_sm = "smiles",
    output_rt = out_rt_edge,
    output_sop = out_sop_edge
  )
  expect_true(file.exists(res_edge["rt"]))
})
