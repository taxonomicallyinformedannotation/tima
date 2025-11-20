# Test Suite: prepare_libraries_rt ----

library(testthat)

## Input Validation ----

test_that("prepare_libraries_rt validates unit_rt parameter", {
  # Invalid unit should error
  expect_error(
    prepare_libraries_rt(
      mgf_exp = NULL,
      mgf_is = NULL,
      temp_exp = NULL,
      temp_is = NULL,
      unit_rt = "hours",
      output_rt = file.path(tempdir(), "rt.tsv"),
      output_sop = file.path(tempdir(), "sop.tsv")
    ),
    "unit_rt must be 'seconds' or 'minutes'"
  )

  # Empty string should error
  expect_error(
    prepare_libraries_rt(
      mgf_exp = NULL,
      mgf_is = NULL,
      temp_exp = NULL,
      temp_is = NULL,
      unit_rt = "",
      output_rt = file.path(tempdir(), "rt.tsv"),
      output_sop = file.path(tempdir(), "sop.tsv")
    ),
    "unit_rt must be 'seconds' or 'minutes'"
  )
})

test_that("prepare_libraries_rt validates output_rt parameter", {
  # Non-character should error
  expect_error(
    prepare_libraries_rt(
      mgf_exp = NULL,
      mgf_is = NULL,
      temp_exp = NULL,
      temp_is = NULL,
      unit_rt = "minutes",
      output_rt = 123,
      output_sop = file.path(tempdir(), "sop.tsv")
    ),
    "output_rt must be a single character string"
  )

  # Vector should error
  expect_error(
    prepare_libraries_rt(
      mgf_exp = NULL,
      mgf_is = NULL,
      temp_exp = NULL,
      temp_is = NULL,
      unit_rt = "minutes",
      output_rt = c("file1.tsv", "file2.tsv"),
      output_sop = file.path(tempdir(), "sop.tsv")
    ),
    "output_rt must be a single character string"
  )
})

test_that("prepare_libraries_rt validates output_sop parameter", {
  # Non-character should error
  expect_error(
    prepare_libraries_rt(
      mgf_exp = NULL,
      mgf_is = NULL,
      temp_exp = NULL,
      temp_is = NULL,
      unit_rt = "minutes",
      output_rt = file.path(tempdir(), "rt.tsv"),
      output_sop = NULL
    ),
    "output_sop must be a single character string"
  )
})

## Edge Cases and Empty Input ----

test_that("prepare_libraries_rt handles all NULL inputs", {
  output_rt <- file.path(tempdir(), "rt_library.tsv")
  output_sop <- file.path(tempdir(), "sop.tsv")

  # Should handle all NULL inputs gracefully
  result <- prepare_libraries_rt(
    mgf_exp = NULL,
    mgf_is = NULL,
    temp_exp = NULL,
    temp_is = NULL,
    unit_rt = "minutes",
    output_rt = output_rt,
    output_sop = output_sop
  )

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
  output_rt <- file.path(tempdir(), "rt_library.tsv")
  output_sop <- file.path(tempdir(), "sop.tsv")

  # Empty character vectors should be treated as NULL
  result <- prepare_libraries_rt(
    mgf_exp = character(0),
    mgf_is = character(0),
    temp_exp = character(0),
    temp_is = character(0),
    unit_rt = "minutes",
    output_rt = output_rt,
    output_sop = output_sop
  )

  expect_type(result, "character")
  expect_true(file.exists(result["rt"]))
  expect_true(file.exists(result["sop"]))
})

test_that("prepare_libraries_rt handles NA in input vectors", {
  output_rt <- file.path(tempdir(), "rt_library.tsv")
  output_sop <- file.path(tempdir(), "sop.tsv")

  # NA values should be filtered out
  result <- prepare_libraries_rt(
    mgf_exp = c(NA_character_, NA_character_),
    mgf_is = NA_character_,
    temp_exp = c(NA, ""),
    temp_is = NULL,
    unit_rt = "minutes",
    output_rt = output_rt,
    output_sop = output_sop
  )

  expect_type(result, "character")
  expect_true(file.exists(result["rt"]))
})

# Functional Tests - CSV Input ----

test_that("prepare_libraries_rt processes CSV with experimental RT", {
  # Create experimental CSV with RT data
  csv_exp <- file.path(tempdir(), "exp_rt.csv")

  exp_data <- tidytable::tidytable(
    rt = c(5.2, 7.8, 10.3),
    inchikey = c(
      "LFQSCWFLJHTTHZ-UHFFFAOYSA-N",
      "RYYVLZVUVIJVGH-UHFFFAOYSA-N",
      "GHVNFZFCNZKVNT-UHFFFAOYSA-N"
    ),
    smiles = c("CC(C)C", "CCO", "CC(=O)O")
  )
  export_output(x = exp_data, file = csv_exp)

  output_rt <- file.path(tempdir(), "rt_library.tsv")
  output_sop <- file.path(tempdir(), "sop.tsv")

  result <- prepare_libraries_rt(
    mgf_exp = NULL,
    mgf_is = NULL,
    temp_exp = csv_exp,
    temp_is = NULL,
    unit_rt = "minutes",
    name_rt = "rt",
    name_inchikey = "inchikey",
    name_smiles = "smiles",
    name_name = "structure_name",
    output_rt = output_rt,
    output_sop = output_sop
  )

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
  # Create predicted CSV with RT data
  csv_is <- file.path(tempdir(), "predicted_rt.csv")

  is_data <- tidytable::tidytable(
    rt = c(4.5, 6.2, 9.8),
    inchikey = c(
      "LFQSCWFLJHTTHZ-UHFFFAOYSA-N",
      "RYYVLZVUVIJVGH-UHFFFAOYSA-N",
      "GHVNFZFCNZKVNT-UHFFFAOYSA-N"
    ),
    smiles = c("CC(C)C", "CCO", "CC(=O)O")
  )
  export_output(x = is_data, file = csv_is)

  output_rt <- file.path(tempdir(), "rt_library.tsv")
  output_sop <- file.path(tempdir(), "sop.tsv")

  result <- prepare_libraries_rt(
    mgf_exp = NULL,
    mgf_is = NULL,
    temp_exp = NULL,
    temp_is = csv_is,
    unit_rt = "minutes",
    name_rt = "rt",
    name_inchikey = "inchikey",
    name_smiles = "smiles",
    name_name = "structure_name",
    output_rt = output_rt,
    output_sop = output_sop
  )

  # Check RT library
  rt_table <- tidytable::fread(result["rt"], colClasses = "character")
  expect_true(nrow(rt_table) > 0)

  # Check all entries are marked as predicted
  expect_true(all(rt_table$type == "predicted"))
})

test_that("prepare_libraries_rt converts seconds to minutes", {
  # Create CSV with RT in seconds
  csv_exp <- file.path(tempdir(), "exp_rt_seconds.csv")

  exp_data <- tidytable::tidytable(
    rt = c(312, 468, 618), # 5.2, 7.8, 10.3 minutes in seconds
    inchikey = c(
      "LFQSCWFLJHTTHZ-UHFFFAOYSA-N",
      "RYYVLZVUVIJVGH-UHFFFAOYSA-N",
      "GHVNFZFCNZKVNT-UHFFFAOYSA-N"
    ),
    smiles = c("CC(C)C", "CCO", "CC(=O)O")
  )
  export_output(x = exp_data, file = csv_exp)

  output_rt <- file.path(tempdir(), "rt_library.tsv")
  output_sop <- file.path(tempdir(), "sop.tsv")

  result <- prepare_libraries_rt(
    mgf_exp = NULL,
    mgf_is = NULL,
    temp_exp = csv_exp,
    temp_is = NULL,
    unit_rt = "seconds", # Input in seconds
    name_rt = "rt",
    name_inchikey = "inchikey",
    name_smiles = "smiles",
    name_name = "structure_name",
    output_rt = output_rt,
    output_sop = output_sop
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
  # Setup temporary test environment

  # Create CSV with some invalid entries
  csv_exp <- file.path(tempdir(), "exp_mixed.csv")

  exp_data <- tidytable::tidytable(
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

  output_rt <- file.path(tempdir(), "rt_library.tsv")
  output_sop <- file.path(tempdir(), "sop.tsv")

  result <- prepare_libraries_rt(
    mgf_exp = NULL,
    mgf_is = NULL,
    temp_exp = csv_exp,
    temp_is = NULL,
    unit_rt = "minutes",
    name_rt = "rt",
    name_inchikey = "inchikey",
    name_smiles = "smiles",
    name_name = "structure_name",
    output_rt = output_rt,
    output_sop = output_sop
  )

  # Check RT library - should filter out NA rt and NA inchikey
  rt_table <- tidytable::fread(result["rt"], colClasses = "character")

  # Should have fewer entries than input (filtered out NAs)
  expect_true(nrow(rt_table) < 5)

  # No NA values in rt or inchikey columns
  expect_true(all(!is.na(rt_table$rt)))
  expect_true(all(
    !is.na(rt_table$candidate_structure_inchikey_connectivity_layer)
  ))
})

# Functional Tests - Combined Sources ----

test_that("prepare_libraries_rt combines experimental and predicted data", {
  # Setup temporary test environment

  # Create experimental CSV
  csv_exp <- file.path(tempdir(), "exp_rt.csv")

  exp_data <- tidytable::tidytable(
    rt = c(5.2, 7.8),
    inchikey = c(
      "LFQSCWFLJHTTHZ-UHFFFAOYSA-N",
      "RYYVLZVUVIJVGH-UHFFFAOYSA-N"
    ),
    smiles = c("CC(C)C", "CCO")
  )
  export_output(x = exp_data, file = csv_exp)

  # Create predicted CSV
  csv_is <- file.path(tempdir(), "pred_rt.csv")
  is_data <- tidytable::tidytable(
    rt = c(10.3, 12.5),
    inchikey = c(
      "GHVNFZFCNZKVNT-UHFFFAOYSA-N",
      "XLYOFNOQVPJJNP-UHFFFAOYSA-M"
    ),
    smiles = c("CC(=O)O", "CCCC")
  )
  export_output(x = is_data, file = csv_is)

  output_rt <- file.path(tempdir(), "rt_library.tsv")
  output_sop <- file.path(tempdir(), "sop.tsv")

  result <- prepare_libraries_rt(
    mgf_exp = NULL,
    mgf_is = NULL,
    temp_exp = csv_exp,
    temp_is = csv_is,
    unit_rt = "minutes",
    name_rt = "rt",
    name_inchikey = "inchikey",
    name_smiles = "smiles",
    name_name = "structure_name",
    output_rt = output_rt,
    output_sop = output_sop
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
  # Setup temporary test environment

  # Create CSV with duplicate structures but different RTs
  csv_exp <- file.path(tempdir(), "duplicates.csv")

  exp_data <- tidytable::tidytable(
    rt = c(5.2, 5.3, 7.8), # Same structure with slightly different RTs
    inchikey = c(
      "LFQSCWFLJHTTHZ-UHFFFAOYSA-N",
      "LFQSCWFLJHTTHZ-UHFFFAOYSA-N",
      "RYYVLZVUVIJVGH-UHFFFAOYSA-N"
    ),
    smiles = c("CC(C)C", "CC(C)C", "CCO")
  )
  export_output(x = exp_data, file = csv_exp)

  file_rt <- file.path(tempdir(), "rt_library.tsv")
  file_sop <- file.path(tempdir(), "sop.tsv")

  result <- prepare_libraries_rt(
    mgf_exp = NULL,
    mgf_is = NULL,
    temp_exp = csv_exp,
    temp_is = NULL,
    unit_rt = "minutes",
    name_rt = "rt",
    name_inchikey = "inchikey",
    name_name = "structure_name",
    name_smiles = "smiles",
    output_rt = file_rt,
    output_sop = file_sop
  )

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
  # Setup temporary test environment

  # Create simple CSV
  csv_exp <- file.path(tempdir(), "simple_rt.csv")

  exp_data <- tidytable::tidytable(
    rt = c(5.2),
    inchikey = c("LFQSCWFLJHTTHZ-UHFFFAOYSA-N"),
    smiles = c("CC(C)C")
  )
  export_output(x = exp_data, file = csv_exp)

  output_rt <- file.path(tempdir(), "rt_library.tsv")
  output_sop <- file.path(tempdir(), "sop.tsv")

  result <- prepare_libraries_rt(
    mgf_exp = NULL,
    mgf_is = NULL,
    temp_exp = csv_exp,
    temp_is = NULL,
    unit_rt = "minutes",
    name_rt = "rt",
    name_inchikey = "inchikey",
    name_smiles = "smiles",
    name_name = "structure_name",
    output_rt = output_rt,
    output_sop = output_sop
  )

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
  # Create CSV with 500 RT entries
  csv_exp <- file.path(tempdir(), "large_rt.csv")

  n_entries <- 500
  exp_data <- tidytable::tidytable(
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

  output_rt <- file.path(tempdir(), "rt_library.tsv")
  output_sop <- file.path(tempdir(), "sop.tsv")

  # Should complete in reasonable time (<3 seconds)
  start_time <- Sys.time()

  result <- prepare_libraries_rt(
    mgf_exp = NULL,
    mgf_is = NULL,
    temp_exp = csv_exp,
    temp_is = NULL,
    unit_rt = "minutes",
    name_rt = "rt",
    name_inchikey = "inchikey",
    name_smiles = "smiles",
    name_name = "structure_name",
    output_rt = output_rt,
    output_sop = output_sop
  )

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
  skip("API-dependent test - requires naturalproducts.net")

  # Setup temporary test environment

  # Create CSV with SMILES but no InChIKey
  csv_exp <- file.path(tempdir(), "exp_no_ik.csv")

  exp_data <- tidytable::tidytable(
    rt = c(5.2, 7.8),
    smiles = c("CC(C)C", "CCO")
  )
  export_output(x = exp_data, file = csv_exp)

  output_rt <- file.path(tempdir(), "rt_library.tsv")
  output_sop <- file.path(tempdir(), "sop.tsv")

  # Should warn about missing InChIKeys
  expect_warning(
    {
      result <- prepare_libraries_rt(
        mgf_exp = NULL,
        mgf_is = NULL,
        temp_exp = csv_exp,
        temp_is = NULL,
        unit_rt = "minutes",
        name_rt = "rt",
        name_inchikey = "inchikey",
        name_smiles = "smiles",
        name_name = "structure_name",
        output_rt = output_rt,
        output_sop = output_sop
      )
    },
    "entries without InChIKey"
  )

  expect_type(result, "character")
})

# test_that(
#   skip("Not implemented")
# )
# test_that("prepare_libraries_rt works with experimental data", {
#   local_test_project(copy = TRUE)
#   paths <- get_default_paths()
#
#   get_file(
#     url = paths$urls$examples$spectral_lib_mini$with_rt,
#     export = paths$data$source$libraries$spectra$exp$with_rt
#   )
#   get_file(
#     url = paths$urls$examples$lib_mini$rt,
#     export = paths$data$source$libraries$rt$example_mini
#   )
#
#   expect_no_error(
#     prepare_libraries_rt(
#       mgf_exp = list(pos = paths$data$source$libraries$spectra$exp$with_rt),
#       temp_exp = paths$data$source$libraries$rt$example_mini
#     )
#   )
# })

# test_that(
#   skip("Not implemented")
# )
# test_that("prepare_libraries_rt works with in-silico data", {
#   local_test_project(copy = TRUE)
#   paths <- get_default_paths()
#
#   get_file(
#     url = paths$urls$examples$spectral_lib_mini$with_rt,
#     export = paths$data$source$libraries$spectra$exp$with_rt
#   )
#   get_file(
#     url = paths$urls$examples$lib_mini$rt,
#     export = paths$data$source$libraries$rt$example_mini
#   )
#
#   expect_no_error(
#     prepare_libraries_rt(
#       mgf_is = list(pos = paths$data$source$libraries$spectra$exp$with_rt),
#       temp_is = paths$data$source$libraries$rt$example_mini
#     )
#   )
# })

# test_that(
#   skip("Not implemented")
# )
# test_that("prepare_libraries_rt warns on invalid SMILES", {
#   local_test_project(copy = TRUE)
#
#   tidytable::tidytable(
#     "rt" = 0.1,
#     "smiles" = "wrongSMILES",
#     "inchikey" = NA
#   ) |>
#     tidytable::fwrite("data/source/libraries/rt/example_bad.tsv")
#
#   expect_warning(
#     prepare_libraries_rt(
#       temp_exp = "data/source/libraries/rt/example_bad.tsv"
#     )
#   )
# })
