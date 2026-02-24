test_that("mzTab utility functions work correctly", {
  # Test adduct parsing ----
  test_that("mzTab adduct notation is parsed correctly", {
    # mzTab format: "[M+H]1+" → TIMA format: "[M+H]+"
    result <- tima:::.parse_mztab_adduct("[M+H]1+")
    expect_equal(result, "[M+H]+")

    result <- tima:::.parse_mztab_adduct("[M+Na]1+")
    expect_equal(result, "[M+Na]+")

    result <- tima:::.parse_mztab_adduct("[M-H]1-")
    expect_equal(result, "[M-H]-")

    # Handle NULL/NA
    result <- tima:::.parse_mztab_adduct(NA_character_)
    expect_true(is.na(result))

    result <- tima:::.parse_mztab_adduct("null")
    expect_true(is.na(result))
  })

  # Test TIMA to mzTab adduct conversion ----
  test_that("TIMA adduct notation is converted to mzTab format", {
    # TIMA format: "[M+H]+" → mzTab format: "[M+H]1+"
    result <- tima:::.tima_adduct_to_mztab("[M+H]+")
    expect_equal(result, "[M+H]1+")

    result <- tima:::.tima_adduct_to_mztab("[M+Na]+")
    expect_equal(result, "[M+Na]1+")

    result <- tima:::.tima_adduct_to_mztab("[M-H]-")
    expect_equal(result, "[M-H]1-")

    # Handle NULL/NA
    result <- tima:::.tima_adduct_to_mztab(NA_character_)
    expect_true(is.na(result))
  })

  # Test column mapping ----
  test_that("SML columns are mapped correctly to TIMA features", {
    # Create mock SML data
    sml_data <- data.frame(
      SML_ID = c("1", "2", "3"),
      exp_mass_to_charge = c(181.0501, 342.1162, 256.0847),
      retention_time_in_seconds = c(120.5, 245.8, 180.3),
      adduct_ions = c("[M+H]1+", "[M+Na]1+", "[M-H]1-"),
      abundance_assay_1 = c(1000000, 500000, 750000),
      abundance_assay_2 = c(950000, 480000, 720000),
      stringsAsFactors = FALSE
    )

    result <- tima:::.map_mztab_sml_to_features(sml_data)

    # Check feature IDs
    expect_equal(result$feature_id, c("1", "2", "3"))

    # Check m/z values
    expect_equal(result$mz, c(181.0501, 342.1162, 256.0847))

    # Check retention times (should be converted to minutes)
    expect_equal(result$rt, c(120.5 / 60, 245.8 / 60, 180.3 / 60))

    # Check adducts (should be harmonized)
    expect_equal(result$adduct, c("[M+H]+", "[M+Na]+", "[M-H]-"))

    # Check abundance columns are preserved
    expect_true("abundance_assay_1" %in% names(result))
    expect_true("abundance_assay_2" %in% names(result))
  })

  # Test metadata extraction ----
  test_that("Metadata is extracted from MTD section", {
    # Create mock metadata
    mtd_data <- data.frame(
      V1 = c("sample[1]", "sample[2]", "assay[1]", "assay[2]"),
      V2 = c("Sample_A", "Sample_B", "Assay_1", "Assay_2"),
      stringsAsFactors = FALSE
    )

    result <- tima:::.extract_mztab_metadata(mtd_data)

    # Check samples extracted
    expect_true("samples" %in% names(result))
    expect_equal(nrow(result$samples), 2)

    # Check assays extracted
    expect_true("assays" %in% names(result))
    expect_equal(nrow(result$assays), 2)
  })

  # Test mzTab structure validation ----
  test_that("mzTab structure validation catches errors", {
    # Missing SML table should error
    mztab_obj <- list(
      Metadata = data.frame(key = "test", value = "test")
    )

    expect_error(
      tima:::.validate_mztab_structure(mztab_obj),
      "Missing required Small Molecule table"
    )

    # Valid mzTab should pass without error
    mztab_obj <- list(
      Metadata = data.frame(key = "test", value = "test"),
      Small_Molecule = data.frame(
        SML_ID = c("1", "2"),
        exp_mass_to_charge = c(181.05, 342.11)
      )
    )

    expect_no_error(tima:::.validate_mztab_structure(mztab_obj))
  })
})
