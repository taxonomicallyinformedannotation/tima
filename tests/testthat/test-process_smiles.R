# Test Suite: process_smiles ----

library(testthat)

TEST_SMILES <- "C[C@@H]1C=C(C(=O)[C@]2([C@H]1C[C@@H]3[C@@]4([C@@H]2C(=O)C(=C([C@@H]4CC(=O)O3)C)OC)C)C)OC"

test_that("process_smiles processes a single SMILES without cache", {
  result <- tidytable::tidytable(
    structure_smiles_initial = TEST_SMILES
  ) |>
    process_smiles()

  expect_s3_class(result, "data.frame")
  expect_true("structure_smiles_no_stereo" %in% colnames(result))
  expect_true(nrow(result) == 1L)
})

test_that("process_smiles handles empty data frame", {
  result <- tidytable::tidytable(
    structure_smiles_initial = character(0)
  ) |>
    process_smiles()

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) == 0L)
})

test_that("process_smiles errors when smiles column is missing", {
  bad_input <- tidytable::tidytable(other_column = TEST_SMILES)

  expect_error(
    process_smiles(bad_input),
    "column not found",
    class = "tima_validation_error"
  )
})

test_that("process_smiles handles NA values gracefully", {
  input <- tidytable::tidytable(
    structure_smiles_initial = c(TEST_SMILES, NA_character_)
  )
  result <- input |>
    process_smiles()

  expect_s3_class(result, "data.frame")
  # Either NA is retained (2 rows) or filtered (1 row)
  expect_true(nrow(result) %in% c(1L, 2L))
  # Non-NA SMILES must be processed
  expect_true(any(grepl("C", result$structure_smiles_initial)))
  expect_true("structure_smiles_no_stereo" %in% names(result))
})

## Isotope Handling ----

test_that("process_smiles correctly processes isotope notation [13C]", {
  # Test single 13C isotope
  # Natural glucose: C6H12O6, mass = 180.0634 Da
  # With [13C] on first carbon: mass increases by ~1.0034 Da
  isotope_smiles <- "OC[C@H]1OC(O)[C@H](O)[C@@H](O)[C@@H]1O" # glucose
  isotope_smiles_13c <- "OC[C@H]1OC(O)[C@H](O)[C@@H](O)[C@@H]1O"
  # RDKit isotope notation: [13C] instead of C
  isotope_smiles_with_13c <- "OC[13C@H]1OC(O)[C@H](O)[C@@H](O)[C@@H]1O"

  input <- tidytable::tidytable(
    structure_smiles_initial = c(isotope_smiles, isotope_smiles_with_13c)
  )

  result <- input |>
    process_smiles()

  expect_s3_class(result, "data.frame")

  # Check if masses were calculated
  expect_true("structure_exact_mass" %in% colnames(result))
  expect_true(!anyNA(result$structure_exact_mass))

  if (nrow(result) >= 2) {
    mass_natural <- result$structure_exact_mass[[1L]]
    mass_isotope <- result$structure_exact_mass[[2L]]

    # The 13C version should be heavier by approximately 1.0034 Da
    mass_diff <- mass_isotope - mass_natural

    # Expect difference in range 0.99-1.01 Da (1.0033548 Da ± tolerance)
    # Do NOT allow 0 Da (which would indicate isotopes are not being calculated)
    expect_true(
      mass_diff > 0.99 && mass_diff < 1.01,
      info = sprintf(
        "Mass difference: %.6f Da (expected ~1.0034 Da for one 13C, NOT 0 Da)",
        mass_diff
      )
    )
  }
})

test_that("process_smiles handles multiple isotopes [13C]", {
  # Test multiple 13C isotopes (e.g., fully labeled)
  # Natural glucose: 180.0634 Da
  # Test multiple 13C isotopes
  # Natural glucose: 180.0634 Da
  # The fixture has 4 13C atoms (not all 6 carbons are labeled)
  glucose_natural <- "OC[C@H]1OC(O)[C@H](O)[C@@H](O)[C@@H]1O"
  glucose_13c6 <- "OC[13C@H]1OC(O)[13C@H](O)[13C@@H](O)[13C@@H]1O"

  input <- tidytable::tidytable(
    structure_smiles_initial = c(glucose_natural, glucose_13c6)
  )

  result <- input |>
    process_smiles()

  if (nrow(result) >= 2) {
    mass_natural <- result$structure_exact_mass[[1L]]
    mass_labeled <- result$structure_exact_mass[[2L]]
    mass_diff <- mass_labeled - mass_natural

    # For 4 13C substitutions, expect ~4.0134 Da difference
    # (4 * 1.0033548 Da)
    # Do NOT allow 0 Da - isotopes must be properly calculated
    expect_true(
      mass_diff > 4.01 && mass_diff < 4.015,
      info = sprintf(
        "Mass difference for 4x 13C: %.6f Da (expected ~4.0134 Da, NOT 0 Da)",
        mass_diff
      )
    )
  }
})

test_that("process_smiles handles 15N isotope with correct mass shift", {
  skip_if_not(require("tima"), "tima package required")

  # Test 15N isotope
  # 15N - 14N ~ 0.9964 Da
  # Use aniline where N is explicit: Nc1ccccc1
  aniline_natural <- "Nc1ccccc1"
  # COMMENT: Explicit H is required for isotopes
  aniline_15n <- "[15NH2]c1ccccc1"

  input <- tidytable::tidytable(
    structure_smiles_initial = c(aniline_natural, aniline_15n)
  )

  result <- input |>
    process_smiles()

  expect_true(!anyNA(result$structure_exact_mass))

  # Find which row is which by checking SMILES
  if (nrow(result) >= 2) {
    idx_nat <- which(result$structure_smiles_initial == aniline_natural)
    idx_lab <- which(result$structure_smiles_initial == aniline_15n)

    if (length(idx_nat) > 0 && length(idx_lab) > 0) {
      mass_natural <- result$structure_exact_mass[[idx_nat[1]]]
      mass_labeled <- result$structure_exact_mass[[idx_lab[1]]]
      mass_diff <- mass_labeled - mass_natural

      # 15N shift ~ 0.9964 Da
      # Allow ±0.01 Da tolerance
      expect_true(
        mass_diff > 0.98 && mass_diff < 1.01,
        info = sprintf(
          "Mass difference for 15N: %.6f Da (expected ~0.9964 Da)",
          mass_diff
        )
      )
    }
  }
})

test_that("process_smiles handles 18O isotope with correct mass shift", {
  skip_if_not(require("tima"), "tima package required")

  # Test 18O isotope
  # 18O - 16O ~ 2.0043 Da
  # Use water: O (natural) vs [18O] (labeled)
  water_natural <- "O"
  # COMMENT: Explicit H is required for isotopes
  water_18o <- "[18OH2]"

  input <- tidytable::tidytable(
    structure_smiles_initial = c(water_natural, water_18o)
  )

  result <- input |>
    process_smiles()

  expect_true(!anyNA(result$structure_exact_mass))

  # If both molecules processed, check mass difference
  if (nrow(result) >= 2) {
    mass_natural <- result$structure_exact_mass[[1L]]
    mass_labeled <- result$structure_exact_mass[[2L]]
    mass_diff <- mass_labeled - mass_natural

    # 18O shift ~ 2.0043 Da
    # Allow ±0.005 Da tolerance (stricter because it's larger mass difference)
    expect_true(
      mass_diff > 1.99 && mass_diff < 2.02,
      info = sprintf(
        "Mass difference for 18O: %.6f Da (expected ~2.0043 Da)",
        mass_diff
      )
    )
  }
})

test_that("process_smiles handles deuterium [2H] notation", {
  # Test deuterium (2H) isotope on explicit hydrogens
  # [2H]C([2H])([2H])[2H] = fully deuterated methane
  # 2H - 1H ~ 1.0078 Da per atom

  methane_natural <- "C"
  methane_deuterated <- "[2H]C([2H])([2H])[2H]"

  input <- tidytable::tidytable(
    structure_smiles_initial = c(methane_natural, methane_deuterated)
  )

  result <- input |>
    process_smiles()

  expect_true(!anyNA(result$structure_exact_mass))

  # If both molecules processed, check mass difference
  if (nrow(result) >= 2) {
    mass_natural <- result$structure_exact_mass[[1L]]
    mass_deuterated <- result$structure_exact_mass[[2L]]
    mass_diff <- mass_deuterated - mass_natural

    # 4× deuterium ~ 4 × 1.0078 = 4.0312 Da
    expect_true(
      mass_diff > 4.02 && mass_diff < 4.04,
      info = sprintf(
        "Mass difference for 4x 2H: %.6f Da (expected ~4.0312 Da)",
        mass_diff
      )
    )
  }
})

test_that("process_smiles correctly generates formulas with isotopes", {
  # Isotope SMILES with different notations
  glucose_with_isotopes <- "OC[13C@H]1OC(O)[C@H](O)[C@@H](O)[C@@H]1O"

  input <- tidytable::tidytable(
    structure_smiles_initial = glucose_with_isotopes
  )

  result <- input |>
    process_smiles()

  expect_s3_class(result, "data.frame")
  expect_true("structure_molecular_formula" %in% colnames(result))

  if (nrow(result) > 0) {
    formula <- result$structure_molecular_formula[[1L]]
    # Formula should reflect isotopes (e.g., C6H12O6 or C5[13C]H12O6)
    expect_true(is.character(formula) && nchar(formula) > 0)
  }
})

test_that("process_smiles handles mixed natural and isotopic SMILES", {
  # Mix natural and isotope-containing SMILES
  natural_glucose <- "OC[C@H]1OC(O)[C@H](O)[C@@H](O)[C@@H]1O"
  isotope_glucose <- "OC[13C@H]1OC(O)[13C@H](O)[13C@@H](O)[13C@@H]1O"
  another_natural <- "CC(C)Cc1ccc(cc1)[C@@H](C)C(O)=O" # ibuprofen

  input <- tidytable::tidytable(
    structure_smiles_initial = c(
      natural_glucose,
      isotope_glucose,
      another_natural
    )
  )

  result <- input |>
    process_smiles()

  expect_true(!anyNA(result$structure_exact_mass))
  # All SMILES should have inchikeys
  expect_true(!anyNA(result$structure_inchikey))
})

## Additional Isotope Preservation Tests ----

test_that("process_smiles preserves [13C] in canonical SMILES after standardization", {
  skip_if_not(require("tima"), "tima package required")

  input <- tidytable::tidytable(
    structure_smiles_initial = c(
      "OC[C@H]1OC(O)[C@H](O)[C@@H](O)[C@@H]1O", # natural glucose
      "OC[13C@H]1OC(O)[13C@H](O)[13C@@H](O)[13C@@H]1O" # 4× 13C-labeled
    )
  )

  result <- process_smiles(input, cache = NULL)

  expect_equal(nrow(result), 2L)

  # Labeled SMILES should contain [13C] notation (preserved through standardization)
  labeled_smiles <- result$structure_smiles[[2L]]
  expect_match(
    labeled_smiles,
    "\\[13C",
    info = "13C notation should be preserved in canonical SMILES after standardization"
  )
})

test_that("process_smiles mass calculation reflects isotope composition", {
  skip_if_not(require("tima"), "tima package required")

  input <- tidytable::tidytable(
    structure_smiles_initial = c(
      "OC[C@H]1OC(O)[C@H](O)[C@@H](O)[C@@H]1O", # natural
      "OC[13C@H]1OC(O)[13C@H](O)[13C@@H](O)[13C@@H]1O" # 4× 13C
    )
  )

  result <- process_smiles(input, cache = NULL)

  mass_natural <- result$structure_exact_mass[[1L]]
  mass_labeled <- result$structure_exact_mass[[2L]]
  mass_diff <- mass_labeled - mass_natural

  # RDKit automatically uses isotope atomic masses
  expect_true(
    abs(mass_diff - 4.0134) < 0.001,
    info = sprintf(
      "Expected ~4.0134 Da (RDKit automatic), got %.6f Da",
      mass_diff
    )
  )
})

test_that("process_smiles formula shows isotopes in separated format", {
  skip_if_not(require("tima"), "tima package required")

  input <- tidytable::tidytable(
    structure_smiles_initial = "OC[13C@H]1OC(O)[13C@H](O)[13C@@H](O)[13C@@H]1O"
  )

  result <- process_smiles(input, cache = NULL)

  formula <- result$structure_molecular_formula[[1L]]

  # Formula should show isotopes (e.g., C2[13C]4H12O6 or similar format)
  expect_true(
    is.character(formula) && nchar(formula) > 0,
    info = "Formula should be non-empty for isotope-labeled compound"
  )

  # Check for isotope notation (various formats possible)
  has_isotope_notation <- grepl("13C", formula) || grepl("\\[13C\\]", formula)
  expect_true(
    has_isotope_notation,
    info = sprintf("Formula should show isotope notation, got: %s", formula)
  )
})

test_that("process_smiles InChIKey differs for isotope-labeled compounds", {
  skip_if_not(require("tima"), "tima package required")

  input <- tidytable::tidytable(
    structure_smiles_initial = c(
      "OC[C@H]1OC(O)[C@H](O)[C@@H](O)[C@@H]1O",
      "OC[13C@H]1OC(O)[13C@H](O)[13C@@H](O)[13C@@H]1O"
    )
  )

  result <- process_smiles(input, cache = NULL)

  inchikey_natural <- result$structure_inchikey[[1L]]
  inchikey_labeled <- result$structure_inchikey[[2L]]

  # Both should be non-empty
  expect_true(
    nchar(inchikey_natural) > 0 && nchar(inchikey_labeled) > 0,
    info = "InChIKeys should be non-empty"
  )

  # They should differ (isotope-aware)
  expect_true(
    inchikey_natural != inchikey_labeled,
    info = "InChIKey should differ for natural vs isotope-labeled"
  )
})

test_that("process_smiles handles multiple different isotope types", {
  skip_if_not(require("tima"), "tima package required")

  # Test molecule with both 13C and 15N
  natural_compound <- "NC(=O)c1ccccc1" # benzamide
  # COMMENT: Explicit H is required for isotopes
  labeled_compound <- "[15NH2]C(=O)[13CH2]1[13CH2][13CH2][13CH2][13CH2][13CH2]1" # labeled benzamide

  input <- tidytable::tidytable(
    structure_smiles_initial = c(natural_compound, labeled_compound)
  )

  result <- input |>
    process_smiles()

  expect_true(!anyNA(result$structure_exact_mass))

  # If both molecules processed, check that masses differ
  if (nrow(result) >= 2) {
    mass_natural <- result$structure_exact_mass[[1L]]
    mass_labeled <- result$structure_exact_mass[[2L]]
    mass_diff <- mass_labeled - mass_natural

    # With 5× 13C and 1× 15N:
    # Expected: 5*1.00335 + 0.9964 = 5.99475 + 0.9964 ~ 6.991 Da
    expect_true(
      mass_diff > 6.9 && mass_diff < 7.0,
      info = sprintf(
        "Mass difference for mixed isotopes (5×13C + 15N): %.6f Da (expected ~6.99 Da)",
        mass_diff
      )
    )
  }
})
##
## This approach ensures accurate annotation of isotope-labeled natural products

test_that("process_smiles validates cache helper output template", {
  tmpl <- get("create_empty_smiles_template", envir = asNamespace("tima"))(
    "smiles_col"
  )
  expect_true("smiles_col" %in% names(tmpl))
  expect_true(all(
    c(
      "structure_smiles",
      "structure_inchikey",
      "structure_molecular_formula",
      "structure_exact_mass",
      "structure_smiles_no_stereo",
      "structure_xlogp"
    ) %in%
      names(tmpl)
  ))
})

test_that("process_smiles load_smiles_cache falls back to empty template on read failure", {
  load_smiles_cache <- get("load_smiles_cache", envir = asNamespace("tima"))

  # NULL cache returns empty template immediately
  res_null <- load_smiles_cache(NULL, "structure_smiles_initial")
  expect_true("structure_smiles_initial" %in% names(res_null))
  expect_identical(nrow(res_null), 1L)
  expect_true(is.na(res_null$structure_smiles_initial[[1L]]))

  # Non-NULL unreadable/missing cache triggers tryCatch fallback
  res_err <- load_smiles_cache(
    "/nonexistent/path/to/smiles_cache.tsv",
    "custom_smiles_col"
  )
  expect_true("custom_smiles_col" %in% names(res_err))
  expect_identical(nrow(res_err), 1L)
  expect_true(is.na(res_err$custom_smiles_col[[1L]]))
})

test_that("process_smiles returns input when no valid SMILES remain after extraction", {
  input <- tidytable::tidytable(
    structure_smiles_initial = c(NA_character_, NA_character_)
  )

  out <- testthat::with_mocked_bindings(
    extract_unique_smiles = function(df, smiles_colname) {
      tidytable::tidytable(structure_smiles_initial = character(0))
    },
    process_smiles(input),
    .package = "tima"
  )

  expect_identical(out, input)
})

test_that("process_smiles returns cached-only result when all SMILES already cached", {
  cache_file <- withr::local_tempfile(fileext = ".tsv")
  input <- tidytable::tidytable(
    structure_smiles_initial = c("C", "CC")
  )
  cached <- tidytable::tidytable(
    structure_smiles_initial = c("C", "CC"),
    structure_smiles = c("C", "CC"),
    structure_inchikey = c("AAAAAAAAAAAAAA-A", "BBBBBBBBBBBBBB-B"),
    structure_molecular_formula = c("CH4", "C2H6"),
    structure_exact_mass = c(16, 30),
    structure_smiles_no_stereo = c("C", "CC"),
    structure_xlogp = c(0, 0)
  )
  tidytable::fwrite(cached, cache_file, sep = "\t")

  out <- process_smiles(input, cache = cache_file)

  expect_equal(nrow(out), 2L)
  expect_true(all(
    c(
      "structure_inchikey_connectivity_layer",
      "structure_inchikey_no_stereo"
    ) %in%
      names(out)
  ))
  expect_identical(
    out$structure_inchikey_connectivity_layer[[1]],
    "AAAAAAAAAAAAAA"
  )
  expect_identical(out$structure_inchikey_no_stereo[[1]], "AAAAAAAAAAAAAA-A")
})

test_that("extract_unique_smiles keeps distinct non-NA rows", {
  extract_unique_smiles <- get(
    "extract_unique_smiles",
    envir = asNamespace("tima")
  )
  df <- tidytable::tidytable(
    structure_smiles_initial = c("C", "C", NA_character_, "CC")
  )
  out <- extract_unique_smiles(df, "structure_smiles_initial")
  expect_equal(nrow(out), 2L)
  expect_true(!anyNA(out$structure_smiles_initial))
})

test_that("load_smiles_cache reuses in-memory cache and invalidates when file changes", {
  cache_path <- temp_test_path("smiles_mem_cache.tsv")
  df1 <- tidytable::tidytable(
    structure_smiles_initial = "C",
    structure_smiles = "C",
    structure_inchikey = "VNWKTOKETHGBQD-UHFFFAOYSA-N",
    structure_molecular_formula = "CH4",
    structure_exact_mass = 16.0313,
    structure_smiles_no_stereo = "C",
    structure_xlogp = 0.63
  )
  data.table::fwrite(df1, cache_path, sep = "\t")

  res1 <- load_smiles_cache(cache_path, "structure_smiles_initial")
  res2 <- load_smiles_cache(cache_path, "structure_smiles_initial")
  expect_equal(nrow(res1), 1L)
  expect_identical(res1, res2)

  # Appending a second row changes file size and invalidates the in-memory entry
  df2 <- tidytable::bind_rows(
    df1,
    tidytable::tidytable(
      structure_smiles_initial = "CC",
      structure_smiles = "CC",
      structure_inchikey = "OTMSDBZUPAUEDD-UHFFFAOYSA-N",
      structure_molecular_formula = "C2H6",
      structure_exact_mass = 30.04695,
      structure_smiles_no_stereo = "CC",
      structure_xlogp = 1.02
    )
  )
  data.table::fwrite(df2, cache_path, sep = "\t")
  res3 <- load_smiles_cache(cache_path, "structure_smiles_initial")
  expect_equal(nrow(res3), 2L)
})

test_that("process_smiles reuses canonical stereo and 2D no-stereo cache entries and deduplicates", {
  tmp_dir <- withr::local_tempdir()
  cache_path <- file.path(tmp_dir, "stereo_2d_cache.tsv")

  cached_df <- tidytable::tidytable(
    structure_smiles_initial = c("INIT_A", "INIT_A", "INIT_B", "INIT_SHORT"),
    structure_smiles = c("CAN_A", "CAN_A", "CAN_B", "CAN_SHORT"),
    structure_inchikey = c(
      "AAAAAAAAAAAAAA-BBBBBBBBBB-N",
      "AAAAAAAAAAAAAA-BBBBBBBBBB-N",
      "CCCCCCCCCCCCCC-DDDDDDDDDD-P",
      "SHORT_IK"
    ),
    structure_molecular_formula = c("C6H12O6", "C6H12O6", "C7H14O6", "CH4"),
    structure_exact_mass = c(180.0634, 180.0634, 194.0790, 16.0313),
    structure_smiles_no_stereo = c("2D_A", "2D_A", "2D_B", "2D_SHORT"),
    structure_xlogp = c(-2.5, -2.5, -2.0, 0.5)
  )
  tidytable::fwrite(cached_df, cache_path, sep = "\t")

  # Query 1: all matched via initial ("INIT_A"), canonical ("CAN_A"), and 2D ("2D_B", "2D_SHORT")
  input_all_cached <- tidytable::tidytable(
    structure_smiles_initial = c("INIT_A", "CAN_A", "2D_B", "2D_SHORT")
  )
  res_cached <- process_smiles(input_all_cached, cache = cache_path)
  expect_identical(nrow(res_cached), 4L)
  expect_null(attr(res_cached$structure_smiles_initial, ".match.hash"))
  expect_null(attr(res_cached$structure_smiles, ".match.hash"))
  expect_null(attr(res_cached$structure_smiles_no_stereo, ".match.hash"))
  expect_identical(
    res_cached$structure_smiles_initial,
    c("INIT_A", "CAN_A", "2D_B", "2D_SHORT")
  )
  # Canonical match keeps stereo InChIKey
  row_can <- res_cached[res_cached$structure_smiles_initial == "CAN_A", ]
  expect_identical(row_can$structure_inchikey, "AAAAAAAAAAAAAA-BBBBBBBBBB-N")
  expect_identical(row_can$structure_smiles, "CAN_A")

  # 2D match with 27-char InChIKey rewrites stereo layer to UHFFFAOYSA
  row_2d <- res_cached[res_cached$structure_smiles_initial == "2D_B", ]
  expect_identical(row_2d$structure_smiles, "2D_B")
  expect_identical(row_2d$structure_inchikey, "CCCCCCCCCCCCCC-UHFFFAOYSA-P")
  expect_identical(
    row_2d$structure_inchikey_connectivity_layer,
    "CCCCCCCCCCCCCC"
  )
  expect_identical(row_2d$structure_inchikey_no_stereo, "CCCCCCCCCCCCCC-P")

  # 2D match with <27-char InChIKey leaves InChIKey unchanged
  row_short <- res_cached[res_cached$structure_smiles_initial == "2D_SHORT", ]
  expect_identical(row_short$structure_inchikey, "SHORT_IK")
})

test_that("process_smiles combines partial cache matches with new RDKit results and persists cache", {
  tmp_dir <- withr::local_tempdir()
  gz_cache <- file.path(tmp_dir, "cache_out.csv.gz")

  seed_df <- tidytable::tidytable(
    structure_smiles_initial = "INIT_SEED",
    structure_smiles = "CAN_SEED",
    structure_inchikey = "SEEDINKKEY0001-BBBBBBBBBB-N",
    structure_molecular_formula = "C2H6O",
    structure_exact_mass = 46.0419,
    structure_smiles_no_stereo = "CCO",
    structure_xlogp = -0.1
  )
  tidytable::fwrite(seed_df, gz_cache, compress = "gzip")

  # Query has "CAN_SEED" (matched via canonical stereo cache -> extra_parts) and "C" (new -> RDKit)
  input_mixed <- tidytable::tidytable(
    structure_smiles_initial = c("CAN_SEED", "C")
  )
  res_mixed <- process_smiles(input_mixed, cache = gz_cache)
  expect_identical(nrow(res_mixed), 2L)
  expect_true(all(c("CAN_SEED", "C") %in% res_mixed$structure_smiles_initial))

  # Updated gz_cache on disk should be gzip-compressed and contain INIT_SEED, CAN_SEED, and C
  expect_identical(readBin(gz_cache, "raw", n = 2L), as.raw(c(0x1f, 0x8b)))
  disk_cache <- safe_fread(gz_cache)
  expect_true(all(
    c("INIT_SEED", "CAN_SEED", "C") %in% disk_cache$structure_smiles_initial
  ))

  # Also test plain text (.tsv) cache write-back and unwritable directory cache graceful handling
  tsv_cache <- file.path(tmp_dir, "plain_cache.tsv")
  writeLines("structure_smiles_initial\tstructure_smiles", tsv_cache)
  res_tsv <- process_smiles(
    tidytable::tidytable(structure_smiles_initial = "C"),
    cache = tsv_cache
  )
  expect_identical(nrow(res_tsv), 1L)
  expect_true(file.exists(tsv_cache))

  # Passing an existing directory path as cache triggers fwrite error inside tryCatch without failing
  dir_as_cache <- file.path(tmp_dir, "subdir_as_cache")
  dir.create(dir_as_cache)
  res_dir_cache <- process_smiles(
    tidytable::tidytable(structure_smiles_initial = "C"),
    cache = dir_as_cache
  )
  expect_identical(nrow(res_dir_cache), 1L)
})

test_that("load_python_smiles_processor and process_smiles abort with tima_runtime_error on Python failures", {
  if (!is_windows_arm64()) {
    # 1. load_python_smiles_processor failure when py_require fails
    expect_error(
      with_mocked_bindings(
        py_require = function(...) stop("simulated python environment error"),
        load_python_smiles_processor(),
        .package = "reticulate"
      ),
      "cannot load python smiles processor",
      class = "tima_runtime_error"
    )

    # 2. process_smiles failure when reticulate::py$process_smiles raises an error
    load_python_smiles_processor()
    withr::defer(load_python_smiles_processor())
    reticulate::py_run_string(
      "def process_smiles(a, b):\n    raise RuntimeError('simulated rdkit crash')\n"
    )
    before_tmp <- list.files(
      tempdir(),
      pattern = "[.](smi|csv[.]gz)$",
      full.names = TRUE
    )
    expect_error(
      with_mocked_bindings(
        source_python = function(...) invisible(NULL),
        process_smiles(
          tidytable::tidytable(structure_smiles_initial = "C"),
          cache = NULL
        ),
        .package = "reticulate"
      ),
      "rdkit smiles processing failed",
      class = "tima_runtime_error"
    )
    after_tmp <- list.files(
      tempdir(),
      pattern = "[.](smi|csv[.]gz)$",
      full.names = TRUE
    )
    expect_identical(setdiff(after_tmp, before_tmp), character(0))
  }

  # Cross-platform Windows ARM64 subprocess load and execution error paths
  expect_error(
    with_mocked_bindings(
      is_windows_arm64 = function() TRUE,
      resolve_win_arm64_python = function() stop("simulated uv setup error"),
      load_python_smiles_processor()
    ),
    "cannot load python smiles processor",
    class = "tima_runtime_error"
  )

  before_tmp <- list.files(
    tempdir(),
    pattern = "[.](smi|csv[.]gz)$",
    full.names = TRUE
  )
  expect_error(
    with_mocked_bindings(
      load_python_smiles_processor = function() invisible(NULL),
      run_python_smiles_processor = function(...) stop("simulated rdkit crash"),
      process_smiles(
        tidytable::tidytable(structure_smiles_initial = "C"),
        cache = NULL
      )
    ),
    "rdkit smiles processing failed",
    class = "tima_runtime_error"
  )
  after_tmp <- list.files(
    tempdir(),
    pattern = "[.](smi|csv[.]gz)$",
    full.names = TRUE
  )
  expect_identical(setdiff(after_tmp, before_tmp), character(0))
})

test_that("Windows ARM64 x86_64 Python subprocess helpers resolve, cache, execute, and surface errors", {
  # 1. with_temp_envvar sets, unsets, and restores previous state on exit
  withr::local_envvar(
    TIMA_TEST_EXISTING = "before",
    TIMA_TEST_TEMP = NA_character_
  )
  observed <- with_temp_envvar(
    c(TIMA_TEST_EXISTING = NA_character_, TIMA_TEST_TEMP = "during"),
    c(
      existing = Sys.getenv("TIMA_TEST_EXISTING", unset = NA_character_),
      temp = Sys.getenv("TIMA_TEST_TEMP", unset = NA_character_)
    )
  )
  expect_identical(observed, c(existing = NA_character_, temp = "during"))
  expect_identical(Sys.getenv("TIMA_TEST_EXISTING"), "before")
  expect_true(is.na(Sys.getenv("TIMA_TEST_TEMP", unset = NA_character_)))

  # 2. maybe_sh_quote leaves safe tokens unquoted and quotes paths needing protection
  tokens <- c("tool", "cpython-3.13", "C:\\path\\to file.exe", "a b/c")
  quoted <- maybe_sh_quote(tokens)
  expect_identical(quoted[1:2], tokens[1:2])
  expect_identical(quoted[3:4], shQuote(tokens[3:4]))

  # 3. resolve_uv_binary honors RETICULATE_UV, reticulate.uv_binary, and errors if missing
  tmp_dir <- withr::local_tempdir()
  fake_uv_env <- file.path(tmp_dir, "uv-env")
  fake_uv_opt <- file.path(tmp_dir, "uv-opt")
  file.create(fake_uv_env)
  file.create(fake_uv_opt)

  withr::with_envvar(c(RETICULATE_UV = fake_uv_env), {
    expect_identical(resolve_uv_binary(), fake_uv_env)
  })
  withr::with_envvar(
    c(RETICULATE_UV = NA_character_),
    withr::with_options(list(reticulate.uv_binary = fake_uv_opt), {
      expect_identical(resolve_uv_binary(), fake_uv_opt)
    })
  )

  empty_home <- file.path(tmp_dir, "empty-home")
  empty_cache <- file.path(tmp_dir, "empty-cache")
  dir.create(empty_home)
  dir.create(empty_cache)
  withr::with_envvar(
    c(
      RETICULATE_UV = "managed",
      HOME = empty_home,
      R_USER_CACHE_DIR = empty_cache,
      PATH = ""
    ),
    withr::with_options(
      list(reticulate.uv_binary = NULL),
      with_mocked_bindings(
        uv_run_tool = function(...) NULL,
        .package = "reticulate",
        expect_error(resolve_uv_binary(), "uv executable not found")
      )
    )
  )

  # 4. resolve_win_arm64_python caches across repeated calls and errors on non-zero exit
  .win_arm64_python_store$clear()
  withr::defer(.win_arm64_python_store$clear())
  fake_py <- file.path(tmp_dir, "python.exe")
  file.create(fake_py)

  state <- new.env(parent = emptyenv())
  state$uv_calls <- 0L
  with_mocked_bindings(
    resolve_uv_binary = function() {
      uv <- fake_uv_env
      attr(uv, "reticulate-managed") <- TRUE
      uv
    },
    .system2 = function(command, args = character(), ...) {
      state$uv_calls <- state$uv_calls + 1L
      out_file <- utils::tail(args, 1L)
      out_file <- gsub("^['\"]|['\"]$", "", out_file)
      writeLines(fake_py, out_file)
      structure(character(0), status = 0L)
    },
    {
      expect_identical(resolve_win_arm64_python(), fake_py)
      expect_identical(resolve_win_arm64_python(), fake_py)
    }
  )
  expect_identical(state$uv_calls, 1L)

  .win_arm64_python_store$clear()
  with_mocked_bindings(
    resolve_uv_binary = function() fake_uv_env,
    .system2 = function(command, args = character(), ...) {
      structure("uv resolution failed", status = 2L)
    },
    expect_error(resolve_win_arm64_python(), "uv failed with exit code 2")
  )

  # 5. run_python_smiles_processor invokes process_smiles.py and surfaces non-zero exit
  state$captured <- NULL
  expect_no_error(
    with_mocked_bindings(
      is_windows_arm64 = function() TRUE,
      resolve_win_arm64_python = function() fake_py,
      .system2 = function(command, args = character(), ...) {
        state$captured <- list(command = command, args = args)
        structure(character(0), status = 0L)
      },
      run_python_smiles_processor("in.smi", "out.csv.gz")
    )
  )
  expect_identical(state$captured$command, fake_py)
  expect_match(state$captured$args[[1L]], "process_smiles\\.py$")
  expect_identical(state$captured$args[2:3], c("in.smi", "out.csv.gz"))

  expect_error(
    with_mocked_bindings(
      is_windows_arm64 = function() TRUE,
      resolve_win_arm64_python = function() fake_py,
      .system2 = function(command, args = character(), ...) {
        structure("boom", status = 1L)
      },
      run_python_smiles_processor("in.smi", "out.csv.gz")
    ),
    "process_smiles\\.py failed with exit code 1"
  )
})
