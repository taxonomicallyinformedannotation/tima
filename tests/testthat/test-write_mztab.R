# Test Suite: write_mztab ----

library(testthat)

## .filter_top_annotations ----

test_that(".filter_top_annotations filters by threshold", {
  ann <- tidytable::tidytable(
    feature_id = c("F1", "F1", "F2", "F2"),
    candidate_score_similarity = c(0.9, 0.3, 0.7, 0.1)
  )

  result <- tima:::.filter_top_annotations(ann, top_n = NULL, score_threshold = 0.5)
  expect_equal(nrow(result), 2)
  expect_true(all(result$candidate_score_similarity >= 0.5))
})

test_that(".filter_top_annotations selects top_n per feature", {
  ann <- tidytable::tidytable(
    feature_id = c("F1", "F1", "F1", "F2", "F2"),
    candidate_score_similarity = c(0.9, 0.8, 0.7, 0.6, 0.5)
  )

  result <- tima:::.filter_top_annotations(ann, top_n = 2, score_threshold = 0)
  # F1 should have 2, F2 should have 2
  expect_equal(nrow(result), 4)
  f1 <- result[result$feature_id == "F1", ]
  expect_equal(nrow(f1), 2)
  expect_equal(f1$candidate_score_similarity, c(0.9, 0.8))
})

test_that(".filter_top_annotations handles missing score columns", {
  ann <- tidytable::tidytable(
    feature_id = c("F1", "F2"),
    some_other_col = c("a", "b")
  )

  result <- tima:::.filter_top_annotations(ann, top_n = 1, score_threshold = 0)
  # Should return all rows when no score column found
  expect_equal(nrow(result), 2)
})

## .tima_adduct_to_mztab ----

test_that(".tima_adduct_to_mztab converts TIMA to mzTab format", {
  expect_equal(tima:::.tima_adduct_to_mztab("[M+H]+"), "[M+H]1+")
  expect_equal(tima:::.tima_adduct_to_mztab("[M-H]-"), "[M-H]1-")
  # Already has charge number
  expect_equal(tima:::.tima_adduct_to_mztab("[M+2H]2+"), "[M+2H]2+")
  # NA handling
  expect_true(is.na(tima:::.tima_adduct_to_mztab(NA_character_)))
})

## .merge_tima_annotations_to_mztab ----

test_that(".merge_tima_annotations_to_mztab creates new mzTab object", {
  ann <- tidytable::tidytable(
    feature_id = c("1", "2"),
    candidate_structure_name = c("Quercetin", "Kaempferol"),
    candidate_structure_smiles_no_stereo = c("OC1=CC(O)=C2C(=O)", "OC1=CC"),
    candidate_adduct = c("[M+H]+", "[M-H]-"),
    candidate_score_similarity = c(0.9, 0.7)
  )

  result <- tima:::.merge_tima_annotations_to_mztab(
    mztab_obj = NULL,
    tima_annotations = ann
  )

  expect_true("Metadata" %in% names(result))
  expect_true("Small_Molecule" %in% names(result))
  expect_equal(nrow(result$Small_Molecule), 2)
  expect_true("SML_ID" %in% names(result$Small_Molecule))
  expect_true("chemical_name" %in% names(result$Small_Molecule))
})

test_that(".merge_tima_annotations_to_mztab merges with existing template", {
  existing <- list(
    Metadata = data.frame(
      key = c("mzTab-version", "description"),
      value = c("2.0.0-M", "Test"),
      stringsAsFactors = FALSE
    ),
    Small_Molecule = data.frame(
      SML_ID = c("1", "3"),
      chemical_name = c("Old1", "Old3"),
      stringsAsFactors = FALSE
    )
  )

  ann <- tidytable::tidytable(
    feature_id = c("1"),
    candidate_structure_name = c("NewName"),
    candidate_score_similarity = c(0.95)
  )

  result <- tima:::.merge_tima_annotations_to_mztab(
    mztab_obj = existing,
    tima_annotations = ann
  )

  sml <- result$Small_Molecule
  # Should have both the updated F1 and the original F3
  expect_true("3" %in% sml$SML_ID)
  # F1 should have the new annotation
  f1_row <- sml[sml$SML_ID == "1", ]
  expect_true(nrow(f1_row) >= 1)
})

## .parse_mztab_for_write ----

test_that(".parse_mztab_for_write parses a minimal mzTab file", {
  mztab_file <- resolve_fixture_path("minimal.mztab")
  result <- tima:::.parse_mztab_for_write(mztab_file)

  expect_true("Metadata" %in% names(result))
  expect_true("Small_Molecule" %in% names(result))
  expect_true(nrow(result$Metadata) > 0)
  expect_true(nrow(result$Small_Molecule) >= 2)
  expect_true("SML_ID" %in% names(result$Small_Molecule))
})

## .write_mztab_fallback (vectorized writer) ----

test_that(".write_mztab_fallback writes valid mzTab format", {
  mztab_obj <- list(
    Metadata = data.frame(
      key = c("mzTab-version", "mzTab-id"),
      value = c("2.0.0-M", "TEST"),
      stringsAsFactors = FALSE
    ),
    Small_Molecule = data.frame(
      SML_ID = c("1", "2"),
      chemical_name = c("Cmpd1", "Cmpd2"),
      smiles = c("C", NA),
      stringsAsFactors = FALSE
    )
  )

  out <- temp_test_path("write_test.mztab")
  tima:::.write_mztab_fallback(mztab_obj, out)

  expect_true(file.exists(out))
  content <- readLines(out)

  expect_true(any(grepl("^COM", content)))
  expect_true(any(grepl("^MTD", content)))
  expect_true(any(grepl("^SMH", content)))
  expect_true(any(grepl("^SML", content)))

  # NA should be written as "null"
  sml_lines <- grep("^SML", content, value = TRUE)
  expect_true(any(grepl("null", sml_lines)))
})

## Round-trip test ----

test_that("write_mztab round-trips with read_mztab", {
  mztab_file <- resolve_fixture_path("minimal.mztab")

  # Read
  out_features <- temp_test_path("rt_features.csv")
  read_result <- read_mztab(
    input = mztab_file,
    output_features = out_features
  )

  # Create minimal annotations from features
  features <- tidytable::fread(out_features)
  ann <- tidytable::tidytable(
    feature_id = features$feature_id[1],
    candidate_structure_name = "TestCompound",
    candidate_score_similarity = 0.85
  )

  ann_path <- temp_test_path("rt_annotations.tsv")
  tidytable::fwrite(ann, ann_path, sep = "\t")

  # Write
  out_mztab <- temp_test_path("rt_output.mztab")
  write_mztab(
    annotations = ann_path,
    original_mztab = mztab_file,
    output = out_mztab,
    top_n = 3,
    score_threshold = 0
  )

  expect_true(file.exists(out_mztab))
  content <- readLines(out_mztab)
  expect_true(any(grepl("^MTD", content)))
  expect_true(any(grepl("^SMH", content)))
  expect_true(any(grepl("^SML", content)))
})

