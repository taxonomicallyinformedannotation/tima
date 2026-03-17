# Test Suite: add_xrefs_to_annotations ----

library(testthat)

## Basic Functionality ----

test_that("add_xrefs_to_annotations adds xref columns to results", {
  # Create mock results list
  results_list <- list(
    full = tidytable::tidytable(
      feature_id = c("F1", "F2", "F3"),
      candidate_structure_inchikey_connectivity_layer = c(
        "INCHIKEY1",
        "INCHIKEY2",
        "INCHIKEY1"
      ),
      candidate_structure_name = c("Comp1", "Comp2", "Comp3")
    ),
    filtered = tidytable::tidytable(
      feature_id = c("F1", "F2"),
      candidate_structure_inchikey_connectivity_layer = c(
        "INCHIKEY1",
        "INCHIKEY2"
      ),
      candidate_structure_name = c("Comp1", "Comp2")
    ),
    mini = tidytable::tidytable(
      feature_id = c("F1"),
      candidate_structure_inchikey_connectivity_layer = c("INCHIKEY1"),
      candidate_structure_name = c("Comp1")
    )
  )

  # Create mock xrefs file
  xrefs_data <- tidytable::tidytable(
    inchikey = c(
      "INCHIKEY1",
      "INCHIKEY1",
      "INCHIKEY2",
      "INCHIKEY1"
    ),
    prefix = c("chebi", "wikidata", "chembl", "pubchem"),
    id = c("15365", "Q2270", "CHEMBL123", "5891")
  )
  xrefs_file <- temp_test_path("xrefs_test.tsv.gz")
  export_output(x = xrefs_data, file = xrefs_file)

  # Test function
  result <- add_xrefs_to_annotations(results_list, xrefs_file)

  # Verify structure
  expect_type(result, "list")
  expect_true(all(c("full", "filtered", "mini") %in% names(result)))

  # Verify xref columns were added
  expect_true(any(grepl("^candidate_structure_id_", names(result$full))))
  expect_true(any(grepl("^candidate_structure_id_", names(result$filtered))))
  expect_true(any(grepl("^candidate_structure_id_", names(result$mini))))

  # Verify xref content for INCHIKEY1
  inchikey1_rows <- result$full |>
    tidytable::filter(
      candidate_structure_inchikey_connectivity_layer == "INCHIKEY1"
    )
  expect_true(nrow(inchikey1_rows) > 0L)
  expect_true(!is.na(inchikey1_rows$candidate_structure_id_chebi[1]))
  expect_true(!is.na(inchikey1_rows$candidate_structure_id_wikidata[1]))
  expect_true(!is.na(inchikey1_rows$candidate_structure_id_pubchem[1]))

  # Verify xref content for INCHIKEY2
  inchikey2_rows <- result$full |>
    tidytable::filter(
      candidate_structure_inchikey_connectivity_layer == "INCHIKEY2"
    )
  expect_true(nrow(inchikey2_rows) > 0L)
  expect_true(!is.na(inchikey2_rows$candidate_structure_id_chembl[1]))
})

## Edge Cases ----

test_that("add_xrefs_to_annotations handles missing xrefs file gracefully", {
  results_list <- list(
    full = tidytable::tidytable(
      feature_id = "F1",
      candidate_structure_inchikey_connectivity_layer = "INCHIKEY1"
    ),
    filtered = tidytable::tidytable(
      feature_id = "F1",
      candidate_structure_inchikey_connectivity_layer = "INCHIKEY1"
    ),
    mini = tidytable::tidytable(
      feature_id = "F1",
      candidate_structure_inchikey_connectivity_layer = "INCHIKEY1"
    )
  )

  nonexistent_file <- temp_test_path("nonexistent.tsv.gz")

  result <- add_xrefs_to_annotations(results_list, nonexistent_file)

  # Should return unchanged results_list
  expect_equal(names(result$full), names(results_list$full))
})

test_that("add_xrefs_to_annotations handles empty xrefs file", {
  results_list <- list(
    full = tidytable::tidytable(
      feature_id = "F1",
      candidate_structure_inchikey_connectivity_layer = "INCHIKEY1"
    ),
    filtered = tidytable::tidytable(
      feature_id = "F1",
      candidate_structure_inchikey_connectivity_layer = "INCHIKEY1"
    ),
    mini = tidytable::tidytable(
      feature_id = "F1",
      candidate_structure_inchikey_connectivity_layer = "INCHIKEY1"
    )
  )

  # Create empty xrefs file
  empty_xrefs <- tidytable::tidytable(
    inchikey = character(),
    prefix = character(),
    id = character()
  )
  xrefs_file <- temp_test_path("empty_xrefs.tsv.gz")
  export_output(x = empty_xrefs, file = xrefs_file)

  result <- add_xrefs_to_annotations(results_list, xrefs_file)

  # Should return unchanged results_list (no xrefs to add)
  expect_equal(names(result$full), names(results_list$full))
})

test_that("add_xrefs_to_annotations filters out databases with all NA", {
  results_list <- list(
    full = tidytable::tidytable(
      feature_id = c("F1", "F2"),
      candidate_structure_inchikey_connectivity_layer = c(
        "INCHIKEY1",
        "INCHIKEY2"
      )
    ),
    filtered = tidytable::tidytable(
      feature_id = c("F1"),
      candidate_structure_inchikey_connectivity_layer = c("INCHIKEY1")
    ),
    mini = tidytable::tidytable(
      feature_id = c("F1"),
      candidate_structure_inchikey_connectivity_layer = c("INCHIKEY1")
    )
  )

  # Create xrefs with one empty database
  xrefs_data <- tidytable::tidytable(
    inchikey = c("INCHIKEY1", "INCHIKEY1"),
    prefix = c("chebi", "wikidata"),
    id = c("15365", "Q2270")
  )
  xrefs_file <- temp_test_path("xrefs_partial.tsv.gz")
  export_output(x = xrefs_data, file = xrefs_file)

  result <- add_xrefs_to_annotations(results_list, xrefs_file)

  # Should only have columns for non-empty databases
  xref_cols <- grep(
    "^candidate_structure_id_",
    names(result$full),
    value = TRUE
  )
  expect_true(length(xref_cols) > 0L)

  # Databases with coverage should be present
  expect_true(any(grepl("chebi", xref_cols)))
  expect_true(any(grepl("wikidata", xref_cols)))
})

test_that("add_xrefs_to_annotations handles multiple IDs per inchikey", {
  results_list <- list(
    full = tidytable::tidytable(
      feature_id = "F1",
      candidate_structure_inchikey_connectivity_layer = "INCHIKEY1"
    ),
    filtered = tidytable::tidytable(
      feature_id = "F1",
      candidate_structure_inchikey_connectivity_layer = "INCHIKEY1"
    ),
    mini = tidytable::tidytable(
      feature_id = "F1",
      candidate_structure_inchikey_connectivity_layer = "INCHIKEY1"
    )
  )

  # Create xrefs with multiple IDs for same inchikey × prefix
  xrefs_data <- tidytable::tidytable(
    inchikey = c("INCHIKEY1", "INCHIKEY1", "INCHIKEY1"),
    prefix = c("chebi", "chebi", "wikidata"),
    id = c("15365", "15366", "Q2270")
  )
  xrefs_file <- temp_test_path("xrefs_multi.tsv.gz")
  export_output(x = xrefs_data, file = xrefs_file)

  result <- add_xrefs_to_annotations(results_list, xrefs_file)

  # Should collapse multiple IDs with " $ " separator
  chebi_val <- result$full$candidate_structure_id_chebi[1]
  expect_true(grepl(" \\$ ", chebi_val))
  expect_true(grepl("15365", chebi_val))
  expect_true(grepl("15366", chebi_val))
})

## Error Handling ----

test_that("add_xrefs_to_annotations errors on invalid results_list", {
  expect_error(
    add_xrefs_to_annotations(
      list(full = NULL, filtered = NULL),
      "dummy.tsv.gz"
    ),
    "results_list must be a list with elements",
    class = "tima_validation_error"
  )
})

test_that("add_xrefs_to_annotations errors on missing inchikey column", {
  results_list <- list(
    full = tidytable::tidytable(feature_id = "F1"),
    filtered = tidytable::tidytable(feature_id = "F1"),
    mini = tidytable::tidytable(feature_id = "F1")
  )

  xrefs_data <- tidytable::tidytable(
    inchikey = "INCHIKEY1",
    prefix = "chebi",
    id = "15365"
  )
  xrefs_file <- temp_test_path("xrefs_bad.tsv.gz")
  export_output(x = xrefs_data, file = xrefs_file)

  # Should warn but not error (graceful degradation)
  result <- add_xrefs_to_annotations(results_list, xrefs_file)
  expect_equal(names(result$full), names(results_list$full))
})

test_that("add_xrefs_to_annotations errors on malformed xrefs", {
  results_list <- list(
    full = tidytable::tidytable(
      feature_id = "F1",
      candidate_structure_inchikey_connectivity_layer = "INCHIKEY1"
    ),
    filtered = tidytable::tidytable(
      feature_id = "F1",
      candidate_structure_inchikey_connectivity_layer = "INCHIKEY1"
    ),
    mini = tidytable::tidytable(
      feature_id = "F1",
      candidate_structure_inchikey_connectivity_layer = "INCHIKEY1"
    )
  )

  # Create xrefs with missing required column
  bad_xrefs <- tidytable::tidytable(
    inchikey = "INCHIKEY1",
    prefix = "chebi"
    # Missing 'id' column
  )
  xrefs_file <- temp_test_path("xrefs_malformed.tsv.gz")
  export_output(x = bad_xrefs, file = xrefs_file)

  expect_error(
    add_xrefs_to_annotations(results_list, xrefs_file),
    "xrefs file must contain",
    class = "tima_validation_error"
  )
})

## Integration ----

test_that("add_xrefs_to_annotations preserves all original columns", {
  original_cols <- c(
    "feature_id",
    "candidate_structure_inchikey_connectivity_layer",
    "candidate_structure_name",
    "score_final"
  )

  results_list <- list(
    full = tidytable::tidytable(
      feature_id = "F1",
      candidate_structure_inchikey_connectivity_layer = "INCHIKEY1",
      candidate_structure_name = "Comp1",
      score_final = 0.95
    ),
    filtered = tidytable::tidytable(
      feature_id = "F1",
      candidate_structure_inchikey_connectivity_layer = "INCHIKEY1",
      candidate_structure_name = "Comp1",
      score_final = 0.95
    ),
    mini = tidytable::tidytable(
      feature_id = "F1",
      candidate_structure_inchikey_connectivity_layer = "INCHIKEY1",
      candidate_structure_name = "Comp1",
      score_final = 0.95
    )
  )

  xrefs_data <- tidytable::tidytable(
    inchikey = "INCHIKEY1",
    prefix = "chebi",
    id = "15365"
  )
  xrefs_file <- temp_test_path("xrefs_preserve.tsv.gz")
  export_output(x = xrefs_data, file = xrefs_file)

  result <- add_xrefs_to_annotations(results_list, xrefs_file)

  # Verify all original columns are preserved
  invisible(vapply(
    X = original_cols,
    FUN = function(col) {
      expect_true(col %in% names(result$full))
      expect_true(col %in% names(result$filtered))
      expect_true(col %in% names(result$mini))
      TRUE
    },
    FUN.VALUE = logical(1L)
  ))

  # Verify original data is unchanged
  expect_equal(result$full$feature_id, results_list$full$feature_id)
  expect_equal(
    result$full$candidate_structure_name,
    results_list$full$candidate_structure_name
  )
  expect_equal(result$full$score_final, results_list$full$score_final)
})

test_that("add_xrefs_to_annotations matches on 14-char inchikey connectivity layer", {
  results_list <- list(
    full = tidytable::tidytable(
      feature_id = "F1",
      candidate_structure_inchikey_connectivity_layer = "ABCDEFGHIJKLMN"
    ),
    filtered = tidytable::tidytable(
      feature_id = "F1",
      candidate_structure_inchikey_connectivity_layer = "ABCDEFGHIJKLMN"
    ),
    mini = tidytable::tidytable(
      feature_id = "F1",
      candidate_structure_inchikey_connectivity_layer = "ABCDEFGHIJKLMN"
    )
  )

  xrefs_data <- tidytable::tidytable(
    inchikey = "ABCDEFGHIJKLMN-OPQRSTUVWX-Y",
    prefix = "wikidata",
    id = "Q123"
  )
  xrefs_file <- temp_test_path("xrefs_connectivity_layer.tsv.gz")
  export_output(x = xrefs_data, file = xrefs_file)

  result <- add_xrefs_to_annotations(results_list, xrefs_file)

  expect_identical(result$full$candidate_structure_id_wikidata[[1L]], "Q123")
  expect_identical(
    result$filtered$candidate_structure_id_wikidata[[1L]],
    "Q123"
  )
})
