# ==============================================================================
# Test Suite: prepare_annotations_sirius integration (v6 spectral matches)
# ==============================================================================

library(testthat)
library(tima)

# test_that("prepare_annotations_sirius integrates spectral matches for v6", {
#   skip_on_cran()
#   local_test_project(copy = TRUE)
#
#   # Get example sirius data (includes v6 zip)
#   get_example_sirius()
#
#   # Run preparation explicitly forcing v6
#   outputs <- expect_no_error(prepare_annotations_sirius(
#     input_directory = "data/interim/annotations/example_sirius_6.zip",
#     sirius_version = 6
#   ))
#
#   expect_true(file.exists(outputs[["structural"]]))
#   df <- data.table::fread(outputs[["structural"]])
#
#   # Similarity columns may be NA if spectral_matches file absent, but column should exist
#   expect_true("candidate_score_similarity" %in% names(df))
#   expect_true("candidate_count_similarity_peaks_matched" %in% names(df))
# })
