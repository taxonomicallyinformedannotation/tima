# ==============================================================================
# Tests for filter_annotations()
# ==============================================================================

context("filter_annotations")

# ------------------------------------------------------------------------------
# Input validation
# ------------------------------------------------------------------------------

test_that("filter_annotations validates tolerance_rt positive", {
  tmp <- withr::local_tempdir()
  withr::local_dir(tmp)

  # Minimal required files
  features <- file.path(tmp, "features.tsv")
  writeLines("feature_id\trt\tmz\nFT0001\t1\t100", features)

  ann <- file.path(tmp, "ann.tsv")
  writeLines(
    "feature_id\tcandidate_structure_inchikey_connectivity_layer\nFT0001\tABCDEF",
    ann
  )

  expect_error(
    filter_annotations(
      annotations = ann,
      features = features,
      rts = NULL,
      output = file.path(tmp, "out.tsv"),
      tolerance_rt = 0
    ),
    "positive number"
  )
})

# ------------------------------------------------------------------------------
# Functional - MS1 removal when spectral present
# ------------------------------------------------------------------------------

# test_that("filter_annotations removes MS1 when spectral match exists", {
#   # Use project fixtures for features table
#   features_path <- testthat::test_path("fixtures", "features.csv")
#   expect_true(file.exists(features_path))
#
#   # Create two annotation files: identical keys for MS1 and spectral
#   tmp <- withr::local_tempdir()
#   withr::local_dir(tmp)
#   ms1_path <- file.path(tmp, "ms1.tsv")
#   spectral_path <- file.path(tmp, "spectral.tsv")
#   out_path <- file.path(tmp, "filtered.tsv")
#
#   # Build a minimal annotation row using fixtures values
#   annot_df <- tidytable::tidytable(
#     feature_id = "FT0001",
#     candidate_structure_inchikey_connectivity_layer = "LFQSCWFLJHTTHZ",
#     candidate_score_similarity = NA_character_,
#     candidate_score_sirius_csi = NA_character_
#   )
#
#   # Write both files; same row but different file names
#   data.table::fwrite(annot_df, ms1_path, sep = "\t", na = "NA")
#   data.table::fwrite(annot_df, spectral_path, sep = "\t", na = "NA")
#
#   # Named vector so function knows which is ms1
#   ann_list <- c(ms1 = ms1_path, spectral = spectral_path)
#
#   out <- filter_annotations(
#     annotations = ann_list,
#     features = features_path,
#     rts = NULL,
#     output = out_path,
#     tolerance_rt = 1
#   )
#
#   expect_true(file.exists(out))
#   df <- tidytable::fread(out)
#
#   # Only one annotation for the key should remain after MS1 anti-join
#   kept <- df |>
#     tidytable::filter(
#       feature_id == "FT0001" &
#         candidate_structure_inchikey_connectivity_layer == "LFQSCWFLJHTTHZ"
#     )
#   expect_equal(nrow(kept), 1L)
#
#   # With no RT library, candidate_structure_error_rt should be NA
#   expect_true(all(is.na(df$candidate_structure_error_rt)))
# })
