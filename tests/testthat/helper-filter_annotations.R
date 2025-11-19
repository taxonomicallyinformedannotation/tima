# Helper fixtures for filter_annotations tests ----

fa_create_min_files <- function(root, with_rt = FALSE) {
  features <- file.path(root, "features.tsv")
  ann1 <- file.path(root, "ann1.tsv")
  ann2 <- file.path(root, "ann2.tsv")
  writeLines("feature_id", features)
  writeLines("feature_id", ann1)
  writeLines("feature_id", ann2)
  rts <- character(0)
  if (with_rt) {
    rt <- file.path(root, "rt.tsv")
    writeLines("candidate_structure_inchikey_connectivity_layer\trt_target", rt)
    rts <- rt
  }
  list(features = features, annotations = c(ann1, ann2), rts = rts)
}

fa_ms1_table <- function() {
  tidytable::tidytable(
    feature_id = c("F1"),
    candidate_structure_inchikey_connectivity_layer = c("AAA")
  )
}
fa_spectral_table <- function() {
  tidytable::tidytable(
    feature_id = c("F1"),
    candidate_structure_inchikey_connectivity_layer = c("AAA"),
    candidate_score_similarity = c("0.9")
  )
}

# Build RT library table
fa_rt_lib <- function() {
  tidytable::tidytable(
    candidate_structure_inchikey_connectivity_layer = c("AAA"),
    rt_target = c("1.0")
  )
}
