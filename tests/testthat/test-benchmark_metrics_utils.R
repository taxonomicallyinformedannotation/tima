library(testthat)


test_that("benchmark MCC handles regular and degenerate confusion matrices", {
  expect_equal(
    .benchmark_compute_mcc(tp = 1, tn = 1, fp = 0, fn = 0),
    1
  )

  expect_true(is.na(.benchmark_compute_mcc(
    tp = 0,
    tn = 0,
    fp = 0,
    fn = 0
  )))
})


test_that("benchmark split and pad helpers handle empty, null-like and vector inputs", {
  expect_equal(.benchmark_split_multivalue("a|b|c"), c("a", "b", "c"))
  expect_equal(
    .benchmark_split_multivalue(c("x|y", "z")),
    c("x", "y", "z")
  )
  expect_true(is.na(.benchmark_split_multivalue("")))
  expect_true(is.na(.benchmark_split_multivalue("NULL")))

  expect_equal(
    .benchmark_pad_to(c("a", "b"), 4),
    c("a", "b", NA_character_, NA_character_)
  )
  expect_equal(.benchmark_pad_to(c("a", "b", "c"), 2), c("a", "b"))
})


test_that("benchmark expand predictions returns empty table when required columns missing", {
  bad_df <- tidytable::tidytable(feature_id = "F1")
  expanded <- .benchmark_expand_predictions(bad_df, pred_name = "demo")

  expect_s3_class(expanded, "data.frame")
  expect_equal(nrow(expanded), 0)
})


test_that("benchmark expand predictions normalizes values and supports scalar rank/score", {
  df <- tidytable::tidytable(
    feature_id = c("F1", "F2"),
    candidate_structure_inchikey_connectivity_layer = c(
      "AAAAAAAAAAAAAA|BBBBBBBBBBBBBB-XXXX-XX",
      "CCCCCCCCCCCCCC"
    ),
    rank_final = c("1|2", "1"),
    score_final = c("0.9|0.4", "0.7")
  )

  expanded <- .benchmark_expand_predictions(df, pred_name = "predA")

  expect_true(all(
    c("feature_id", "candidate_ik", "prediction") %in% names(expanded)
  ))
  expect_equal(unique(expanded$prediction), "predA")
  expect_true(any(expanded$candidate_ik == "AAAAAAAAAAAAAA"))
  expect_true(any(expanded$candidate_ik == "BBBBBBBBBBBBBB"))
  expect_true(any(expanded$rank_final == "2"))
})

test_that("benchmark expand predictions handles 0-row input, missing optional columns, and mismatched pipe lengths", {
  empty_df <- tidytable::tidytable(
    feature_id = character(0),
    candidate_structure_inchikey_connectivity_layer = character(0)
  )
  res_empty <- .benchmark_expand_predictions(empty_df, pred_name = "p0")
  expect_equal(nrow(res_empty), 0L)
  expect_equal(
    names(res_empty),
    c("feature_id", "candidate_ik", "rank_final", "score_final", "prediction")
  )

  # Missing rank_final and score_final, plus repeated candidate_ik values
  df_no_opt <- tidytable::tidytable(
    feature_id = c("F1", "F2"),
    candidate_structure_inchikey_connectivity_layer = c(
      "AAAAAAAAAAAAAA-UHFFFAOYSA-N|BBBBBBBBBBBBBB-UHFFFAOYSA-N",
      "AAAAAAAAAAAAAA-UHFFFAOYSA-N"
    )
  )
  res_no_opt <- .benchmark_expand_predictions(df_no_opt, pred_name = "p1")
  expect_equal(nrow(res_no_opt), 3L)
  expect_equal(
    res_no_opt$candidate_ik,
    c("AAAAAAAAAAAAAA", "BBBBBBBBBBBBBB", "AAAAAAAAAAAAAA")
  )
  expect_true(all(is.na(res_no_opt$rank_final)))
  expect_true(all(is.na(res_no_opt$score_final)))

  # Mismatched pipe lengths (3 IKs, 2 ranks, 1 score) -> padded with NA
  df_mismatch <- tidytable::tidytable(
    feature_id = "F1",
    candidate_structure_inchikey_connectivity_layer = "IK1-A|IK2-B|IK3-C",
    rank_final = "1|2",
    score_final = "0.9"
  )
  res_mm <- .benchmark_expand_predictions(df_mismatch, pred_name = "p2")
  expect_equal(nrow(res_mm), 3L)
  expect_equal(res_mm$candidate_ik, c("IK1", "IK2", "IK3"))
  expect_equal(res_mm$rank_final, c("1", "2", NA_character_))
  expect_equal(res_mm$score_final, c("0.9", NA_character_, NA_character_))
})


test_that("benchmark evaluate predictions handles missing prediction files", {
  truth_file <- temp_test_path("bench_truth_missing.tsv")
  output_file <- temp_test_path("bench_metrics_missing.tsv")

  truth <- tidytable::tidytable(
    feature_id = c("F1"),
    inchikey_connectivity_layer = c("AAAAAAAAAAAAAA")
  )
  tidytable::fwrite(truth, truth_file, sep = "\t")

  result <- suppressWarnings(
    benchmark_evaluate_predictions(
      truth_file = truth_file,
      prediction_files = c(predA = temp_test_path("does_not_exist.tsv")),
      output_file = output_file,
      mode = "test"
    )
  )

  expect_equal(result, output_file)
  expect_true(file.exists(output_file))
  expect_identical(file.size(output_file), 0)
})


test_that("benchmark evaluate predictions computes top-k and threshold metrics", {
  truth_file <- temp_test_path("bench_truth.tsv")
  pred_file <- temp_test_path("bench_pred.tsv")
  output_file <- temp_test_path("bench_metrics.tsv")

  truth <- tidytable::tidytable(
    feature_id = c("F1", "F2"),
    inchikey_connectivity_layer = c("AAAAAAAAAAAAAA", "BBBBBBBBBBBBBB")
  )

  preds <- tidytable::tidytable(
    feature_id = c("F1", "F2"),
    candidate_structure_inchikey_connectivity_layer = c(
      "AAAAAAAAAAAAAA|ZZZZZZZZZZZZZZ",
      "YYYYYYYYYYYYYY|BBBBBBBBBBBBBB"
    ),
    rank_final = c("1|2", "1|2"),
    score_final = c("0.95|0.20", "0.80|0.65")
  )

  tidytable::fwrite(truth, truth_file, sep = "\t")
  tidytable::fwrite(preds, pred_file, sep = "\t")

  result <- benchmark_evaluate_predictions(
    truth_file = truth_file,
    prediction_files = c(predA = pred_file),
    output_file = output_file,
    mode = "demo"
  )

  expect_equal(result, output_file)
  metrics <- tidytable::fread(output_file, sep = "\t")

  expect_true("metric_family" %in% names(metrics))
  expect_true("mcc" %in% names(metrics))
  expect_equal(sum(metrics$metric_family == "top_k"), 5)
  expect_equal(sum(metrics$metric_family == "score_threshold"), 3)
  expect_true(all(metrics$prediction == "predA"))
  expect_true(any(is.finite(metrics$mcc) | is.na(metrics$mcc)))
})
