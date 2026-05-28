library(testthat)


test_that("benchmark MCC handles regular and degenerate confusion matrices", {
  expect_equal(
    tima:::.benchmark_compute_mcc(tp = 1, tn = 1, fp = 0, fn = 0),
    1
  )

  expect_true(is.na(tima:::.benchmark_compute_mcc(tp = 0, tn = 0, fp = 0, fn = 0)))
})


test_that("benchmark split and pad helpers handle empty, null-like and vector inputs", {
  expect_equal(tima:::.benchmark_split_multivalue("a|b|c"), c("a", "b", "c"))
  expect_equal(tima:::.benchmark_split_multivalue(c("x|y", "z")), c("x", "y", "z"))
  expect_true(is.na(tima:::.benchmark_split_multivalue("")))
  expect_true(is.na(tima:::.benchmark_split_multivalue("NULL")))

  expect_equal(
    tima:::.benchmark_pad_to(c("a", "b"), 4),
    c("a", "b", NA_character_, NA_character_)
  )
  expect_equal(tima:::.benchmark_pad_to(c("a", "b", "c"), 2), c("a", "b"))
})


test_that("benchmark expand predictions returns empty table when required columns missing", {
  bad_df <- tidytable::tidytable(feature_id = "F1")
  expanded <- tima:::.benchmark_expand_predictions(bad_df, pred_name = "demo")

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

  expanded <- tima:::.benchmark_expand_predictions(df, pred_name = "predA")

  expect_true(all(c("feature_id", "candidate_ik", "prediction") %in% names(expanded)))
  expect_equal(unique(expanded$prediction), "predA")
  expect_true(any(expanded$candidate_ik == "AAAAAAAAAAAAAA"))
  expect_true(any(expanded$candidate_ik == "BBBBBBBBBBBBBB"))
  expect_true(any(expanded$rank_final == "2"))
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
    tima:::benchmark_evaluate_predictions(
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

  result <- tima:::benchmark_evaluate_predictions(
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




