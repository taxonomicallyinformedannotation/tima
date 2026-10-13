# Test Suite: score_combination ----

library(testthat)

.bio_narrow <- function() {
  data.frame(
    feature_id = c("F1", "F1", "F2"),
    candidate_structure_inchikey_connectivity_layer = c("A", "B", "A"),
    score_biological = c(0.8, 0.4, 0.6),
    candidate_score_pseudo_initial = c(0.5, NA, 0.2),
    score_weighted_bio = c(0.7, 0.3, 0.5),
    stringsAsFactors = FALSE
  )
}

.chemo_narrow <- function() {
  data.frame(
    feature_id = c("F1", "F1", "F3"),
    candidate_structure_inchikey_connectivity_layer = c("A", "B", "Z"),
    score_chemical = c(0.6, 0.9, 0.3),
    score_weighted_chemo = c(0.5, 0.7, 0.2),
    stringsAsFactors = FALSE
  )
}

test_that("extract_bio_scores narrows the weight_bio output to five columns", {
  out <- extract_bio_scores(.bio_narrow())

  expect_named(
    out,
    c(
      "feature_id",
      "candidate_structure_inchikey_connectivity_layer",
      "score_biological",
      "candidate_score_pseudo_initial",
      "score_weighted_bio"
    )
  )
  expect_equal(
    as.data.frame(out),
    as.data.frame(.bio_narrow())[, names(out)]
  )
})

test_that("extract_bio_scores returns a typed empty table", {
  out <- extract_bio_scores(.bio_narrow()[0, ])

  expect_equal(nrow(out), 0L)
  expect_named(
    out,
    c(
      "feature_id",
      "candidate_structure_inchikey_connectivity_layer",
      "score_biological",
      "candidate_score_pseudo_initial",
      "score_weighted_bio"
    )
  )
  expect_type(out$score_biological, "double")
  expect_type(out$candidate_score_pseudo_initial, "double")
})

test_that("extract_bio_scores rejects a non-data-frame input", {
  expect_error(extract_bio_scores("nope"), "data frame")
})

test_that("combine_weighted_scores joins on feature_id and inchikey only", {
  out <- as.data.frame(combine_weighted_scores(
    .bio_narrow(),
    .chemo_narrow(),
    weight_biological = 0.5,
    weight_chemical = 0.3,
    weight_spectral = 0.2
  ))

  # Only (F1, A) and (F1, B) exist on both sides: F2 and F3 are dropped.
  expect_equal(nrow(out), 2L)
  expect_equal(
    out$candidate_structure_inchikey_connectivity_layer[out$feature_id == "F1"],
    c("A", "B")
  )
  expect_false("F2" %in% out$feature_id)
  expect_false("F3" %in% out$feature_id)

  expect_named(
    out,
    c(
      "feature_id",
      "candidate_structure_inchikey_connectivity_layer",
      "score_initial",
      "score_biological",
      "score_chemical",
      "score_final"
    )
  )
  # candidate_score_pseudo_initial is renamed to score_initial.
  expect_equal(out$score_initial, c(0.5, NA))
})

test_that("combine_weighted_scores applies the three weights and renormalizes", {
  out <- as.data.frame(combine_weighted_scores(
    .bio_narrow(),
    .chemo_narrow(),
    weight_biological = 0.5,
    weight_chemical = 0.3,
    weight_spectral = 0.2
  ))

  # (F1, A): full spectral evidence, weights already sum to 1.
  expect_equal(out$score_final[1], (0.8 * 0.5 + 0.6 * 0.3 + 0.5 * 0.2) / 1)
  # (F1, B): MS1-only, the missing spectral score counts as 0.
  expect_equal(out$score_final[2], (0.4 * 0.5 + 0.9 * 0.3 + 0 * 0.2) / 1)

  # Dropping the spectral weight renormalizes over the remaining 0.8, so the
  # same candidates score higher than with a zero-spectral placeholder.
  out2 <- as.data.frame(combine_weighted_scores(
    .bio_narrow(),
    .chemo_narrow(),
    weight_biological = 0.5,
    weight_chemical = 0.3,
    weight_spectral = 0
  ))
  expect_equal(out2$score_final[1], (0.8 * 0.5 + 0.6 * 0.3) / 0.8)
  expect_equal(out2$score_final[2], (0.4 * 0.5 + 0.9 * 0.3) / 0.8)
  expect_gt(out2$score_final[1], out$score_final[1])

  # Weight order follows the components, not the column order in the input.
  out3 <- as.data.frame(combine_weighted_scores(
    .bio_narrow(),
    .chemo_narrow(),
    weight_biological = 0.2,
    weight_chemical = 0.5,
    weight_spectral = 0.3
  ))
  expect_equal(out3$score_final[1], (0.8 * 0.2 + 0.6 * 0.5 + 0.5 * 0.3) / 1)
})

test_that("combine_weighted_scores returns a typed empty table when either side is empty", {
  schema <- c(
    "feature_id",
    "candidate_structure_inchikey_connectivity_layer",
    "score_initial",
    "score_biological",
    "score_chemical",
    "score_final"
  )

  for (args in list(
    list(.bio_narrow()[0, ], .chemo_narrow()),
    list(.bio_narrow(), .chemo_narrow()[0, ]),
    list(.bio_narrow()[0, ], .chemo_narrow()[0, ])
  )) {
    out <- combine_weighted_scores(args[[1]], args[[2]], 0.5, 0.3, 0.2)
    expect_equal(nrow(out), 0L)
    expect_named(out, schema)
    expect_type(out$score_final, "double")
    expect_type(out$score_initial, "double")
  }
})

test_that("combine_weighted_scores rejects non-data-frame inputs", {
  expect_error(
    combine_weighted_scores("x", .chemo_narrow(), 0.5, 0.3, 0.2),
    class = "tima_validation_error"
  )
  expect_error(
    combine_weighted_scores(.bio_narrow(), "x", 0.5, 0.3, 0.2),
    class = "tima_validation_error"
  )
})

test_that("expand_combined_scores_for_filtering merges score_final into the wide table", {
  wide_bio <- data.frame(
    feature_id = c("F1", "F1", "F2"),
    candidate_structure_inchikey_connectivity_layer = c("A", "B", "A"),
    score_biological = c(0.8, 0.4, 0.6),
    candidate_score_pseudo_initial = c(0.5, NA, 0.2),
    score_weighted_bio = c(0.7, 0.3, 0.5),
    stringsAsFactors = FALSE
  )
  wide_chemo <- data.frame(
    feature_id = c("F1", "F1", "F3"),
    candidate_structure_inchikey_connectivity_layer = c("A", "B", "Z"),
    score_chemical = c(0.6, 0.9, 0.3),
    score_weighted_chemo = c(0.5, 0.7, 0.2),
    classifier = c("c1", "c2", "c3"),
    stringsAsFactors = FALSE
  )
  combined <- combine_weighted_scores(wide_bio, wide_chemo, 0.5, 0.3, 0.2)

  out <- as.data.frame(expand_combined_scores_for_filtering(
    combined,
    wide_bio,
    wide_chemo
  ))

  # Only the candidates present in combined_scores survive the semi-join.
  expect_equal(nrow(out), 2L)
  expect_equal(
    out$candidate_structure_inchikey_connectivity_layer,
    c("A", "B")
  )

  # score_final is *renamed onto* score_weighted_chemo, not kept alongside it:
  # the ranking column must carry the combined evidence.
  expect_false("score_final" %in% names(out))
  expect_true("score_weighted_chemo" %in% names(out))
  expect_equal(
    out$score_weighted_chemo,
    as.data.frame(combined)$score_final
  )

  # Chemo-only metadata columns are added; bio-only ones are preserved.
  expect_true("classifier" %in% names(out))
  expect_equal(out$classifier, c("c1", "c2"))
  expect_true("score_weighted_bio" %in% names(out))
  expect_equal(out$score_weighted_bio, c(0.7, 0.3))
  # Column names shared by both wide tables are not duplicated.
  expect_equal(sum(names(out) == "score_chemical"), 1L)
})

test_that("expand_combined_scores_for_filtering tolerates a missing score_final", {
  wide_bio <- data.frame(
    feature_id = c("F1", "F1"),
    candidate_structure_inchikey_connectivity_layer = c("A", "B"),
    score_biological = c(0.8, 0.4),
    candidate_score_pseudo_initial = c(0.5, NA),
    score_weighted_bio = c(0.7, 0.3),
    stringsAsFactors = FALSE
  )
  wide_chemo <- data.frame(
    feature_id = c("F1", "F1"),
    candidate_structure_inchikey_connectivity_layer = c("A", "B"),
    score_chemical = c(0.6, 0.9),
    score_weighted_chemo = c(0.5, 0.7),
    stringsAsFactors = FALSE
  )
  combined_no_final <- .bio_narrow()[
    c(1L),
    c("feature_id", "candidate_structure_inchikey_connectivity_layer")
  ]

  out <- as.data.frame(expand_combined_scores_for_filtering(
    combined_no_final,
    wide_bio,
    wide_chemo
  ))

  expect_equal(nrow(out), 1L)
  expect_false("score_final" %in% names(out))
  # Without a score_final to merge, score_weighted_chemo keeps its original
  # value from the chemo table.
  expect_equal(out$score_weighted_chemo, 0.5)
})

test_that("expand_combined_scores_for_filtering rejects non-data-frame inputs", {
  expect_error(
    expand_combined_scores_for_filtering(
      "x",
      .bio_narrow(),
      .chemo_narrow()
    ),
    class = "tima_validation_error"
  )
})
