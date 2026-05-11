#' Benchmark evaluation helpers
#'
#' @description Internal helpers to evaluate benchmark predictions against
#' feature-level truth and compute top-k/threshold metrics including MCC.
#' @keywords internal
#' @name benchmark_metrics_utils
NULL

.benchmark_compute_mcc <- function(tp, tn, fp, fn) {
  tp <- as.double(tp)
  tn <- as.double(tn)
  fp <- as.double(fp)
  fn <- as.double(fn)

  den <- sqrt((tp + fp) * (tp + fn) * (tn + fp) * (tn + fn))
  if (!is.finite(den) || den == 0) {
    return(NA_real_)
  }
  (tp * tn - fp * fn) / den
}

.benchmark_split_multivalue <- function(x) {
  if (length(x) == 0L) {
    return(NA_character_)
  }

  x <- as.character(x)

  if (length(x) > 1L) {
    return(unlist(lapply(x, .benchmark_split_multivalue), use.names = FALSE))
  }

  if (is.na(x) || !nzchar(x) || x %in% c("NA", "null", "NULL")) {
    return(NA_character_)
  }

  strsplit(x, "|", fixed = TRUE)[[1L]]
}

.benchmark_pad_to <- function(x, n) {
  x <- as.character(x)
  if (length(x) < n) {
    c(x, rep(NA_character_, n - length(x)))
  } else {
    x[seq_len(n)]
  }
}

.benchmark_expand_predictions <- function(df, pred_name) {
  required <- c("feature_id", "candidate_structure_inchikey_connectivity_layer")
  if (!all(required %in% colnames(df))) {
    return(tidytable::tidytable())
  }

  out <- lapply(seq_len(nrow(df)), function(i) {
    ik <- .benchmark_split_multivalue(df$candidate_structure_inchikey_connectivity_layer[[
      i
    ]])
    rk <- if ("rank_final" %in% colnames(df)) {
      .benchmark_split_multivalue(df$rank_final[[i]])
    } else {
      NA_character_
    }
    sc <- if ("score_final" %in% colnames(df)) {
      .benchmark_split_multivalue(df$score_final[[i]])
    } else {
      NA_character_
    }

    n <- max(length(ik), length(rk), length(sc), 1L)

    data.frame(
      feature_id = rep(as.character(df$feature_id[[i]]), n),
      candidate_ik = .benchmark_pad_to(ik, n),
      rank_final = .benchmark_pad_to(rk, n),
      score_final = .benchmark_pad_to(sc, n),
      prediction = pred_name,
      stringsAsFactors = FALSE
    )
  })

  tidytable::as_tidytable(tidytable::bind_rows(out)) |>
    tidytable::mutate(
      candidate_ik = gsub("-.*", "", candidate_ik, perl = TRUE),
      candidate_ik = tidytable::na_if(candidate_ik, "")
    )
}

#' @keywords internal
benchmark_evaluate_predictions <- function(
  truth_file,
  prediction_files,
  output_file,
  mode
) {
  truth <- safe_fread(
    file = truth_file,
    file_type = "benchmark metadata",
    na.strings = c("", "NA", "null", "NULL"),
    colClasses = "character"
  ) |>
    tidytable::as_tidytable() |>
    tidytable::select(feature_id, truth_ik = inchikey_connectivity_layer) |>
    tidytable::mutate(truth_ik = gsub("-.*", "", truth_ik, perl = TRUE)) |>
    tidytable::distinct(feature_id, .keep_all = TRUE)

  preds_long <- lapply(names(prediction_files), function(nm) {
    p <- prediction_files[[nm]]
    if (!file.exists(p)) {
      return(tidytable::tidytable())
    }

    df <- safe_fread(
      file = p,
      file_type = "benchmark annotations",
      na.strings = c("", "NA", "null", "NULL"),
      colClasses = "character"
    ) |>
      tidytable::as_tidytable()

    .benchmark_expand_predictions(df, nm)
  })

  preds <- tidytable::bind_rows(preds_long)
  if (nrow(preds) == 0L) {
    export_output(x = tidytable::tidytable(), file = output_file)
    return(output_file)
  }

  eval_df <- preds |>
    tidytable::left_join(truth, by = "feature_id") |>
    tidytable::mutate(
      correct = !is.na(candidate_ik) &
        !is.na(truth_ik) &
        candidate_ik == truth_ik,
      rank_num = suppressWarnings(as.integer(rank_final)),
      score_num = suppressWarnings(as.numeric(score_final))
    ) |>
    tidytable::arrange(prediction, feature_id, rank_num, desc(score_num)) |>
    tidytable::mutate(
      rank_use = tidytable::if_else(
        is.na(rank_num),
        tidytable::row_number(),
        rank_num
      ),
      .by = c("prediction", "feature_id")
    )

  ks <- c(1L, 5L, 10L, 25L, 100L)
  topk_rows <- lapply(ks, function(k) {
    eval_df |>
      tidytable::mutate(pred_pos = rank_use <= k) |>
      tidytable::summarize(
        tp = sum(pred_pos & correct),
        tn = sum(!pred_pos & !correct),
        fp = sum(pred_pos & !correct),
        fn = sum(!pred_pos & correct),
        hit_rate_all = mean(pred_pos),
        hit_rate_truth = if (sum(correct, na.rm = TRUE) > 0L) {
          mean(pred_pos[correct])
        } else {
          NA_real_
        },
        .by = "prediction"
      ) |>
      tidytable::mutate(
        mode = mode,
        metric_family = "top_k",
        threshold = NA_real_,
        k = k,
        mcc = mapply(.benchmark_compute_mcc, tp, tn, fp, fn)
      )
  })

  thresholds <- c(0.30, 0.50, 0.70)
  score_df <- eval_df |>
    tidytable::filter(!is.na(score_num))

  score_rows <- lapply(thresholds, function(th) {
    score_df |>
      tidytable::mutate(pred_pos = score_num >= th) |>
      tidytable::summarize(
        tp = sum(pred_pos & correct),
        tn = sum(!pred_pos & !correct),
        fp = sum(pred_pos & !correct),
        fn = sum(!pred_pos & correct),
        hit_rate_all = NA_real_,
        hit_rate_truth = NA_real_,
        .by = "prediction"
      ) |>
      tidytable::mutate(
        mode = mode,
        metric_family = "score_threshold",
        threshold = th,
        k = NA_integer_,
        mcc = mapply(.benchmark_compute_mcc, tp, tn, fp, fn)
      )
  })

  out <- tidytable::bind_rows(topk_rows, score_rows)
  export_output(x = out, file = output_file)
  output_file
}
