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

.benchmark_split_values <- function(x) {
  x <- as.character(x)
  if (length(x) == 0L) {
    return(list(NA_character_))
  }

  invalid <- is.na(x) | !nzchar(x) | x %in% c("NA", "null", "NULL")
  out <- vector("list", length(x))
  out[invalid] <- list(NA_character_)

  if (any(!invalid)) {
    out[!invalid] <- lapply(x[!invalid], function(value) {
      parts <- strsplit(value, "|", fixed = TRUE)[[1L]]
      parts[nzchar(parts) & parts != "null" & parts != "NULL"]
    })
  }

  out
}

.benchmark_split_multivalue <- function(x) {
  if (length(x) == 0L) {
    return(NA_character_)
  }

  split_vals <- .benchmark_split_values(x)
  if (length(split_vals) == 1L) {
    return(split_vals[[1L]])
  }

  unlist(split_vals, use.names = FALSE)
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

  ik_parts <- .benchmark_split_values(
    df$candidate_structure_inchikey_connectivity_layer
  )
  rk_parts <- if ("rank_final" %in% colnames(df)) {
    .benchmark_split_values(df$rank_final)
  } else {
    rep(list(NA_character_), nrow(df))
  }
  sc_parts <- if ("score_final" %in% colnames(df)) {
    .benchmark_split_values(df$score_final)
  } else {
    rep(list(NA_character_), nrow(df))
  }

  n_per_row <- pmax(
    lengths(ik_parts),
    lengths(rk_parts),
    lengths(sc_parts),
    1L
  )

  out <- data.frame(
    feature_id = rep(as.character(df$feature_id), times = n_per_row),
    candidate_ik = unlist(
      Map(.benchmark_pad_to, ik_parts, n_per_row),
      use.names = FALSE
    ),
    rank_final = unlist(
      Map(.benchmark_pad_to, rk_parts, n_per_row),
      use.names = FALSE
    ),
    score_final = unlist(
      Map(.benchmark_pad_to, sc_parts, n_per_row),
      use.names = FALSE
    ),
    prediction = rep(pred_name, sum(n_per_row)),
    stringsAsFactors = FALSE
  )

  tidytable::as_tidytable(out) |>
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
