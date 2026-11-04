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

  if (!all(invalid)) {
    valid_idx <- which(!invalid)
    vx <- x[valid_idx]
    u_vx <- unique(vx)
    u_parts <- strsplit(u_vx, "|", fixed = TRUE)
    u_parts <- lapply(u_parts, function(parts) {
      parts[nzchar(parts) & parts != "null" & parts != "NULL"]
    })
    out[valid_idx] <- u_parts[fastmatch::fmatch(vx, u_vx)]
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

  n_df <- nrow(df)
  if (n_df == 0L) {
    return(tidytable::tidytable(
      feature_id = character(0),
      candidate_ik = character(0),
      rank_final = character(0),
      score_final = character(0),
      prediction = character(0)
    ))
  }

  ik_parts <- .benchmark_split_values(
    df$candidate_structure_inchikey_connectivity_layer
  )
  has_rk <- "rank_final" %in% colnames(df)
  rk_parts <- if (has_rk) {
    .benchmark_split_values(df$rank_final)
  } else {
    NULL
  }
  has_sc <- "score_final" %in% colnames(df)
  sc_parts <- if (has_sc) {
    .benchmark_split_values(df$score_final)
  } else {
    NULL
  }

  len_ik <- lengths(ik_parts)
  len_rk <- if (has_rk) lengths(rk_parts) else 1L
  len_sc <- if (has_sc) lengths(sc_parts) else 1L
  n_per_row <- pmax(len_ik, len_rk, len_sc, 1L)

  .flatten_padded <- function(parts, lens, target_lens) {
    if (is.null(parts)) {
      return(rep(NA_character_, sum(target_lens)))
    }
    diff_idx <- which(lens != target_lens)
    if (length(diff_idx) > 0L) {
      for (i in diff_idx) {
        parts[[i]] <- .benchmark_pad_to(parts[[i]], target_lens[[i]])
      }
    }
    res <- unlist(parts, use.names = FALSE)
    if (is.null(res)) character(0) else as.character(res)
  }

  total_rows <- sum(n_per_row)
  feature_id <- rep(as.character(df$feature_id), times = n_per_row)
  candidate_ik <- .flatten_padded(ik_parts, len_ik, n_per_row)
  rank_final <- .flatten_padded(rk_parts, len_rk, n_per_row)
  score_final <- .flatten_padded(sc_parts, len_sc, n_per_row)

  rm(df, n_per_row, ik_parts, rk_parts, sc_parts)

  non_na_ik <- which(!is.na(candidate_ik))
  if (length(non_na_ik) > 0L) {
    u_ik <- unique(candidate_ik[non_na_ik])
    u_ik_clean <- gsub("-.*", "", u_ik, perl = TRUE)
    u_ik_clean[!nzchar(u_ik_clean)] <- NA_character_
    candidate_ik[non_na_ik] <- u_ik_clean[
      fastmatch::fmatch(candidate_ik[non_na_ik], u_ik)
    ]
  }

  tidytable::tidytable(
    feature_id = feature_id,
    candidate_ik = candidate_ik,
    rank_final = rank_final,
    score_final = score_final,
    prediction = rep(pred_name, total_rows)
  )
}

#' @keywords internal
#' @include safe_fread.R
#' @include export_output.R
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
  rm(out)
  output_file
}
