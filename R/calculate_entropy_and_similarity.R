# Helper Functions ----

#' Count matched peaks between query and library spectra
#' @keywords internal
.count_matched_peaks <- function(query_mz, lib_mz, dalton, ppm) {
  if (length(query_mz) == 0 || length(lib_mz) == 0) {
    return(0L)
  }

  # Avoid re-sorting spectra that are already sanitized and m/z-sorted.
  lib_mz_sorted <- if (is.unsorted(lib_mz, na.rm = FALSE)) {
    sort(lib_mz)
  } else {
    lib_mz
  }

  # Calculate tolerances for all query peaks at once
  tolerances <- pmax(dalton, ppm * query_mz * 1E-6)

  # Use findInterval for fast binary search
  lower_bounds <- query_mz - tolerances
  upper_bounds <- query_mz + tolerances
  low_idx <- findInterval(lower_bounds, lib_mz_sorted)
  high_idx <- findInterval(upper_bounds, lib_mz_sorted, rightmost.closed = TRUE)

  sum(high_idx > low_idx)
}

#' @title Calculate entropy score
#'
#' @description This function calculates spectral entropy and similarity scores
#'     by comparing query spectra against library spectra. Uses entropy-based
#'     similarity measures to match MS2 fragmentation patterns.
#'
#' @include calculate_similarity.R
#' @include validations_utils.R
#'
#' @param lib_ids [character] Character vector of library spectrum IDs
#' @param lib_precursors [numeric] Numeric vector of library precursor m/z
#'     values
#' @param lib_spectra [list] List of library spectra (each a matrix of
#'     mz/intensity)
#' @param query_ids [character] Character vector of query spectrum IDs
#' @param query_precursors [numeric] Numeric vector of query precursor m/z
#'     values
#' @param query_spectra [list] List of query spectra (each a matrix of
#'     mz/intensity)
#' @param method [character] Character string similarity method to use
#' @param dalton [numeric] Numeric absolute mass tolerance in Daltons
#' @param ppm [numeric] Numeric relative mass tolerance in ppm
#' @param threshold [numeric] Numeric minimum similarity threshold (0-1)
#' @param approx [logical] Logical whether to perform approximate matching
#'     without
#'     precursor mass filtering
#' @param compute_forward_reverse [logical] Compute forward/reverse GNPS scores
#'     for retained candidates. Defaults to TRUE for entropy and FALSE for
#'     other methods.
#' @param compute_entropy [logical] Compute per-spectrum entropy values for the
#'     returned candidates. Set to FALSE to skip the entropy pass when the
#'     caller only needs similarity and forward/reverse scores.
#'
#' @return Data frame with spectrum IDs, entropy scores, and similarity scores
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # Calculate entropy-based similarity
#' results <- calculate_entropy_and_similarity(
#'   lib_ids = library_ids,
#'   lib_precursors = library_mz,
#'   lib_spectra = library_spectra_list,
#'   query_ids = feature_ids,
#'   query_precursors = feature_mz,
#'   query_spectra = feature_spectra_list,
#'   method = "entropy",
#'   dalton = 0.01,
#'   ppm = 10,
#'   threshold = 0.7,
#'   approx = FALSE
#' )
#' }
calculate_entropy_and_similarity <- function(
  lib_ids,
  lib_precursors,
  lib_spectra,
  query_ids,
  query_precursors,
  query_spectra,
  method,
  dalton,
  ppm,
  threshold,
  approx,
  query_adducts = NULL,
  lib_adducts = NULL,
  compute_forward_reverse = NULL,
  compute_entropy = TRUE
) {
  # Simplified, chunked driver (balanced defaults)
  chunk_size <- 256L

  ctx <- log_operation(
    "calculate_entropy_similarity",
    n_library = length(lib_ids),
    n_query = length(query_ids),
    method = method
  )

  assert_choice(method, VALID_SIMILARITY_METHODS, "method")
  if (
    length(lib_ids) != length(lib_spectra) ||
      length(lib_ids) != length(lib_precursors)
  ) {
    cli::cli_abort(
      "lib_ids, lib_precursors, and lib_spectra must have the same length",
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }
  if (
    length(query_ids) != length(query_spectra) ||
      length(query_ids) != length(query_precursors)
  ) {
    cli::cli_abort(
      "query_ids, query_precursors, and query_spectra must have the same length",
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }
  assert_scalar_numeric(dalton, "dalton", min = 0, max = Inf)
  assert_scalar_numeric(ppm, "ppm", min = 0, max = Inf)
  assert_scalar_numeric(threshold, "threshold", min = 0, max = 1)
  assert_flag(approx, "approx")
  if (!is.null(compute_forward_reverse)) {
    assert_flag(compute_forward_reverse, "compute_forward_reverse")
  }
  assert_flag(compute_entropy, "compute_entropy")
  compute_forward_reverse <- compute_forward_reverse %||% (method == "entropy")

  if (!is.null(query_adducts) && length(query_adducts) != length(query_ids)) {
    cli::cli_abort(
      "query_adducts must have the same length as query_ids",
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }
  if (!is.null(lib_adducts) && length(lib_adducts) != length(lib_ids)) {
    cli::cli_abort(
      "lib_adducts must have the same length as lib_ids",
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }
  if (!is.null(query_adducts)) {
    query_adducts <- as.character(query_adducts)
  }
  if (!is.null(lib_adducts)) {
    lib_adducts <- as.character(lib_adducts)
  }

  log_info(
    "Calculating entropy and similarity for %d spectra",
    length(query_ids)
  )
  log_debug("Method: %s, PPM: %.2f, Dalton: %.2f", method, ppm, dalton)

  n_queries <- length(query_ids)
  n_lib <- length(lib_spectra)

  # Pre-sort library precursors for fast binary search
  lib_precursors_sorted <- sort(lib_precursors, index.return = TRUE)
  lib_precursors_sorted_vals <- lib_precursors_sorted$x
  lib_precursors_sorted_idx <- lib_precursors_sorted$ix

  # Lazy caches: avoid expensive full-library precomputes for very large libraries
  lib_neutral_precursors <- rep(NA_real_, n_lib)
  adduct_cache <- new.env(parent = emptyenv())

  # Initialize entropy cache (computed on first use per library spectrum)
  lib_entropy <- rep(NA_real_, n_lib)
  # Note: computing entropy for all library spectra up-front on multi-million libraries
  # can be extremely slow. Compute per-needed candidate and store in lib_entropy.

  call_gnps <- gnps_chain_dp_wrapper

  has_adduct_metadata <- !is.null(query_adducts) && !is.null(lib_adducts)
  get_neutral_precursor <- function(precursor, adduct) {
    if (
      !is.finite(precursor) ||
        is.na(precursor) ||
        is.na(adduct) ||
        !nzchar(adduct)
    ) {
      return(NA_real_)
    }
    key <- paste0(adduct, "|", sprintf("%.12f", precursor))
    if (exists(key, envir = adduct_cache, inherits = FALSE)) {
      return(get(key, envir = adduct_cache, inherits = FALSE))
    }
    converted <- convert_precursor_to_neutral_if_possible(
      precursors = precursor,
      adducts = adduct
    )
    converted_value <- if (
      length(converted) == 1L && is.finite(converted) && converted > 0
    ) {
      converted
    } else {
      NA_real_
    }
    assign(key, converted_value, envir = adduct_cache)
    converted_value
  }

  out_results <- vector("list", n_queries)
  progress_counter <- 0L

  # Process queries in chunks to limit peak memory and reuse buffers
  for (start_idx in seq(1L, n_queries, by = chunk_size)) {
    end_idx <- min(n_queries, start_idx + chunk_size - 1L)
    for (spectrum_idx in seq.int(start_idx, end_idx)) {
      progress_counter <- progress_counter + 1L
      if (progress_counter %% 500L == 0L) {
        log_info("Processed %d / %d queries", progress_counter, n_queries)
      }

      current_spectrum <- query_spectra[[spectrum_idx]]
      if (
        !is.matrix(current_spectrum) ||
          nrow(current_spectrum) == 0L ||
          ncol(current_spectrum) < 2L
      ) {
        out_results[[spectrum_idx]] <- NULL
        next
      }
      current_precursor <- query_precursors[spectrum_idx]
      current_id <- query_ids[spectrum_idx]

      # Determine library candidate indices via binary search on precursors
      if (!approx) {
        low_val <- min(
          current_precursor - dalton,
          current_precursor * (1 - (1E-6 * ppm))
        )
        high_val <- max(
          current_precursor + dalton,
          current_precursor * (1 + (1E-6 * ppm))
        )
        low_idx <- findInterval(
          low_val,
          lib_precursors_sorted_vals,
          left.open = FALSE
        ) +
          1L
        high_idx <- findInterval(
          high_val,
          lib_precursors_sorted_vals,
          left.open = TRUE
        )
        if (low_idx <= high_idx) {
          lib_indices_sub <- lib_precursors_sorted_idx[low_idx:high_idx]
        } else {
          lib_indices_sub <- integer(0)
        }
      } else {
        lib_indices_sub <- seq_len(n_lib)
      }

      if (length(lib_indices_sub) == 0L) {
        out_results[[spectrum_idx]] <- NULL
        next
      }

      n_candidates <- length(lib_indices_sub)
      scores <- rep(NA_real_, n_candidates)
      entropies <- rep(NA_real_, n_candidates)
      matched_counts <- integer(n_candidates)
      scores_forward <- rep(NA_real_, n_candidates)
      scores_reverse <- rep(NA_real_, n_candidates)
      similarity_space <- rep("precursor_mz", n_candidates)

      current_query_adduct <- if (!is.null(query_adducts)) {
        query_adducts[[spectrum_idx]]
      } else {
        NA_character_
      }
      query_neutral_precursor <- NA_real_
      if (
        has_adduct_metadata &&
          !is.na(current_query_adduct) &&
          nzchar(current_query_adduct)
      ) {
        query_neutral_precursor <- get_neutral_precursor(
          current_precursor,
          current_query_adduct
        )
      }

      for (pos_idx in seq_len(n_candidates)) {
        lib_idx <- lib_indices_sub[pos_idx]
        lib_spectrum <- lib_spectra[[lib_idx]]
        if (
          !is.matrix(lib_spectrum) ||
            nrow(lib_spectrum) == 0L ||
            ncol(lib_spectrum) < 2L
        ) {
          next
        }

        target_precursor <- lib_precursors[[lib_idx]]
        target_query_adduct <- if (!is.null(lib_adducts)) {
          lib_adducts[[lib_idx]]
        } else {
          NA_character_
        }
        query_precursor_value <- current_precursor
        target_precursor_value <- target_precursor
        space_label <- "precursor_mz"

        if (
          has_adduct_metadata &&
            !is.na(current_query_adduct) &&
            nzchar(current_query_adduct) &&
            !is.na(target_query_adduct) &&
            nzchar(target_query_adduct) &&
            !identical(current_query_adduct, target_query_adduct)
        ) {
          # Lazy compute library neutral precursor if needed
          target_neutral <- lib_neutral_precursors[lib_idx]
          if (
            is.na(target_neutral) &&
              !is.null(lib_adducts) &&
              !is.na(target_query_adduct) &&
              nzchar(target_query_adduct)
          ) {
            target_neutral <- get_neutral_precursor(
              target_precursor,
              target_query_adduct
            )
            lib_neutral_precursors[lib_idx] <- target_neutral
          }
          if (
            is.finite(query_neutral_precursor) &&
              is.finite(target_neutral) &&
              query_neutral_precursor > 0 &&
              target_neutral > 0
          ) {
            query_precursor_value <- query_neutral_precursor
            target_precursor_value <- target_neutral
            space_label <- "neutral_M"
          }
        }

        if (method == "entropy") {
          # Entropy similarity is computed via msentropy and only call GNPS for forward/reverse if requested
          score <- tryCatch(
            .entropy_similarity_call(
              current_spectrum,
              lib_spectrum,
              dalton,
              ppm
            ),
            error = function(e) {
              log_warn(
                "Entropy similarity failed for %s vs %s: %s",
                current_id,
                lib_ids[[lib_idx]],
                e$message
              )
              return(0.0)
            }
          )
          scores[pos_idx] <- as.numeric(score)
          if (as.numeric(score) >= threshold) {
            matched_counts[pos_idx] <- .count_matched_peaks(
              current_spectrum[, 1L],
              lib_spectrum[, 1L],
              dalton,
              ppm
            )
          }
          if (
            isTRUE(compute_forward_reverse) && as.numeric(score) >= threshold
          ) {
            # compute forward/reverse using GNPS chain if requested
            res2 <- tryCatch(
              call_gnps(
                current_spectrum,
                lib_spectrum,
                query_precursor_value,
                target_precursor_value,
                dalton,
                ppm,
                matchedPeaksCount = TRUE
              ),
              error = function(e) {
                log_warn(
                  "GNPS chain failed for forward/reverse %s vs %s: %s",
                  current_id,
                  lib_ids[[lib_idx]],
                  e$message
                )
                return(c(NA_real_, NA_integer_, NA_real_, NA_real_))
              }
            )
            scores_forward[pos_idx] <- as.numeric(res2[[3L]])
            scores_reverse[pos_idx] <- as.numeric(res2[[4L]])
          }
        } else {
          # GNPS / Cosine methods: use fused GNPS chain DP for matching + scoring
          res <- tryCatch(
            call_gnps(
              current_spectrum,
              lib_spectrum,
              query_precursor_value,
              target_precursor_value,
              dalton,
              ppm,
              matchedPeaksCount = TRUE
            ),
            error = function(e) {
              log_warn(
                "GNPS chain failed for %s vs %s: %s",
                current_id,
                lib_ids[[lib_idx]],
                e$message
              )
              return(c(0.0, 0L, 0.0, 0.0))
            }
          )
          scores[pos_idx] <- as.numeric(res[[1L]])
          matched_counts[pos_idx] <- as.integer(res[[2L]])
          if (identical(method, "gnps")) {
            # GNPS method always exposes forward/reverse scores
            scores_forward[pos_idx] <- as.numeric(res[[3L]])
            scores_reverse[pos_idx] <- as.numeric(res[[4L]])
          } else if (
            isTRUE(compute_forward_reverse) &&
              as.numeric(res[[1L]]) >= threshold
          ) {
            # For other methods (e.g., cosine) compute forward/reverse only when requested and candidate passes threshold
            scores_forward[pos_idx] <- as.numeric(res[[3L]])
            scores_reverse[pos_idx] <- as.numeric(res[[4L]])
          }
        }

        similarity_space[pos_idx] <- space_label
        if (isTRUE(compute_entropy)) {
          if (is.na(lib_entropy[lib_idx])) {
            # compute and cache entropy for this library spectrum
            lib_entropy[lib_idx] <- if (
              is.matrix(lib_spectrum) &&
                nrow(lib_spectrum) > 0L &&
                ncol(lib_spectrum) >= 2L
            ) {
              msentropy::calculate_spectral_entropy(lib_spectrum)
            } else {
              NA_real_
            }
          }
          entropies[pos_idx] <- lib_entropy[lib_idx]
        }
      }

      valid_indices <- which(!is.na(scores) & scores >= threshold)
      if (length(valid_indices) > 0L) {
        df_out <- data.frame(
          feature_id = rep(current_id, length(valid_indices)),
          precursorMz = rep(current_precursor, length(valid_indices)),
          target_id = lib_ids[lib_indices_sub[valid_indices]],
          candidate_spectrum_entropy = if (isTRUE(compute_entropy)) {
            entropies[valid_indices]
          } else {
            rep(NA_real_, length(valid_indices))
          },
          candidate_score_similarity = scores[valid_indices],
          candidate_score_similarity_forward = scores_forward[valid_indices],
          candidate_score_similarity_reverse = scores_reverse[valid_indices],
          candidate_count_similarity_peaks_matched = as.integer(matched_counts[
            valid_indices
          ]),
          .similarity_space = similarity_space[valid_indices],
          stringsAsFactors = FALSE
        )
        out_results[[spectrum_idx]] <- df_out
      } else {
        out_results[[spectrum_idx]] <- NULL
      }
    }
  }

  # Consolidate results
  non_null <- out_results[!vapply(out_results, is.null, logical(1))]
  if (length(non_null) == 0L) {
    result <- tidytable::tidytable(
      feature_id = NA_integer_,
      precursorMz = NA_real_,
      target_id = NA_integer_,
      candidate_spectrum_entropy = NA_real_,
      candidate_score_similarity = NA_real_,
      candidate_score_similarity_forward = NA_real_,
      candidate_score_similarity_reverse = NA_real_,
      candidate_count_similarity_peaks_matched = NA_integer_,
      .similarity_space = NA_character_
    )
  } else {
    if (all(vapply(non_null, is.data.frame, logical(1)))) {
      result_df <- do.call(rbind, non_null)
      result_df$candidate_count_similarity_peaks_matched <- as.integer(
        result_df$candidate_count_similarity_peaks_matched
      )
      result <- tidytable::as_tidytable(result_df)
    } else {
      result <- tidytable::bind_rows(non_null)
    }
  }

  log_complete(ctx, n_comparisons = nrow(result))
  result
}
