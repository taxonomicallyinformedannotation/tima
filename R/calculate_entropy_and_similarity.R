# Helper Functions ----

#' Count matched peaks between query and sorted library spectra
#' @keywords internal
.count_matched_peaks <- function(query_mz, lib_mz_sorted, dalton, ppm) {
  if (length(query_mz) == 0 || length(lib_mz_sorted) == 0) {
    return(0L)
  }

  # Calculate tolerances for all query peaks at once
  tolerances <- pmax(dalton, ppm * query_mz * 1E-6)

  # Use findInterval for fast binary search on the pre-sorted vector
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
#' @include logs_utils.R
#' @include adduct_universe.R
#' @include c_wrappers.R
#' @include annotate_spectra.R
#' @include assert_utils.R
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
  compute_forward_reverse = TRUE,
  compute_entropy = TRUE
) {
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

  log_info(
    "Calculating entropy and similarity for %d spectra",
    length(query_ids)
  )
  log_debug(
    "Method: %s, PPM: %.2f, Dalton: %.2f",
    method,
    ppm,
    dalton
  )

  # Pre-calculate length once for efficiency
  n_queries <- length(query_ids)
  n_lib <- length(lib_spectra)

  # Decide similarity space: precursor m/z or neutral-M (adduct-aware)
  space_label <- "precursor_mz"
  query_precursors_used <- query_precursors
  lib_precursors_used <- lib_precursors
  if (!is.null(query_adducts) && !is.null(lib_adducts)) {
    space_label <- "neutral_M"
    query_precursors_used <- convert_precursor_to_neutral_if_possible(
      precursors = query_precursors,
      adducts = query_adducts
    )
    lib_precursors_used <- convert_precursor_to_neutral_if_possible(
      precursors = lib_precursors,
      adducts = lib_adducts
    )
  }

  # Pre-sort library precursors for fast binary search
  lib_prec_ord <- order(lib_precursors_used, na.last = NA)
  lib_precursors_sorted_vals <- lib_precursors_used[lib_prec_ord]
  lib_precursors_sorted_idx <- lib_prec_ord

  # Pre-compute library spectrum properties lazily on demand.
  # Previously this loop computed `msentropy::calculate_spectral_entropy()` and
  # `sort(sp[, 1L])` upfront across all `n_lib` spectra (~1M spectra), taking
  # ~45-60s per library pass even when only a small subset of library spectra
  # matched above `threshold` and when `lib_mz_sorted_list` was completely
  # unused on the default GNPS batch path.
  need_mz_sorted <- !(method == "gnps" && compute_forward_reverse)
  lib_mz_sorted_list <- if (need_mz_sorted) {
    vector("list", n_lib)
  } else {
    NULL
  }

  # Progress counter and query batch size for efficient C calls

  results <- lapply(
    X = seq_along(query_spectra),
    FUN = function(spectrum_idx) {
      # lapply() runs in order, so the index is the progress count
      if (spectrum_idx %% 500L == 0L) {
        log_info("Processed %d / %d queries", spectrum_idx, n_queries)
      }

      current_spectrum <- query_spectra[[spectrum_idx]]
      current_precursor <- query_precursors_used[spectrum_idx]
      current_id <- query_ids[spectrum_idx]

      # Filter library spectra by precursor mass if not approximating
      if (!approx) {
        low_val <- min(
          current_precursor - dalton,
          current_precursor * (1 - (1E-6 * ppm))
        )
        high_val <- max(
          current_precursor + dalton,
          current_precursor * (1 + (1E-6 * ppm))
        )
        # Closed window [low_val, high_val], as in reduce_library_by_precursor()
        low_idx <- findInterval(
          low_val,
          lib_precursors_sorted_vals,
          left.open = TRUE
        ) +
          1L
        high_idx <- findInterval(
          high_val,
          lib_precursors_sorted_vals,
          left.open = FALSE
        )
        if (low_idx <= high_idx) {
          lib_indices_sub <- lib_precursors_sorted_idx[low_idx:high_idx]
        } else {
          lib_indices_sub <- integer(0)
        }
      } else {
        lib_indices_sub <- seq_along(lib_spectra)
      }

      if (length(lib_indices_sub) == 0) {
        return(NULL)
      }

      # Pre-allocate result vectors once
      n_candidates <- length(lib_indices_sub)
      scores <- rep(NA_real_, n_candidates)
      matched_counts <- integer(n_candidates)
      scores_forward <- rep(NA_real_, n_candidates)
      scores_reverse <- rep(NA_real_, n_candidates)
      use_gnps <- (method == "gnps")
      q_mz <- current_spectrum[, 1L]

      # Process library batch using vectorized C call when possible
      # (reduces per-pair R↔C overhead vs. individual wrapper calls)
      if (use_gnps && compute_forward_reverse) {
        # For GNPS: use batch call if available, falls back to individual calls
        lib_batch_indices <- lib_indices_sub
        lib_batch_precursors <- lib_precursors_used[lib_batch_indices]
        lib_batch_spectra <- lib_spectra[lib_batch_indices]

        # Try fused batch call; falls back to per-pair if it fails
        batch_result <- tryCatch(
          {
            gnps_chain_dp_batch_wrapper(
              x = current_spectrum,
              xPrecursorMz = current_precursor,
              y_list = lib_batch_spectra,
              yPrecursorMz = lib_batch_precursors,
              tolerance = dalton,
              ppm = ppm
            )
          },
          error = function(e) NULL
        )

        if (!is.null(batch_result) && is.matrix(batch_result)) {
          # Batch call succeeded: unpack results
          scores <- as.numeric(batch_result[, 1L])
          matched_counts <- as.integer(batch_result[, 2L])
          scores_forward <- as.numeric(batch_result[, 3L])
          scores_reverse <- as.numeric(batch_result[, 4L])
        } else {
          # Batch call failed or returned NULL: fall back to per-pair loop
          for (pos_idx in seq_len(n_candidates)) {
            lib_idx <- lib_indices_sub[[pos_idx]]
            lib_spectrum <- lib_spectra[[lib_idx]]

            if (
              !is.matrix(lib_spectrum) ||
                nrow(lib_spectrum) == 0L ||
                ncol(lib_spectrum) < 2L
            ) {
              next
            }

            target_precursor <- lib_precursors_used[[lib_idx]]
            res <- gnps_chain_dp_wrapper(
              current_spectrum,
              lib_spectrum,
              current_precursor,
              target_precursor,
              dalton,
              ppm,
              matchedPeaksCount = TRUE
            )
            scores[[pos_idx]] <- as.numeric(res[[1L]])
            matched_counts[[pos_idx]] <- as.integer(res[[2L]])
            scores_forward[[pos_idx]] <- as.numeric(res[[3L]])
            scores_reverse[[pos_idx]] <- as.numeric(res[[4L]])
          }
        }
      } else {
        # Non-GNPS path or forward/reverse not needed: use per-pair calls
        for (pos_idx in seq_len(n_candidates)) {
          lib_idx <- lib_indices_sub[[pos_idx]]
          lib_spectrum <- lib_spectra[[lib_idx]]

          if (
            !is.matrix(lib_spectrum) ||
              nrow(lib_spectrum) == 0L ||
              ncol(lib_spectrum) < 2L
          ) {
            next
          }

          target_precursor <- lib_precursors_used[[lib_idx]]
          scores[[pos_idx]] <- as.numeric(calculate_similarity(
            method = method,
            query_spectrum = current_spectrum,
            target_spectrum = lib_spectrum,
            query_precursor = current_precursor,
            target_precursor = target_precursor,
            dalton = dalton,
            ppm = ppm
          ))
          lib_mz_sorted <- if (!is.null(lib_mz_sorted_list)) {
            lib_mz_sorted_list[[lib_idx]]
          } else {
            NULL
          }
          if (is.null(lib_mz_sorted)) {
            mz_col <- lib_spectrum[, 1L]
            lib_mz_sorted <- if (is.unsorted(mz_col)) sort(mz_col) else mz_col
          }
          matched_counts[[pos_idx]] <- .count_matched_peaks(
            q_mz,
            lib_mz_sorted,
            dalton,
            ppm
          )

          # Compute forward/reverse only for above-threshold matches
          if (
            !is.na(scores[[pos_idx]]) &&
              scores[[pos_idx]] >= threshold &&
              compute_forward_reverse
          ) {
            fwd_rev <- gnps_chain_dp_wrapper(
              current_spectrum,
              lib_spectrum,
              current_precursor,
              target_precursor,
              dalton,
              ppm,
              matchedPeaksCount = TRUE
            )
            scores_forward[[pos_idx]] <- as.numeric(fwd_rev[[3L]])
            scores_reverse[[pos_idx]] <- as.numeric(fwd_rev[[4L]])
          } else {
            scores_forward[[pos_idx]] <- NA_real_
            scores_reverse[[pos_idx]] <- NA_real_
          }
        }
      }

      valid_indices <- which(!is.na(scores) & scores >= threshold)

      if (length(valid_indices) > 0L) {
        n_v <- length(valid_indices)
        return(
          list(
            feature_id = rep(current_id, n_v),
            precursorMz = rep.int(current_precursor, n_v),
            lib_pos = lib_indices_sub[valid_indices],
            candidate_score_similarity = scores[valid_indices],
            candidate_score_similarity_forward = scores_forward[valid_indices],
            candidate_score_similarity_reverse = scores_reverse[valid_indices],
            candidate_count_similarity_peaks_matched = matched_counts[
              valid_indices
            ]
          )
        )
      }

      NULL
    }
  )

  # Log progress summary
  log_info("Processed %d / %d queries", n_queries, n_queries)

  non_null <- results[!vapply(results, is.null, logical(1))]
  rm(results)

  if (length(non_null) == 0L) {
    result <- tidytable::tidytable(
      feature_id = NA_integer_,
      precursorMz = NA_real_,
      target_id = NA_integer_,
      candidate_spectrum_id = NA,
      candidate_spectrum_entropy = NA_real_,
      candidate_score_similarity = NA_real_,
      candidate_score_similarity_forward = NA_real_,
      candidate_score_similarity_reverse = NA_real_,
      candidate_count_similarity_peaks_matched = NA_integer_,
      .similarity_space = NA_character_
    )
  } else {
    matched_lib_pos <- unlist(
      lapply(non_null, `[[`, "lib_pos"),
      use.names = FALSE
    )
    n_total <- length(matched_lib_pos)
    entropies_all <- rep.int(NA_real_, n_total)
    if (compute_entropy && n_total > 0L) {
      u_pos <- unique(matched_lib_pos)
      u_ent <- vapply(
        u_pos,
        function(idx) {
          sp <- lib_spectra[[idx]]
          if (is.matrix(sp) && nrow(sp) > 0L && ncol(sp) >= 2L) {
            msentropy::calculate_spectral_entropy(sp)
          } else {
            NA_real_
          }
        },
        numeric(1)
      )
      entropies_all <- u_ent[fastmatch::fmatch(matched_lib_pos, u_pos)]
    }

    result <- tidytable::tidytable(
      feature_id = unlist(
        lapply(non_null, `[[`, "feature_id"),
        use.names = FALSE
      ),
      precursorMz = unlist(
        lapply(non_null, `[[`, "precursorMz"),
        use.names = FALSE
      ),
      target_id = lib_ids[matched_lib_pos],
      candidate_spectrum_entropy = entropies_all,
      candidate_score_similarity = unlist(
        lapply(non_null, `[[`, "candidate_score_similarity"),
        use.names = FALSE
      ),
      candidate_score_similarity_forward = unlist(
        lapply(non_null, `[[`, "candidate_score_similarity_forward"),
        use.names = FALSE
      ),
      candidate_score_similarity_reverse = unlist(
        lapply(non_null, `[[`, "candidate_score_similarity_reverse"),
        use.names = FALSE
      ),
      candidate_count_similarity_peaks_matched = unlist(
        lapply(non_null, `[[`, "candidate_count_similarity_peaks_matched"),
        use.names = FALSE
      ),
      .similarity_space = rep.int(space_label, n_total)
    )
  }

  rm(non_null, lib_mz_sorted_list)

  log_complete(ctx, n_comparisons = nrow(result))

  result
}
