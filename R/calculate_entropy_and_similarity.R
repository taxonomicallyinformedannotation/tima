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

  # Lazy sanitize-on-first-use state.
  # This avoids up-front full scans and only sanitizes spectra that need it.
  n_query <- length(query_spectra)
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

  query_checked <- rep(FALSE, n_query)
  lib_checked <- rep(FALSE, n_lib)
  query_sanitized <- logical(n_query)
  lib_sanitized <- logical(n_lib)
  lib_entropy <- rep(NA_real_, n_lib)
  # Lazy cache for neutral precursor conversions (filled on first use)
  lib_neutral_precursors <- rep(NA_real_, n_lib)
  adduct_cache <- new.env(parent = emptyenv())
  # Cache sorted m/z vectors for library spectra to avoid repeated sorts
  lib_mz_sorted_list <- vector("list", n_lib)

  ensure_query_ready <- function(idx) {
    if (!query_checked[[idx]]) {
      sp <- query_spectra[[idx]]
      if (
        is.matrix(sp) &&
          nrow(sp) >= 2L &&
          !is_spectrum_sanitized(sp, tolerance = dalton, ppm = ppm)
      ) {
        query_spectra[[idx]] <<- sanitize_spectrum_matrix(
          sp,
          tolerance = dalton,
          ppm = ppm
        )
        query_sanitized[[idx]] <<- TRUE
      }
      query_checked[[idx]] <<- TRUE
    }
    query_spectra[[idx]]
  }

  ensure_lib_ready <- function(idx) {
    if (!lib_checked[[idx]]) {
      sp <- lib_spectra[[idx]]
      if (
        is.matrix(sp) &&
          nrow(sp) >= 2L &&
          !is_spectrum_sanitized(sp, tolerance = dalton, ppm = ppm)
      ) {
        lib_spectra[[idx]] <<- sanitize_spectrum_matrix(
          sp,
          tolerance = dalton,
          ppm = ppm
        )
        lib_sanitized[[idx]] <<- TRUE
      }
      if (
        is.matrix(lib_spectra[[idx]]) &&
          nrow(lib_spectra[[idx]]) > 0L &&
          ncol(lib_spectra[[idx]]) >= 2L
      ) {
        if (compute_entropy) {
          lib_entropy[[idx]] <<- msentropy::calculate_spectral_entropy(
            lib_spectra[[idx]]
          )
        } else {
          lib_entropy[[idx]] <<- NA_real_
        }
        # Cache sorted m/z vector for this library spectrum to avoid
        # re-sorting inside tight loops.
        lib_mz_sorted_list[[idx]] <<- sort(lib_spectra[[idx]][, 1L])
      }
      lib_checked[[idx]] <<- TRUE
    }
    lib_spectra[[idx]]
  }

  # Progress and local function alias used in the hot loop closure
  progress_counter <- 0L
  call_gnps <- gnps_chain_dp_wrapper

  results <- lapply(
    X = seq_along(query_spectra),
    FUN = function(spectrum_idx) {
      progress_counter <<- progress_counter + 1L
      if (progress_counter %% 500L == 0L) {
        log_info("Processed %d / %d queries", progress_counter, n_queries)
      }

      current_spectrum <- ensure_query_ready(spectrum_idx)
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
        lib_indices_sub <- seq_along(lib_spectra)
      }

      if (length(lib_indices_sub) == 0) {
        return(NULL)
      }

      # Calculate similarities with pre-allocated vectors to reduce overhead.
      n_candidates <- length(lib_indices_sub)
      # Keep invalid/degenerate candidates as NA so they are never selected.
      scores <- rep(NA_real_, n_candidates)
      entropies <- rep(NA_real_, n_candidates)
      matched_counts <- integer(n_candidates)
      scores_forward <- rep(NA_real_, n_candidates)
      scores_reverse <- rep(NA_real_, n_candidates)
      use_gnps <- (method == "gnps")
      q_mz <- current_spectrum[, 1L]

      for (pos_idx in seq_len(n_candidates)) {
        lib_idx <- lib_indices_sub[[pos_idx]]
        lib_spectrum <- ensure_lib_ready(lib_idx)

        if (
          !is.matrix(lib_spectrum) ||
            nrow(lib_spectrum) == 0L ||
            ncol(lib_spectrum) < 2L
        ) {
          next
        }

        target_precursor <- lib_precursors_used[[lib_idx]]
        if (use_gnps) {
          res <- call_gnps(
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
          if (compute_forward_reverse) {
            scores_forward[[pos_idx]] <- as.numeric(res[[3L]])
            scores_reverse[[pos_idx]] <- as.numeric(res[[4L]])
          } else {
            scores_forward[[pos_idx]] <- NA_real_
            scores_reverse[[pos_idx]] <- NA_real_
          }
        } else {
          scores[[pos_idx]] <- as.numeric(calculate_similarity(
            method = method,
            query_spectrum = current_spectrum,
            target_spectrum = lib_spectrum,
            query_precursor = current_precursor,
            target_precursor = target_precursor,
            dalton = dalton,
            ppm = ppm
          ))
          # Use cached sorted m/z vector for the library spectrum to
          # avoid sorting inside the tight loop.
          matched_counts[[pos_idx]] <- .count_matched_peaks(
            q_mz,
            if (length(lib_mz_sorted_list[[lib_idx]]) > 0L) {
              lib_mz_sorted_list[[lib_idx]]
            } else {
              sort(lib_spectrum[, 1L])
            },
            dalton,
            ppm
          )

          # For non-GNPS methods, compute forward/reverse via the C GNPS
          # engine only when needed: after passing threshold and when
          # forward/reverse computation is requested. This avoids unnecessary
          # calls (and respects test mocks that expect no GNPS invocation).
          if (
            !is.na(scores[[pos_idx]]) &&
              scores[[pos_idx]] >= threshold &&
              compute_forward_reverse
          ) {
            fwd_rev <- call_gnps(
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

        entropies[[pos_idx]] <- lib_entropy[[lib_idx]]
      }

      valid_indices <- which(!is.na(scores) & scores >= threshold)

      if (length(valid_indices) > 0L) {
        return(
          tidytable::tidytable(
            feature_id = current_id,
            precursorMz = current_precursor,
            target_id = lib_ids[lib_indices_sub[valid_indices]],
            candidate_spectrum_entropy = entropies[valid_indices],
            candidate_score_similarity = scores[valid_indices],
            candidate_score_similarity_forward = scores_forward[valid_indices],
            candidate_score_similarity_reverse = scores_reverse[valid_indices],
            candidate_count_similarity_peaks_matched = matched_counts[
              valid_indices
            ],
            .similarity_space = space_label
          )
        )
      }

      NULL
    }
  )

  if (any(query_sanitized)) {
    log_warn(
      "Sanitized %d/%d query spectra on-demand before similarity scoring.",
      sum(query_sanitized),
      n_query
    )
  }
  if (any(lib_sanitized)) {
    log_warn(
      "Sanitized %d/%d library spectra on-demand before similarity scoring.",
      sum(lib_sanitized),
      n_lib
    )
  }

  # Log progress summary
  log_info("Processed %d / %d queries", n_queries, n_queries)

  if (all(vapply(X = results, FUN = is.null, FUN.VALUE = logical(1)))) {
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
    result <- tidytable::bind_rows(
      results[!vapply(X = results, FUN = is.null, FUN.VALUE = logical(1))]
    )
  }

  log_complete(ctx, n_comparisons = nrow(result))

  result
}
