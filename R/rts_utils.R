#' @title Normalize retention times to minutes
#' @description Converts a numeric retention time vector to minutes based on a specified
#' unit or automatic heuristic detection. Automatic detection treats values as seconds
#' if all non-missing RTs are > 120 or if the 95th percentile exceeds 180 (common upper
#' bound for minutes in typical gradients). Otherwise assumes minutes.
#'
#' @param rt_vec Numeric vector (character allowed; will be coerced) of retention times.
#' @param unit Character scalar: one of 'auto', 'seconds', or 'minutes'.
#' @param quiet Logical; if FALSE, logs detection/conversion decisions.
#'
#' @return Numeric vector of RTs in minutes.
#' @keywords internal
normalize_rt_to_minutes <- function(
  rt_vec,
  unit = c("auto", "seconds", "minutes"),
  quiet = FALSE
) {
  unit <- match.arg(unit)
  if (is.null(rt_vec)) {
    return(rt_vec)
  }
  if (is.character(rt_vec)) {
    rt_vec <- suppressWarnings(as.numeric(rt_vec))
  }

  if (!is.numeric(rt_vec)) {
    stop("rt_vec must be coercible to numeric")
  }
  if (length(rt_vec) == 0L) {
    return(rt_vec)
  }

  rts <- rt_vec
  rts_non_na <- rts[!is.na(rts)]
  if (length(rts_non_na) == 0L) {
    return(rts)
  }

  decided_unit <- unit
  if (unit == "auto") {
    q95 <- stats::quantile(x = rts_non_na, probs = 0.95, na.rm = TRUE, type = 7)
    min_val <- min(rts_non_na, na.rm = TRUE)
    max_val <- max(rts_non_na, na.rm = TRUE)
    # Heuristic: treat as seconds if all RT > 2 OR 95th percentile > 45 OR max > 180
    if ((min_val > 2) || (q95 > 45) || (max_val > 180)) {
      decided_unit <- "seconds"
    } else {
      decided_unit <- "minutes"
    }
  }

  if (!quiet) {
    logger::log_debug(
      "Feature RT normalization: detected unit '{decided_unit}' (requested: '{unit}')"
    )
  }

  if (decided_unit == "seconds") {
    rts <- rts / 60
    if (!quiet) {
      logger::log_info(
        "Converted feature retention times from seconds to minutes"
      )
    }
  }

  rts
}
