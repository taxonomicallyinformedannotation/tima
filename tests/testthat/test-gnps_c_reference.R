# Test Suite: gnps C core (src/gnps.c) -----------------------------------------
#
# The C entry points are only reachable from R through the .Call() wrappers in
# c_wrappers.R, so every test here drives them through those wrappers.
#
# What the previous suite left uncovered:
#
#   1. The conflict path in score_matched() (gnps.c:594-767). When a shifted
#      match and a direct match claim the same library peak, the greedy per-peak
#      max is no longer globally optimal and the code falls back to an exact
#      Hungarian solve on the dirty cluster. It needs a precursor difference
#      large enough to enable the shifted pass but small enough that x+pdiff
#      still lands on a library peak that is also a direct match for a
#      *different* query peak. That combination is what the .sp_conflict_*
#      fixtures below are built to produce.
#
#   2. gnps_chain_dp_core_api()'s degenerate-input guards (gnps.c:801-824):
#      empty either side, zero total intensity, and a NaN precursor disabling
#      the shifted pass.
#
#   3. gnps_chain_dp_batch_core_api() and C_gnps_chain_dp_batch's argument
#      checks, including the column-major fill of the result matrix.
#
#   4. gnps_reference_score() below is a transliteration of the C algorithm (same
#      steps, same tie-breaks, same order, 0-based indices translated to
#      1-based) used as an independent cross-check on the whole pipeline:
#      direct pass, shifted pass, conflict detection, the greedy branch, the
#      Hungarian branch with its per-cluster intensity accounting, and the
#      forward/reverse normalization. It is deliberately brute force over the
#      permutations of the (at most 4x4) cluster matrix rather than an
#      incremental Hungarian, so it shares no logic with gnps.c:718-765.
#
# Writing this cross-check surfaced one subtlety worth recording, since it is
# easy to get wrong: the Hungarian loop does not just add the score, it also
# accumulates matched_xsum / matched_ysum (gnps.c:762-763). Omitting that makes
# forward and reverse clamp to 1 whenever a library peak went unmatched --
# which is exactly what a naive reference does, and why the fixtures below use
# libraries with unmatched peaks.

library(testthat)

# ---- fixtures ----------------------------------------------------------------

sp_two <- cbind(mz = c(100, 200), intensity = c(100, 80))
sp_two_off <- cbind(mz = c(100.005, 200.005), intensity = c(90, 70))
sp_empty <- cbind(mz = numeric(0), intensity = numeric(0))
sp_zero <- cbind(mz = c(100, 200), intensity = c(0, 0))

# Conflict pair: pdiff = 10 puts x[1] + 10 = 110 on top of y[2] = 110, while
# y[2] is also the direct match of x[2] = 110. One library peak, two claimants.
.sp_conflict_x <- cbind(mz = c(100, 110), intensity = c(100, 80))
.sp_conflict_y <- cbind(mz = c(100, 110), intensity = c(90, 70))

# Conflict with three peaks in the cluster.
.sp_conflict3_x <- cbind(mz = c(100, 105, 110), intensity = c(100, 80, 60))
.sp_conflict3_y <- cbind(mz = c(100, 105, 110), intensity = c(90, 70, 50))

# A pair where the shifted pass is the only way to match x[1]: the library has
# 110 but no 100, so x[1] = 100 only reaches y[1] = 110 once x[1] + 10 = 110.
.sp_shift_only_x <- cbind(mz = c(100, 300), intensity = c(100, 50))
.sp_shift_only_y <- cbind(mz = c(110, 300), intensity = c(90, 45))

# Conflict with deliberately lopsided intensities, plus a clean third peak that
# must stay out of the cluster.
.sp_conflict_mixed_x <- cbind(mz = c(100, 110, 130), intensity = c(10, 100, 40))
.sp_conflict_mixed_y <- cbind(mz = c(100, 110, 130), intensity = c(90, 20, 60))

# ---- independent reference implementation -----------------------------------

.gnps_pair_weight <- function(xi, xsum, yj, ysum) {
  sqrt(xi) / sqrt(xsum) * sqrt(yj) / sqrt(ysum)
}

# Transliteration of gnps_chain_dp_core_api() + score_matched(). Returns the
# documented c(score, matches, score_forward, score_reverse).
gnps_reference_score <- function(x, y, x_pre, y_pre, tol, ppm) {
  x_mz <- as.double(x[, 1L])
  x_int <- as.double(x[, 2L])
  y_mz <- as.double(y[, 1L])
  y_int <- as.double(y[, 2L])
  nx <- length(x_mz)
  ny <- length(y_mz)

  if (nx == 0L || ny == 0L) {
    return(c(0, 0, 0, 0))
  }
  xsum <- sum(x_int)
  ysum <- sum(y_int)
  if (xsum == 0 || ysum == 0) {
    return(c(0, 0, 0, 0))
  }

  eps <- sqrt(.Machine$double.eps)
  do_shift <- !is.na(x_pre) && !is.na(y_pre)
  pdiff <- 0
  if (do_shift) {
    pdiff <- y_pre - x_pre
    if (abs(pdiff) <= tol + ppm * max(x_pre, y_pre) * 1e-6) do_shift <- FALSE
  }

  # Step 1: direct matching, y-centric closest-match, single pass.
  x_tol <- tol + ppm * x_mz * 1e-6 + eps
  dm <- rep(-1L, nx)
  bd <- rep(-1L, ny)
  ilo <- 1L
  for (j in seq_len(ny)) {
    ym <- y_mz[j]
    while (ilo <= nx && x_mz[ilo] + x_tol[ilo] < ym) {
      ilo <- ilo + 1L
    }
    best_i <- -1L
    best_d <- Inf
    if (ilo <= nx) {
      for (i in ilo:nx) {
        xm <- x_mz[i]
        ai <- x_tol[i]
        if (xm - ai > ym) {
          break
        }
        dij <- abs(xm - ym)
        if (dij <= ai && dij < best_d) {
          best_d <- dij
          best_i <- i
        }
      }
    }
    if (best_i >= 1L) {
      if (dm[best_i] < 0L) {
        dm[best_i] <- j
        bd[j] <- best_i
      } else {
        prev_j <- dm[best_i]
        if (best_d < abs(x_mz[best_i] - y_mz[prev_j])) {
          bd[prev_j] <- -1L
          dm[best_i] <- j
          bd[j] <- best_i
        }
      }
    }
  }

  # Step 2: shifted matching on x + pdiff.
  sm <- rep(-1L, nx)
  if (do_shift) {
    sv <- x_mz + pdiff
    s_tol <- tol + ppm * abs(sv) * 1e-6 + eps
    ilo <- 1L
    for (j in seq_len(ny)) {
      ym <- y_mz[j]
      while (ilo <= nx && sv[ilo] + s_tol[ilo] < ym) {
        ilo <- ilo + 1L
      }
      best_i <- -1L
      best_d <- Inf
      if (ilo <= nx) {
        for (i in ilo:nx) {
          if (sv[i] - s_tol[i] > ym) {
            break
          }
          dij <- abs(sv[i] - ym)
          if (dij <= s_tol[i] && dij < best_d) {
            best_d <- dij
            best_i <- i
          }
        }
      }
      if (best_i >= 1L) {
        if (sm[best_i] < 0L) {
          sm[best_i] <- j
        } else {
          prev_j <- sm[best_i]
          if (best_d < abs(sv[best_i] - y_mz[prev_j])) sm[best_i] <- j
        }
      }
    }
  }

  scaled_x <- sqrt(x_int) / sqrt(xsum)
  scaled_y <- sqrt(y_int) / sqrt(ysum)

  total <- 0
  matched <- 0L
  matched_xsum <- 0
  matched_ysum <- 0
  has_conflict <- FALSE
  for (i in seq_len(nx)) {
    sj <- sm[i]
    if (sj < 0L) {
      next
    }
    k <- bd[sj]
    if (k >= 1L && k != i) {
      has_conflict <- TRUE
      break
    }
  }

  # Step 3a: no conflict - greedy max(direct, shifted) per query peak.
  if (!has_conflict) {
    for (i in seq_len(nx)) {
      d <- dm[i]
      s <- sm[i]
      if (d < 0L && s < 0L) {
        next
      }
      ds <- if (d >= 1L) scaled_x[i] * scaled_y[d] else 0
      ss <- if (s >= 1L) scaled_x[i] * scaled_y[s] else 0
      if (ds >= ss) {
        total <- total + ds
        matched_xsum <- matched_xsum + x_int[i]
        matched_ysum <- matched_ysum + y_int[d]
      } else {
        total <- total + ss
        matched_xsum <- matched_xsum + x_int[i]
        matched_ysum <- matched_ysum + y_int[s]
      }
      matched <- matched + 1L
    }
  } else {
    # Step 3b: build the dirty cluster, then solve it exactly.
    x_dirty <- logical(nx)
    y_dirty <- logical(ny)
    for (i in seq_len(nx)) {
      sj <- sm[i]
      if (sj < 0L) {
        next
      }
      k <- bd[sj]
      if (k >= 1L && k != i) {
        x_dirty[i] <- TRUE
        x_dirty[k] <- TRUE
        y_dirty[sj] <- TRUE
        if (dm[i] >= 1L) {
          y_dirty[dm[i]] <- TRUE
        }
        if (sm[k] >= 1L) {
          y_dirty[sm[k]] <- TRUE
        }
        if (dm[k] >= 1L) {
          y_dirty[dm[k]] <- TRUE
        }
        if (sm[i] >= 1L && sm[i] != sj) y_dirty[sm[i]] <- TRUE
      }
    }

    for (i in which(!x_dirty)) {
      d <- dm[i]
      s <- sm[i]
      if (d >= 1L && y_dirty[d]) {
        d <- -1L
      }
      if (s >= 1L && y_dirty[s]) {
        s <- -1L
      }
      if (d < 0L && s < 0L) {
        next
      }
      ds <- if (d >= 1L) scaled_x[i] * scaled_y[d] else 0
      ss <- if (s >= 1L) scaled_x[i] * scaled_y[s] else 0
      if (ds >= ss) {
        total <- total + ds
        matched_xsum <- matched_xsum + x_int[i]
        matched_ysum <- matched_ysum + y_int[d]
      } else {
        total <- total + ss
        matched_xsum <- matched_xsum + x_int[i]
        matched_ysum <- matched_ysum + y_int[s]
      }
      matched <- matched + 1L
    }

    dirty_x <- which(x_dirty)
    dirty_y <- integer(0)
    for (i in dirty_x) {
      if (dm[i] >= 1L && y_dirty[dm[i]] && !(dm[i] %in% dirty_y)) {
        dirty_y <- c(dirty_y, dm[i])
      }
      if (sm[i] >= 1L && y_dirty[sm[i]] && !(sm[i] %in% dirty_y)) {
        dirty_y <- c(dirty_y, sm[i])
      }
    }
    N <- max(length(dirty_x), length(dirty_y))
    if (N > 0) {
      smat <- matrix(0, N, N)
      for (i in dirty_x) {
        r <- which(dirty_x == i)
        if (dm[i] >= 1L) {
          c <- which(dirty_y == dm[i])
          if (length(c)) {
            smat[r, c] <- max(smat[r, c], scaled_x[i] * scaled_y[dm[i]])
          }
        }
        if (sm[i] >= 1L) {
          c <- which(dirty_y == sm[i])
          if (length(c)) {
            smat[r, c] <- max(smat[r, c], scaled_x[i] * scaled_y[sm[i]])
          }
        }
      }
      # Brute force over the permutations - N is at most 4 for a conflict
      # cluster, so this stays a handful of candidates.
      if (N == 1L) {
        perms <- matrix(1L, nrow = 1L, ncol = 1L)
      } else {
        g <- expand.grid(lapply(seq_len(N), function(k) seq_len(N)))
        perms <- as.matrix(g[
          apply(g, 1L, function(r) length(unique(r)) == N),
          ,
          drop = FALSE
        ])
      }
      best <- 0
      best_xy <- c(0, 0)
      best_n <- 0L
      for (p in seq_len(nrow(perms))) {
        perm <- perms[p, ]
        sc <- 0
        mx <- 0
        my <- 0
        nn <- 0L
        for (cc in seq_len(N)) {
          v <- smat[perm[cc], cc]
          if (v > 0) {
            sc <- sc + v
            nn <- nn + 1L
            if (perm[cc] <= length(dirty_x)) {
              mx <- mx + x_int[dirty_x[perm[cc]]]
            }
            if (cc <= length(dirty_y)) my <- my + y_int[dirty_y[cc]]
          }
        }
        if (sc > best) {
          best <- sc
          best_xy <- c(mx, my)
          best_n <- nn
        }
      }
      total <- total + best
      matched <- matched + best_n
      matched_xsum <- matched_xsum + best_xy[1L]
      matched_ysum <- matched_ysum + best_xy[2L]
    }
  }

  score_fwd <- 0
  score_rev <- 0
  if (matched_ysum > 0) {
    score_fwd <- total * sqrt(ysum) / sqrt(matched_ysum)
    if (score_fwd > 1) score_fwd <- 1
  }
  if (matched_xsum > 0) {
    score_rev <- total * sqrt(xsum) / sqrt(matched_xsum)
    if (score_rev > 1) score_rev <- 1
  }
  c(total, matched, score_fwd, score_rev)
}

# Branch-flag introspection for the randomized cross-check below. Duplicates
# only the decision logic (which peaks the direct and shifted passes claim and
# whether they collide), not the scoring, so a test can assert the case it
# generated actually reached the branch it means to exercise.
gnps_reference_branches <- function(x, y, x_pre, y_pre, tol, ppm) {
  x_mz <- as.double(x[, 1L])
  x_int <- as.double(x[, 2L])
  y_mz <- as.double(y[, 1L])
  y_int <- as.double(y[, 2L])
  nx <- length(x_mz)
  ny <- length(y_mz)
  if (nx == 0L || ny == 0L || sum(x_int) == 0 || sum(y_int) == 0) {
    return(c(had_conflict = FALSE, did_shift = FALSE, n_matches = 0L))
  }
  eps <- sqrt(.Machine$double.eps)
  do_shift <- !is.na(x_pre) && !is.na(y_pre)
  pdiff <- 0
  if (do_shift) {
    pdiff <- y_pre - x_pre
    if (abs(pdiff) <= tol + ppm * max(x_pre, y_pre) * 1e-6) do_shift <- FALSE
  }
  x_tol <- tol + ppm * x_mz * 1e-6 + eps
  dm <- rep(-1L, nx)
  bd <- rep(-1L, ny)
  ilo <- 1L
  for (j in seq_len(ny)) {
    ym <- y_mz[j]
    while (ilo <= nx && x_mz[ilo] + x_tol[ilo] < ym) {
      ilo <- ilo + 1L
    }
    best_i <- -1L
    best_d <- Inf
    if (ilo <= nx) {
      for (i in ilo:nx) {
        xm <- x_mz[i]
        ai <- x_tol[i]
        if (xm - ai > ym) {
          break
        }
        dij <- abs(xm - ym)
        if (dij <= ai && dij < best_d) {
          best_d <- dij
          best_i <- i
        }
      }
    }
    if (best_i >= 1L) {
      if (dm[best_i] < 0L) {
        dm[best_i] <- j
        bd[j] <- best_i
      } else {
        prev_j <- dm[best_i]
        if (best_d < abs(x_mz[best_i] - y_mz[prev_j])) {
          bd[prev_j] <- -1L
          dm[best_i] <- j
          bd[j] <- best_i
        }
      }
    }
  }
  sm <- rep(-1L, nx)
  if (do_shift) {
    sv <- x_mz + pdiff
    s_tol <- tol + ppm * abs(sv) * 1e-6 + eps
    ilo <- 1L
    for (j in seq_len(ny)) {
      ym <- y_mz[j]
      while (ilo <= nx && sv[ilo] + s_tol[ilo] < ym) {
        ilo <- ilo + 1L
      }
      best_i <- -1L
      best_d <- Inf
      if (ilo <= nx) {
        for (i in ilo:nx) {
          if (sv[i] - s_tol[i] > ym) {
            break
          }
          dij <- abs(sv[i] - ym)
          if (dij <= s_tol[i] && dij < best_d) {
            best_d <- dij
            best_i <- i
          }
        }
      }
      if (best_i >= 1L) {
        if (sm[best_i] < 0L) {
          sm[best_i] <- j
        } else {
          prev_j <- sm[best_i]
          if (best_d < abs(sv[best_i] - y_mz[prev_j])) sm[best_i] <- j
        }
      }
    }
  }
  had_conflict <- FALSE
  for (i in seq_len(nx)) {
    sj <- sm[i]
    if (sj < 0L) {
      next
    }
    k <- bd[sj]
    if (k >= 1L && k != i) {
      had_conflict <- TRUE
      break
    }
  }
  c(
    had_conflict = had_conflict,
    did_shift = do_shift,
    n_matches = sum(dm > 0L) + sum(sm > 0L)
  )
}

# ---- the conflict / Hungarian path ------------------------------------------

test_that("a shifted match competing with a direct match is resolved exactly", {
  # Hand-derived: the greedy per-peak max would take the shifted match for
  # x[1] (0.4930) and the direct match for x[2] (0.4410), but those share
  # y[2], so only one can have it. The exact optimum is the two direct
  # matches, 0.5590 + 0.4410 = 1.0000.
  res <- gnps_chain_dp_wrapper(
    x = .sp_conflict_x,
    y = .sp_conflict_y,
    xPrecursorMz = 500,
    yPrecursorMz = 510,
    tolerance = 0.001,
    ppm = 1,
    matchedPeaksCount = TRUE
  )
  expect_equal(res[1L], 0.9999755, tolerance = 1e-6)
  expect_equal(res[2L], 2L)
  ref <- gnps_reference_score(
    .sp_conflict_x,
    .sp_conflict_y,
    500,
    510,
    0.001,
    1
  )
  expect_equal(res, ref, tolerance = 1e-9)
})

test_that("the conflict path matches the reference on multi-peak clusters", {
  for (pdiff in c(-20, -10, -5, 5, 10, 20)) {
    withr::local_options(list()) # keep each iteration independent
    res <- gnps_chain_dp_wrapper(
      x = .sp_conflict3_x,
      y = .sp_conflict3_y,
      xPrecursorMz = 500,
      yPrecursorMz = 500 + pdiff,
      tolerance = 0.001,
      ppm = 1,
      matchedPeaksCount = TRUE
    )
    ref <- gnps_reference_score(
      .sp_conflict3_x,
      .sp_conflict3_y,
      500,
      500 + pdiff,
      0.001,
      1
    )
    expect_equal(res, ref, tolerance = 1e-9, info = paste("pdiff", pdiff))
  }
})

test_that("the conflict path keeps clean peaks out of the cluster", {
  # x[3] = 130 / y[3] = 130 is a direct match untouched by the shifted pass,
  # so it must be scored greedily while x[1] and x[2] go through Hungarian.
  # This is also the case where the cluster's intensity accounting shows up in
  # the forward/reverse normalization: matched_ysum must include both y[1]
  # (from the cluster) and y[3] (from the clean peak), i.e. 90 + 60 = 150.
  res <- gnps_chain_dp_wrapper(
    x = .sp_conflict_mixed_x,
    y = .sp_conflict_mixed_y,
    xPrecursorMz = 500,
    yPrecursorMz = 490,
    tolerance = 0.001,
    ppm = 1,
    matchedPeaksCount = TRUE
  )
  ref <- gnps_reference_score(
    .sp_conflict_mixed_x,
    .sp_conflict_mixed_y,
    500,
    490,
    0.001,
    1
  )
  expect_equal(res, ref, tolerance = 1e-9)

  # The cluster optimum is the single strongest pair (x[2] -> y[1]), so the
  # cluster contributes one match and the clean peak contributes another.
  expect_equal(res[2L], 2L)
  # matched_xsum = 100 (x[2]) + 40 (x[3]) = 140, matched_ysum = 90 + 60 = 150.
  expect_equal(res[3L], res[1L] * sqrt(170) / sqrt(150), tolerance = 1e-9)
  expect_equal(res[4L], res[1L] * sqrt(150) / sqrt(140), tolerance = 1e-9)
})

test_that("a negative precursor difference is symmetric to its positive", {
  forward <- gnps_chain_dp_wrapper(
    sp_two,
    sp_two_off,
    510,
    500,
    0.01,
    10,
    matchedPeaksCount = TRUE
  )
  backward <- gnps_chain_dp_wrapper(
    sp_two,
    sp_two_off,
    500,
    510,
    0.01,
    10,
    matchedPeaksCount = TRUE
  )
  expect_equal(forward[1L], backward[1L], tolerance = 1e-12)
  expect_equal(forward[2L], backward[2L])
})

# ---- shifted matching -------------------------------------------------------

test_that("the shifted pass is skipped when the precursor difference is noise", {
  # gnps.c:818-824 - |pdiff| <= tol + ppm*max_pre*1e-6 disables the shifted
  # pass, so the result must equal the same call with a zero precursor gap.
  noshift <- gnps_chain_dp_wrapper(
    .sp_conflict_x,
    .sp_conflict_y,
    500,
    500.0000001,
    0.01,
    10,
    matchedPeaksCount = TRUE
  )
  zero_gap <- gnps_chain_dp_wrapper(
    .sp_conflict_x,
    .sp_conflict_y,
    500,
    500,
    0.01,
    10,
    matchedPeaksCount = TRUE
  )
  expect_equal(noshift, zero_gap)

  # A precursor difference that enables a shifted-only match does move the
  # result, and the reference tracks it.
  shifted <- gnps_chain_dp_wrapper(
    .sp_shift_only_x,
    .sp_shift_only_y,
    500,
    510,
    0.001,
    1,
    matchedPeaksCount = TRUE
  )
  expect_equal(shifted, c(1, 2, 1, 1))
  expect_equal(
    shifted,
    gnps_reference_score(
      .sp_shift_only_x,
      .sp_shift_only_y,
      500,
      510,
      0.001,
      1
    ),
    tolerance = 1e-9
  )

  # The same pair with a precursor gap inside the noise window cannot use the
  # shifted match, so only the direct 300/300 pair survives.
  unshifted <- gnps_chain_dp_wrapper(
    .sp_shift_only_x,
    .sp_shift_only_y,
    500,
    500,
    0.001,
    1,
    matchedPeaksCount = TRUE
  )
  expect_equal(unshifted, c(1 / 3, 1L, 0.5773503, 0.5773503), tolerance = 1e-6)
  expect_equal(
    unshifted,
    gnps_reference_score(
      .sp_shift_only_x,
      .sp_shift_only_y,
      500,
      500,
      0.001,
      1
    ),
    tolerance = 1e-9
  )

  # A precursor difference too large for the shifted values to land anywhere
  # useful falls back to the direct-only result.
  too_far <- gnps_chain_dp_wrapper(
    .sp_shift_only_x,
    .sp_shift_only_y,
    500,
    520,
    0.001,
    1,
    matchedPeaksCount = TRUE
  )
  expect_equal(too_far, unshifted)
})

test_that("a NaN precursor disables the shifted pass", {
  for (args in list(list(NaN, 510), list(500, NaN), list(NaN, NaN))) {
    res <- gnps_chain_dp_wrapper(
      .sp_conflict_x,
      .sp_conflict_y,
      args[[1L]],
      args[[2L]],
      0.001,
      1,
      matchedPeaksCount = TRUE
    )
    ref <- gnps_reference_score(
      .sp_conflict_x,
      .sp_conflict_y,
      args[[1L]],
      args[[2L]],
      0.001,
      1
    )
    expect_equal(
      res,
      ref,
      tolerance = 1e-9,
      info = paste(args[[1L]], args[[2L]])
    )
  }
})

# ---- degenerate inputs ------------------------------------------------------

test_that("empty spectra score zero on either side", {
  for (args in list(
    list(sp_empty, sp_two),
    list(sp_two, sp_empty),
    list(sp_empty, sp_empty)
  )) {
    expect_equal(
      gnps_chain_dp_wrapper(
        args[[1L]],
        args[[2L]],
        500,
        500,
        0.01,
        10,
        matchedPeaksCount = TRUE
      ),
      c(0, 0, 0, 0)
    )
  }
})

test_that("all-zero intensities score zero", {
  # gnps.c:814 - a zero intensity sum returns before any matching.
  expect_equal(
    gnps_chain_dp_wrapper(sp_zero, sp_two, 500, 500, 0.01, 10, TRUE),
    c(0, 0, 0, 0)
  )
  expect_equal(
    gnps_chain_dp_wrapper(sp_two, sp_zero, 500, 500, 0.01, 10, TRUE),
    c(0, 0, 0, 0)
  )
})

test_that("a single-peak spectrum scores 1 against itself and 0 against a disjoint one", {
  one <- cbind(mz = 100, intensity = 100)
  expect_equal(
    gnps_chain_dp_wrapper(one, one, 500, 500, 0.01, 10, TRUE),
    c(1, 1, 1, 1)
  )
  expect_equal(
    gnps_chain_dp_wrapper(
      one,
      cbind(mz = 900, intensity = 100),
      500,
      500,
      0.01,
      10,
      TRUE
    ),
    c(0, 0, 0, 0)
  )
})

# ---- tolerance semantics ----------------------------------------------------

test_that("the tolerance is the wider of the Da and ppm windows", {
  # 0.005 Da offsets at m/z 100-200.
  expect_equal(
    gnps_chain_dp_wrapper(sp_two, sp_two_off, 500, 500, 0.01, 0, TRUE)[2L],
    2L
  )
  expect_equal(
    gnps_chain_dp_wrapper(sp_two, sp_two_off, 500, 500, 0, 1000, TRUE)[2L],
    2L
  )
  expect_equal(
    gnps_chain_dp_wrapper(sp_two, sp_two_off, 500, 500, 0.001, 10, TRUE)[2L],
    0L
  )
  expect_equal(
    gnps_chain_dp_wrapper(sp_two, sp_two_off, 500, 500, 0, 10, TRUE)[2L],
    0L
  )
})

test_that("the ppm window scales with mass", {
  q <- cbind(mz = 100, intensity = 100)
  l <- cbind(mz = 100.05, intensity = 100)
  expect_equal(gnps_chain_dp_wrapper(q, l, 500, 500, 0, 400, TRUE)[2L], 0L)
  expect_equal(gnps_chain_dp_wrapper(q, l, 500, 500, 0, 600, TRUE)[2L], 1L)

  q2 <- cbind(mz = 1000, intensity = 100)
  l2 <- cbind(mz = 1000.05, intensity = 100)
  expect_equal(gnps_chain_dp_wrapper(q2, l2, 500, 500, 0, 40, TRUE)[2L], 0L)
  expect_equal(gnps_chain_dp_wrapper(q2, l2, 500, 500, 0, 60, TRUE)[2L], 1L)
})

# ---- forward / reverse normalization ---------------------------------------

test_that("forward and reverse are clamped to 1", {
  # score_fwd = score * sqrt(ysum) / sqrt(matched_ysum), and matched_ysum <=
  # ysum, so the ratio is >= 1; the clamp at gnps.c:837 is what keeps the
  # forward score inside [0, 1] when the library has unmatched peaks.
  q <- cbind(mz = c(100, 200), intensity = c(100, 80))
  l <- cbind(mz = c(100.005, 900), intensity = c(90, 500))
  res <- gnps_chain_dp_wrapper(
    q,
    l,
    500,
    500,
    0.01,
    10,
    matchedPeaksCount = TRUE
  )

  expect_gt(res[3L], res[1L])
  expect_lte(res[3L], 1)
  expect_gte(res[4L], res[1L])
  expect_lte(res[4L], 1)
})

test_that("forward and reverse are zero when nothing matches", {
  expect_equal(
    gnps_chain_dp_wrapper(
      cbind(mz = c(100, 200), intensity = c(50, 50)),
      cbind(mz = c(500, 600), intensity = c(50, 50)),
      500,
      500,
      0.01,
      10,
      matchedPeaksCount = TRUE
    ),
    c(0, 0, 0, 0)
  )
})

# ---- batch API -------------------------------------------------------------

test_that("the batch wrapper scores every library entry independently", {
  libs <- list(sp_two, sp_two_off, sp_empty, .sp_conflict_y)
  out <- gnps_chain_dp_batch_wrapper(
    x = sp_two,
    xPrecursorMz = 500,
    y_list = libs,
    yPrecursorMz = c(500, 500, 500, 510),
    tolerance = 0.01,
    ppm = 10
  )

  expect_equal(dim(out), c(length(libs), 4L))
  for (k in seq_along(libs)) {
    single <- gnps_chain_dp_wrapper(
      sp_two,
      libs[[k]],
      500,
      c(500, 500, 500, 510)[k],
      0.01,
      10,
      matchedPeaksCount = TRUE
    )
    expect_equal(out[k, ], single, info = paste("library", k))
  }
})

test_that("the batch wrapper handles a single library and an empty list", {
  one <- gnps_chain_dp_batch_wrapper(sp_two, 500, list(sp_two), 500, 0.01, 10)
  expect_equal(dim(one), c(1L, 4L))
  expect_equal(
    one[1L, ],
    gnps_chain_dp_wrapper(
      sp_two,
      sp_two,
      500,
      500,
      0.01,
      10,
      matchedPeaksCount = TRUE
    )
  )

  none <- gnps_chain_dp_batch_wrapper(sp_two, 500, list(), numeric(0), 0.01, 10)
  expect_equal(dim(none), c(0L, 4L))
})

test_that("the batch wrapper validates its R-facing arguments", {
  expect_error(
    gnps_chain_dp_batch_wrapper(
      sp_two,
      500,
      list(sp_two, sp_two),
      500,
      0.01,
      10
    ),
    "one entry per y_list element"
  )
  expect_error(
    gnps_chain_dp_batch_wrapper(sp_two, 500, sp_two, 500, 0.01, 10),
    "y_list must be a list"
  )
})

# ---- score-only form -------------------------------------------------------

test_that("matchedPeaksCount = FALSE drops the three auxiliary values", {
  full <- gnps_chain_dp_wrapper(
    sp_two,
    sp_two_off,
    500,
    500,
    0.01,
    10,
    matchedPeaksCount = TRUE
  )
  bare <- gnps_chain_dp_wrapper(
    sp_two,
    sp_two_off,
    500,
    500,
    0.01,
    10,
    matchedPeaksCount = FALSE
  )
  expect_length(bare, 1L)
  expect_equal(bare[[1L]], full[1L])
})

# ---- randomized cross-check -------------------------------------------------

# Fixed seed so a failure is always reproducible. Spectra are generated to
# satisfy the documented preconditions (sorted, well-separated, non-negative
# intensities) and the precursor gap is drawn from a range that straddles the
# enable/disable threshold, so all the shifted/conflict branches get hit.
# Half of each iteration is random (broad regression net) and half is drawn from
# a conflict-shaped template, so the Hungarian branch is reached on every run
# rather than depending on the seed. The template places a shifted value exactly
# on a library peak that is also a direct match of a different query peak.
test_that("the C core agrees with the reference on generated spectra", {
  set.seed(42)

  conflict_iterations <- 0L
  shifted_iterations <- 0L
  zero_match_iterations <- 0L

  for (iter in seq_len(60L)) {
    if (iter %% 2L == 1L) {
      # Random pair, peaks on a 5 Da grid so the sanitized-spectra assumptions
      # the chain algorithm depends on hold.
      base <- sample(seq(100, 200, by = 5), 1L)
      n <- sample(2:4, 1L)
      x <- cbind(
        mz = as.double(base + 5 * sample(0:8, n)),
        intensity = as.double(sample(1:200, n))
      )
      y <- cbind(
        mz = as.double(base + 5 * sample(0:8, n)),
        intensity = as.double(sample(1:200, n))
      )
      tol <- 5
      ppm <- 10
      pdiff <- sample(c(-30, -15, -5, 0, 5, 15, 30), 1L)
    } else {
      # Conflict template. With x = (a, a+d, a+2d) and y = (a, a+2d):
      #   direct  y[1] = a is claimed by x[1], y[2] = a+2d is claimed by x[3]
      #   shifted x[2] + d = a+2d wants y[2] as well
      # so y[2] has a direct claimant (x[3]) different from its shifted
      # claimant (x[2]) - exactly the collision at gnps.c:569-575 that sends
      # control down the Hungarian branch.
      a <- sample(seq(100, 300, by = 10), 1L)
      pdiff <- sample(c(-20, -10, 10, 20), 1L)
      x <- cbind(
        mz = as.double(sort(c(a, a + pdiff, a + 2 * pdiff))),
        intensity = as.double(sample(1:200, 3L))
      )
      y <- cbind(
        mz = as.double(sort(c(a, a + 2 * pdiff))),
        intensity = as.double(sample(1:200, 2L))
      )
      tol <- 0.001
      ppm <- 0
    }
    x_pre <- 500
    y_pre <- 500 + pdiff

    res <- gnps_chain_dp_wrapper(
      x,
      y,
      x_pre,
      y_pre,
      tol,
      ppm,
      matchedPeaksCount = TRUE
    )
    ref <- gnps_reference_score(x, y, x_pre, y_pre, tol, ppm)
    expect_equal(res, ref, tolerance = 1e-9, info = paste("iteration", iter))

    flags <- gnps_reference_branches(x, y, x_pre, y_pre, tol, ppm)
    if (as.logical(flags[["had_conflict"]])) {
      conflict_iterations <- conflict_iterations + 1L
    }
    if (as.logical(flags[["did_shift"]])) {
      shifted_iterations <- shifted_iterations + 1L
    }
    if (res[2L] == 0L) zero_match_iterations <- zero_match_iterations + 1L
  }

  # The generator has to actually reach the branches it is here to cover.
  expect_true(
    conflict_iterations >= 5L,
    info = paste(
      "only",
      conflict_iterations,
      "iterations reached the conflict path"
    )
  )
  expect_true(
    shifted_iterations >= 5L,
    info = paste("only", shifted_iterations, "iterations ran the shifted pass")
  )
  expect_true(
    zero_match_iterations >= 1L,
    info = paste(
      "only",
      zero_match_iterations,
      "iterations produced a zero match"
    )
  )
})
