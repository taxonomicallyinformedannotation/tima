#' @title Evidence-based ion species discovery
#'
#' @description
#' Scalable discovery of supported ion species from a feature table and a
#' typed adduct universe. Each (feature, adduct) hypothesis implies a neutral
#' mass `M`; hypotheses agreeing on `M` (within ppm tolerance) and RT window
#' constitute mutual **evidence** for all such hypotheses.
#'
#' **Evidence model**: `evidence_count` is NOT a quality score. It is the count
#' of independent features that mutually support this (feature, adduct) hypothesis
#' via mass/RT consistency. A count of 1 means the hypothesis is self-consistent
#' with library masses. A count of 2+ means peer features agree on the same
#' neutral mass under different (or same) adduct assumptions. Higher counts
#' provide additional confidence but do not invalidate count=1 hypotheses.
#'
#' Discovers uniformly:
#'   * adduct switches    (`[M+H]+` vs `[M+Na]+`)
#'   * multimers          (`[M+H]+` vs `[2M+H]+`)
#'   * multicharged       (`[M+H]+` vs `[M+2H]2+`)
#'   * neutral losses     (`[M+H]+` vs `[M+H-H2O]+`)
#'   * isotopologues      (`[M+H]+` vs `[M1+H]+`)
#'
#' Memory/time scaling: NO feature×universe Cartesian materialization. For
#' each adduct row we compute implied neutral mass vectorially over all
#' features at once, then prefilter against the supplied library exact masses
#' (sorted + `findInterval`, O(log m)). A hard per-feature cap further
#' protects against pathological universes. With 50k features and a typical
#' library this stays in low GB and runs in seconds.
#'
#' @include adduct_universe.R
#' @include parse_adduct.R
#' @keywords internal
#' @name mass_evidence
NULL

#' Isotope mass unit (Da) for `Mn` isotopologues — set to the 13C/12C mass
#' difference; identical to legacy `ISOTOPE_MASS_SHIFT_DALTONS`.
#' @keywords internal
EVIDENCE_ISOTOPE_SHIFT_DA <- 1.003355

#' Compute implied neutral mass for a (feature mz, adduct row) pair, taking
#' isotopologue shifts, per-monomer cluster/loss attachment (the "inside-
#' multimer" variants such as `[2(M-H2O)+H]+`), and signed charge into account.
#'
#' Formula:
#'   M = (|z| * (mz - iso_shift) - adduct_mass) / n_mer - adduct_mass_per_monomer
#'
#' For the historical outside-only universe rows `adduct_mass_per_monomer == 0`
#' which reduces to the legacy formula.
#'
#' @keywords internal
implied_neutral_mass <- function(
  mz,
  n_mer,
  z,
  adduct_mass,
  n_iso = 0L,
  adduct_mass_per_monomer = 0
) {
  iso_shift <- as.integer(n_iso) * EVIDENCE_ISOTOPE_SHIFT_DA
  (abs(as.integer(z)) * (as.numeric(mz) - iso_shift) - adduct_mass) /
    as.integer(n_mer) -
    adduct_mass_per_monomer
}

#' Test whether each query mass has at least one library mass within the given
#' ppm tolerance. Uses two `findInterval()` calls (much cheaper than computing
#' nearest distances for every query). Returns a logical vector.
#' @keywords internal
.has_library_match_within_ppm <- function(
  masses,
  exact_masses_sorted,
  tolerance_ppm
) {
  n <- length(masses)
  if (n == 0L || length(exact_masses_sorted) == 0L) {
    return(rep(FALSE, n))
  }
  tol <- tolerance_ppm * 1e-6
  lo <- masses * (1 - tol)
  hi <- masses * (1 + tol)
  # findInterval(x, vec) returns largest i with vec[i] <= x.
  # A library mass is in [lo, hi] iff findInterval(hi) > findInterval(lo - eps).
  fi_lo <- findInterval(lo, exact_masses_sorted, left.open = TRUE)
  fi_hi <- findInterval(hi, exact_masses_sorted, left.open = FALSE)
  fi_hi > fi_lo
}

#' Minimum ppm error to nearest exact mass for each query mass. Vectorized
#' binary search via [findInterval()].
#' @keywords internal
.nearest_exact_mass_ppm <- function(masses, exact_masses_sorted) {
  n <- length(masses)
  if (n == 0L || length(exact_masses_sorted) == 0L) {
    return(rep(Inf, n))
  }
  idx <- findInterval(masses, exact_masses_sorted, all.inside = FALSE)
  L <- length(exact_masses_sorted)
  lo_idx <- pmax.int(1L, idx)
  hi_idx <- pmin.int(L, idx + 1L)
  lo <- exact_masses_sorted[lo_idx]
  hi <- exact_masses_sorted[hi_idx]
  d_lo <- abs(masses - lo)
  d_hi <- abs(masses - hi)
  d <- pmin.int(d_lo, d_hi)
  # ppm relative to the closer library mass
  ref <- ifelse(d_lo <= d_hi, lo, hi)
  d / pmax.int(ref, 1e-9) * 1e6
}

#' Closest exact-mass index for each query mass.
#' @keywords internal
.nearest_exact_mass_index <- function(masses, exact_masses_sorted) {
  n <- length(masses)
  if (n == 0L || length(exact_masses_sorted) == 0L) {
    return(integer(n))
  }
  idx <- findInterval(masses, exact_masses_sorted, all.inside = FALSE)
  L <- length(exact_masses_sorted)
  lo_idx <- pmax.int(1L, idx)
  hi_idx <- pmin.int(L, idx + 1L)
  lo <- exact_masses_sorted[lo_idx]
  hi <- exact_masses_sorted[hi_idx]
  d_lo <- abs(masses - lo)
  d_hi <- abs(masses - hi)
  ifelse(d_lo <= d_hi, lo_idx, hi_idx)
}

#' Greedy ppm-tolerance clustering of a sorted numeric vector. NA values
#' each get their own singleton cluster.
#'
#' Fully vectorized: with sorted ascending values, single-link clustering at a
#' relative tolerance reduces to consecutive-pair gap tests + cumsum.
#'
#' @keywords internal
.ppm_cluster_sorted <- function(values, tolerance_ppm) {
  n <- length(values)
  if (n == 0L) {
    return(integer(0L))
  }
  if (n == 1L) {
    return(1L)
  }
  tol <- tolerance_ppm * 1e-6
  # New-cluster mask: TRUE wherever the gap to the previous (sorted) value
  # exceeds tol * current value, or either side is NA.
  prev_v <- c(NA_real_, values[-n])
  gap <- values - prev_v
  boundary <- is.na(values) | is.na(prev_v) | (gap > tol * values)
  # First element always starts a cluster.
  boundary[1L] <- TRUE
  cumsum(boundary)
}

#' Add chemically informed plausibility metadata to the typed universe.
#'
#' The evidence engine uses these tiers to prefer common/core ion species and
#' to require stronger support for exotic combinations (metals, uncommon
#' neutral losses, solvent clusters, etc.).
#' @keywords internal
annotate_adduct_universe_metadata <- function(universe, polarity) {
  common_carriers <- switch(
    polarity,
    pos = c("H", "Na", "K", "NH4", "Mg", "Ca"),
    neg = c("H", "F", "Cl", "Br"),
    character()
  )
  common_losses <- c("H2O", "H4O2", "H6O3", "H8O4", "NH3", "H3N", "CO", "CO2")
  common_clusters <- c("H2O", "C2H3N")

  out <- tidytable::as_tidytable(universe)
  carrier_names <- lapply(out$carriers, names)
  loss_names <- lapply(out$losses, names)
  cluster_names <- lapply(out$clusters, names)

  has_only_common_carriers <- vapply(
    X = carrier_names,
    FUN = function(x) {
      if (length(x) == 0L) {
        return(TRUE)
      }
      all(x %in% common_carriers)
    },
    FUN.VALUE = logical(1L)
  )
  has_only_common_losses <- vapply(
    X = loss_names,
    FUN = function(x) {
      if (length(x) == 0L) {
        return(TRUE)
      }
      all(x %in% common_losses)
    },
    FUN.VALUE = logical(1L)
  )
  has_only_common_clusters <- vapply(
    X = cluster_names,
    FUN = function(x) {
      if (length(x) == 0L) {
        return(TRUE)
      }
      all(x %in% common_clusters)
    },
    FUN.VALUE = logical(1L)
  )

  baseline_name <- switch(
    polarity,
    pos = "[M+H]+",
    neg = "[M-H]-",
    NA_character_
  )
  out <- out |>
    tidytable::mutate(
      n_carrier_terms = lengths(carrier_names),
      n_loss_terms = lengths(loss_names),
      n_cluster_terms = lengths(cluster_names),
      has_cluster = n_cluster_terms > 0L,
      has_loss = n_loss_terms > 0L,
      has_only_common_carriers = has_only_common_carriers,
      has_only_common_losses = has_only_common_losses,
      has_only_common_clusters = has_only_common_clusters,
      has_exotic_carrier = !has_only_common_carriers,
      is_baseline = adduct == baseline_name,
      adduct_tier = as.integer(
        ifelse(
          is_baseline,
          0L,
          ifelse(
            !has_loss &
              !has_cluster &
              has_only_common_carriers &
              abs(z) == 1L &
              n_mer <= 2L,
            1L,
            ifelse(
              !has_cluster &
                has_only_common_carriers &
                has_only_common_losses &
                n_loss_terms <= 1L &
                abs(z) <= 2L &
                n_mer <= 2L,
              2L,
              ifelse(
                !has_exotic_carrier &
                  n_loss_terms <= 2L &
                  n_cluster_terms <= 1L &
                  abs(z) <= 2L &
                  n_mer <= 3L,
                3L,
                4L
              )
            )
          )
        )
      ),
      requires_core_support = adduct_tier >= 2L,
      min_cluster_support = as.integer(
        ifelse(
          adduct_tier <= 1L,
          1L,
          ifelse(adduct_tier == 2L, 2L, ifelse(adduct_tier == 3L, 3L, 4L))
        )
      ),
      is_core_adduct = adduct_tier <= 1L
    )
  out
}

#' Build the evidence-supported hypothesis table (scalable).
#'
#' @param df_fea_min tidytable with columns `feature_id`, `rt`, `mz`, `sample`.
#' @param universe typed universe returned by [build_adduct_universe()].
#' @param tolerance_ppm numeric mass tolerance (ppm).
#' @param tolerance_rt numeric RT tolerance.
#' @param ms_mode "pos" or "neg".
#' @param min_neutral_mass,max_neutral_mass plausibility bounds.
#' @param exact_masses optional numeric vector of library exact masses. When
#'   provided, only hypotheses whose implied neutral mass is within
#'   `tolerance_ppm` of at least one library mass are retained. This is the
#'   primary memory-control lever.
#' @param max_hypotheses_per_feature integer cap per feature (excluding the
#'   baseline adduct, which is always retained). Set to `Inf` to disable.
#'
#' @return tidytable with one row per supported (feature, adduct) hypothesis.
#'
#' @keywords internal
build_evidence_supported_hypotheses <- function(
  df_fea_min,
  universe,
  tolerance_ppm,
  tolerance_rt,
  ms_mode,
  min_neutral_mass = 1,
  max_neutral_mass = 5000,
  exact_masses = NULL,
  max_hypotheses_per_feature = 64L
) {
  baseline_adduct <- switch(
    ms_mode,
    "pos" = "[M+H]+",
    "neg" = "[M-H]-",
    NA_character_
  )

  feats_dt <- tidytable::as_tidytable(df_fea_min)[,
    .(feature_id, rt = as.numeric(rt), mz = as.numeric(mz), sample)
  ]
  feats_dt <- unique(feats_dt)

  if (nrow(feats_dt) == 0L || nrow(universe) == 0L) {
    return(empty_evidence_table())
  }

  # Pre-extract feature columns to plain vectors for vectorized math.
  feature_ids <- as.character(feats_dt$feature_id)
  feature_rts <- as.numeric(feats_dt$rt)
  feature_mzs <- as.numeric(feats_dt$mz)
  feature_samples <- as.character(feats_dt$sample)

  uni_dt <- annotate_adduct_universe_metadata(universe, polarity = ms_mode)
  if (!"adduct_mass_per_monomer" %in% colnames(uni_dt)) {
    uni_dt <- uni_dt |> tidytable::mutate(adduct_mass_per_monomer = 0)
  }
  uni_dt <- uni_dt[,
    .(
      adduct,
      n_mer,
      z,
      adduct_mass,
      adduct_mass_per_monomer,
      n_iso,
      adduct_tier,
      is_core_adduct,
      requires_core_support,
      min_cluster_support
    )
  ]
  uni_dt <- unique(uni_dt)
  adduct_v <- as.character(uni_dt$adduct)
  n_mer_v <- as.integer(uni_dt$n_mer)
  z_v <- as.integer(uni_dt$z)
  am_v <- as.numeric(uni_dt$adduct_mass)
  am_pm_v <- as.numeric(uni_dt$adduct_mass_per_monomer)
  niso_v <- as.integer(uni_dt$n_iso)
  tier_v <- as.integer(uni_dt$adduct_tier)
  core_v <- as.logical(uni_dt$is_core_adduct)
  req_core_v <- as.logical(uni_dt$requires_core_support)
  # min_cluster_support available for future tier-based evidence requirements
  # min_support_v <- as.integer(uni_dt$min_cluster_support)
  K <- length(adduct_v)

  exact_masses_sorted <- sort(as.numeric(exact_masses))
  exact_masses_sorted <- exact_masses_sorted[
    is.finite(exact_masses_sorted) & exact_masses_sorted > 0
  ]
  use_library_prefilter <- length(exact_masses_sorted) > 0L
  tol_ppm_num <- as.numeric(tolerance_ppm)

  # Adaptive cap for scale: keep runtime/memory roughly linear with features.
  # Target at most ~1.5M rows after capping.
  n_feat <- length(feature_ids)
  if (is.infinite(max_hypotheses_per_feature)) {
    cap_eff <- Inf
    cap_in <- Inf
  } else {
    cap_in <- suppressWarnings(as.integer(max_hypotheses_per_feature))
    if (is.na(cap_in) || cap_in < 1L) {
      cap_in <- 64L
    }
    cap_adaptive <- as.integer(max(
      8L,
      floor(1.5e6 / max(1L, n_feat))
    ))
    cap_eff <- min(cap_in, cap_adaptive)
  }

  if (is.finite(cap_in) && is.finite(cap_eff) && cap_eff < cap_in) {
    log_warn(
      paste0(
        "Evidence engine reduced max_hypotheses_per_feature from %d to %d ",
        "for scale control (~1.5M candidate-row target). If you suspect rare ",
        "ion species are being truncated, rerun on smaller batches or disable ",
        "the cap explicitly."
      ),
      as.integer(cap_in),
      as.integer(cap_eff)
    )
  }

  log_info(
    "Evidence engine: %d features x %d adducts (prefilter=%s, cap=%s)",
    n_feat,
    K,
    ifelse(use_library_prefilter, "on", "off"),
    ifelse(is.finite(cap_eff), as.character(cap_eff), "Inf")
  )

  # Streaming, vectorized hypothesis generation per adduct row.
  # Hot path uses a CHEAP inclusion check (two findIntervals) — no nearest
  # distance computation. The actual ppm error is computed in ONE batched
  # call after the loop, on the (much smaller) kept set.
  kept_feat_idx <- vector("list", K)
  kept_aidx <- vector("list", K)
  kept_M <- vector("list", K)
  filled <- 0L

  t_loop <- Sys.time()
  log_every <- max(1L, K %/% 10L)

  for (ui in seq_len(K)) {
    iso_shift <- niso_v[ui] * EVIDENCE_ISOTOPE_SHIFT_DA
    implied <- (abs(z_v[ui]) * (feature_mzs - iso_shift) - am_v[ui]) /
      n_mer_v[ui] -
      am_pm_v[ui]

    ok <- is.finite(implied) &
      implied >= min_neutral_mass &
      implied <= max_neutral_mass
    if (!any(ok)) {
      next
    }

    if (use_library_prefilter) {
      kept_pos <- which(ok)
      hit <- .has_library_match_within_ppm(
        masses = implied[kept_pos],
        exact_masses_sorted = exact_masses_sorted,
        tolerance_ppm = tol_ppm_num
      )
      if (!any(hit)) {
        next
      }
      pos <- kept_pos[hit]
    } else {
      pos <- which(ok)
    }

    filled <- filled + 1L
    kept_feat_idx[[filled]] <- as.integer(pos)
    kept_aidx[[filled]] <- rep.int(ui, length(pos))
    kept_M[[filled]] <- implied[pos]

    if (ui %% log_every == 0L) {
      log_debug(
        "Evidence pass: %d/%d adducts processed (%.1fs)",
        ui,
        K,
        as.numeric(Sys.time() - t_loop, units = "secs")
      )
    }
  }
  log_debug(
    "Evidence pass loop: %d adducts in %.2fs (%d kept hypotheses)",
    K,
    as.numeric(Sys.time() - t_loop, units = "secs"),
    sum(lengths(kept_feat_idx))
  )

  total_kept <- sum(lengths(kept_feat_idx))

  if (filled == 0L || total_kept == 0L) {
    return(.materialize_baseline_only(
      feature_ids = feature_ids,
      feature_rts = feature_rts,
      feature_mzs = feature_mzs,
      feature_samples = feature_samples,
      universe = uni_dt,
      baseline_adduct = baseline_adduct
    ))
  }

  kept_feat_idx <- kept_feat_idx[seq_len(filled)]
  kept_aidx <- kept_aidx[seq_len(filled)]
  kept_M <- kept_M[seq_len(filled)]

  hyps <- tidytable::tidytable(
    feat_idx = unlist(kept_feat_idx, use.names = FALSE),
    adduct_idx = unlist(kept_aidx, use.names = FALSE),
    implied_M = unlist(kept_M, use.names = FALSE)
  )
  rm(kept_feat_idx, kept_aidx, kept_M)
  log_info(
    "Evidence engine candidate materialization: %d rows",
    nrow(hyps)
  )
  hyps <- hyps |> tidytable::mutate(nearest_mass_error_ppm = NA_real_)

  # Per-feature cap on compact representation (much cheaper than after join).
  baseline_idx <- which(adduct_v == baseline_adduct)
  if (length(baseline_idx) == 0L) {
    baseline_idx <- NA_integer_
  }

  if (is.finite(cap_eff) && cap_eff > 0L) {
    t_cap <- Sys.time()
    hyps[, .__tier := tier_v[adduct_idx]]
    hyps <- hyps |> tidytable::arrange(feat_idx, .__tier, adduct_idx)
    hyps[, .__rank := seq_len(.N), by = feat_idx]
    hyps <- hyps[
      .__rank <= cap_eff |
        adduct_idx %in% baseline_idx
    ]
    hyps[, c(".__rank", ".__tier") := NULL]
    log_debug(
      "Per-feature cap requested=%s effective=%d: %d rows in %.2fs",
      as.character(max_hypotheses_per_feature),
      as.integer(cap_eff),
      nrow(hyps),
      as.numeric(Sys.time() - t_cap, units = "secs")
    )
  }

  # Backfill nearest-library ppm error only when reasonably sized; this value
  # is diagnostic and not used by evidence clustering.
  if (use_library_prefilter) {
    if (nrow(hyps) <= 500000L) {
      t_ppm <- Sys.time()
      hyps[,
        nearest_mass_error_ppm := .nearest_exact_mass_ppm(
          implied_M,
          exact_masses_sorted
        )
      ]
      log_debug(
        "Nearest-library ppm error backfill: %d rows in %.2fs",
        nrow(hyps),
        as.numeric(Sys.time() - t_ppm, units = "secs")
      )
    } else {
      log_debug(
        "Skipping nearest-library ppm error backfill for %d rows (scale guard)",
        nrow(hyps)
      )
    }
  }

  # Ensure baseline per feature even if library prefilter dropped it.
  if (!is.na(baseline_idx)) {
    have_baseline <- unique(hyps$feat_idx[hyps$adduct_idx == baseline_idx])
    missing_feats <- setdiff(seq_along(feature_ids), have_baseline)
    if (length(missing_feats) > 0L) {
      b <- baseline_idx[1L]
      iso_shift_b <- niso_v[b] * EVIDENCE_ISOTOPE_SHIFT_DA
      implied_b <- (abs(z_v[b]) *
        (feature_mzs[missing_feats] - iso_shift_b) -
        am_v[b]) /
        n_mer_v[b]
      ok_b <- is.finite(implied_b) & implied_b > 0
      if (any(ok_b)) {
        if (use_library_prefilter) {
          ppm_b <- .nearest_exact_mass_ppm(
            implied_b[ok_b],
            exact_masses_sorted
          )
        } else {
          ppm_b <- rep(NA_real_, sum(ok_b))
        }
        add_dt <- tidytable::tidytable(
          feat_idx = as.integer(missing_feats[ok_b]),
          adduct_idx = b,
          implied_M = implied_b[ok_b],
          nearest_mass_error_ppm = ppm_b
        )
        hyps <- tidytable::bind_rows(hyps, add_dt)
        rm(add_dt)
      }
    }
  }

  if (nrow(hyps) == 0L) {
    return(empty_evidence_table())
  }

  hyps <- tidytable::as_tidytable(hyps) |>
    tidytable::mutate(
      rt = feature_rts[feat_idx],
      sample = feature_samples[feat_idx]
    )

  if (use_library_prefilter) {
    t_mc <- Sys.time()
    hyps <- hyps |>
      tidytable::mutate(
        nearest_mass_error_ppm = .nearest_exact_mass_ppm(
          implied_M,
          exact_masses_sorted
        ),
        matched_exact_mass_idx = .nearest_exact_mass_index(
          implied_M,
          exact_masses_sorted
        ),
        matched_exact_mass_idx = ifelse(
          nearest_mass_error_ppm > tol_ppm_num,
          NA_integer_,
          matched_exact_mass_idx
        )
      )
    log_debug(
      "Exact-mass anchoring: %d matched targets in %.2fs",
      tidytable::n_distinct(stats::na.omit(hyps$matched_exact_mass_idx)),
      as.numeric(Sys.time() - t_mc, units = "secs")
    )

    t_rt <- Sys.time()
    hyps <- hyps |> tidytable::arrange(sample, matched_exact_mass_idx, rt)
    hyps <- hyps |>
      tidytable::mutate(
        rt_cluster = {
          pv <- c(NA_real_, utils::head(rt, -1L))
          cumsum(
            is.na(matched_exact_mass_idx) |
              is.na(pv) |
              is.na(rt) |
              (rt - pv) > tolerance_rt
          )
        },
        by = c("sample", "matched_exact_mass_idx")
      ) |>
      tidytable::mutate(
        evidence_cluster = ifelse(
          is.na(matched_exact_mass_idx),
          NA_character_,
          paste(sample, matched_exact_mass_idx, rt_cluster, sep = "|")
        )
      )
    log_debug(
      "RT clustering on exact-mass anchors: %d evidence clusters in %.2fs",
      tidytable::n_distinct(stats::na.omit(hyps$evidence_cluster)),
      as.numeric(Sys.time() - t_rt, units = "secs")
    )
  } else {
    # Fallback without library exact masses: cluster by implied neutral mass.
    t_mc <- Sys.time()
    hyps <- hyps |> tidytable::arrange(implied_M)
    hyps <- hyps |>
      tidytable::mutate(
        mass_cluster = .ppm_cluster_sorted(implied_M, tolerance_ppm)
      )
    log_debug(
      "Mass clustering: %d clusters in %.2fs",
      tidytable::n_distinct(hyps$mass_cluster),
      as.numeric(Sys.time() - t_mc, units = "secs")
    )

    t_rt <- Sys.time()
    hyps <- hyps |> tidytable::arrange(sample, mass_cluster, rt)
    hyps <- hyps |>
      tidytable::mutate(
        rt_cluster = {
          pv <- c(NA_real_, utils::head(rt, -1L))
          cumsum(is.na(pv) | is.na(rt) | (rt - pv) > tolerance_rt)
        },
        by = c("sample", "mass_cluster")
      ) |>
      tidytable::mutate(
        evidence_cluster = paste(sample, mass_cluster, rt_cluster, sep = "|")
      )
    log_debug(
      "RT clustering: %d evidence clusters in %.2fs",
      tidytable::n_distinct(hyps$evidence_cluster),
      as.numeric(Sys.time() - t_rt, units = "secs")
    )
  }

  # Count distinct features per evidence cluster.
  # n_evidence_features includes the focal feature; evidence_count is peer-only
  # support (excluding self), which keeps baseline fallback semantics explicit.
  # Filtering: baseline adducts always pass; others pass if evidence_count >= 1
  # and tier/core constraints are met. Set min_degree (in conditional mode) to
  # activate stricter support requirements in high-connectivity regions.
  t_score <- Sys.time()
  pair_counts <- hyps |>
    tidytable::filter(!is.na(evidence_cluster)) |>
    tidytable::distinct(evidence_cluster, feat_idx) |>
    tidytable::summarize(n_evidence_features_pair = .N, by = evidence_cluster)
  hyps <- hyps |>
    tidytable::left_join(pair_counts, by = "evidence_cluster") |>
    tidytable::mutate(
      n_evidence_features = ifelse(
        is.na(n_evidence_features_pair),
        1L,
        n_evidence_features_pair
      )
    ) |>
    tidytable::select(-n_evidence_features_pair)
  rm(pair_counts)
  # evidence_count = number of independent PEER features supporting this
  # hypothesis (cluster size minus one self feature).
  hyps <- hyps |>
    tidytable::mutate(
      evidence_count = as.integer(pmax.int(0L, n_evidence_features - 1L))
    )

  cluster_meta <- hyps |>
    tidytable::filter(!is.na(evidence_cluster)) |>
    tidytable::summarize(
      cluster_has_core = any(core_v[adduct_idx]),
      cluster_n_adducts = tidytable::n_distinct(adduct_idx),
      by = evidence_cluster
    )
  hyps <- hyps |>
    tidytable::left_join(cluster_meta, by = "evidence_cluster") |>
    tidytable::mutate(
      cluster_has_core = ifelse(
        is.na(cluster_has_core),
        FALSE,
        cluster_has_core
      ),
      cluster_n_adducts = ifelse(
        is.na(cluster_n_adducts),
        1L,
        cluster_n_adducts
      )
    )

  keep_mask <-
    adduct_v[hyps$adduct_idx] == baseline_adduct |
    (hyps$evidence_count >= 1L &
      (!req_core_v[hyps$adduct_idx] | hyps$cluster_has_core) &
      (tier_v[hyps$adduct_idx] <= 3L | hyps$cluster_n_adducts >= 2L))
  hyps <- hyps |>
    tidytable::filter(keep_mask) |>
    tidytable::select(-cluster_has_core, -cluster_n_adducts)
  rm(cluster_meta, keep_mask)
  log_debug(
    "Evidence counting: max count %d in %.2fs",
    max(hyps$evidence_count, na.rm = TRUE),
    as.numeric(Sys.time() - t_score, units = "secs")
  )

  # Finally join the string/derived columns from the compact lookup tables.
  t_finalize <- Sys.time()
  hyps <- hyps |>
    tidytable::mutate(
      feature_id = feature_ids[feat_idx],
      mz = feature_mzs[feat_idx],
      sample = feature_samples[feat_idx],
      adduct = adduct_v[adduct_idx],
      n_mer = n_mer_v[adduct_idx],
      z = z_v[adduct_idx],
      adduct_mass = am_v[adduct_idx],
      n_iso = niso_v[adduct_idx],
      adduct_tier = tier_v[adduct_idx],
      evidence_cluster = as.character(evidence_cluster),
      candidate_adduct_origin = ifelse(
        evidence_count >= 1L | adduct != baseline_adduct,
        "supported",
        "enforced"
      ),
      source = ifelse(
        candidate_adduct_origin == "enforced",
        "baseline",
        "evidence"
      ),
      # Backward-compatible alias used by some downstream tests/callers.
      evidence_score = as.integer(evidence_count)
    ) |>
    tidytable::select(-feat_idx, -adduct_idx)
  log_debug(
    "Finalize join: %d rows in %.2fs",
    nrow(hyps),
    as.numeric(Sys.time() - t_finalize, units = "secs")
  )

  log_info(
    "Evidence engine complete: %d rows, %d supported clusters",
    nrow(hyps),
    tidytable::n_distinct(hyps$evidence_cluster)
  )

  tidytable::as_tidytable(hyps)
}

#' Materialize baseline-only output when no library-supported hypothesis was
#' found. Keeps the pipeline robust: every feature still receives the
#' protonated/deprotonated form as a fallback.
#' @keywords internal
.materialize_baseline_only <- function(
  feature_ids,
  feature_rts,
  feature_mzs,
  feature_samples,
  universe,
  baseline_adduct
) {
  base_row <- universe[adduct == baseline_adduct][1L]
  if (nrow(base_row) == 0L) {
    return(empty_evidence_table())
  }
  iso_shift <- base_row$n_iso * EVIDENCE_ISOTOPE_SHIFT_DA
  implied <- (abs(base_row$z) *
    (feature_mzs - iso_shift) -
    base_row$adduct_mass) /
    base_row$n_mer
  ok <- is.finite(implied) & implied > 0
  if (!any(ok)) {
    return(empty_evidence_table())
  }
  hyps <- tidytable::tidytable(
    feature_id = feature_ids[ok],
    rt = feature_rts[ok],
    mz = feature_mzs[ok],
    sample = feature_samples[ok],
    adduct = baseline_adduct,
    n_mer = base_row$n_mer,
    z = base_row$z,
    adduct_mass = base_row$adduct_mass,
    n_iso = base_row$n_iso,
    implied_M = implied[ok],
    nearest_mass_error_ppm = NA_real_,
    mass_cluster = NA_integer_,
    rt_cluster = NA_integer_,
    evidence_cluster = NA_character_,
    n_evidence_features = 1L,
    evidence_count = 0L,
    evidence_score = 0L,
    candidate_adduct_origin = "enforced",
    source = "baseline"
  )
  tidytable::as_tidytable(hyps)
}

#' Inject baseline rows for any feature missing the baseline adduct after the
#' library prefilter. Without this, features whose baseline implied mass is
#' not in the library would lose their fallback hypothesis.
#' @keywords internal
.ensure_baseline_rows <- function(
  hyps,
  feature_ids,
  feature_rts,
  feature_mzs,
  feature_samples,
  universe,
  baseline_adduct,
  exact_masses_sorted
) {
  base_row <- universe[adduct == baseline_adduct][1L]
  if (nrow(base_row) == 0L) {
    return(hyps)
  }
  have_baseline <- unique(hyps$feature_id[hyps$adduct == baseline_adduct])
  missing_mask <- !(feature_ids %in% have_baseline)
  if (!any(missing_mask)) {
    return(hyps)
  }
  iso_shift <- base_row$n_iso * EVIDENCE_ISOTOPE_SHIFT_DA
  implied <- (abs(base_row$z) *
    (feature_mzs[missing_mask] - iso_shift) -
    base_row$adduct_mass) /
    base_row$n_mer
  ok <- is.finite(implied) & implied > 0
  if (!any(ok)) {
    return(hyps)
  }
  if (length(exact_masses_sorted) > 0L) {
    ppm_err <- .nearest_exact_mass_ppm(implied[ok], exact_masses_sorted)
  } else {
    ppm_err <- rep(NA_real_, sum(ok))
  }
  idx <- which(missing_mask)[ok]
  add_rows <- tidytable::tidytable(
    feature_id = feature_ids[idx],
    rt = feature_rts[idx],
    mz = feature_mzs[idx],
    sample = feature_samples[idx],
    adduct = baseline_adduct,
    n_mer = base_row$n_mer,
    z = base_row$z,
    adduct_mass = base_row$adduct_mass,
    n_iso = base_row$n_iso,
    implied_M = implied[ok],
    nearest_mass_error_ppm = ppm_err
  )
  tidytable::bind_rows(hyps, add_rows)
}

#' Empty hypothesis table with the canonical schema.
#' @keywords internal
empty_evidence_table <- function() {
  tidytable::tidytable(
    feature_id = character(),
    rt = numeric(),
    mz = numeric(),
    sample = character(),
    adduct = character(),
    n_mer = integer(),
    z = integer(),
    adduct_mass = numeric(),
    n_iso = integer(),
    implied_M = numeric(),
    nearest_mass_error_ppm = numeric(),
    mass_cluster = integer(),
    rt_cluster = integer(),
    evidence_cluster = character(),
    n_evidence_features = integer(),
    evidence_count = integer(),
    evidence_score = integer(),
    candidate_adduct_origin = character(),
    source = character()
  )
}

#' Build adduct co-occurrence edges from an evidence-supported hypothesis
#' table.
#'
#' To remain scalable, each (evidence_cluster, feature) is reduced to one
#' representative adduct (the highest-scoring), then features are linked as
#' an m/z chain inside the cluster — O(n) edges per cluster instead of O(n^2).
#'
#' @keywords internal
build_evidence_edges <- function(hyps) {
  if (nrow(hyps) == 0L) {
    return(tidytable::tidytable(
      feature_id = character(),
      adduct = character(),
      feature_id_dest = character(),
      adduct_dest = character()
    ))
  }

  dt <- tidytable::as_tidytable(hyps)[,
    .(feature_id, mz, adduct, evidence_cluster, evidence_count)
  ]
  dt <- dt[!is.na(evidence_cluster)]
  if (nrow(dt) == 0L) {
    return(tidytable::tidytable(
      feature_id = character(),
      adduct = character(),
      feature_id_dest = character(),
      adduct_dest = character()
    ))
  }

  dt <- dt |>
    tidytable::arrange(
      evidence_cluster,
      feature_id,
      tidytable::desc(evidence_count),
      adduct
    )
  reps <- dt[, .SD[1L], by = .(evidence_cluster, feature_id)]
  reps <- reps |>
    tidytable::arrange(evidence_cluster, mz, feature_id)

  reps[,
    feature_id_dest := c(as.character(feature_id[-1L]), NA_character_),
    by = evidence_cluster
  ]
  reps[,
    adduct_dest := c(as.character(adduct[-1L]), NA_character_),
    by = evidence_cluster
  ]

  out <- reps[
    !is.na(feature_id_dest),
    .(feature_id, adduct, feature_id_dest, adduct_dest)
  ]
  out <- unique(out)
  tidytable::as_tidytable(out)
}
