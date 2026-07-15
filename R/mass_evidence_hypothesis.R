#' Build the evidence-supported hypothesis table (scalable).
#'
#' @param df_fea_min tidytable with columns `feature_id`, `rt`, `mz`, `sample`.
#' @param universe typed universe returned by [build_adduct_universe()].
#' @param tolerance_ppm numeric mass tolerance (ppm).
#' @param tolerance_dalton numeric absolute mass tolerance (Da). When provided,
#'   the pre-filter uses max(dalton, ppm * mass * 1e-6) per mass.
#' @param tolerance_rt numeric RT tolerance.
#' @param ms_mode "pos" or "neg".
#' @param min_neutral_mass,max_neutral_mass plausibility bounds.
#' @param exact_masses optional numeric vector of library exact masses. When
#'   provided, only hypotheses whose implied neutral mass is within the combined
#'   tolerance of at least one library mass are retained. This is the primary
#'   memory-control lever.
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
  tolerance_dalton = NULL,
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

  # Pre-extract feature columns to plain vectors.
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

  # Hypothesis generation per adduct row.
  # Hot path uses a CHEAP inclusion check (two findIntervals) -- no nearest
  # distance computation. The actual ppm error is computed in ONE batched
  # call after the loop, on the (much smaller) kept set.
  kept_feat_idx <- vector("list", K)
  kept_aidx <- vector("list", K)
  kept_M <- vector("list", K)
  kept_lengths <- integer(K)
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

    implied_ok <- implied[ok]
    if (use_library_prefilter) {
      hit <- .has_library_match_within_tolerance(
        masses = implied_ok,
        exact_masses_sorted = exact_masses_sorted,
        tolerance_ppm = tol_ppm_num,
        tolerance_dalton = tolerance_dalton
      )
      if (!any(hit)) {
        next
      }
      implied_ok <- implied_ok[hit]
      pos <- which(ok)[hit]
    } else {
      pos <- which(ok)
    }

    filled <- filled + 1L
    kept_feat_idx[[filled]] <- as.integer(pos)
    kept_aidx[[filled]] <- rep.int(ui, length(pos))
    kept_M[[filled]] <- implied_ok
    kept_lengths[filled] <- length(pos)

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
        .by = c(sample, matched_exact_mass_idx)
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
        .by = c(sample, mass_cluster)
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
    tidytable::summarize(
      n_evidence_features_pair = .N,
      .by = evidence_cluster
    )
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
      .by = evidence_cluster
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

#' Empty hypothesis table with the canonical schema.
#' @keywords internal
