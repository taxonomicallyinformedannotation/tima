#' Build the evidence-supported hypothesis table (scalable).
#'
#' @include logs_utils.R
#' @include mass_evidence.R
#' @include mass_evidence_utils.R
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

  feats_dt <- tidytable::as_tidytable(df_fea_min) |>
    tidytable::select(feature_id, rt, mz, sample) |>
    tidytable::mutate(rt = as.numeric(rt), mz = as.numeric(mz)) |>
    tidytable::distinct()

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

  # Pre-allocate working vectors to avoid repeated allocation in loop
  n_feat <- length(feature_ids)
  implied_work <- numeric(n_feat)
  ok_work <- logical(n_feat)

  t_loop <- Sys.time()
  log_every <- max(1L, K %/% 10L)

  for (ui in seq_len(K)) {
    iso_shift <- niso_v[ui] * EVIDENCE_ISOTOPE_SHIFT_DA
    z_abs <- abs(z_v[ui])
    am_u <- am_v[ui]
    n_mer_u <- n_mer_v[ui]
    am_pm_u <- am_pm_v[ui]

    # Vectorized implied mass computation
    implied_work <- (z_abs * (feature_mzs - iso_shift) - am_u) /
      n_mer_u -
      am_pm_u

    # Vectorized bounds check
    ok_work <- is.finite(implied_work) &
      implied_work >= min_neutral_mass &
      implied_work <= max_neutral_mass
    if (!any(ok_work)) {
      next
    }

    # Get valid indices in one pass
    implied_ok <- implied_work[ok_work]

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
      # Use which.ok_work to get positions - more efficient than which(ok)[hit]
      which_ok <- which(ok_work)
      pos <- which_ok[hit]
      implied_ok <- implied_ok[hit]
    } else {
      pos <- which(ok_work)
    }

    filled <- filled + 1L
    kept_feat_idx[[filled]] <- pos
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

  f_idx_vec <- unlist(kept_feat_idx[seq_len(filled)], use.names = FALSE)
  a_idx_vec <- unlist(kept_aidx[seq_len(filled)], use.names = FALSE)
  m_vec <- unlist(kept_M[seq_len(filled)], use.names = FALSE)
  rm(kept_feat_idx, kept_aidx, kept_M)
  log_info(
    "Evidence engine candidate materialization: %d rows",
    length(f_idx_vec)
  )

  # Per-feature cap on compact representation (much cheaper than after join).
  baseline_idx <- which(adduct_v == baseline_adduct)
  if (length(baseline_idx) == 0L) {
    baseline_idx <- NA_integer_
  }

  if (is.finite(cap_eff) && cap_eff > 0L && cap_eff < cap_in) {
    t_cap <- Sys.time()
    tier_col <- tier_v[a_idx_vec]
    ord_cap <- order(f_idx_vec, tier_col, a_idx_vec, method = "radix")
    f_ord <- f_idx_vec[ord_cap]
    a_ord <- a_idx_vec[ord_cap]
    n_h <- length(f_ord)
    new_f <- if (n_h > 1L) c(TRUE, f_ord[-1L] != f_ord[-n_h]) else TRUE
    rank_v <- seq_len(n_h) - cummax(ifelse(new_f, seq_len(n_h), 0L)) + 1L
    keep_cap <- ord_cap[rank_v <= cap_eff | a_ord %in% baseline_idx]
    f_idx_vec <- f_idx_vec[keep_cap]
    a_idx_vec <- a_idx_vec[keep_cap]
    m_vec <- m_vec[keep_cap]
    log_debug(
      "Per-feature cap requested=%s effective=%d: %d rows in %.2fs",
      as.character(max_hypotheses_per_feature),
      as.integer(cap_eff),
      length(f_idx_vec),
      as.numeric(Sys.time() - t_cap, units = "secs")
    )
  }

  # Ensure baseline per feature even if library prefilter dropped it.
  if (!is.na(baseline_idx)) {
    b <- baseline_idx[1L]
    have_b <- logical(n_feat)
    have_b[f_idx_vec[a_idx_vec == b]] <- TRUE
    missing_feats <- which(!have_b)
    if (length(missing_feats) > 0L) {
      iso_shift_b <- niso_v[b] * EVIDENCE_ISOTOPE_SHIFT_DA
      implied_b <- (abs(z_v[b]) *
        (feature_mzs[missing_feats] - iso_shift_b) -
        am_v[b]) /
        n_mer_v[b]
      ok_b <- is.finite(implied_b) & implied_b > 0
      if (any(ok_b)) {
        f_idx_vec <- c(f_idx_vec, as.integer(missing_feats[ok_b]))
        a_idx_vec <- c(a_idx_vec, rep.int(b, sum(ok_b)))
        m_vec <- c(m_vec, implied_b[ok_b])
      }
    }
  }

  if (length(f_idx_vec) == 0L) {
    return(empty_evidence_table())
  }

  u_samples <- sort(unique(feature_samples), method = "radix")
  feat_sample_id <- if (anyNA(u_samples)) {
    match(feature_samples, u_samples)
  } else {
    fastmatch::fmatch(feature_samples, u_samples)
  }
  s_id_vec <- feat_sample_id[f_idx_vec]
  rt_vec <- feature_rts[f_idx_vec]

  if (use_library_prefilter) {
    t_mc <- Sys.time()
    idx_em <- findInterval(m_vec, exact_masses_sorted, all.inside = FALSE)
    L_em <- length(exact_masses_sorted)
    lo_idx <- pmax.int(1L, idx_em)
    hi_idx <- pmin.int(L_em, idx_em + 1L)
    lo_em <- exact_masses_sorted[lo_idx]
    hi_em <- exact_masses_sorted[hi_idx]
    d_lo <- abs(m_vec - lo_em)
    d_hi <- abs(m_vec - hi_em)
    use_lo <- d_lo <= d_hi
    d_min <- pmin.int(d_lo, d_hi)
    ref_em <- hi_em
    ref_em[use_lo] <- lo_em[use_lo]
    ppm_err <- d_min / pmax.int(ref_em, 1e-9) * 1e6
    m_idx <- hi_idx
    m_idx[use_lo] <- lo_idx[use_lo]
    m_idx[ppm_err > tol_ppm_num] <- NA_integer_
    rm(
      idx_em,
      lo_idx,
      hi_idx,
      lo_em,
      hi_em,
      d_lo,
      d_hi,
      use_lo,
      d_min,
      ref_em
    )
    log_debug(
      "Exact-mass anchoring: %d matched targets in %.2fs",
      length(unique(m_idx[!is.na(m_idx)])),
      as.numeric(Sys.time() - t_mc, units = "secs")
    )

    t_rt <- Sys.time()
    ord_rt <- order(s_id_vec, m_idx, rt_vec, method = "radix")
    f_idx_vec <- f_idx_vec[ord_rt]
    a_idx_vec <- a_idx_vec[ord_rt]
    m_vec <- m_vec[ord_rt]
    ppm_err <- ppm_err[ord_rt]
    s_id_vec <- s_id_vec[ord_rt]
    m_v <- m_idx[ord_rt]
    rt_vec <- rt_vec[ord_rt]
    rm(m_idx, ord_rt)

    n_h <- length(f_idx_vec)
    same_sm <- if (n_h > 1L) {
      c(
        FALSE,
        s_id_vec[-1L] == s_id_vec[-n_h] &
          ((is.na(m_v[-1L]) & is.na(m_v[-n_h])) |
            (!is.na(m_v[-1L]) & !is.na(m_v[-n_h]) & m_v[-1L] == m_v[-n_h]))
      )
    } else {
      FALSE
    }
    prev_rt <- if (n_h > 1L) c(NA_real_, rt_vec[-n_h]) else NA_real_
    step_rt <- !same_sm |
      is.na(m_v) |
      is.na(prev_rt) |
      is.na(rt_vec) |
      (rt_vec - prev_rt) > tolerance_rt
    cs_step <- cumsum(step_rt)
    grp_base <- cummax(ifelse(!same_sm, cs_step, 0L))
    rt_cl <- cs_step - grp_base + 1L
    valid_cl <- !is.na(m_v)
    n_clusters <- sum(step_rt & valid_cl)
    cid <- ifelse(valid_cl, cumsum(step_rt & valid_cl), NA_integer_)
    rm(
      same_sm,
      prev_rt,
      step_rt,
      cs_step,
      grp_base,
      valid_cl
    )
    log_debug(
      "RT clustering on exact-mass anchors: %d evidence clusters in %.2fs",
      n_clusters,
      as.numeric(Sys.time() - t_rt, units = "secs")
    )
  } else {
    # Fallback without library exact masses: cluster by implied neutral mass.
    t_mc <- Sys.time()
    ppm_err <- rep(NA_real_, length(f_idx_vec))
    ord_m <- order(m_vec, method = "radix")
    f_idx_vec <- f_idx_vec[ord_m]
    a_idx_vec <- a_idx_vec[ord_m]
    m_vec <- m_vec[ord_m]
    ppm_err <- ppm_err[ord_m]
    s_id_vec <- s_id_vec[ord_m]
    rt_vec <- rt_vec[ord_m]
    rm(ord_m)

    mc_v <- .ppm_cluster_sorted(m_vec, tolerance_ppm)
    log_debug(
      "Mass clustering: %d clusters in %.2fs",
      if (length(mc_v) > 0L) mc_v[length(mc_v)] else 0L,
      as.numeric(Sys.time() - t_mc, units = "secs")
    )

    t_rt <- Sys.time()
    ord_rt <- order(s_id_vec, mc_v, rt_vec, method = "radix")
    f_idx_vec <- f_idx_vec[ord_rt]
    a_idx_vec <- a_idx_vec[ord_rt]
    m_vec <- m_vec[ord_rt]
    ppm_err <- ppm_err[ord_rt]
    s_id_vec <- s_id_vec[ord_rt]
    m_v <- mc_v[ord_rt]
    rt_vec <- rt_vec[ord_rt]
    rm(mc_v, ord_rt)

    n_h <- length(f_idx_vec)
    same_sm <- if (n_h > 1L) {
      c(FALSE, s_id_vec[-1L] == s_id_vec[-n_h] & m_v[-1L] == m_v[-n_h])
    } else {
      FALSE
    }
    prev_rt <- if (n_h > 1L) c(NA_real_, rt_vec[-n_h]) else NA_real_
    step_rt <- !same_sm |
      is.na(prev_rt) |
      is.na(rt_vec) |
      (rt_vec - prev_rt) > tolerance_rt
    cs_step <- cumsum(step_rt)
    grp_base <- cummax(ifelse(!same_sm, cs_step, 0L))
    rt_cl <- cs_step - grp_base + 1L
    n_clusters <- if (n_h > 0L) cs_step[n_h] else 0L
    cid <- cs_step
    rm(same_sm, prev_rt, step_rt, cs_step, grp_base)
    log_debug(
      "RT clustering: %d evidence clusters in %.2fs",
      n_clusters,
      as.numeric(Sys.time() - t_rt, units = "secs")
    )
  }

  # Count distinct features per evidence cluster using contiguous integer IDs.
  # n_evidence_features includes the focal feature; evidence_count is peer-only
  # support (excluding self), which keeps baseline fallback semantics explicit.
  # Filtering: baseline adducts always pass; others pass if evidence_count >= 1
  # and tier/core constraints are met. Set min_degree (in conditional mode) to
  # activate stricter support requirements in high-connectivity regions.
  t_score <- Sys.time()
  n_h <- length(f_idx_vec)
  valid_idx <- which(!is.na(cid))
  if (length(valid_idx) > 0L && n_clusters > 0L) {
    c_v <- cid[valid_idx]
    f_v <- f_idx_vec[valid_idx]
    a_v <- a_idx_vec[valid_idx]

    cf_key <- as.double(c_v - 1L) * (n_feat + 1) + f_v
    n_feat_lut <- tabulate(c_v[!duplicated(cf_key)], nbins = n_clusters)

    has_core_lut <- logical(n_clusters)
    has_core_lut[c_v[core_v[a_v]]] <- TRUE

    ca_key <- as.double(c_v - 1L) * (K + 1L) + a_v
    n_add_lut <- tabulate(c_v[!duplicated(ca_key)], nbins = n_clusters)

    n_ev_feat <- rep(1L, n_h)
    n_ev_feat[valid_idx] <- n_feat_lut[c_v]
    cl_has_core <- logical(n_h)
    cl_has_core[valid_idx] <- has_core_lut[c_v]
    cl_n_add <- rep(1L, n_h)
    cl_n_add[valid_idx] <- n_add_lut[c_v]
    rm(c_v, f_v, a_v, cf_key, n_feat_lut, has_core_lut, ca_key, n_add_lut)
  } else {
    n_ev_feat <- rep(1L, n_h)
    cl_has_core <- logical(n_h)
    cl_n_add <- rep(1L, n_h)
  }
  rm(cid, valid_idx, s_id_vec)

  ev_count <- as.integer(pmax.int(0L, n_ev_feat - 1L))

  keep_mask <- which(
    adduct_v[a_idx_vec] == baseline_adduct |
      (ev_count >= 1L &
        (!req_core_v[a_idx_vec] | cl_has_core) &
        (tier_v[a_idx_vec] <= 3L | cl_n_add >= 2L))
  )

  t_finalize <- Sys.time()
  f_k <- f_idx_vec[keep_mask]
  a_k <- a_idx_vec[keep_mask]
  m_k <- m_vec[keep_mask]
  ppm_k <- ppm_err[keep_mask]
  rt_k <- rt_vec[keep_mask]
  mv_k <- m_v[keep_mask]
  rtcl_k <- rt_cl[keep_mask]
  nev_k <- as.integer(n_ev_feat[keep_mask])
  evc_k <- ev_count[keep_mask]
  rm(
    f_idx_vec,
    a_idx_vec,
    m_vec,
    ppm_err,
    rt_vec,
    m_v,
    rt_cl,
    n_ev_feat,
    cl_has_core,
    cl_n_add,
    ev_count,
    keep_mask
  )

  samp_k <- feature_samples[f_k]
  add_k <- adduct_v[a_k]
  is_supported <- evc_k >= 1L | add_k != baseline_adduct

  if (use_library_prefilter) {
    ev_cl <- rep(NA_character_, length(f_k))
    ok_mv <- !is.na(mv_k)
    if (any(ok_mv)) {
      ev_cl[ok_mv] <- paste(
        samp_k[ok_mv],
        mv_k[ok_mv],
        rtcl_k[ok_mv],
        sep = "|"
      )
    }
    hyps <- tidytable::tidytable(
      implied_M = m_k,
      nearest_mass_error_ppm = ppm_k,
      rt = rt_k,
      sample = samp_k,
      matched_exact_mass_idx = mv_k,
      rt_cluster = rtcl_k,
      n_evidence_features = nev_k,
      evidence_count = evc_k,
      evidence_cluster = ev_cl,
      feature_id = feature_ids[f_k],
      mz = feature_mzs[f_k],
      adduct = add_k,
      n_mer = n_mer_v[a_k],
      z = z_v[a_k],
      adduct_mass = am_v[a_k],
      n_iso = niso_v[a_k],
      adduct_tier = tier_v[a_k],
      candidate_adduct_origin = ifelse(is_supported, "supported", "enforced"),
      source = ifelse(is_supported, "evidence", "baseline"),
      # Backward-compatible alias used by some downstream tests/callers.
      evidence_score = evc_k
    )
  } else {
    ev_cl <- paste(samp_k, mv_k, rtcl_k, sep = "|")
    hyps <- tidytable::tidytable(
      implied_M = m_k,
      nearest_mass_error_ppm = ppm_k,
      rt = rt_k,
      sample = samp_k,
      mass_cluster = mv_k,
      rt_cluster = rtcl_k,
      n_evidence_features = nev_k,
      evidence_count = evc_k,
      evidence_cluster = ev_cl,
      feature_id = feature_ids[f_k],
      mz = feature_mzs[f_k],
      adduct = add_k,
      n_mer = n_mer_v[a_k],
      z = z_v[a_k],
      adduct_mass = am_v[a_k],
      n_iso = niso_v[a_k],
      adduct_tier = tier_v[a_k],
      candidate_adduct_origin = ifelse(is_supported, "supported", "enforced"),
      source = ifelse(is_supported, "evidence", "baseline"),
      # Backward-compatible alias used by some downstream tests/callers.
      evidence_score = evc_k
    )
  }
  log_debug(
    "Evidence counting: max count %d in %.2fs",
    if (nrow(hyps) > 0L) max(hyps$evidence_count, na.rm = TRUE) else 0L,
    as.numeric(Sys.time() - t_score, units = "secs")
  )
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
