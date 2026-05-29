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
  common_losses <- c(
    "H2O",
    "H4O2",
    "H6O3",
    "H8O4",
    "NH3",
    "H3N",
    "CO",
    "CO2",
    "SO2",
    "SO3",
    "HF",
    "CF2",
    "CF2O",
    "CF3",
    "CHF2",
    "C2F2",
    "C2F4",
    "C3F6"
  )
  common_clusters <- c(
    "C2H3N",
    "C2H7N",
    "NaCl",
    "C2H6OS"
  )

  out <- tidytable::as_tidytable(
    as.data.frame(universe, stringsAsFactors = FALSE)
  )
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
