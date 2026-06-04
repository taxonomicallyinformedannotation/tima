empty_adduct_universe <- function() {
  tidytable::tidytable(
    adduct = character(),
    n_mer = integer(),
    n_iso = integer(),
    z = integer(),
    adduct_mass = numeric(),
    adduct_mass_per_monomer = numeric(),
    carriers = list(),
    clusters = list(),
    losses = list()
  )
}

#' Build the typed adduct universe from a (possibly legacy) configuration.
#'
#' If the configuration is structured (declarative components), the universe
#' is enumerated systematically. If the configuration is a legacy flat list
#' of adduct strings, each string is parsed via [parse_adduct()] and joined
#' to a minimal typed row that still carries the canonical adduct string and
#' the exact `adduct_mass` (so the rest of the pipeline does not care which
#' code path produced the row).
#'
#' @keywords internal
build_adduct_universe <- function(
  adducts_list,
  clusters_list,
  neutral_losses_list,
  polarity,
  max_clusters_per_adduct = 1L,
  max_losses_per_adduct = 1L,
  plausible_mz_range = c(50, 2000)
) {
  resolved <- resolve_adduct_config(adducts_list, polarity)
  if (resolved$structured) {
    return(generate_adduct_hypotheses(
      spec = resolved$spec,
      polarity = polarity,
      clusters_list = clusters_list,
      neutral_losses_list = neutral_losses_list,
      max_clusters_per_adduct = max_clusters_per_adduct,
      max_losses_per_adduct = max_losses_per_adduct,
      plausible_mz_range = plausible_mz_range
    ))
  }
  build_adduct_universe_from_legacy(
    adducts = resolved$spec,
    clusters = clusters_list[[polarity]],
    polarity = polarity
  )
}

#' Legacy adapter: take a flat list of adduct strings + optional clusters and
#' produce a typed universe by parsing each string.
#'
#' @keywords internal
build_adduct_universe_from_legacy <- function(adducts, clusters, polarity) {
  adducts <- unique(as.character(adducts))
  adducts <- adducts[!is.na(adducts) & nzchar(adducts)]
  if (length(adducts) == 0L) {
    return(empty_adduct_universe())
  }

  # Pre-validate clusters: only keep tokens that are parseable as bare atomic
  # formulas (e.g. "H2O", "C2H3N"). Tokens like "[M]" or comments are dropped
  # silently to avoid bogus adduct strings downstream.
  if (length(clusters) > 0L) {
    cl_clean <- trimws(sub(" .*", "", as.character(clusters)))
    cl_clean <- cl_clean[nzchar(cl_clean)]
    cl_ok <- !vapply(
      cl_clean,
      function(f) {
        is.null(parse_atomic_formula(f)) ||
          length(parse_atomic_formula(f)) == 0L
      },
      logical(1L)
    )
    clusters <- cl_clean[cl_ok]
  } else {
    clusters <- character()
  }

  # Expand each adduct with each cluster (+ a "no cluster" version).
  if (length(clusters) == 0L) {
    expanded <- adducts
  } else {
    # Build expanded adducts using vectorized operations
    # For each adduct, create versions with each cluster
    expanded_list <- lapply(adducts, function(a) {
      inner <- sub("^\\[(.*)\\][0-9]*[+-]+$", "\\1", a, perl = TRUE)
      tail <- sub("^.*\\]", "]", a, perl = TRUE)
      # Create all cluster versions for this adduct at once
      c(a, paste0("[", inner, "+", clusters, tail))
    })
    expanded <- unlist(expanded_list, use.names = FALSE)
  }
  expanded <- unique(expanded)

  # Parse each adduct via the canonical parser (silencing legacy warnings on
  # idiosyncratic notations) and compute adduct_mass at m/z = 0.
  rows <- list()
  for (a in expanded) {
    parsed <- tryCatch(
      suppressWarnings(parse_adduct(a)),
      error = function(e) NULL
    )
    if (is.null(parsed) || is_parse_failed(parsed)) {
      next
    }
    n_mer <- parsed[["n_mer"]]
    n_charges <- parsed[["n_charges"]]
    n_iso <- parsed[["n_iso"]]
    if (n_mer == 0L || n_charges == 0L) {
      next
    }
    sign_char <- regmatches(a, regexpr("[+-]+$", a))
    sign_char <- if (length(sign_char) == 0L) "+" else sign_char[[1L]]
    z_sign <- if (startsWith(sign_char, "-")) -1L else 1L
    z <- z_sign * as.integer(n_charges)
    # Keep legacy parse path but apply physically correct electron correction.
    adduct_mass <- parsed[["los_add_clu"]] - (z * ELECTRON_MASS_DA)
    rows[[length(rows) + 1L]] <- list(
      adduct = a,
      n_mer = as.integer(n_mer),
      n_iso = as.integer(n_iso),
      z = z,
      adduct_mass = as.numeric(adduct_mass)
    )
  }
  if (length(rows) == 0L) {
    return(empty_adduct_universe())
  }
  tidytable::tidytable(
    adduct = vapply(rows, function(r) r$adduct, character(1L)),
    n_mer = vapply(rows, function(r) r$n_mer, integer(1L)),
    n_iso = vapply(rows, function(r) r$n_iso, integer(1L)),
    z = vapply(rows, function(r) r$z, integer(1L)),
    adduct_mass = vapply(rows, function(r) r$adduct_mass, numeric(1L)),
    adduct_mass_per_monomer = rep(0, length(rows)),
    carriers = rep(list(integer()), length(rows)),
    clusters = rep(list(integer()), length(rows)),
    losses = rep(list(integer()), length(rows))
  ) |>
    tidytable::distinct(adduct, .keep_all = TRUE)
}

#' Vectorized neutral mass calculation from observed m/z + typed universe rows.
#'
#' Either `universe` (a typed universe table) OR explicit
#' (`n_mer`, `z`, `adduct_mass`, optional `adduct_mass_per_monomer`) numeric
#' vectors must be provided.
#'
#' Formula (per-monomer aware):
#'   m/z = (n_mer * (M + adduct_mass_per_monomer) + adduct_mass) / |z|
#'   M   = (|z| * m/z - adduct_mass) / n_mer - adduct_mass_per_monomer
#'
#' Default `adduct_mass_per_monomer = 0` reproduces the historical formula.
#'
#' @keywords internal
calculate_neutral_mass <- function(
  mz,
  n_mer = NULL,
  z = NULL,
  adduct_mass = NULL,
  adduct_mass_per_monomer = NULL,
  universe = NULL,
  adduct = NULL
) {
  if (!is.null(universe) && !is.null(adduct)) {
    # Look up by adduct string.
    idx <- match(adduct, universe$adduct)
    n_mer <- universe$n_mer[idx]
    z <- universe$z[idx]
    adduct_mass <- universe$adduct_mass[idx]
    adduct_mass_per_monomer <- if (
      "adduct_mass_per_monomer" %in% colnames(universe)
    ) {
      universe$adduct_mass_per_monomer[idx]
    } else {
      rep(0, length(idx))
    }
  }
  if (is.null(n_mer) || is.null(z) || is.null(adduct_mass)) {
    stop(
      "calculate_neutral_mass(): supply (n_mer, z, adduct_mass) or ",
      "(universe, adduct)"
    )
  }
  if (is.null(adduct_mass_per_monomer)) {
    adduct_mass_per_monomer <- rep(0, length(n_mer))
  }
  (abs(z) * as.numeric(mz) - adduct_mass) / n_mer - adduct_mass_per_monomer
}

#' Vectorized expected-m/z from neutral mass + universe (inverse).
#' @keywords internal
calculate_mz_from_neutral_mass <- function(
  neutral_mass,
  n_mer,
  z,
  adduct_mass,
  adduct_mass_per_monomer = 0
) {
  (n_mer * (as.numeric(neutral_mass) + adduct_mass_per_monomer) + adduct_mass) /
    abs(z)
}

#' Convert bare negative-mode cation notation to explicit proton-displacement
#' notation while preserving already-signed compound tokens.
#' @keywords internal
.normalize_negative_mode_carrier <- function(token) {
  token <- as.character(token)
  if (is.na(token) || !nzchar(token) || grepl("[+-]", token, perl = TRUE)) {
    return(token)
  }
  z0 <- CARRIER_INTRINSIC_CHARGE[token]
  if (is.na(z0) || z0 <= 0L) {
    return(token)
  }
  if (token == "H") {
    return("-H")
  }
  if (z0 == 1L) {
    return(paste0(token, "-2H"))
  }
  if (z0 == 2L) {
    return(paste0(token, "-3H"))
  }
  if (z0 == 3L) {
    return(paste0(token, "-4H"))
  }
  token
}

# Tiny default helper (mirrors `rlang::%||%` to avoid pulling rlang here).
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a
