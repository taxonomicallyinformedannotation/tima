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
    clusters = if (
      is.list(clusters_list) && !is.null(clusters_list[[polarity]])
    ) {
      clusters_list[[polarity]]
    } else {
      clusters_list
    },
    neutral_losses = neutral_losses_list,
    polarity = polarity
  )
}

#' Validate and clean a vector of atomic formula modifier tokens.
#'
#' Keeps only tokens parseable as bare atomic formulas (e.g. "H2O", "C2H3N").
#' Strips comment suffixes ("H2O (water)" -> "H2O") and drops anything
#' unparseable, returning a clean character vector.
#'
#' @keywords internal
.validate_formula_tokens <- function(x) {
  if (length(x) == 0L) {
    return(character())
  }
  clean <- trimws(sub(" .*", "", as.character(x)))
  clean <- clean[nzchar(clean)]
  if (length(clean) == 0L) {
    return(character())
  }
  ok <- !vapply(
    clean,
    function(f) {
      pf <- parse_atomic_formula(f)
      is.null(pf) || length(pf) == 0L
    },
    logical(1L)
  )
  clean[ok]
}

#' Parse a single legacy adduct string to a base row list.
#'
#' Returns NULL if the string cannot be parsed or has zero n_mer/charges.
#' @keywords internal
.parse_single_legacy_adduct_row <- function(a) {
  parsed <- tryCatch(
    suppressWarnings(parse_adduct(a)),
    error = function(...) NULL
  )
  if (is.null(parsed) || is_parse_failed(parsed)) {
    return(NULL)
  }
  n_mer <- parsed[["n_mer"]]
  n_charges <- parsed[["n_charges"]]
  n_iso <- parsed[["n_iso"]]
  if (n_mer == 0L || n_charges == 0L) {
    return(NULL)
  }
  sign_char <- regmatches(a, regexpr("[+-]+$", a))
  sign_char <- if (length(sign_char) == 0L) "+" else sign_char[[1L]]
  z_sign <- if (startsWith(sign_char, "-")) -1L else 1L
  z <- z_sign * as.integer(n_charges)
  # Physically correct electron correction.
  adduct_mass <- parsed[["los_add_clu"]] - (z * ELECTRON_MASS_DA)
  list(
    adduct = a,
    n_mer = as.integer(n_mer),
    n_iso = as.integer(n_iso),
    z = z,
    adduct_mass = as.numeric(adduct_mass),
    clusters = integer(),
    losses = integer()
  )
}

#' Assemble a typed universe tidytable from a flat list of row lists.
#' @keywords internal
.assemble_legacy_universe <- function(rows) {
  if (length(rows) == 0L) {
    return(empty_adduct_universe())
  }
  n <- length(rows)
  tidytable::tidytable(
    adduct = vapply(rows, function(r) r$adduct, character(1L)),
    n_mer = vapply(rows, function(r) r$n_mer, integer(1L)),
    n_iso = vapply(rows, function(r) r$n_iso, integer(1L)),
    z = vapply(rows, function(r) r$z, integer(1L)),
    adduct_mass = vapply(rows, function(r) r$adduct_mass, numeric(1L)),
    adduct_mass_per_monomer = rep(0, n),
    carriers = rep(list(integer()), n),
    clusters = lapply(rows, function(r) r$clusters),
    losses = lapply(rows, function(r) r$losses)
  ) |>
    tidytable::distinct(adduct, .keep_all = TRUE)
}

#' Legacy adapter: take a flat list of adduct strings + optional clusters and
#' produce a typed universe.
#'
#' Base adducts are parsed **once**. Modifier rows are derived from the
#' already-parsed base rows via mass arithmetic and string synthesis — no
#' re-parsing of expanded strings. The `clusters` and `losses` list-columns
#' are populated for modifier rows (matching the structured-path schema),
#' so callers can distinguish base rows from modifier rows with
#' `lengths(clusters) == 0L & lengths(losses) == 0L`.
#'
#' @keywords internal
build_adduct_universe_from_legacy <- function(
  adducts,
  clusters,
  neutral_losses,
  polarity
) {
  adducts <- unique(as.character(adducts))
  adducts <- adducts[!is.na(adducts) & nzchar(adducts)]
  if (length(adducts) == 0L) {
    return(empty_adduct_universe())
  }

  # Validate modifier formula tokens (both kinds).
  clusters <- .validate_formula_tokens(clusters)
  neutral_losses <- .validate_formula_tokens(neutral_losses)

  # --- Parse base adducts ONCE ---
  base_rows <- list()
  for (a in adducts) {
    row <- .parse_single_legacy_adduct_row(a)
    if (!is.null(row)) {
      base_rows[[length(base_rows) + 1L]] <- row
    }
  }
  if (length(base_rows) == 0L) {
    return(empty_adduct_universe())
  }

  # Canonical adduct strings for the base (for vectorised modifier application).
  base_adducts <- vapply(base_rows, function(r) r$adduct, character(1L))

  # --- Pre-compute modifier masses ONCE (not per base row) ---
  cluster_masses <- vapply(
    clusters,
    function(f) formula_mass(parse_atomic_formula(f)),
    numeric(1L)
  )
  loss_masses <- vapply(
    neutral_losses,
    function(f) formula_mass(parse_atomic_formula(f)),
    numeric(1L)
  )

  # --- Collect all rows: base first, then modifier expansions ---
  all_rows <- base_rows

  # Cluster-only modifier rows
  for (ci in seq_along(clusters)) {
    cl <- clusters[[ci]]
    cl_mass <- cluster_masses[[ci]]
    cl_named <- stats::setNames(1L, cl)
    new_strings <- apply_modifier_to_adducts(base_adducts, cl, "+")
    for (ri in seq_along(base_rows)) {
      new_a <- new_strings[[ri]]
      if (is.na(new_a) || !nzchar(new_a)) {
        next
      }
      br <- base_rows[[ri]]
      all_rows[[length(all_rows) + 1L]] <- list(
        adduct = new_a,
        n_mer = br$n_mer,
        n_iso = br$n_iso,
        z = br$z,
        adduct_mass = br$adduct_mass + cl_mass,
        clusters = cl_named,
        losses = integer()
      )
    }
  }

  # Loss-only modifier rows
  for (li in seq_along(neutral_losses)) {
    lo <- neutral_losses[[li]]
    lo_mass <- loss_masses[[li]]
    lo_named <- stats::setNames(1L, lo)
    new_strings <- apply_modifier_to_adducts(base_adducts, lo, "-")
    for (ri in seq_along(base_rows)) {
      new_a <- new_strings[[ri]]
      if (is.na(new_a) || !nzchar(new_a)) {
        next
      }
      br <- base_rows[[ri]]
      all_rows[[length(all_rows) + 1L]] <- list(
        adduct = new_a,
        n_mer = br$n_mer,
        n_iso = br$n_iso,
        z = br$z,
        adduct_mass = br$adduct_mass - lo_mass,
        clusters = integer(),
        losses = lo_named
      )
    }
  }

  # Cluster + loss combo rows (apply loss first, then cluster)
  if (length(clusters) > 0L && length(neutral_losses) > 0L) {
    for (li in seq_along(neutral_losses)) {
      lo <- neutral_losses[[li]]
      lo_mass <- loss_masses[[li]]
      lo_named <- stats::setNames(1L, lo)
      loss_strings <- apply_modifier_to_adducts(base_adducts, lo, "-")
      for (ci in seq_along(clusters)) {
        cl <- clusters[[ci]]
        cl_mass <- cluster_masses[[ci]]
        cl_named <- stats::setNames(1L, cl)
        new_strings <- apply_modifier_to_adducts(loss_strings, cl, "+")
        for (ri in seq_along(base_rows)) {
          new_a <- new_strings[[ri]]
          if (is.na(new_a) || !nzchar(new_a)) {
            next
          }
          br <- base_rows[[ri]]
          all_rows[[length(all_rows) + 1L]] <- list(
            adduct = new_a,
            n_mer = br$n_mer,
            n_iso = br$n_iso,
            z = br$z,
            adduct_mass = br$adduct_mass + cl_mass - lo_mass,
            clusters = cl_named,
            losses = lo_named
          )
        }
      }
    }
  }

  .assemble_legacy_universe(all_rows)
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
    cli::cli_abort(
      "calculate_neutral_mass(): supply (n_mer, z, adduct_mass) or (universe, adduct)",
      class = c("tima_validation_error", "tima_error"),
      call = NULL
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
