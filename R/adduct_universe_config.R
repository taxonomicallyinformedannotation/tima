#' Build the canonical adduct string from typed components.
#'
#' Format (outside-multimer, canonical):
#'   `[<n>M<-losses><-carrier-losses><+clusters><+carriers>]<|z|><sign>`
#' Format (inside-multimer, when `loss_inside_multimer` or
#' `cluster_inside_multimer` is `TRUE` and `n_mer >= 2`):
#'   `[<n>(M<inside-losses><inside-clusters>)<outside-losses><outside-clusters><carriers>]<|z|><sign>`
#'
#' The "inside" variant captures the chemistry where each monomer carries
#' the cluster/loss BEFORE the multimer assembles, e.g. `[2(M-H2O)+H]+`
#' (two M-H2O monomers dimerize, then protonate) or `[2(M+NaCl)+H]+` (each
#' M binds NaCl first, then dimerizes). These have *different* implied
#' neutral masses than their outside-multimer counterparts.
#'
#' * n omitted when n_mer == 1
#' * |z| omitted when |z| == 1
#'
#' @param n_mer Integer multimer count.
#' @param carriers Named integer vector of carrier counts/signs (e.g. H, Na).
#' @param clusters Named integer vector of neutral cluster additions.
#' @param losses Named integer vector of neutral losses.
#' @param z Integer signed charge.
#' @param loss_inside_multimer Logical; place losses inside `n(M...)` when TRUE.
#' @param cluster_inside_multimer Logical; place clusters inside `n(M...)` when TRUE.
#'
#' @return Canonical adduct string.
#'
#' @export
adduct_to_string <- function(
  n_mer,
  carriers,
  clusters,
  losses,
  z,
  loss_inside_multimer = FALSE,
  cluster_inside_multimer = FALSE
) {
  n_mer <- as.integer(n_mer)
  z <- as.integer(z)
  if (!is.finite(n_mer) || n_mer < 1L) {
    cli::cli_abort(
      "n_mer must be a positive integer, got: {.val {n_mer}}",
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }
  if (!is.finite(z) || z == 0L) {
    cli::cli_abort(
      "z must be a non-zero integer, got: {.val {z}}",
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }
  # Split carriers into negative (deprotonations) and positive (cations)
  carriers_neg <- carriers[carriers < 0L]
  carriers_pos <- carriers[carriers > 0L]

  # Inside-multimer wrapping only makes sense for n_mer >= 2.
  use_inside <- (n_mer >= 2L) &&
    (isTRUE(loss_inside_multimer) || isTRUE(cluster_inside_multimer))
  if (use_inside) {
    inside_clusters <- if (isTRUE(cluster_inside_multimer)) {
      clusters
    } else {
      integer()
    }
    outside_clusters <- if (isTRUE(cluster_inside_multimer)) {
      integer()
    } else {
      clusters
    }
    inside_losses <- if (isTRUE(loss_inside_multimer)) losses else integer()
    outside_losses <- if (isTRUE(loss_inside_multimer)) losses else integer()
    # Default routing: when only one flag is TRUE, the other component stays
    # outside. When both flags are TRUE, both go inside.
    if (isTRUE(loss_inside_multimer) && !isTRUE(cluster_inside_multimer)) {
      inside_losses <- losses
      outside_losses <- integer()
      inside_clusters <- integer()
      outside_clusters <- clusters
    }
    if (!isTRUE(loss_inside_multimer) && isTRUE(cluster_inside_multimer)) {
      inside_losses <- integer()
      outside_losses <- losses
      inside_clusters <- clusters
      outside_clusters <- integer()
    }
    # Canonical body inside parentheses: losses first, then clusters
    inside_body <- paste0(
      "M",
      .format_neutrals(inside_losses, "-"),
      .format_neutrals(inside_clusters, "+")
    )
    # Canonical body outside: losses + neg carriers → clusters → pos carriers
    body <- paste0(
      n_mer,
      "(",
      inside_body,
      ")",
      .format_neg_terms(outside_losses, carriers_neg),
      .format_pos_terms(outside_clusters, carriers_pos)
    )
  } else {
    m_prefix <- if (n_mer == 1L) "M" else paste0(n_mer, "M")
    # Canonical ordering: losses + neg carriers → clusters → pos carriers
    body <- paste0(
      m_prefix,
      .format_neg_terms(losses, carriers_neg),
      .format_pos_terms(clusters, carriers_pos)
    )
  }
  z_abs <- abs(z)
  z_prefix <- if (z_abs == 1L) "" else as.character(z_abs)
  z_sign <- if (z > 0L) "+" else "-"
  paste0("[", body, "]", z_prefix, z_sign)
}

# ---------------------------------------------------------------------------
# Universe generation
# ---------------------------------------------------------------------------

#' Build all integer compositions of `target` into exactly `k` parts (each
#' at least 0), used to enumerate carrier multisets with replacement.
#' @keywords internal
.compositions <- function(target, k) {
  if (k == 1L) {
    return(tidytable::tidytable(x = as.integer(target)))
  }
  out <- vector("list", target + 1L)
  for (i in seq.int(0L, target)) {
    sub <- .compositions(target - i, k - 1L)
    out[[i + 1L]] <- cbind(i, sub)
  }
  tidytable::bind_rows(out)
}

#' Resolve a possibly-legacy adduct configuration to a unified structured
#' specification. Supports both:
#'   - structured form: list(M, charge_carriers, charges, clusters,
#'                           neutral_losses)
#'   - legacy form: a flat character vector of canonical adduct strings.
#'
#' Returns a list(structured = TRUE/FALSE, spec = ...).
#' @keywords internal
resolve_adduct_config <- function(adducts_list, polarity) {
  # Structured form detection: presence of any of the structured keys at the
  # top level of `adducts_list`.
  if (
    is.list(adducts_list) &&
      any(c("M", "charge_carriers", "charges") %in% names(adducts_list))
  ) {
    return(list(structured = TRUE, spec = adducts_list))
  }
  list(
    structured = FALSE,
    spec = adducts_list[[polarity]]
  )
}

#' Generate the typed adduct universe from a structured spec for one polarity.
#'
#' @param spec list with components M (int vec), charge_carriers (list with
#'   polarity-keyed character vectors), charges (list polarity-keyed signed
#'   int vec). May also carry `clusters` (polarity-keyed character vec) and
#'   `neutral_losses` (character vec); when absent these are taken from the
#'   `clusters_list` / `neutral_losses_list` arguments.
#' @param polarity "pos" or "neg".
#' @param clusters_list optional polarity-keyed list of cluster formulas
#'   (legacy `ms.clusters` fallback).
#' @param neutral_losses_list optional character vector of neutral-loss
#'   formulas (legacy `ms.neutral_losses` fallback).
#' @param max_clusters_per_adduct maximum number of cluster molecules per
#'   hypothesis (0 or 1 by default).
#' @param max_losses_per_adduct maximum number of neutral-loss molecules per
#'   hypothesis (0 or 1 by default).
#' @return A `tidytable` with one row per hypothesis and columns:
#'   adduct, n_mer, z, adduct_mass, carriers (list), clusters (list),
#'   losses (list), source ("structured" or "legacy").
#' @keywords internal
generate_adduct_hypotheses <- function(
  spec,
  polarity,
  clusters_list = NULL,
  neutral_losses_list = NULL,
  max_clusters_per_adduct = 1L,
  max_losses_per_adduct = 1L,
  plausible_mz_range = c(50, 2000)
) {
  m_vals <- as.integer(spec$M %||% c(1L, 2L, 3L))
  carriers_polarity <- spec$charge_carriers[[polarity]] %||% character()
  charges_polarity <- as.integer(spec$charges[[polarity]] %||% integer())
  if (length(carriers_polarity) == 0L || length(charges_polarity) == 0L) {
    cli::cli_abort(
      "generate_adduct_hypotheses(): empty charge_carriers/charges for {.val {polarity}}",
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }

  # In negative mode, bare cation tokens are interpreted as proton-displacement
  # carriers so they remain chemically meaningful in signed-composition space.
  # Examples: H -> -H ; Na -> Na-2H ; K -> K-2H.
  if (polarity == "neg") {
    carriers_polarity <- vapply(
      carriers_polarity,
      .normalize_negative_mode_carrier,
      character(1L)
    )
  }

  # Clusters and losses can be embedded in the spec or supplied separately.
  cluster_formulas <- spec$clusters %||% clusters_list %||% character()
  if (is.list(cluster_formulas)) {
    cluster_formulas <- cluster_formulas[[polarity]] %||% character()
  }
  loss_formulas <- spec$neutral_losses %||% neutral_losses_list %||% character()
  # Strip "(comment)" suffixes from formulas, e.g. "H2O (water)".
  cluster_formulas <- trimws(sub(" .*", "", cluster_formulas))
  loss_formulas <- trimws(sub(" .*", "", loss_formulas))
  cluster_formulas <- cluster_formulas[nzchar(cluster_formulas)]
  loss_formulas <- loss_formulas[nzchar(loss_formulas)]

  # Pre-parse carriers, clusters, losses (each ONCE, regardless of universe
  # size).
  parsed_carriers <- lapply(carriers_polarity, parse_carrier_token)
  names(parsed_carriers) <- carriers_polarity
  bad <- vapply(parsed_carriers, is.null, logical(1L))
  if (any(bad)) {
    cli::cli_abort(
      "Unparseable charge carrier token(s) for {.val {polarity}}: {.val {carriers_polarity[bad]}}",
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }
  carrier_mass <- vapply(parsed_carriers, carrier_token_mass, numeric(1L))
  carrier_zcontrib <- vapply(
    parsed_carriers,
    function(p) p$z_contribution,
    integer(1L)
  )

  cluster_parsed <- lapply(cluster_formulas, parse_atomic_formula)
  bad_c <- vapply(cluster_parsed, is.null, logical(1L)) |
    vapply(cluster_parsed, function(x) length(x) == 0L, logical(1L))
  cluster_parsed <- cluster_parsed[!bad_c]
  cluster_formulas <- cluster_formulas[!bad_c]
  cluster_mass <- vapply(cluster_parsed, formula_mass, numeric(1L))

  loss_parsed <- lapply(loss_formulas, parse_atomic_formula)
  bad_l <- vapply(loss_parsed, is.null, logical(1L)) |
    vapply(loss_parsed, function(x) length(x) == 0L, logical(1L))
  loss_parsed <- loss_parsed[!bad_l]
  loss_formulas <- loss_formulas[!bad_l]
  loss_mass <- vapply(loss_parsed, formula_mass, numeric(1L))

  n_carriers <- length(parsed_carriers)
  rows <- .generate_structured_adduct_rows(
    m_vals = m_vals,
    charges_polarity = charges_polarity,
    n_carriers = n_carriers,
    parsed_carriers = parsed_carriers,
    carrier_mass = carrier_mass,
    carrier_zcontrib = carrier_zcontrib,
    cluster_parsed = cluster_parsed,
    cluster_formulas = cluster_formulas,
    cluster_mass = cluster_mass,
    loss_parsed = loss_parsed,
    loss_formulas = loss_formulas,
    loss_mass = loss_mass,
    max_clusters_per_adduct = max_clusters_per_adduct,
    max_losses_per_adduct = max_losses_per_adduct,
    plausible_mz_range = plausible_mz_range
  )

  if (length(rows) == 0L) {
    return(empty_adduct_universe())
  }

  # Assemble as a tidytable (list-cols for carriers/clusters/losses).
  out <- tidytable::tidytable(
    adduct = vapply(rows, function(r) r$adduct, character(1L)),
    n_mer = vapply(rows, function(r) r$n_mer, integer(1L)),
    n_iso = vapply(rows, function(r) r$n_iso, integer(1L)),
    z = vapply(rows, function(r) r$z, integer(1L)),
    adduct_mass = vapply(rows, function(r) r$adduct_mass, numeric(1L)),
    adduct_mass_per_monomer = vapply(
      rows,
      function(r) r$adduct_mass_per_monomer %||% 0,
      numeric(1L)
    ),
    carriers = lapply(rows, function(r) r$carriers[[1L]]),
    clusters = lapply(rows, function(r) r$clusters[[1L]]),
    losses = lapply(rows, function(r) r$losses[[1L]])
  )
  # Filter forbidden canonical strings (kept for backward-compatibility with
  # adducts_utils.R).
  if (exists("adducts_forbidden", inherits = TRUE)) {
    out <- out[!out$adduct %in% adducts_forbidden, ]
  }
  # Deduplicate on canonical string (safety; should be unique already).
  out <- tidytable::distinct(out, adduct, .keep_all = TRUE)
  out
}

.generate_structured_adduct_rows <- function(
  m_vals,
  charges_polarity,
  n_carriers,
  parsed_carriers,
  carrier_mass,
  carrier_zcontrib,
  cluster_parsed,
  cluster_formulas,
  cluster_mass,
  loss_parsed,
  loss_formulas,
  loss_mass,
  max_clusters_per_adduct,
  max_losses_per_adduct,
  plausible_mz_range
) {
  carrier_idx <- seq_len(n_carriers)
  rows <- list()
  row_i <- 0L

  for (m_val in m_vals) {
    for (z_val in charges_polarity) {
      z_abs <- abs(z_val)
      if (z_abs < 1L || z_abs > 6L) {
        next
      }
      comp <- .compositions(z_abs, n_carriers)
      for (ri in seq_len(nrow(comp))) {
        counts <- comp[ri, ]
        if (sum(counts) != z_abs) {
          next
        }
        z_net <- as.integer(sum(counts * carrier_zcontrib))
        if (z_net != z_val) {
          next
        }

        carrier_state <- .adduct_carrier_state(
          counts = counts,
          carrier_idx = carrier_idx,
          parsed_carriers = parsed_carriers,
          carrier_mass = carrier_mass
        )

        rows_out <- .enumerate_cluster_loss_scope_rows(
          m_val = m_val,
          z_val = z_val,
          z_abs = z_abs,
          carriers_named = carrier_state$carriers_named,
          base_carrier_offset = carrier_state$base_carrier_offset,
          cluster_parsed = cluster_parsed,
          cluster_formulas = cluster_formulas,
          cluster_mass = cluster_mass,
          loss_parsed = loss_parsed,
          loss_formulas = loss_formulas,
          loss_mass = loss_mass,
          max_clusters_per_adduct = max_clusters_per_adduct,
          max_losses_per_adduct = max_losses_per_adduct,
          plausible_mz_range = plausible_mz_range
        )

        if (length(rows_out) > 0L) {
          for (row in rows_out) {
            row_i <- row_i + 1L
            rows[[row_i]] <- row
          }
        }
      }
    }
  }

  rows
}

.adduct_carrier_state <- function(
  counts,
  carrier_idx,
  parsed_carriers,
  carrier_mass
) {
  per_atom <- integer()
  base_carrier_offset <- 0
  for (slot in carrier_idx) {
    if (counts[[slot]] == 0L) {
      next
    }
    token_symbols <- parsed_carriers[[slot]]$symbols
    for (sym in names(token_symbols)) {
      inc <- counts[[slot]] * token_symbols[[sym]]
      per_atom[sym] <- (if (is.na(per_atom[sym])) 0L else per_atom[sym]) + inc
    }
    base_carrier_offset <- base_carrier_offset +
      counts[[slot]] * carrier_mass[[slot]]
  }
  per_atom <- per_atom[per_atom != 0L]
  carriers_named <- if (length(per_atom) == 0L) integer() else per_atom
  list(
    carriers_named = carriers_named,
    base_carrier_offset = base_carrier_offset
  )
}

.enumerate_cluster_loss_scope_rows <- function(
  m_val,
  z_val,
  z_abs,
  carriers_named,
  base_carrier_offset,
  cluster_parsed,
  cluster_formulas,
  cluster_mass,
  loss_parsed,
  loss_formulas,
  loss_mass,
  max_clusters_per_adduct,
  max_losses_per_adduct,
  plausible_mz_range
) {
  out <- list()
  out_i <- 0L

  # Determine available cluster and loss choices early
  cluster_choices <- if (max_clusters_per_adduct < 1L) {
    list(NULL)
  } else {
    c(list(NULL), as.list(seq_along(cluster_parsed)))
  }
  loss_choices <- if (max_losses_per_adduct < 1L) {
    list(NULL)
  } else {
    c(list(NULL), as.list(seq_along(loss_parsed)))
  }

  for (ci in cluster_choices) {
    for (li in loss_choices) {
      clusters_named <- integer()
      losses_named <- integer()
      cluster_mass_val <- 0
      loss_mass_val <- 0
      if (!is.null(ci)) {
        clusters_named <- 1L
        names(clusters_named) <- cluster_formulas[[ci]]
        cluster_mass_val <- cluster_mass[[ci]]
      }
      if (!is.null(li)) {
        losses_named <- 1L
        names(losses_named) <- loss_formulas[[li]]
        loss_mass_val <- loss_mass[[li]]
      }

      carrier_offset_signed <- base_carrier_offset - (z_val * ELECTRON_MASS_DA)
      scopes <- .adduct_scope_options(
        m_val = m_val,
        has_loss = !is.null(li),
        has_cluster = !is.null(ci)
      )

      for (scope in scopes) {
        per_monomer_mass <- 0
        outside_mass <- 0
        if (isTRUE(scope$cluster)) {
          per_monomer_mass <- per_monomer_mass + cluster_mass_val
        } else {
          outside_mass <- outside_mass + cluster_mass_val
        }
        if (isTRUE(scope$loss)) {
          per_monomer_mass <- per_monomer_mass - loss_mass_val
        } else {
          outside_mass <- outside_mass - loss_mass_val
        }

        adduct_mass <- carrier_offset_signed + outside_mass
        mz_high <- as.numeric(plausible_mz_range[[2L]])
        neutral_high <- (z_abs * mz_high - adduct_mass) /
          m_val -
          per_monomer_mass
        if (!is.finite(neutral_high) || neutral_high <= 0) {
          next
        }

        adduct <- adduct_to_string(
          n_mer = m_val,
          carriers = carriers_named,
          clusters = clusters_named,
          losses = losses_named,
          z = z_val,
          loss_inside_multimer = isTRUE(scope$loss),
          cluster_inside_multimer = isTRUE(scope$cluster)
        )

        out_i <- out_i + 1L
        out[[out_i]] <- list(
          adduct = adduct,
          n_mer = as.integer(m_val),
          n_iso = 0L,
          z = as.integer(z_val),
          adduct_mass = as.numeric(adduct_mass),
          adduct_mass_per_monomer = as.numeric(per_monomer_mass),
          carriers = list(carriers_named),
          clusters = list(clusters_named),
          losses = list(losses_named)
        )
      }
    }
  }

  out
}

.adduct_scope_options <- function(m_val, has_loss, has_cluster) {
  scopes <- list(list(loss = FALSE, cluster = FALSE))
  if (m_val >= 2L) {
    if (has_loss) {
      scopes[[length(scopes) + 1L]] <- list(loss = TRUE, cluster = FALSE)
    }
    if (has_cluster) {
      scopes[[length(scopes) + 1L]] <- list(loss = FALSE, cluster = TRUE)
    }
    if (has_loss && has_cluster) {
      scopes[[length(scopes) + 1L]] <- list(loss = TRUE, cluster = TRUE)
    }
  }
  scopes
}
