#' @title Typed adduct hypothesis system
#'
#' @description
#' This module is the structured, regex-free replacement for the historical
#' string-concatenation logic used to enumerate adduct hypotheses and to
#' compute neutral masses.
#'
#' Adducts are modelled as **orthogonal components**:
#'
#'   * `n_mer`   : oligomer multiplicity (positive integer, typically 1..3)
#'   * `carriers`: named integer vector of charge carriers (e.g. c(H = 2, Na = 1))
#'   * `clusters`: named integer vector of neutral adducts (e.g. c(H2O = 1))
#'   * `losses`  : named integer vector of neutral losses (e.g. c(H2O = 1))
#'   * `z`       : signed net charge of the resulting ion
#'
#' The canonical adduct string is generated **from** these components, never
#' parsed **into** them inside the pipeline. All mass arithmetic happens on
#' the pre-computed `adduct_mass` offset (independent of m/z and n_mer).
#'
#' Mass formula (signed, regex-free):
#'   m/z = (n_mer * M + adduct_mass) / |z|  (with adduct_mass including
#'                                            electron-mass correction: -z*m_e)
#'   M   = (|z| * m/z - adduct_mass) / n_mer
#'
#' This is physically accurate for charged ions and independent of adduct text
#' parsing.
#'
#' @include constants.R
#' @keywords internal
#' @name adduct_universe
NULL

# ---------------------------------------------------------------------------
# Element / ion lookup tables (monoisotopic, neutral atoms / radicals)
# Sources: NIST 2018 / CODATA 2018 (electron mass).
# ---------------------------------------------------------------------------

#' Monoisotopic neutral mass (Da) for the atomic / radical building blocks
#' used to compose charge carriers.
#' @keywords internal
ATOMIC_MONOISOTOPIC_MASS <- c(
  H = 1.0078250319,
  Li = 7.0160034366,
  B = 11.0093055,
  C = 12.0000000,
  N = 14.0030740052,
  O = 15.9949146221,
  F = 18.9984032,
  Na = 22.98976967,
  Mg = 23.98504190,
  Al = 26.98153863,
  Si = 27.9769265325,
  P = 30.97376151,
  S = 31.97207069,
  Cl = 34.96885271,
  K = 38.96370668,
  Ca = 39.96259098,
  Mn = 54.93804510,
  Fe = 55.93493750,
  Cu = 62.92959740,
  Zn = 63.92914210,
  Br = 78.91833710,
  I = 126.9044680
)

#' Intrinsic ion charge for charge-carrier *names* as declared in YAML.
#' Compound carriers like "Na-2H" are decomposed by [parse_carrier_token()].
#' @keywords internal
CARRIER_INTRINSIC_CHARGE <- c(
  H = 1L,
  Na = 1L,
  K = 1L,
  Li = 1L,
  NH4 = 1L,
  H4N = 1L,
  Ca = 2L,
  Mg = 2L,
  Zn = 2L,
  Fe = 3L,
  Cu = 1L,
  Mn = 2L,
  Al = 3L,
  F = -1L,
  Cl = -1L,
  Br = -1L,
  I = -1L
)

#' Electron rest mass (Da), CODATA 2018.
#' @keywords internal
ELECTRON_MASS_DA <- 0.000548579909065

# ---------------------------------------------------------------------------
# Formula and carrier parsing (regex used ONLY on atomic-formula tokens, never
# on adduct strings).
# ---------------------------------------------------------------------------

#' Parse a chemical formula like "H2O" or "C2H3N" into a named integer vector
#' of element counts. Returns NULL if the formula cannot be parsed.
#'
#' @keywords internal
parse_atomic_formula <- function(formula) {
  if (is.null(formula) || is.na(formula) || !nzchar(formula)) {
    return(integer())
  }
  m <- gregexpr("([A-Z][a-z]?)([0-9]*)", formula, perl = TRUE)
  matches <- regmatches(formula, m)[[1L]]
  matches <- matches[nzchar(matches)]
  if (length(matches) == 0L) {
    return(NULL)
  }
  out <- integer()
  for (tok in matches) {
    el <- sub("^([A-Z][a-z]?).*$", "\\1", tok, perl = TRUE)
    cnt <- sub("^[A-Z][a-z]?([0-9]*)$", "\\1", tok, perl = TRUE)
    cnt <- if (!nzchar(cnt)) 1L else as.integer(cnt)
    if (!el %in% names(ATOMIC_MONOISOTOPIC_MASS)) {
      return(NULL)
    }
    out[el] <- (if (is.na(out[el])) 0L else out[el]) + cnt
  }
  out
}

#' Compute the monoisotopic mass of a parsed atomic-formula vector.
#' @keywords internal
formula_mass <- function(formula_vec) {
  if (length(formula_vec) == 0L) {
    return(0)
  }
  sum(ATOMIC_MONOISOTOPIC_MASS[names(formula_vec)] * formula_vec)
}

#' Parse a single carrier token like "H", "Na", "NH4" or compound forms
#' like "Na-2H", "Fe-2H", "K-2H" into:
#'   list(symbols = c(symbol = signed_count, ...), z_contribution = integer)
#'
#' The signed counts represent additions/subtractions of charge carriers
#' (so "Na-2H" -> c(Na = 1, H = -2), contributing +1 -2 = -1 to z).
#'
#' Compound carriers MUST start with a positive ion and then list
#' `+/-N<symbol>` segments.
#'
#' Returns NULL for tokens that cannot be parsed.
#'
#' @keywords internal
parse_carrier_token <- function(token) {
  token <- as.character(token)
  if (is.null(token) || is.na(token) || !nzchar(token)) {
    return(NULL)
  }
  # Split at +/- boundaries while keeping the sign
  parts <- regmatches(
    token,
    gregexpr("([+-]?)([0-9]*)([A-Za-z][A-Za-z0-9]*)", token, perl = TRUE)
  )[[1L]]
  parts <- parts[nzchar(parts)]
  if (length(parts) == 0L) {
    return(NULL)
  }

  symbols <- integer()
  z <- 0L
  for (i in seq_along(parts)) {
    p <- parts[[i]]
    sign <- if (startsWith(p, "-")) -1L else 1L
    body <- sub("^[+-]", "", p)
    coef_match <- regmatches(body, regexpr("^[0-9]+", body, perl = TRUE))
    coef <- if (length(coef_match) == 0L || !nzchar(coef_match)) {
      1L
    } else {
      as.integer(coef_match[[1L]])
    }
    sym <- sub("^[0-9]+", "", body, perl = TRUE)
    if (!nzchar(sym)) {
      return(NULL)
    }
    # Normalise NH4 / H4N to a canonical symbol so charge lookup works.
    if (sym == "NH4") {
      sym <- "NH4"
    }
    if (!sym %in% names(CARRIER_INTRINSIC_CHARGE)) {
      return(NULL)
    }
    symbols[sym] <- (if (is.na(symbols[sym])) 0L else symbols[sym]) +
      sign * coef
    z <- z + sign * coef * CARRIER_INTRINSIC_CHARGE[[sym]]
  }
  list(symbols = symbols, z_contribution = as.integer(z))
}

#' Compute the neutral monoisotopic mass of a (possibly compound) carrier
#' token. For "NH4" we use H4N as the formula; for "Na-2H" we add Na and
#' subtract 2*H, etc.
#' @keywords internal
carrier_token_mass <- function(parsed_carrier) {
  if (is.null(parsed_carrier)) {
    return(NA_real_)
  }
  total <- 0
  for (sym in names(parsed_carrier$symbols)) {
    cnt <- parsed_carrier$symbols[[sym]]
    if (sym == "NH4") {
      # H4N
      total <- total +
        cnt *
          (4 *
            ATOMIC_MONOISOTOPIC_MASS[["H"]] +
            ATOMIC_MONOISOTOPIC_MASS[["N"]])
    } else if (sym %in% names(ATOMIC_MONOISOTOPIC_MASS)) {
      total <- total + cnt * ATOMIC_MONOISOTOPIC_MASS[[sym]]
    } else {
      return(NA_real_)
    }
  }
  total
}

# ---------------------------------------------------------------------------
# Canonical string serialisation
# ---------------------------------------------------------------------------

#' Format the carrier section of a canonical adduct string.
#'
#' Carriers are emitted in a deterministic order: sorted by decreasing
#' absolute count, ties broken alphabetically by symbol.
#'
#' @keywords internal
.format_carriers <- function(carriers) {
  if (length(carriers) == 0L) {
    return("")
  }
  # Drop zero counts.
  carriers <- carriers[carriers != 0L]
  if (length(carriers) == 0L) {
    return("")
  }
  ord <- order(-abs(carriers), names(carriers))
  carriers <- carriers[ord]
  parts <- vapply(
    seq_along(carriers),
    function(i) {
      cnt <- carriers[[i]]
      sym <- names(carriers)[[i]]
      sign <- if (cnt < 0L) "-" else "+"
      acnt <- abs(cnt)
      coef <- if (acnt == 1L) "" else as.character(acnt)
      paste0(sign, coef, sym)
    },
    character(1L)
  )
  paste(parts, collapse = "")
}

#' Format a cluster or loss component vector (always positive counts).
#' `sign_char` is "+" for clusters, "-" for losses.
#' @keywords internal
.format_neutrals <- function(vec, sign_char) {
  if (length(vec) == 0L) {
    return("")
  }
  vec <- vec[vec > 0L]
  if (length(vec) == 0L) {
    return("")
  }
  ord <- order(-vec, names(vec))
  vec <- vec[ord]
  parts <- vapply(
    seq_along(vec),
    function(i) {
      cnt <- vec[[i]]
      coef <- if (cnt == 1L) "" else as.character(cnt)
      paste0(sign_char, coef, names(vec)[[i]])
    },
    character(1L)
  )
  paste(parts, collapse = "")
}

#' Build the canonical adduct string from typed components.
#'
#' Format (outside-multimer, default):
#'   `[<n>M<carriers><+clusters><-losses>]<|z|><sign>`
#' Format (inside-multimer, when `loss_inside_multimer` or
#' `cluster_inside_multimer` is `TRUE` and `n_mer >= 2`):
#'   `[<n>(M<inside-clusters><inside-losses>)<carriers><+outside-clusters><-outside-losses>]<|z|><sign>`
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
#' @export
#' @keywords internal
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
    stop("n_mer must be a positive integer")
  }
  if (!is.finite(z) || z == 0L) {
    stop("z must be a non-zero integer")
  }
  carrier_part <- .format_carriers(carriers)
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
    inside_body <- paste0(
      "M",
      .format_neutrals(inside_clusters, "+"),
      .format_neutrals(inside_losses, "-")
    )
    body <- paste0(
      n_mer,
      "(",
      inside_body,
      ")",
      carrier_part,
      .format_neutrals(outside_clusters, "+"),
      .format_neutrals(outside_losses, "-")
    )
  } else {
    m_prefix <- if (n_mer == 1L) "M" else paste0(n_mer, "M")
    body <- paste0(
      m_prefix,
      carrier_part,
      .format_neutrals(clusters, "+"),
      .format_neutrals(losses, "-")
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
    return(matrix(as.integer(target), nrow = 1L))
  }
  out <- vector("list", target + 1L)
  for (i in seq.int(0L, target)) {
    sub <- .compositions(target - i, k - 1L)
    out[[i + 1L]] <- cbind(i, sub)
  }
  do.call(rbind, out)
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
    stop(
      "generate_adduct_hypotheses(): empty charge_carriers/charges for ",
      polarity
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
  cluster_formulas <- spec$clusters[[polarity]] %||%
    clusters_list[[polarity]] %||%
    character()
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
    stop(
      "Unparseable charge carrier token(s) for ",
      polarity,
      ": ",
      paste(carriers_polarity[bad], collapse = ", ")
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
  carrier_idx <- seq_len(n_carriers)
  mz_low <- as.numeric(plausible_mz_range[[1L]])

  rows <- list()
  row_i <- 0L

  for (m_val in m_vals) {
    for (z_val in charges_polarity) {
      z_abs <- abs(z_val)
      if (z_abs < 1L || z_abs > 6L) {
        next
      }
      # Enumerate carrier MULTISETS of size z_abs (compositions of z_abs
      # into n_carriers non-negative integer parts).
      comp <- .compositions(z_abs, n_carriers)
      for (ri in seq_len(nrow(comp))) {
        counts <- comp[ri, ]
        if (sum(counts) != z_abs) {
          next
        } # safety
        # Net charge contribution
        z_net <- as.integer(sum(counts * carrier_zcontrib))
        if (z_net != z_val) {
          next
        }
        # Expand slot counts into per-atom signed counts.
        per_atom <- integer()
        base_carrier_offset <- 0
        for (slot in carrier_idx) {
          if (counts[[slot]] == 0L) {
            next
          }
          token_symbols <- parsed_carriers[[slot]]$symbols
          for (sym in names(token_symbols)) {
            inc <- counts[[slot]] * token_symbols[[sym]]
            per_atom[sym] <- (if (is.na(per_atom[sym])) 0L else per_atom[sym]) +
              inc
          }
          base_carrier_offset <- base_carrier_offset +
            counts[[slot]] * carrier_mass[[slot]]
        }
        per_atom <- per_atom[per_atom != 0L]
        carriers_named <- if (length(per_atom) == 0L) integer() else per_atom
        # Now expand 0..max clusters and 0..max losses (multiplicities of a
        # single cluster/loss formula stay at 0 or 1 here to bound the
        # combinatorial blowup; users can add specific multi-water entries
        # like "H4O2" to the loss list if needed).
        cluster_choices <- c(list(NULL), as.list(seq_along(cluster_parsed)))
        if (max_clusters_per_adduct < 1L) {
          cluster_choices <- list(NULL)
        }
        loss_choices <- c(list(NULL), as.list(seq_along(loss_parsed)))
        if (max_losses_per_adduct < 1L) {
          loss_choices <- list(NULL)
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
            outside_neutral_mass <- cluster_mass_val - loss_mass_val
            carrier_offset_signed <- base_carrier_offset -
              (z_val * ELECTRON_MASS_DA)

            # Enumerate the "scope" of the cluster/loss attachment:
            #   * "outside"          — current default; the cluster/loss is
            #     applied to the assembled multimer (e.g. [2M-H2O+H]+).
            #   * "loss_inside"      — each monomer lost the group before
            #     multimerization (e.g. [2(M-H2O)+H]+).
            #   * "cluster_inside"   — each monomer bound the cluster before
            #     multimerization (e.g. [2(M+NaCl)+H]+).
            #   * "both_inside"      — both attach per-monomer.
            # For n_mer == 1 the inside/outside split is mathematically a
            # no-op so we only emit the outside row.
            scopes <- list(list(loss = FALSE, cluster = FALSE))
            if (m_val >= 2L) {
              has_loss <- !is.null(li)
              has_cluster <- !is.null(ci)
              if (has_loss) {
                scopes[[length(scopes) + 1L]] <- list(
                  loss = TRUE,
                  cluster = FALSE
                )
              }
              if (has_cluster) {
                scopes[[length(scopes) + 1L]] <- list(
                  loss = FALSE,
                  cluster = TRUE
                )
              }
              if (has_loss && has_cluster) {
                scopes[[length(scopes) + 1L]] <- list(
                  loss = TRUE,
                  cluster = TRUE
                )
              }
            }

            for (scope in scopes) {
              # Split neutral mass between "outside" (applied once, to the
              # assembled species) and "per-monomer" (applied to each of the
              # n_mer monomers BEFORE assembly).
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

              # adduct_mass = "once" component: applied OUTSIDE the n_mer
              # multiplication when computing m/z. The per-monomer term is
              # tracked separately so the implied-mass formula can subtract
              # it from M.
              adduct_mass <- carrier_offset_signed + outside_mass

              # Discard hypotheses that are physically impossible: there must
              # exist *some* observed m/z in the plausible range that yields a
              # positive implied neutral mass. Equivalently, the implied M at
              # the HIGH end of the plausible m/z range must be positive. The
              # previous low-end check rejected legitimate cluster hypotheses
              # (e.g. [M+NaCl+H]+) whose minimal observable m/z is necessarily
              # above mz_low.
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
              row_i <- row_i + 1L
              rows[[row_i]] <- list(
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
      }
    }
  }

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

#' @keywords internal
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
  expanded <- character()
  for (a in adducts) {
    expanded <- c(expanded, a)
    for (cl in clusters) {
      inner <- sub("^\\[(.*)\\][0-9]*[+-]+$", "\\1", a, perl = TRUE)
      tail <- sub("^.*\\]", "]", a, perl = TRUE)
      expanded <- c(expanded, paste0("[", inner, "+", cl, tail))
    }
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
