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
