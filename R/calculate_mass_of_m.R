#' @title Calculate mass of M
#'
#' @description This function calculates the neutral mass (M) from an observed
#'     m/z value and adduct notation. It accounts for charge, multimers,
#'     isotopes, and adduct modifications.
#'
#'     The calculation follows the formula:
#'     M = (|z| * (m/z - iso_shift) - modifications + z * e_mass) / n_mer
#'
#'     where:
#'     - |z| = absolute number of charges
#'     - z = signed charge (`|z| * polarity`)
#'     - m/z = observed mass-to-charge ratio
#'     - iso_shift = `n_iso * ISOTOPE_MASS_SHIFT_DALTONS`
#'     - modifications = total neutral mass change from adduct modifications
#'     - e_mass = electron mass
#'     - n_mer = multimer count
#'
#' @include constants.R
#' @include parse_adduct.R
#' @include validations_utils.R
#'
#' @param mz [numeric] Observed m/z value in Daltons. Must be positive.
#' @param adduct_string [character] Adduct notation string
#'     (e.g., \code{[M+H]+}, \code{[2M+Na]+}, \code{[M-H2O+H]+})
#' @param electron_mass [numeric] Electron mass in Daltons
#'     (default: ELECTRON_MASS_DALTONS from constants.R - CODATA 2018 value)
#'
#' @return Numeric neutral mass (M) in Daltons. Returns 0 if:
#'     - Adduct parsing fails
#'     - Invalid input parameters
#'     - Division by zero would occur (n_mer = 0 or n_charges = 0)
#'     Returns NA if calculated mass is negative (physically impossible)
#'
#' @family mass-spectrometry
#'
#' @export
#'
#' @examples
#' # Simple protonated molecule
#' calculate_mass_of_m(mz = 123.4567, adduct_string = "[M+H]+")
#' # Expected: ~122.45 Da
#'
#' # Sodium adduct
#' calculate_mass_of_m(mz = 145.4421, adduct_string = "[M+Na]+")
#' # Expected: ~122.45 Da
#'
#' # Complex adduct with water loss
#' calculate_mass_of_m(mz = 105.4467, adduct_string = "[M-H2O+H]+")
#' # Expected: ~122.45 Da
#'
#' # Dimer
#' calculate_mass_of_m(mz = 245.9053, adduct_string = "[2M+H]+")
#' # Expected: ~122.45 Da
#'
#' # Doubly charged
#' calculate_mass_of_m(mz = 62.2311, adduct_string = "[M+2H]2+")
#' # Expected: ~122.45 Da
calculate_mass_of_m <- function(
  mz,
  adduct_string,
  electron_mass = ELECTRON_MASS_DALTONS
) {
  # Validate required inputs
  if (missing(adduct_string) || missing(mz)) {
    cli::cli_abort(
      c(
        "both adduct_string and mz must be provided",
        "i" = "usage: calculate_mass_of_m(adduct_string = '[M+H]+', mz = 123.456)"
      ),
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }

  # Validate m/z value
  validate_mz(mz)

  # Validate electron mass
  validate_electron_mass(electron_mass)

  # Parse the adduct string
  parsed_adduct <- parse_adduct(adduct_string)

  # Check if parsing failed (all zeros returned)
  if (is_parse_failed(parsed_adduct)) {
    msg <- paste0(
      "Failed to parse adduct '",
      adduct_string,
      "', ",
      "cannot calculate neutral mass"
    )
    warning(msg, call. = FALSE)
    log_warn(msg)
    return(0)
  }

  # Extract parsed components with descriptive names
  n_charges <- parsed_adduct["n_charges"][[1L]]
  charge_sign <- parsed_adduct["charge"][[1L]]
  n_mer <- parsed_adduct["n_mer"][[1L]]
  n_iso <- parsed_adduct["n_iso"][[1L]]
  mass_modifications <- parsed_adduct["los_add_clu"][[1L]]

  # Validate critical values to avoid division by zero
  if (n_mer == 0L) {
    log_error(
      "Invalid multimer count (n_mer = 0) in adduct '",
      adduct_string,
      "'. ",
      "Cannot calculate neutral mass."
    )
    return(0)
  }

  if (n_charges == 0L) {
    log_error(
      "Invalid charge count (n_charges = 0) in adduct '",
      adduct_string,
      "'. ",
      "Cannot calculate neutral mass."
    )
    return(0)
  }

  # Calculate neutral mass using the adduct formula
  # Formula: M = (|z| * (m/z - iso_shift) - modifications + z * e_mass) / n_mer
  neutral_mass <- calculate_neutral_mass_formula(
    mz = mz,
    n_charges = n_charges,
    charge_sign = charge_sign,
    n_iso = n_iso,
    mass_modifications = mass_modifications,
    n_mer = n_mer,
    electron_mass = electron_mass
  )

  # Validate result
  if (is.na(neutral_mass) || !is.finite(neutral_mass)) {
    log_error(
      "Calculated neutral mass is not finite for adduct '",
      adduct_string,
      "' and m/z ",
      mz
    )
    return(0)
  }

  ## COMMENT: This is actually llowed for neutral mass calculations
  # if (neutral_mass < 0) {
  #   log_warn(
  #     "Calculated negative neutral mass (",
  #     round(neutral_mass, 4),
  #     " Da) ",
  #     "for adduct '",
  #     adduct_string,
  #     "' and m/z ",
  #     mz,
  #     ". ",
  #     "This is physically impossible. Returning NA."
  #   )
  #   return(NA_real_)
  # }

  # Log successful calculation at trace level
  ## COMMENT too many logs
  #   "Calculated neutral mass: ",
  #   round(neutral_mass, 4),
  #   " Da ",
  #   "from m/z ",
  #   mz,
  #   " with adduct '",
  #   adduct_string,
  #   "'"
  # )

  neutral_mass
}

#' @title Batch-calculate neutral mass from adduct + m/z vectors
#'
#' @description Vectorized version of [calculate_mass_of_m()] that parses each
#'     unique adduct string only once and applies the neutral-mass formula as
#'     vectorized arithmetic over all rows sharing that adduct.  This is orders
#'     of magnitude faster than calling `calculate_mass_of_m()` row-by-row when
#'     a large feature table has low adduct cardinality (typically <100 unique
#'     adducts for millions of rows).
#'
#' @param adducts [character] Vector of adduct notation strings.
#' @param mzs [numeric] Observed m/z values (same length as \code{adducts}, or
#'     scalar 0 for adduct-mass computation).
#' @param electron_mass [numeric] Electron mass in Daltons.
#'
#' @return Numeric vector of neutral masses (NA for unparseable adducts or
#'     non-finite results).
#'
#' @keywords internal
calculate_mass_of_m_batch <- function(
  adducts,
  mzs,
  electron_mass = ELECTRON_MASS_DALTONS
) {
  validate_electron_mass(electron_mass)
  n <- length(adducts)
  if (length(mzs) == 1L) {
    mzs <- rep(as.numeric(mzs), n)
  } else {
    mzs <- as.numeric(mzs)
  }
  out <- rep(NA_real_, n)

  unique_adducts <- unique(adducts[!is.na(adducts)])

  # Pre-compute a compact index mapping once and then use grouped indexing.
  # This avoids repeatedly scanning the whole vector for each adduct.
  if (length(unique_adducts) > 0L) {
    adduct_idx_map <- match(adducts, unique_adducts)
    valid_idx <- which(!is.na(adducts))
    idx_groups <- split(valid_idx, adduct_idx_map[valid_idx])

    for (i in seq_along(unique_adducts)) {
      a <- unique_adducts[[i]]
      parsed <- tryCatch(parse_adduct(a), error = function(...) NULL)
      if (is.null(parsed) || all(parsed == 0L)) {
        next
      }
      n_charges <- parsed[["n_charges"]]
      charge_sign <- parsed[["charge"]]
      n_mer <- parsed[["n_mer"]]
      n_iso <- parsed[["n_iso"]]
      mass_mods <- parsed[["los_add_clu"]]
      if (n_mer == 0L || n_charges == 0L) {
        next
      }

      idx <- idx_groups[[i]]
      if (length(idx) == 0L) {
        next
      }
      mz_sub <- mzs[idx]
      iso_shift <- n_iso * ISOTOPE_MASS_SHIFT_DALTONS
      z_signed <- n_charges * charge_sign
      masses <- (n_charges *
        (mz_sub - iso_shift) -
        mass_mods +
        z_signed * electron_mass) /
        n_mer
      out[idx] <- masses
    }
  }
  out
}

#' @title Batch-calculate m/z from neutral mass + adduct vectors
#'
#' @description Vectorized version of [calculate_mz_from_mass()] that parses each
#'     unique adduct string only once and applies the inverse mass formula over
#'     all rows sharing that adduct.
#'
#' @param neutral_masses [numeric] Neutral mass values (same length as
#'     \code{adducts}).
#' @param adducts [character] Vector of adduct notation strings.
#' @param electron_mass [numeric] Electron mass in Daltons.
#'
#' @return Numeric vector of m/z values.
#'
#' @keywords internal
calculate_mz_from_mass_batch <- function(
  neutral_masses,
  adducts,
  electron_mass = ELECTRON_MASS_DALTONS
) {
  validate_electron_mass(electron_mass)
  n <- length(neutral_masses)
  if (length(adducts) == 1L) {
    adducts <- rep(adducts, n)
  } else {
    adducts <- as.character(adducts)
  }
  out <- rep(NA_real_, n)

  valid_idx <- !is.na(neutral_masses) & !is.na(adducts)
  valid_idx[is.na(valid_idx)] <- FALSE
  if (!any(valid_idx)) {
    return(out)
  }

  neutral_vals <- as.numeric(neutral_masses[valid_idx])
  adduct_vals <- adducts[valid_idx]
  unique_adducts <- unique(adduct_vals[
    !is.na(adduct_vals) & nzchar(adduct_vals)
  ])
  if (length(unique_adducts) > 0L) {
    adduct_idx_map <- match(adduct_vals, unique_adducts)
    idx_groups <- split(seq_along(adduct_idx_map), adduct_idx_map)

    for (i in seq_along(unique_adducts)) {
      a <- unique_adducts[[i]]
      parsed <- tryCatch(parse_adduct(a), error = function(...) NULL)
      if (is.null(parsed) || all(parsed == 0L)) {
        next
      }
      n_charges <- parsed[["n_charges"]]
      charge_sign <- parsed[["charge"]]
      n_mer <- parsed[["n_mer"]]
      n_iso <- parsed[["n_iso"]]
      mass_mods <- parsed[["los_add_clu"]]
      if (n_charges == 0L) {
        next
      }

      idx <- idx_groups[[i]]
      if (length(idx) == 0L) {
        next
      }
      mz_sub <- (neutral_vals[idx] *
        n_mer +
        mass_mods -
        (n_charges * charge_sign) * electron_mass) /
        n_charges +
        n_iso * ISOTOPE_MASS_SHIFT_DALTONS
      out_valid <- out[valid_idx]
      out_valid[idx] <- mz_sub
      out[valid_idx] <- out_valid
    }
  }
  out
}

# Helper Functions ----

#' Validate m/z value
#'
#' @description Validates mass-to-charge ratio input value
#'
#' @include validations_utils.R
#'
#' @param mz Numeric m/z value
#'
#' @return Invisible TRUE if valid, stops with error otherwise
#'
#' @keywords internal
validate_mz <- function(mz) {
  # Use centralized numeric validation
  validate_numeric_range(
    value = mz,
    min_value = MIN_MASS_DALTONS,
    max_value = Inf,
    param_name = "mz"
  )

  # Sanity check: warn if m/z is suspiciously high
  if (mz > MAX_MASS_DALTONS) {
    log_warn(
      "m/z value ({mz} Da) exceeds typical small molecule range ",
      "({MAX_MASS_DALTONS} Da). Please verify this is correct."
    )
  }

  invisible(TRUE)
}

#' Validate electron mass value
#'
#' @description Validates electron mass parameter (should be close to CODATA
#'     value)
#'
#' @param electron_mass Numeric electron mass in Daltons
#'
#' @return Invisible TRUE if valid, stops with error otherwise
#'
#' @keywords internal
validate_electron_mass <- function(electron_mass) {
  # Basic numeric validation
  validate_numeric_range(
    value = electron_mass,
    min_value = 0,
    max_value = 0.001, # Sanity check: electron mass should be ~5.486e-4
    param_name = "electron_mass"
  )

  # Warn if significantly different from CODATA value
  expected_value <- ELECTRON_MASS_DALTONS
  relative_diff <- abs(electron_mass - expected_value) / expected_value

  if (relative_diff > 0.01) {
    # More than 1% difference
    log_warn(
      "electron_mass ({electron_mass}) differs from CODATA 2018 value ",
      "({expected_value}) by {round(relative_diff * 100, 2)}%. ",
      "Please verify this is intentional."
    )
  }

  invisible(TRUE)
}

#' Check if adduct parsing failed
#' @keywords internal
is_parse_failed <- function(parsed_adduct) {
  # Parsing fails when all values are zero
  all(parsed_adduct == 0)
}

#' Calculate neutral mass using the adduct formula
#' @keywords internal
calculate_neutral_mass_formula <- function(
  mz,
  n_charges,
  charge_sign,
  n_iso,
  mass_modifications,
  n_mer,
  electron_mass
) {
  # Physically-correct neutral mass inversion with signed electron correction:
  # m/z = (n_mer * M + mass_modifications - z * m_e) / |z| + isotope_shift
  # z = charge_sign * n_charges
  # => M = (|z| * (m/z - isotope_shift) - mass_modifications + z * m_e) / n_mer

  isotope_shift <- n_iso * ISOTOPE_MASS_SHIFT_DALTONS
  z_signed <- n_charges * charge_sign

  charged_mass <- n_charges * (mz - isotope_shift)
  corrected_mass <- charged_mass - mass_modifications + z_signed * electron_mass
  neutral_mass <- corrected_mass / n_mer

  neutral_mass
}

#' Calculate m/z from neutral mass (inverse operation)
#'
#' @description This is the inverse of calculate_mass_of_m. Given a neutral
#'     mass and adduct, it calculates the expected m/z value.
#'
#' @param neutral_mass Numeric neutral mass (M) in Daltons
#' @param adduct_string Character string representing the adduct
#' @param electron_mass Numeric electron mass in Daltons
#'
#' @return Numeric m/z value in Daltons
#'
#' @family mass-spectrometry
#'
#' @export
#'
#' @examples
#' # Calculate m/z for a protonated molecule
#' calculate_mz_from_mass(neutral_mass = 122.45, adduct_string = "[M+H]+")
#' # Expected: ~123.4567
#'
#' # Verify round-trip calculation
#' mass <- 122.45
#' adduct <- "[M+H]+"
#' mz <- calculate_mz_from_mass(mass, adduct)
#' mass_back <- calculate_mass_of_m(mz, adduct)
#' all.equal(mass, mass_back) # Should be TRUE
calculate_mz_from_mass <- function(
  neutral_mass,
  adduct_string,
  electron_mass = ELECTRON_MASS_DALTONS
) {
  # Validate inputs
  if (missing(neutral_mass) || missing(adduct_string)) {
    cli::cli_abort(
      "both neutral_mass and adduct_string must be provided",
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }

  validate_numeric_range(
    neutral_mass,
    min_value = 0,
    max_value = Inf,
    param_name = "neutral_mass"
  )

  # Validate electron mass
  validate_electron_mass(electron_mass)

  # Parse adduct
  parsed_adduct <- parse_adduct(adduct_string)

  if (is_parse_failed(parsed_adduct)) {
    msg <- paste0("Failed to parse adduct: ", adduct_string)
    warning(msg, call. = FALSE)
    log_warn(msg)
    return(0)
  }

  # Extract components
  n_charges <- parsed_adduct["n_charges"][[1L]]
  charge_sign <- parsed_adduct["charge"][[1L]]
  n_mer <- parsed_adduct["n_mer"][[1L]]
  n_iso <- parsed_adduct["n_iso"][[1L]]
  mass_modifications <- parsed_adduct["los_add_clu"][[1L]]

  # Check for division by zero
  if (n_charges == 0L) {
    log_error("Cannot calculate m/z with zero charges")
    return(0)
  }

  # Calculate m/z using inverse formula with signed electron correction.
  # m/z = (n_mer * M + mass_modifications - z * m_e) / |z| + isotope_shift
  # with z = charge_sign * n_charges.

  isotope_shift <- n_iso * ISOTOPE_MASS_SHIFT_DALTONS
  z_signed <- n_charges * charge_sign

  total_mass <- neutral_mass *
    n_mer +
    mass_modifications -
    z_signed * electron_mass
  mz <- (total_mass / n_charges) + isotope_shift

  mz
}
