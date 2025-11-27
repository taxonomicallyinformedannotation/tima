#' @title Calculate mass of M
#'
#' @description This function calculates the neutral mass (M) from an observed
#'     m/z value and adduct notation. It accounts for charge, multimers,
#'     isotopes, and adduct modifications.
#'
#'     The calculation follows the formula:
#'     M = ((z * (m/z + iso) - modifications - (z * sign * e_mass)) / n_mer)
#'
#'     where:
#'     - z = number of charges
#'     - m/z = observed mass-to-charge ratio
#'     - iso = isotope shift (mass units)
#'     - modifications = total mass change from adduct modifications
#'     - sign = charge polarity (+1 or -1)
#'     - e_mass = electron mass
#'     - n_mer = multimer count
#'
#' @include constants.R
#' @include parse_adduct.R
#' @include validations_utils.R
#'
#' @param mz Numeric observed m/z value in Daltons. Must be positive.
#' @param adduct_string Character string representing the adduct
#'     (e.g., \code{[M+H]+}, \code{[2M+Na]+}, \code{[M-H2O+H]+})
#' @param electron_mass Numeric electron mass in Daltons
#'     (default: ELECTRON_MASS_DALTONS from constants.R - CODATA 2018 value)
#'
#' @return Numeric neutral mass (M) in Daltons. Returns 0 if:
#'     - Adduct parsing fails
#'     - Invalid input parameters
#'     - Division by zero would occur (n_mer = 0 or n_charges = 0)
#'     Returns NA if calculated mass is negative (physically impossible)
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Simple protonated molecule
#' calculate_mass_of_m(mz = 123.4567, adduct_string = "[M+H]+")
#' # Expected: ~122.45 Da
#'
#' # Sodium adduct
#' calculate_mass_of_m(mz = 145.4421, adduct_string = "[M+Na]+")
#' # Expected: ~122.45 Da
#'
#' # Complex adduct with water loss
#' calculate_mass_of_m(mz = 105.4467, adduct_string = "[M+H-H2O]+")
#' # Expected: ~122.45 Da
#'
#' # Dimer
#' calculate_mass_of_m(mz = 245.9053, adduct_string = "[2M+H]+")
#' # Expected: ~122.45 Da
#'
#' # Doubly charged
#' calculate_mass_of_m(mz = 62.2311, adduct_string = "[M+2H]2+")
#' # Expected: ~122.45 Da
#'
#' # M+1 isotopologue
#' calculate_mass_of_m(mz = 124.4600, adduct_string = "[M1+H]+")
#' # Expected: ~122.45 Da
#'
#' # Using custom electron mass (for testing)
#' calculate_mass_of_m(
#'   mz = 123.4567,
#'   adduct_string = "[M+H]+",
#'   electron_mass = 0.0005486
#' )
#' }
calculate_mass_of_m <- function(
  mz,
  adduct_string,
  electron_mass = ELECTRON_MASS_DALTONS
) {
  # Validate required inputs
  if (missing(adduct_string) || missing(mz)) {
    stop(
      "Both adduct_string and mz must be provided.\n",
      "Usage: calculate_mass_of_m(adduct_string = '[M+H]+', mz = 123.456)"
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
    logger::log_warn(msg)
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
    logger::log_error(
      "Invalid multimer count (n_mer = 0) in adduct '",
      adduct_string,
      "'. ",
      "Cannot calculate neutral mass."
    )
    return(0)
  }

  if (n_charges == 0L) {
    logger::log_error(
      "Invalid charge count (n_charges = 0) in adduct '",
      adduct_string,
      "'. ",
      "Cannot calculate neutral mass."
    )
    return(0)
  }

  # Calculate neutral mass using the adduct formula
  # Formula: M = ((z * (m/z + iso) - modifications - (z * sign * e_mass)) / n_mer)
  neutral_mass <- calculate_neutral_mass_formula(
    mz = mz,
    n_charges = n_charges,
    charge_sign = charge_sign,
    n_iso = n_iso,
    mass_modifications = mass_modifications,
    electron_mass = electron_mass,
    n_mer = n_mer
  )

  # Validate result
  if (is.na(neutral_mass) || !is.finite(neutral_mass)) {
    logger::log_error(
      "Calculated neutral mass is not finite for adduct '",
      adduct_string,
      "' and m/z ",
      mz
    )
    return(0)
  }

  ## COMMENT: This is actually llowed for neutral mass calculations
  # if (neutral_mass < 0) {
  #   logger::log_warn(
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
  # logger::log_trace(
  #   "Calculated neutral mass: ",
  #   round(neutral_mass, 4),
  #   " Da ",
  #   "from m/z ",
  #   mz,
  #   " with adduct '",
  #   adduct_string,
  #   "'"
  # )

  return(neutral_mass)
}

# Helper Functions ----
# Implement Single Responsibility Principle

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
    logger::log_warn(
      "m/z value ({mz} Da) exceeds typical small molecule range ",
      "({MAX_MASS_DALTONS} Da). Please verify this is correct."
    )
  }

  invisible(TRUE)
}

#' Validate electron mass value
#'
#' @description Validates electron mass parameter (should be close to CODATA value)
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
    logger::log_warn(
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
  electron_mass,
  n_mer
) {
  # Apply the neutral mass calculation formula
  # M = ((z * (m/z + iso) - modifications - (z * sign * e_mass)) / n_mer)

  charged_mass <- n_charges * (mz + n_iso)
  electron_correction <- n_charges * charge_sign * electron_mass
  corrected_mass <- charged_mass - mass_modifications - electron_correction
  neutral_mass <- corrected_mass / n_mer

  return(neutral_mass)
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
#' @export
#'
#' @examples
#' \dontrun{
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
#' }
calculate_mz_from_mass <- function(
  neutral_mass,
  adduct_string,
  electron_mass = ELECTRON_MASS_DALTONS
) {
  # Validate inputs
  if (missing(neutral_mass) || missing(adduct_string)) {
    stop("Both neutral_mass and adduct_string must be provided")
  }

  validate_numeric_range(
    neutral_mass,
    min_value = 0,
    max_value = Inf,
    param_name = "neutral_mass"
  )

  # Parse adduct
  parsed_adduct <- parse_adduct(adduct_string)

  if (is_parse_failed(parsed_adduct)) {
    msg <- paste0("Failed to parse adduct: ", adduct_string)
    warning(msg, call. = FALSE)
    logger::log_warn(msg)
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
    logger::log_error("Cannot calculate m/z with zero charges")
    return(0)
  }

  # Calculate m/z using inverse formula
  # m/z = ((M * n_mer + modifications + (z * sign * e_mass)) / z) - iso
  total_mass <- neutral_mass * n_mer + mass_modifications
  charged_mass <- total_mass + (n_charges * charge_sign * electron_mass)
  mz <- (charged_mass / n_charges) - n_iso

  # logger::log_trace(
  #  "Calculated m/z: ",
  #  round(mz, 4),
  #  " from mass ",
  #  round(neutral_mass, 4),
  #  " with adduct '",
  #  adduct_string,
  #  "'"
  #)

  return(mz)
}
