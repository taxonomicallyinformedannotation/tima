#' @title Calculate mass of M
#'
#' @description This function calculates the neutral mass (M) from an observed
#'     m/z value and adduct notation. It accounts for charge, multimers,
#'     isotopes, and adduct modifications.
#'
#' @include parse_adduct.R
#'
#' @param adduct_string Character string representing the adduct
#'     (e.g., \code{[M+H]+}, \code{[2M+Na]+})
#' @param mz Numeric observed m/z value
#' @param electron_mass Numeric electron mass in Daltons
#'     (default: 5.485799E-4, CODATA 2018 value)
#'
#' @return Numeric neutral mass (M) in Daltons. Returns 0 if adduct
#'     parsing fails.
#'
#' @export
#'
#' @examples
#' calculate_mass_of_m(mz = 123.4567, adduct_string = "[M+H]+")
#' calculate_mass_of_m(mz = 123.4567, adduct_string = "[M+Na]+")
#' calculate_mass_of_m(mz = 123.4567, adduct_string = "[2M1-C6H12O6 (hexose)+NaCl+H]2+")
calculate_mass_of_m <- function(
  adduct_string,
  mz,
  electron_mass = 5.485799E-4
) {
  # Validate inputs
  if (missing(adduct_string) || missing(mz)) {
    stop("Both adduct_string and mz must be provided")
  }

  if (!is.numeric(mz) || mz < 0) {
    stop("mz must be a positive numeric value")
  }

  # Parse the adduct string
  parsed_adduct <- parse_adduct(adduct_string)

  # Check if parsing failed
  if (all(parsed_adduct == 0)) {
    logger::log_warn("Failed to parse adduct: ", adduct_string)
    return(0)
  }

  # Extract parsed components for clarity
  n_charges <- parsed_adduct["n_charges"][[1L]]
  charge_sign <- parsed_adduct["charge"][[1L]]
  n_mer <- parsed_adduct["n_mer"][[1L]]
  n_iso <- parsed_adduct["n_iso"][[1L]]
  los_add_clu <- parsed_adduct["los_add_clu"][[1L]]

  # Avoid division by zero
  if (n_mer == 0 || n_charges == 0) {
    logger::log_warn("Invalid multimer or charge count in adduct")
    return(0)
  }

  # Calculate neutral mass using adduct formula:
  # M = ((z * (m/z + iso) - modifications - (z * sign * e_mass)) / n_mer)
  neutral_mass <- ((n_charges *
    (mz + n_iso) -
    los_add_clu -
    (n_charges * charge_sign * electron_mass)) /
    n_mer)

  return(neutral_mass)
}
