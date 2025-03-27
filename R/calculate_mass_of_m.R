#' @title Calculate mass of M
#'
#' @description This function calculates the mass of M
#'
#' @include parse_adduct.R
#'
#' @param adduct_string Adduct to be parsed
#' @param mz mz
#' @param electron_mass Electron mass
#'
#' @return A mass
#'
#' @export
#'
#' @examples calculate_mass_of_m(mz = 123.4567, adduct_string = "[M+H]+")
#' calculate_mass_of_m(mz = 123.4567, adduct_string = "[M+Na]+")
#' calculate_mass_of_m(mz = 123.456, adduct_string = "[2M1-C6H12O6 (hexose)+NaCl+H]2+")
calculate_mass_of_m <- function(
  adduct_string,
  mz,
  electron_mass = 5.485799E-4
) {
  parsed_adduct <- parse_adduct(adduct_string)
  if (all(parsed_adduct == 0)) {
    return(0)
  } else {
    return(
      (((parsed_adduct["n_charges"])[[1]] *
        (mz +
          parsed_adduct["n_iso"]) -
        parsed_adduct["los_add_clu"] -
        (parsed_adduct["n_charges"] *
          parsed_adduct["charge"] *
          electron_mass)) /
        parsed_adduct["n_mer"])
    )
  }
}
