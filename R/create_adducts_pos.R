utils::globalVariables(
  c(
    "acetonitrile",
    "ammonium",
    "calcium",
    "dmso",
    "ethylamine",
    "exact_mass",
    "iron",
    "isopropanol",
    "magnesium",
    "methanol",
    "name",
    "potassium",
    "proton",
    "sodium",
    "value"
  )
)

#' @title Create adducts positive
#'
#' @description This function creates positive adducts
#'
#' @param massesTable Table containing the masses of the adducts
#' @param adductsTable Table containing the adducts
#'
#' @return NULL
#'
#' @export
#'
#' @examples NULL
create_adducts_pos <- function(massesTable, adductsTable) {
  ## Calculate the masses for various positive adducts
  adducts_pos <- massesTable |>
    dplyr::mutate(
      `[1M+(H)3]3+` = (exact_mass + 3 * proton) / 3,
      `[1M+(H)2(Na)1]3+` = (exact_mass + 2 * proton + sodium) / 3,
      `[1M+(H)1(Na)2]3+` = (exact_mass + proton + 2 * sodium) / 3,
      `[1M+(Na)3]3+` = (exact_mass + 3 * sodium) / 3,
      `[1M+(H)2]2+` = (exact_mass + 2 * proton) / 2,
      `[1M+(H)2(NH3)1]2+` = (exact_mass + 2 * proton + ammonium) / 2,
      `[1M+(H)1(Na)1]2+` = (exact_mass + proton + sodium) / 2,
      `[1M+(Mg)1]2+` = (exact_mass + magnesium) / 2,
      `[1M+(H)1(K)1]2+` = (exact_mass + proton + potassium) / 2,
      `[1M+(Ca)1]2+` = (exact_mass + calcium) / 2,
      `[1M+(H)2(ACN)1]2+` = (exact_mass + 2 * proton + acetonitrile) / 2,
      `[1M+(Na)2]2+` = (exact_mass + 2 * sodium) / 2,
      `[1M+(Fe)1]2+` = (exact_mass + iron) / 2,
      `[1M+(H)2(ACN)2]2+` = (exact_mass + 2 * proton + 2 * acetonitrile) / 2,
      `[1M+(H)2(ACN)3]2+` = (exact_mass + 2 * proton + 3 * acetonitrile) / 2,
      # `[1M+(H)1-(H2O)2]1+` = exact_mass - 2 * water + proton,
      # `[1M+(H)1-(H2O)1]1+` = exact_mass - water + proton,
      `[1M+(H)1]1+` = exact_mass + proton,
      # `[1M+(Na)1-(H2O)1]1+` = exact_mass - water + sodium,
      `[1M+(H)1(NH3)1]1+` = exact_mass + proton + ammonium,
      `[1M+(Na)1]1+` = exact_mass + sodium,
      `[1M+(Mg)1-(H)1]1+` = exact_mass - proton + magnesium,
      `[1M+(H)1(CH3OH)1]1+` = exact_mass + proton + methanol,
      `[1M+(K)1]1+` = exact_mass + potassium,
      `[1M+(Ca)1-(H)1]1+` = exact_mass - proton + calcium,
      `[1M+(H)1(ACN)1]1+` = exact_mass + proton + acetonitrile,
      `[1M+(Na)2-(H)1]1+` = exact_mass - proton + 2 * sodium,
      `[1M+(H)1(C2H7N)1]1+` = exact_mass + proton + ethylamine,
      `[1M+(Fe)1-(H)1]1+` = exact_mass - proton + iron,
      `[1M+(H)1(IsoProp)1]1+` = exact_mass + proton + isopropanol,
      `[1M+(Na)1(ACN)1]1+` = exact_mass + sodium + acetonitrile,
      `[1M+(K)2-(H)1]1+` = exact_mass - proton + 2 * potassium,
      `[1M+(H)1(DMSO)1]1+` = exact_mass + proton + dmso,
      `[1M+(H)1(ACN)2]1+` = exact_mass + proton + 2 * acetonitrile,
      # `[1M+(Na)1(IsoProp)1]1+` = exact_mass + sodium + isopropanol,
      `[2M+(Mg)1]2+` = (2 * exact_mass + magnesium) / 2,
      `[2M+(Ca)1]2+` = (2 * exact_mass + calcium) / 2,
      `[2M+(Fe)1]2+` = (2 * exact_mass + iron) / 2,
      `[2M+(H)1]1+` = 2 * exact_mass + proton,
      `[2M+(H)1(NH3)1]1+` = 2 * exact_mass + proton + ammonium,
      `[2M+(Na)1]1+` = 2 * exact_mass + sodium,
      `[2M+(K)1]1+` = 2 * exact_mass + potassium,
      `[2M+(H)1(ACN)1]1+` = 2 * exact_mass + proton + acetonitrile,
      `[2M+(Na)1(ACN)1]1+` = 2 * exact_mass + acetonitrile + sodium
    ) |>
    tidytable::select(-colnames(adductsTable))

  n <- ncol(adducts_pos)

  ## Pivot the adducts_pos table to get a long format with adduct and adduct mass as columns
  adducts_pos <- adducts_pos |>
    tidytable::pivot_longer(2:tidytable::all_of(n)) |>
    tidytable::select(tidytable::everything(),
      adduct = name,
      adduct_mass = value
    )

  return(adducts_pos)
}
