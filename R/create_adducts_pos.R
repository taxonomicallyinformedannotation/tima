#' @title Create adducts positive
#'
#' @description This function creates positive adducts
#'
#' @param masses_table Table containing the masses of the adducts
#' @param adducts_table Table containing the adducts
#'
#' @return A tidytable with columns "adduct" and "adduct_mass"
#'
#' @export
#'
#' @examples NULL
create_adducts_pos <- function(masses_table = get("masses_table",
                                 envir = parent.frame()
                               ),
                               adducts_table = get("adducts_table",
                                 envir = parent.frame()
                               )) {
  ## Calculate the masses for various positive adducts
  adducts_pos <- masses_table |>
    tidytable::mutate(
      `[1M+(H)3]3+` = (exact_mass + 3 * proton) / 3,
      `[1M+(H)2(Na)1]3+` = (exact_mass + 2 * proton + sodium) / 3,
      `[1M+(H)1(Na)2]3+` = (exact_mass + proton + 2 * sodium) / 3,
      `[1M+(Na)3]3+` = (exact_mass + 3 * sodium) / 3,
      `[1M+(H)2]2+` = (exact_mass + 2 * proton) / 2,
      `[1M+(H)1(Na)1]2+` = (exact_mass + proton + sodium) / 2,
      `[1M+(Mg)1]2+` = (exact_mass + magnesium) / 2,
      `[1M+(H)1(K)1]2+` = (exact_mass + proton + potassium) / 2,
      `[1M+(Ca)1]2+` = (exact_mass + calcium) / 2,
      `[1M+(Na)2]2+` = (exact_mass + 2 * sodium) / 2,
      `[1M+(Fe)1]2+` = (exact_mass + iron) / 2,
      `[1M+(H)1]1+` = exact_mass + proton,
      `[1M+(Na)1]1+` = exact_mass + sodium,
      `[1M+(K)1]1+` = exact_mass + potassium,
      `[1M+(Cu)1]1+` = exact_mass + copper,
      `[2M+(Mg)1]2+` = (2 * exact_mass + magnesium) / 2,
      `[2M+(Ca)1]2+` = (2 * exact_mass + calcium) / 2,
      `[2M+(Fe)1]2+` = (2 * exact_mass + iron) / 2,
      `[2M+(H)1]1+` = 2 * exact_mass + proton,
      `[2M+(Na)1]1+` = 2 * exact_mass + sodium,
      `[2M+(K)1]1+` = 2 * exact_mass + potassium
    ) |>
    tidytable::select(-colnames(adducts_table))

  ## Pivot the adducts_pos table to get a long format
  ## with adduct and adduct mass as columns
  adducts_pos <- adducts_pos |>
    tidytable::pivot_longer(
      -exact_mass,
      names_to = "adduct",
      values_to = "adduct_mass"
    )

  return(adducts_pos)
}

## See https://github.com/markfairbanks/tidytable/issues/269
.datatable.aware <- TRUE
