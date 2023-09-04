#' @title Create adducts negative
#'
#' @description This function creates negative adducts
#'
#' @param masses_table Table containing the masses of the adducts
#' @param adducts_table Table containing the adducts
#'
#' @return A tidytable with columns "adduct" and "adduct_mass"
#'
#' @export
#'
#' @examples NULL
create_adducts_neg <- function(masses_table = get("masses_table",
                                 envir = parent.frame()
                               ),
                               adducts_table = get("adducts_table",
                                 envir = parent.frame()
                               )) {
  ## Calculate the masses for various negative adducts
  adducts_neg <- masses_table |>
    tidytable::mutate(
      `[1M-(H)3]3-` = (exact_mass - 3 * proton) / 3,
      `[1M-(H)2]2-` = (exact_mass - 2 * proton) / 2,
      `[1M-(H)1]1-` = exact_mass - proton,
      `[1M+(F)1]1-` = exact_mass + fluorine,
      `[1M+(Na)1-(H)2]1-` = exact_mass + sodium - 2 * proton,
      `[1M+(Cl)1]1-` = exact_mass + chlorine,
      `[1M+(K)1-(H)2]1-` = exact_mass + potassium - 2 * proton,
      `[1M+(Br)1]1-` = exact_mass + bromine,
      `[2M-(H)1]1-` = 2 * exact_mass - proton,
      `[3M-(H)1]1-` = 3 * exact_mass - proton
    ) |>
    tidytable::select(-tidytable::all_of(colnames(adducts_table)))

  ## Pivot the adducts_neg table to get a long format
  ## with adduct and adduct mass as columns
  adducts_neg <- adducts_neg |>
    tidytable::pivot_longer(
      -exact_mass,
      names_to = "adduct",
      values_to = "adduct_mass"
    )

  return(adducts_neg)
}

## See https://github.com/markfairbanks/tidytable/issues/269
.datatable.aware <- TRUE
