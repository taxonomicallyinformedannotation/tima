utils::globalVariables(
  c(
    "acetic",
    "bromine",
    "chlorine",
    "exact_mass",
    "formic",
    "name",
    "potassium",
    "proton",
    "sodium",
    "tfa",
    "value"
  )
)

#' @title Create adducts negative
#'
#' @description This function creates negative adducts
#'
#' @param masses_table Table containing the masses of the adducts
#' @param adducts_table Table containing the adducts
#'
#' @return NULL
#'
#' @export
#'
#' @examples NULL
create_adducts_neg <- function(masses_table = get("masses_table", envir = parent.frame()),
                               adducts_table = get("adducts_table", envir = parent.frame())) {
  ## Calculate the masses for various negative adducts
  adducts_neg <- masses_table |>
    tidytable::tidytable() |>
    dplyr::mutate(
      `[1M-(H)3]3-` = (exact_mass - 3 * proton) / 3,
      `[1M-(H)2]2-` = ((exact_mass - 2 * proton) / 2),
      # `[1M-(H)2(H2O)1]1-` = exact_mass - 2 * water - proton,
      # `[1M-(H)2(H2O)1]1-` = exact_mass - water - proton,
      `[1M-(H)1]1-` = exact_mass - proton,
      `[1M+(Na)1-(H)2]1-` = exact_mass + sodium - 2 * proton,
      `[1M+(Cl)1]1-` = exact_mass + chlorine,
      `[1M+(K)1-(H)2]1-` = exact_mass + potassium - 2 * proton,
      `[1M+(FA)1-(H)1]1-` = exact_mass + formic - proton,
      `[1M+(Hac)1-(H)1]1-` = exact_mass + acetic - proton,
      `[1M+(Na)1(FA)1-(H)2]1-` = exact_mass + formic + sodium - 2 * proton,
      `[1M+(Br)1]1-` = exact_mass + bromine,
      `[1M+(TFA)1-(H)1]1-` = exact_mass + tfa - proton,
      `[2M-(H)1]1-` = 2 * exact_mass - proton,
      `[2M+(FA)1-(H)1]1-` = 2 * exact_mass + formic - proton,
      `[2M+(Hac)1-(H)1]1-` = 2 * exact_mass + acetic - proton,
      `[3M-(H)1]1-` = 3 * exact_mass - proton
    ) |>
    tidytable::select(-colnames(adducts_table))

  ## Pivot the adducts_neg table to get a long format with adduct and adduct mass as columns
  n <- ncol(adducts_neg)
  adducts_neg <- adducts_neg |>
    tidytable::pivot_longer(2:tidytable::all_of(n)) |>
    tidytable::select(tidytable::everything(),
      adduct = name,
      adduct_mass = value
    )

  return(adducts_neg)
}
