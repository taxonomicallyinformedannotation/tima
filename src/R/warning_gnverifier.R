require(dplyr)

#' Title
#'
#' @param dataframe
#'
#' @return
#' @export
#'
#' @examples
warning_gnverifier <- function(dataframe) {
  warning <- dataOrganismVerified |>
    dplyr::filter(!is.na(organism)) |>
    dplyr::mutate(
      organismCleaned = ifelse(
        test = organismDbTaxo == "Open Tree of Life",
        yes = organismCleaned,
        no = NA
      )
    ) |>
    dplyr::distinct(organism, organismCleaned) |>
    dplyr::group_by(organism) |>
    dplyr::add_count() |>
    dplyr::ungroup() |>
    dplyr::filter(n == 1) |>
    dplyr::select(-n) |>
    dplyr::filter(is.na(organismCleaned))

  return(warning)
}
