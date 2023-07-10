#' @title Clean collapse
#'
#' @description This function collapses a grouped dataframe and trims it
#'
#' @param grouped_df Grouped dataframe
#'
#' @return NULL
#'
#' @export
#'
#' @examples NULL
clean_collapse <- function(grouped_df) {
  clean_collapse_df <- grouped_df |>
    dplyr::reframe(dplyr::across(
      .cols = dplyr::everything(),
      .fns = function(x) {
        x <- list(paste(unique(x[!is.na(x)]), collapse = " $ "))
      }
    )) |>
    dplyr::ungroup() |>
    dplyr::mutate(dplyr::across(dplyr::everything(), trimws)) |> 
    dplyr::mutate(dplyr::across(dplyr::everything(), .fns = function(x) {
      tidytable::na_if(x, "")
    }))

  return(clean_collapse_df)
}
