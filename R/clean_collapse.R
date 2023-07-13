#' @title Clean collapse
#'
#' @description This function collapses a grouped dataframe and trims it
#'
#' @param grouped_df Grouped dataframe
#' @param cols Column to apply collapse to
#'
#' @return NULL
#'
#' @export
#'
#' @examples NULL
clean_collapse <- function(grouped_df, cols = NA) {
  clean_collapse_df <- grouped_df |>
    dplyr::reframe(dplyr::across(
      .cols = ifelse(
        test = is.na(cols),
        yes = dplyr::everything(),
        no = cols
      ),
      .fns = function(x) {
        x <- list(paste(unique(x[!is.na(x)]), collapse = " $ "))
      }
    )) |>
    tidytable::tidytable() |>
    tidytable::ungroup() |>
    tidyft::mutate_vars(is.list, .func = as.character) |>
    tidyft::mutate_vars(is.character, .func = trimws) |>
    tidyft::mutate_vars(
      is.character,
      .func = function(x) {
        tidytable::na_if(x, "")
      }
    )

  return(clean_collapse_df)
}
