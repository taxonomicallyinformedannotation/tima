#' @title Clean collapse
#'
#' @description This function collapses a grouped dataframe and trims it
#'
#' @importFrom tidytable across everything mutate na_if reframe ungroup where
#'
#' @param grouped_df Grouped dataframe
#' @param cols Column(s) to apply collapse to
#'
#' @return Cleaned and collapsed dataframe
#'
#' @export
#'
#' @examples NULL
clean_collapse <- function(grouped_df, cols = NA) {
  clean_collapse_df <- grouped_df |>
    reframe(across(
      .cols = ifelse(
        test = is.na(cols),
        yes = everything(),
        no = cols
      ),
      .fns = function(x) {
        x <- list(paste(unique(x[!is.na(x)]), collapse = " $ "))
      }
    )) |>
    ungroup() |>
    mutate(
      across(
        .cols = where(is.list),
        .fns = as.character
      )
    ) |>
    mutate(
      across(
        .cols = where(is.character),
        .fns = trimws
      )
    ) |>
    mutate(
      across(
        .cols = where(is.character),
        .fns = function(x) {
          na_if(x, "")
        }
      )
    )

  return(clean_collapse_df)
}
