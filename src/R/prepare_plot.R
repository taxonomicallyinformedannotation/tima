require(dplyr)
require(forcats)

#' Title
#'
#' @param dataframe
#'
#' @return
#' @export
#'
#' @examples
prepare_plot <- function(dataframe, organism = "species") {
  samples <- dataframe |>
    ungroup() |>
    dplyr::filter(
      parents != "" &
        !grepl(pattern = "-", x = parents) &
        !grepl(
          pattern = "notClassified",
          x = parents,
          fixed = TRUE
        )
    ) |>
    dplyr::filter(!is.na(get(organism))) |>
    dplyr::mutate(species =
                    gsub(
                      pattern = "([A-Z]{1})(.* )",
                      replacement = "\\1. ",
                      x = get(organism),
                      perl = TRUE
                    )) |>
    dplyr::group_by(parents) |>
    dplyr::mutate(group = dplyr::cur_group_id()) |>
    dplyr::group_by(group, ids) |>
    dplyr::mutate(subgroup = dplyr::cur_group_id()) |>
    dplyr::group_by(group) |>
    dplyr::mutate(subgroup = dplyr::dense_rank(x = as.numeric(subgroup))) |>
    dplyr::rowwise() |>
    dplyr::group_by(sample) |>
    dplyr::mutate(tot = sum(values)) |>
    dplyr::rowwise() |>
    dplyr::mutate(color = nice_colors[[group]][subgroup]) |>
    dplyr::mutate(relative = values / tot)
  
  samples$ids <-
    forcats::fct_reorder2(
      .f = samples$ids,
      .x = samples$values,
      .y = samples$group,
      .desc = FALSE
    )
  
  samples$color <-
    forcats::fct_reorder2(
      .f = samples$color,
      .x = samples$values,
      .y = samples$group,
      .desc = FALSE
    )
  
  samples$species <-
    forcats::fct_reorder2(
      .f = samples$species,
      .x = samples$values,
      .y = samples$sample,
      .desc = FALSE
    )
  
  return(samples)
}
