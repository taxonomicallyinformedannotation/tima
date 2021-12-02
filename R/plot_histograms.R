if (!require(ggplot2)) {
  install.packages("ggplot2")
  require(package = "ggplot2", quietly = TRUE)
}

#' Title
#'
#' @noRd
#'
#' @param dataframe TODO
#' @param label TODO
#'
#' @return TODO
#' @export
#'
#' @examples
plot_histograms <- function(dataframe, label, y = "values") {
  absolute <- ggplot2::ggplot(
    dataframe,
    ggplot2::aes(
      x = sample,
      y = get(y),
      fill = ids
    )
  ) +
    ggplot2::geom_col() +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::scale_fill_manual(
      values = levels(dataframe$color) |>
        as.character(),
      guide = ggplot2::guide_legend(reverse = TRUE, ncol = 1)
    ) +
    ggplot2::scale_x_discrete(labels = levels(dataframe$species)) +
    ggplot2::labs(fill = "Chemical class") +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.title = ggplot2::element_text(face = "bold"),
      # legend.position = "none",
      axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)
    ) +
    ggplot2::xlab(label) +
    ggplot2::ylab("absolute")

  return(absolute)
}
