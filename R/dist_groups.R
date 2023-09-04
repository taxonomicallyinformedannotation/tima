#' @title Get the distance between two elements in a distance matrix
#'
#' @description This function calculates the distance between
#'    two elements in a distance matrix
#'
#' @details Credit goes to usedist package
#'
#' @param d Distance matrix
#' @param idx1 Index of the first element
#' @param idx2 Index of the second element
#'
#' @return Distance between the two elements
#'
#' @export
#'
#' @examples NULL
dist_get <- function(d, idx1, idx2) {
  # Convert input to distance matrix
  d <- stats::as.dist(d)

  # Get size of distance matrix
  n <- attr(d, "Size")

  # Calculate distance
  i <- pmin(idx1, idx2)
  j <- pmax(idx1, idx2)
  idx <- ifelse(i == j, NA, n * (i - 1) - i * (i - 1) / 2 + j - i)

  # Return distance
  ifelse(i == j, 0, d[idx])
}


#' @title Dist groups
#'
#' @description This function gets distances per group
#'
#' @param d A distance object
#' @param g A grouping vector for the distance object
#'
#' @return A data frame containing distance information
#'    between pairs of observations in the distance object,
#'    with columns for the names or indices of the observations,
#'    the group labels for each observation,
#'    and the distance between the observations.
#'    The label column indicates whether the distance is
#'    within a group or between groups.
#'
#' @export
#'
#' @examples NULL
dist_groups <- function(d, g) {
  ## Convert d to a dist object
  d <- stats::as.dist(d)

  ## Convert g to a factor
  g <- as.factor(g)

  ## Check that the length of g matches the number of observations in d
  dsize <- attr(d, "Size")

  ## Get the labels of the observations in d
  dlabels <- attr(d, "Labels")

  ## Get the combinations of indices for each pair of observations in d
  idxs <- utils::combn(dsize, 2)
  idx1 <- idxs[1, ]
  idx2 <- idxs[2, ]

  ## Get the groups of the observations in idx1 and idx2
  level1 <-
    levels(g)[pmin(as.numeric(g[idx1]), as.numeric(g[idx2]))]
  level2 <-
    levels(g)[pmax(as.numeric(g[idx1]), as.numeric(g[idx2]))]

  ## Create the data frame with the pairs of observations and their distances
  data.frame(
    Item1 = idx1,
    Item2 = idx2,
    Group1 = g[idx1],
    Group2 = g[idx2],
    Label = factor(ifelse(
      level1 == level2,
      paste(
        "Within",
        level1
      ),
      paste("Between", level1, "and", level2)
    )),
    Distance = dist_get(d, idx1, idx2),
    stringsAsFactors = FALSE
  )
}
