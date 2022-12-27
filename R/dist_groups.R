#' Credit goes to usedist package

#' @title Get the distance between two elements in a distance matrix
#'
#' @description This function calculates the distance between two elements in a distance matrix
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

  # Check if idx1 and idx2 are character vectors and convert them to indexes
  if (is.character(idx1)) {
    idx1 <- match(idx1, attr(d, "Labels"))
  }
  if (is.character(idx2)) {
    idx2 <- match(idx2, attr(d, "Labels"))
  }

  # Get size of distance matrix
  n <- attr(d, "Size")

  # Check if idx1 and idx2 are within range
  if (any(is.na(idx1) | (idx1 < 1) | (idx1 > n))) {
    stop("idx1 out of range")
  }
  if (any(is.na(idx2) | (idx2 < 1) | (idx2 > n))) {
    stop("idx2 out of range")
  }

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
#' @return A data frame containing distance information between pairs of observations
#'   in the distance object, with columns for the names or indices of the observations,
#'   the group labels for each observation, and the distance between the observations.
#'   The label column indicates whether the distance is within a group or between groups.
#'
#' @export
#'
#' @importFrom utils combn
#'
#' @examples NULL
dist_groups <- function(d, g) {
  # Convert d to a dist object
  d <- stats::as.dist(d)

  # Convert g to a factor
  g <- as.factor(g)

  # Check that the length of g matches the number of observations in d
  dsize <- attr(d, "Size")
  if (length(g) != dsize) {
    stop(
      "Length of grouping vector (g) must equal number of observations in ",
      "dist object (d)"
    )
  }

  # Get the labels of the observations in d
  dlabels <- attr(d, "Labels")

  # Get the combinations of indices for each pair of observations in d
  idxs <- utils::combn(dsize, 2)
  idx1 <- idxs[1, ]
  idx2 <- idxs[2, ]

  # Get the groups of the observations in idx1 and idx2
  level1 <-
    levels(g)[pmin(as.numeric(g[idx1]), as.numeric(g[idx2]))]
  level2 <-
    levels(g)[pmax(as.numeric(g[idx1]), as.numeric(g[idx2]))]

  # Create the data frame with the pairs of observations and their distances
  data.frame(
    Item1 = if (is.null(dlabels)) {
      idx1
    } else {
      dlabels[idx1]
    },
    Item2 = if (is.null(dlabels)) {
      idx2
    } else {
      dlabels[idx2]
    },
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
