#' @title Distance between two elements in a distance matrix
#'
#' @description This function calculates the distance between two elements
#'     in a distance matrix by their indices. Returns 0 for identical elements.
#'
#' @details Credit goes to usedist package for the algorithm
#'
#' @param d Distance matrix or dist object
#' @param idx1 Integer index of the first element
#' @param idx2 Integer index of the second element
#'
#' @return Numeric distance between the two elements. Returns 0 if indices
#'     are identical, NA if indices are invalid.
#'
#' @examples NULL
dist_get <- function(d, idx1, idx2) {
  # Convert input to distance object
  d <- stats::as.dist(d)

  # Get size of distance matrix
  n <- attr(d, "Size")

  # Validate indices
  if (any(idx1 < 1L | idx1 > n | idx2 < 1L | idx2 > n)) {
    warning("Some indices are out of bounds")
  }

  # Calculate linear index into lower triangle of distance matrix
  i <- pmin(idx1, idx2)
  j <- pmax(idx1, idx2)
  linear_idx <- ifelse(
    i == j,
    NA_integer_,
    n * (i - 1L) - i * (i - 1L) / 2L + j - i
  )

  # Return distance (0 for identical indices)
  ifelse(i == j, 0, d[linear_idx])
}

#' @title Dist groups
#'
#' @description This function calculates pairwise distances between observations
#'     and annotates them with group membership information
#'
#' @param d A distance object or matrix
#' @param g A grouping vector for the observations in the distance object.
#'     Must have length equal to the number of observations.
#'
#' @return A data frame containing distance information between pairs of
#'     observations with columns:
#'     \item{Item1}{Index of first observation}
#'     \item{Item2}{Index of second observation}
#'     \item{Group1}{Group label of first observation}
#'     \item{Group2}{Group label of second observation}
#'     \item{Label}{Factor indicating if distance is within or between groups}
#'     \item{Distance}{Numeric distance value rounded to 5 digits}
#'
#' @examples NULL
dist_groups <- function(d, g) {
  # Convert d to a dist object
  d <- stats::as.dist(d)

  # Convert g to a factor
  g <- as.factor(g)

  # Validate that lengths match
  n_obs <- attr(d, "Size")
  if (length(g) != n_obs) {
    stop(
      "Length of grouping vector (",
      length(g),
      ") does not match number of observations (",
      n_obs,
      ")"
    )
  }

  # Get all pairwise combinations of observation indices
  idx_pairs <- utils::combn(n_obs, 2L)
  idx1 <- idx_pairs[1L, ]
  idx2 <- idx_pairs[2L, ]

  # Get group memberships for each pair
  group1 <- g[idx1]
  group2 <- g[idx2]

  # Create descriptive labels for within/between group comparisons
  level1 <- levels(g)[pmin(as.numeric(group1), as.numeric(group2))]
  level2 <- levels(g)[pmax(as.numeric(group1), as.numeric(group2))]

  comparison_labels <- ifelse(
    level1 == level2,
    paste("Within", level1),
    paste("Between", level1, "and", level2)
  )

  # Build result data frame
  data.frame(
    Item1 = idx1,
    Item2 = idx2,
    Group1 = group1,
    Group2 = group2,
    Label = factor(comparison_labels),
    Distance = round(dist_get(d, idx1, idx2), digits = 5L),
    stringsAsFactors = FALSE
  )
}
