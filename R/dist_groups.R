#' @title Get distance between two elements
#'
#' @description Calculates the distance between two elements in a distance
#'     matrix by their indices. Optimized for repeated lookups.
#'
#' @details Credit: Algorithm adapted from usedist package
#'
#' @include validators.R
#'
#' @param d Distance matrix or dist object
#' @param idx1 Integer index of the first element (1-based)
#' @param idx2 Integer index of the second element (1-based)
#'
#' @return Numeric distance between the two elements.
#'     Returns 0 if indices are identical.
#'     Returns NA if indices are out of bounds (with warning).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Create distance matrix
#' data_mat <- matrix(rnorm(20), nrow = 5)
#' d <- dist(data_mat)
#'
#' # Get distance between elements 1 and 3
#' dist_get(d, 1, 3)
#'
#' # Distance to self is 0
#' dist_get(d, 2, 2)
#' }
dist_get <- function(d, idx1, idx2) {
  # Input Validation and Conversion ----
  if (!inherits(d, "dist")) {
    d <- stats::as.dist(m = d)
  }

  n <- attr(d, "Size")

  # Validate indices are within bounds
  if (any(idx1 < 1L | idx1 > n | idx2 < 1L | idx2 > n)) {
    warning(
      "Some indices out of bounds (n=",
      n,
      "), returning NA",
      call. = FALSE
    )
  }

  # Calculate Distance (Vectorized) ----
  # Map to lower triangle indices
  i <- pmin(idx1, idx2)
  j <- pmax(idx1, idx2)

  # Linear index in packed distance matrix
  linear_idx <- ifelse(
    i == j,
    NA_integer_,
    n * (i - 1L) - i * (i - 1L) / 2L + j - i
  )

  # Return: 0 for diagonal, matrix value otherwise
  ifelse(i == j, 0, d[linear_idx])
}

#' @title Calculate pairwise distances with group labels
#'
#' @description Computes pairwise distances between observations and annotates
#'     them with group membership. Useful for analyzing within-group vs
#'     between-group distances.
#'
#' @include validators.R
#'
#' @param d Distance object or matrix
#' @param g Grouping vector for observations. Must have length equal to
#'     number of observations in distance matrix.
#'
#' @return Data frame with columns:
#'   \item{Item1}{Index of first observation}
#'   \item{Item2}{Index of second observation}
#'   \item{Group1}{Group label of first observation}
#'   \item{Group2}{Group label of second observation}
#'   \item{Label}{Factor: "Within" or "Between" groups}
#'   \item{Distance}{Numeric distance (rounded to 5 decimals)}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Sample data with groups
#' data <- matrix(rnorm(30), nrow = 10)
#' groups <- rep(c("A", "B", "C"), c(3, 4, 3))
#' d <- dist(data)
#'
#' # Calculate distances with group info
#' result <- dist_groups(d, groups)
#'
#' # Analyze within vs between group distances
#' aggregate(Distance ~ Label, data = result, FUN = mean)
#' }
dist_groups <- function(d, g) {
  # Input Validation ----
  d <- stats::as.dist(m = d)
  g <- as.factor(g)

  n_obs <- attr(d, "Size")

  if (length(g) != n_obs) {
    stop(
      "Grouping vector length (",
      length(g),
      ") must match ",
      "number of observations (",
      n_obs,
      ")",
      call. = FALSE
    )
  }

  # Generate Pairwise Combinations ----
  idx_pairs <- utils::combn(x = n_obs, m = 2L)
  idx1 <- idx_pairs[1L, ]
  idx2 <- idx_pairs[2L, ]

  # Get group memberships for each pair
  group1 <- g[idx1]
  group2 <- g[idx2]

  # Create Comparison Labels ----

  # Create descriptive labels for within/between group comparisons
  level1 <- levels(g)[pmin(as.numeric(group1), as.numeric(group2))]
  level2 <- levels(g)[pmax(as.numeric(group1), as.numeric(group2))]

  comparison_labels <- ifelse(
    level1 == level2,
    paste("Within", level1),
    paste("Between", level1, "and", level2)
  )

  # Build Result Data Frame ----

  # Build result data frame with all distance information
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
