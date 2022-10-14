#' Credit goes to usedist package

#' @title Dist get
#'
#' @noRd
#'
#' @param d TODO
#' @param idx1 TODO
#' @param idx2 TODO
#'
#' @return TODO
#'
#' @export
#'
#' @examples TODO
dist_get <- function(d, idx1, idx2) {
  d <- stats::as.dist(d)
  if (is.character(idx1)) {
    idx1 <- match(idx1, attr(d, "Labels"))
  }
  if (is.character(idx2)) {
    idx2 <- match(idx2, attr(d, "Labels"))
  }
  n <- attr(d, "Size")
  if (any(is.na(idx1) | (idx1 < 1) | (idx1 > n))) {
    stop("idx1 out of range")
  }
  if (any(is.na(idx2) | (idx2 < 1) | (idx2 > n))) {
    stop("idx2 out of range")
  }
  i <- pmin(idx1, idx2)
  j <- pmax(idx1, idx2)
  idx <- ifelse(i == j, NA, n * (i - 1) - i * (i - 1) / 2 + j -
    i)
  ifelse(i == j, 0, d[idx])
}

#' @title Dist groups
#'
#' @noRd
#'
#' @param d TODO
#' @param g TODO
#'
#' @return TODO
#'
#' @export
#'
#' @examples TODO
dist_groups <- function(d, g) {
  d <- stats::as.dist(d)
  g <- as.factor(g)
  dsize <- attr(d, "Size")
  if (length(g) != dsize) {
    stop(
      "Length of grouping vector (g) must equal number of observations in ",
      "dist object (d)"
    )
  }
  dlabels <- attr(d, "Labels")
  idxs <- utils::combn(dsize, 2)
  idx1 <- idxs[1, ]
  idx2 <- idxs[2, ]
  level1 <-
    levels(g)[pmin(as.numeric(g[idx1]), as.numeric(g[idx2]))]
  level2 <-
    levels(g)[pmax(as.numeric(g[idx1]), as.numeric(g[idx2]))]
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
