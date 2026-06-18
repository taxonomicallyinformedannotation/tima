#' Internal utilities for matching features to adduct universe
#' @include mass_evidence_utils.R
#' @include calculate_mass_of_m.R
#' @include adduct_universe.R
#' @keywords internal
#' @noRd

#' Canonical key for a named integer component vector
#' @keywords internal
component_count_key <- function(x) {
  if (is.null(x) || length(x) == 0L) {
    return("")
  }
  x_vals <- as.integer(unname(x))
  names(x_vals) <- names(x)
  x <- x_vals
  keep <- !is.na(x) & x != 0L & !is.na(names(x)) & nzchar(names(x))
  x <- x[keep]
  if (length(x) == 0L) {
    return("")
  }
  x <- x[order(names(x))]
  paste0(names(x), ":", x, collapse = "|")
}

#' Return the single component added when `to` differs from `from` by +1 term
#' @keywords internal
single_component_increment <- function(from, to) {
  if (is.null(from) || length(from) == 0L) {
    from <- integer()
  } else {
    from_vals <- as.integer(unname(from))
    names(from_vals) <- names(from)
    from <- from_vals
  }
  if (is.null(to) || length(to) == 0L) {
    to <- integer()
  } else {
    to_vals <- as.integer(unname(to))
    names(to_vals) <- names(to)
    to <- to_vals
  }

  all_names <- sort(unique(c(names(from), names(to))))
  if (length(all_names) == 0L) {
    return(NA_character_)
  }

  from_full <- stats::setNames(integer(length(all_names)), all_names)
  to_full <- stats::setNames(integer(length(all_names)), all_names)
  if (length(from) > 0L) {
    from_full[names(from)] <- from
  }
  if (length(to) > 0L) {
    to_full[names(to)] <- to
  }

  delta <- to_full - from_full
  if (sum(delta == 1L) == 1L && all(delta %in% c(0L, 1L)) && sum(delta) == 1L) {
    return(names(delta)[delta == 1L][[1L]])
  }
  NA_character_
}

#' Build a distinct adduct lookup table from the typed universe
#' @keywords internal
build_adduct_lookup <- function(universe) {
  if (nrow(universe) == 0L) {
    return(tidytable::tidytable(
      adduct = character(),
      n_mer = integer(),
      z = integer(),
      n_iso = integer(),
      adduct_mass = numeric(),
      adduct_mass_per_monomer = numeric(),
      mz_slope = numeric(),
      mz_offset = numeric(),
      carrier_key = character(),
      cluster_key = character(),
      loss_key = character()
    ))
  }

  isotope_shift <- get0(
    x = "EVIDENCE_ISOTOPE_SHIFT_DA",
    ifnotfound = ISOTOPE_MASS_SHIFT_DALTONS,
    inherits = TRUE
  )

  out <- tidytable::as_tidytable(universe) |>
    tidytable::mutate(
      adduct_mass_per_monomer = tidytable::coalesce(adduct_mass_per_monomer, 0),
      mz_slope = n_mer / abs(z),
      mz_offset = (
        n_mer * adduct_mass_per_monomer + adduct_mass
      ) / abs(z) + n_iso * isotope_shift,
      carrier_key = vapply(carriers, component_count_key, character(1L)),
      cluster_key = vapply(clusters, component_count_key, character(1L)),
      loss_key = vapply(losses, component_count_key, character(1L))
    ) |>
    tidytable::arrange(adduct) |>
    tidytable::distinct(adduct, .keep_all = TRUE)

  out
}

#' Calculate neutral masses using typed-universe lookup when available
#' @keywords internal
calculate_neutral_mass_from_lookup <- function(
  mzs,
  adducts,
  adduct_lookup = NULL,
  fallback = TRUE
) {
  adducts <- as.character(adducts)
  mzs <- as.numeric(mzs)
  out <- rep(NA_real_, length(adducts))

  if (!is.null(adduct_lookup) && nrow(adduct_lookup) > 0L) {
    idx <- match(adducts, adduct_lookup$adduct)
    matched <- !is.na(idx)
    if (any(matched)) {
      rows <- adduct_lookup[idx[matched], , drop = FALSE]
      out[matched] <- implied_neutral_mass(
        mz = mzs[matched],
        n_mer = rows$n_mer,
        z = rows$z,
        adduct_mass = rows$adduct_mass,
        n_iso = rows$n_iso,
        adduct_mass_per_monomer = rows$adduct_mass_per_monomer
      )
    }
    if (!fallback || all(matched)) {
      return(out)
    }
    unmatched <- !matched & !is.na(adducts)
    if (any(unmatched)) {
      out[unmatched] <- calculate_mass_of_m_batch(
        adducts = adducts[unmatched],
        mzs = mzs[unmatched]
      )
    }
    return(out)
  }

  calculate_mass_of_m_batch(adducts = adducts, mzs = mzs)
}

#' Calculate expected m/z using typed-universe lookup when available
#' @keywords internal
calculate_mz_from_lookup <- function(
  neutral_mass,
  adducts,
  adduct_lookup = NULL,
  fallback = TRUE
) {
  adducts <- as.character(adducts)
  neutral_mass <- as.numeric(neutral_mass)
  if (length(neutral_mass) == 1L) {
    neutral_mass <- rep(neutral_mass, length(adducts))
  }
  out <- rep(NA_real_, length(adducts))

  if (!is.null(adduct_lookup) && nrow(adduct_lookup) > 0L) {
    isotope_shift <- get0(
      x = "EVIDENCE_ISOTOPE_SHIFT_DA",
      ifnotfound = ISOTOPE_MASS_SHIFT_DALTONS,
      inherits = TRUE
    )
    idx <- match(adducts, adduct_lookup$adduct)
    matched <- !is.na(idx)
    if (any(matched)) {
      rows <- adduct_lookup[idx[matched], , drop = FALSE]
      out[matched] <- calculate_mz_from_neutral_mass(
        neutral_mass = neutral_mass[matched],
        n_mer = rows$n_mer,
        z = rows$z,
        adduct_mass = rows$adduct_mass,
        adduct_mass_per_monomer = rows$adduct_mass_per_monomer
      ) + rows$n_iso * isotope_shift
    }
    if (!fallback || all(matched)) {
      return(out)
    }
    unmatched <- !matched & !is.na(adducts)
    if (any(unmatched)) {
      out[unmatched] <- mapply(
        FUN = calculate_mz_from_mass,
        neutral_mass = neutral_mass[unmatched],
        adduct_string = adducts[unmatched],
        USE.NAMES = FALSE
      )
    }
    return(out)
  }

  mapply(
    FUN = calculate_mz_from_mass,
    neutral_mass = neutral_mass,
    adduct_string = adducts,
    USE.NAMES = FALSE
  )
}

#' Build direct pairwise transition tables from the typed universe
#' @keywords internal
build_universe_transition_tables <- function(universe) {
  lookup <- build_adduct_lookup(universe)
  if (nrow(lookup) == 0L) {
    return(list(
      adduct_diffs = tidytable::tidytable(
        Distance = numeric(),
        Group1 = character(),
        Group2 = character()
      ),
      cluster_transitions = tidytable::tidytable(
        feature_id = character(),
        cluster = character(),
        mass = numeric(),
        feature_id_dest = character(),
        adduct = character(),
        adduct_dest = character()
      )[0L, ],
      loss_transitions = tidytable::tidytable(
        feature_id = character(),
        loss = character(),
        mass = numeric(),
        feature_id_dest = character(),
        adduct = character(),
        adduct_dest = character()
      )[0L, ]
    ))
  }

  rows_to_table <- function(rows, template) {
    if (length(rows) == 0L) {
      return(template)
    }
    tidytable::as_tidytable(do.call(
      rbind,
      lapply(rows, function(x) {
        as.data.frame(x, stringsAsFactors = FALSE)
      })
    ))
  }

  adduct_rows <- list()
  cluster_rows <- list()
  loss_rows <- list()
  adduct_i <- 0L
  cluster_i <- 0L
  loss_i <- 0L

  for (i in seq_len(nrow(lookup))) {
    row_i <- lookup[i, ]
    for (j in seq_len(nrow(lookup))) {
      if (i == j) {
        next
      }
      row_j <- lookup[j, ]
      if (!isTRUE(all.equal(row_i$mz_slope, row_j$mz_slope, tolerance = 1e-12))) {
        next
      }
      delta <- as.numeric(row_j$mz_offset - row_i$mz_offset)
      if (!is.finite(delta) || delta <= 0) {
        next
      }

      if (
        identical(row_i$cluster_key[[1L]], row_j$cluster_key[[1L]]) &&
          identical(row_i$loss_key[[1L]], row_j$loss_key[[1L]])
      ) {
        adduct_i <- adduct_i + 1L
        adduct_rows[[adduct_i]] <- list(
          Distance = delta,
          Group1 = row_i$adduct[[1L]],
          Group2 = row_j$adduct[[1L]]
        )
      }

      if (
        row_i$n_mer[[1L]] == row_j$n_mer[[1L]] &&
          row_i$z[[1L]] == row_j$z[[1L]] &&
          row_i$n_iso[[1L]] == row_j$n_iso[[1L]] &&
          identical(row_i$carrier_key[[1L]], row_j$carrier_key[[1L]]) &&
          identical(row_i$loss_key[[1L]], row_j$loss_key[[1L]])
      ) {
        cluster_label <- single_component_increment(
          from = row_i$clusters[[1L]],
          to = row_j$clusters[[1L]]
        )
        if (!is.na(cluster_label) && nzchar(cluster_label)) {
          cluster_i <- cluster_i + 1L
          cluster_rows[[cluster_i]] <- list(
            cluster = cluster_label,
            mass = delta,
            adduct = row_i$adduct[[1L]],
            adduct_dest = row_j$adduct[[1L]]
          )
        }
      }

      if (
        row_i$n_mer[[1L]] == row_j$n_mer[[1L]] &&
          row_i$z[[1L]] == row_j$z[[1L]] &&
          row_i$n_iso[[1L]] == row_j$n_iso[[1L]] &&
          identical(row_i$carrier_key[[1L]], row_j$carrier_key[[1L]]) &&
          identical(row_i$cluster_key[[1L]], row_j$cluster_key[[1L]])
      ) {
        loss_label <- single_component_increment(
          from = row_j$losses[[1L]],
          to = row_i$losses[[1L]]
        )
        if (!is.na(loss_label) && nzchar(loss_label)) {
          loss_i <- loss_i + 1L
          loss_rows[[loss_i]] <- list(
            loss = loss_label,
            mass = delta,
            adduct = row_i$adduct[[1L]],
            adduct_dest = row_j$adduct[[1L]]
          )
        }
      }
    }
  }

  list(
    adduct_diffs = rows_to_table(
      adduct_rows,
      tidytable::tidytable(
        Distance = numeric(),
        Group1 = character(),
        Group2 = character()
      )
    ) |>
      tidytable::distinct(Group1, Group2, Distance),
    cluster_transitions = rows_to_table(
      cluster_rows,
      tidytable::tidytable(
        cluster = character(),
        mass = numeric(),
        adduct = character(),
        adduct_dest = character()
      )
    ) |>
      tidytable::distinct(adduct, adduct_dest, cluster, mass),
    loss_transitions = rows_to_table(
      loss_rows,
      tidytable::tidytable(
        loss = character(),
        mass = numeric(),
        adduct = character(),
        adduct_dest = character()
      )
    ) |>
      tidytable::distinct(adduct, adduct_dest, loss, mass)
  )
}

