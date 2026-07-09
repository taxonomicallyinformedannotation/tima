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
      mz_offset = (n_mer * adduct_mass_per_monomer + adduct_mass) /
        abs(z) +
        n_iso * isotope_shift,
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
      ) +
        rows$n_iso * isotope_shift
    }
    if (!fallback || all(matched)) {
      return(out)
    }
    unmatched <- !matched & !is.na(adducts)
    if (any(unmatched)) {
      out[unmatched] <- calculate_mz_from_mass_batch(
        neutral_masses = neutral_mass[unmatched],
        adducts = adducts[unmatched]
      )
    }
    return(out)
  }

  calculate_mz_from_mass_batch(
    neutral_masses = neutral_mass,
    adducts = adducts
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

  pair_idx <- t(utils::combn(seq_len(nrow(lookup)), 2L))
  if (is.null(dim(pair_idx)) || nrow(pair_idx) == 0L) {
    return(list(
      adduct_diffs = tidytable::tidytable(
        Distance = numeric(),
        Group1 = character(),
        Group2 = character()
      ),
      cluster_transitions = tidytable::tidytable(
        cluster = character(),
        mass = numeric(),
        adduct = character(),
        adduct_dest = character()
      ),
      loss_transitions = tidytable::tidytable(
        loss = character(),
        mass = numeric(),
        adduct = character(),
        adduct_dest = character()
      )
    ))
  }

  left_idx <- pair_idx[, 1L]
  right_idx <- pair_idx[, 2L]
  left_rows <- lookup[left_idx, , drop = FALSE]
  right_rows <- lookup[right_idx, , drop = FALSE]

  delta <- as.numeric(right_rows$mz_offset - left_rows$mz_offset)
  valid_delta <- is.finite(delta) & delta > 0
  same_slope <- abs(left_rows$mz_slope - right_rows$mz_slope) <= 1e-12

  adduct_keep <- valid_delta &
    same_slope &
    left_rows$cluster_key == right_rows$cluster_key &
    left_rows$loss_key == right_rows$loss_key
  adduct_diffs <- if (any(adduct_keep)) {
    tidytable::tidytable(
      Distance = delta[adduct_keep],
      Group1 = left_rows$adduct[adduct_keep],
      Group2 = right_rows$adduct[adduct_keep]
    )
  } else {
    tidytable::tidytable(
      Distance = numeric(),
      Group1 = character(),
      Group2 = character()
    )
  }

  cluster_keep <- valid_delta &
    same_slope &
    left_rows$n_mer == right_rows$n_mer &
    left_rows$z == right_rows$z &
    left_rows$n_iso == right_rows$n_iso &
    left_rows$carrier_key == right_rows$carrier_key &
    left_rows$loss_key == right_rows$loss_key
  cluster_rows <- if (any(cluster_keep)) {
    cluster_idx <- which(cluster_keep)
    cluster_labels <- vapply(
      cluster_idx,
      function(i) {
        single_component_increment(
          from = left_rows$clusters[[i]],
          to = right_rows$clusters[[i]]
        )
      },
      character(1L)
    )
    keep_labels <- !is.na(cluster_labels) & nzchar(cluster_labels)
    if (any(keep_labels)) {
      tidytable::tidytable(
        cluster = cluster_labels[keep_labels],
        mass = delta[cluster_idx[keep_labels]],
        adduct = left_rows$adduct[cluster_idx[keep_labels]],
        adduct_dest = right_rows$adduct[cluster_idx[keep_labels]]
      )
    } else {
      tidytable::tidytable(
        cluster = character(),
        mass = numeric(),
        adduct = character(),
        adduct_dest = character()
      )
    }
  } else {
    tidytable::tidytable(
      cluster = character(),
      mass = numeric(),
      adduct = character(),
      adduct_dest = character()
    )
  }

  loss_keep <- valid_delta &
    same_slope &
    left_rows$n_mer == right_rows$n_mer &
    left_rows$z == right_rows$z &
    left_rows$n_iso == right_rows$n_iso &
    left_rows$carrier_key == right_rows$carrier_key &
    left_rows$cluster_key == right_rows$cluster_key
  loss_rows <- if (any(loss_keep)) {
    loss_idx <- which(loss_keep)
    loss_labels <- vapply(
      loss_idx,
      function(i) {
        single_component_increment(
          from = right_rows$losses[[i]],
          to = left_rows$losses[[i]]
        )
      },
      character(1L)
    )
    keep_labels <- !is.na(loss_labels) & nzchar(loss_labels)
    if (any(keep_labels)) {
      tidytable::tidytable(
        loss = loss_labels[keep_labels],
        mass = delta[loss_idx[keep_labels]],
        adduct = left_rows$adduct[loss_idx[keep_labels]],
        adduct_dest = right_rows$adduct[loss_idx[keep_labels]]
      )
    } else {
      tidytable::tidytable(
        loss = character(),
        mass = numeric(),
        adduct = character(),
        adduct_dest = character()
      )
    }
  } else {
    tidytable::tidytable(
      loss = character(),
      mass = numeric(),
      adduct = character(),
      adduct_dest = character()
    )
  }

  list(
    adduct_diffs = adduct_diffs |>
      tidytable::distinct(Group1, Group2, Distance),
    cluster_transitions = cluster_rows |>
      tidytable::distinct(adduct, adduct_dest, cluster, mass),
    loss_transitions = loss_rows |>
      tidytable::distinct(adduct, adduct_dest, loss, mass)
  )
}
