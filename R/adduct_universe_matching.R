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

#' Vectorized canonical keys for a list of named integer component vectors
#' @keywords internal
.fast_component_count_keys <- function(comp_list) {
  n <- length(comp_list)
  if (n == 0L) {
    return(character(0L))
  }
  lens <- lengths(comp_list)
  out <- character(n)
  non_empty <- which(lens > 0L)
  if (length(non_empty) == 0L) {
    return(out)
  }
  single_idx <- non_empty[lens[non_empty] == 1L]
  if (length(single_idx) > 0L) {
    vals <- vapply(
      comp_list[single_idx],
      function(x) as.integer(x[[1L]]),
      integer(1L)
    )
    nms <- vapply(
      comp_list[single_idx],
      function(x) names(x)[[1L]] %||% "",
      character(1L)
    )
    valid_s <- !is.na(vals) & vals != 0L & !is.na(nms) & nzchar(nms)
    if (any(valid_s)) {
      s_ok <- single_idx[valid_s]
      out[s_ok] <- paste0(nms[valid_s], ":", vals[valid_s])
    }
  }
  multi_idx <- non_empty[lens[non_empty] > 1L]
  if (length(multi_idx) > 0L) {
    out[multi_idx] <- vapply(
      comp_list[multi_idx],
      component_count_key,
      character(1L)
    )
  }
  out
}

#' Vectorized single-component increment labels across index pairs
#' @keywords internal
.fast_single_component_increments <- function(comp_list, from_idx, to_idx) {
  n_pairs <- length(from_idx)
  if (n_pairs == 0L) {
    return(character(0L))
  }
  lens <- lengths(comp_list)
  tots <- vapply(
    comp_list,
    function(x) {
      if (length(x) == 0L) {
        return(0L)
      }
      vals <- as.integer(unname(x))
      nms <- names(x)
      keep <- !is.na(vals) & vals != 0L & !is.na(nms) & nzchar(nms)
      if (!any(keep)) {
        return(0L)
      }
      if (any(vals[keep] < 0L)) {
        return(NA_integer_)
      }
      sum(vals[keep])
    },
    integer(1L)
  )
  t_from <- tots[from_idx]
  t_to <- tots[to_idx]
  l_from <- lens[from_idx]
  l_to <- lens[to_idx]
  out <- rep(NA_character_, n_pairs)

  simple_01 <- which(l_from == 0L & l_to == 1L & !is.na(t_to) & t_to == 1L)
  if (length(simple_01) > 0L) {
    nms <- vapply(
      comp_list[to_idx[simple_01]],
      function(x) names(x)[[1L]] %||% NA_character_,
      character(1L)
    )
    out[simple_01] <- nms
  }

  cand_other <- which(
    !(l_from == 0L & l_to == 1L & !is.na(t_to) & t_to == 1L) &
      (is.na(t_from) | is.na(t_to) | (t_to - t_from == 1L))
  )
  if (length(cand_other) > 0L) {
    out[cand_other] <- vapply(
      cand_other,
      function(k) {
        single_component_increment(
          from = comp_list[[from_idx[[k]]]],
          to = comp_list[[to_idx[[k]]]]
        )
      },
      character(1L)
    )
  }
  out
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

  u_add <- universe$adduct
  ord <- order(u_add, method = "radix")
  keep <- ord[!duplicated(u_add[ord])]

  out <- universe[keep, , drop = FALSE]
  am_pm <- if ("adduct_mass_per_monomer" %in% colnames(out)) {
    tidytable::coalesce(as.numeric(out$adduct_mass_per_monomer), 0)
  } else {
    rep(0, nrow(out))
  }
  abs_z <- abs(out$z)
  out$adduct_mass_per_monomer <- am_pm
  out$mz_slope <- out$n_mer / abs_z
  out$mz_offset <- (out$n_mer *
    am_pm +
    out$adduct_mass +
    out$n_iso * isotope_shift) /
    abs_z
  out$carrier_key <- .fast_component_count_keys(out$carriers)
  out$cluster_key <- .fast_component_count_keys(out$clusters)
  out$loss_key <- .fast_component_count_keys(out$losses)

  tidytable::as_tidytable(out)
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
      m_idx <- idx[matched]
      am_pm_col <- adduct_lookup$adduct_mass_per_monomer %||%
        rep(0, nrow(adduct_lookup))
      out[matched] <- implied_neutral_mass(
        mz = mzs[matched],
        n_mer = adduct_lookup$n_mer[m_idx],
        z = adduct_lookup$z[m_idx],
        adduct_mass = adduct_lookup$adduct_mass[m_idx],
        n_iso = adduct_lookup$n_iso[m_idx],
        adduct_mass_per_monomer = am_pm_col[m_idx]
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
      m_idx <- idx[matched]
      am_pm_col <- adduct_lookup$adduct_mass_per_monomer %||%
        rep(0, nrow(adduct_lookup))
      out[matched] <- calculate_mz_from_neutral_mass(
        neutral_mass = neutral_mass[matched],
        n_mer = adduct_lookup$n_mer[m_idx],
        z = adduct_lookup$z[m_idx],
        adduct_mass = adduct_lookup$adduct_mass[m_idx],
        adduct_mass_per_monomer = am_pm_col[m_idx]
      ) +
        (adduct_lookup$n_iso[m_idx] * isotope_shift) /
          abs(adduct_lookup$z[m_idx])
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
#' @include adduct_universe.R
#' @include calculate_mass_of_m.R
#' @include mass_evidence_utils.R
build_universe_transition_tables <- function(universe) {
  lookup <- build_adduct_lookup(universe)
  n_lu <- nrow(lookup)
  if (n_lu == 0L) {
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

  if (n_lu < 2L) {
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

  reps <- (n_lu - 1L):1L
  left_idx <- rep.int(seq_len(n_lu - 1L), reps)
  right_idx <- sequence(reps, from = 2L:n_lu)

  # Orient every pair lighter -> heavier; the lookup is sorted by name, not
  # by mass, so either member of a combn() pair may be the heavier one
  mz_off <- as.numeric(lookup$mz_offset)
  swap <- mz_off[left_idx] > mz_off[right_idx]
  swap[is.na(swap)] <- FALSE
  if (any(swap)) {
    left_tmp <- left_idx[swap]
    left_idx[swap] <- right_idx[swap]
    right_idx[swap] <- left_tmp
  }

  delta <- mz_off[right_idx] - mz_off[left_idx]
  valid_delta <- is.finite(delta) & delta > 0
  mz_sl <- as.numeric(lookup$mz_slope)
  same_slope <- abs(mz_sl[left_idx] - mz_sl[right_idx]) <= 1e-12

  ck_id <- match(lookup$cluster_key, unique(lookup$cluster_key))
  lk_id <- match(lookup$loss_key, unique(lookup$loss_key))
  ca_id <- match(lookup$carrier_key, unique(lookup$carrier_key))
  n_mer_v <- lookup$n_mer
  z_v <- lookup$z
  n_iso_v <- lookup$n_iso
  add_v <- lookup$adduct

  same_cl_key <- ck_id[left_idx] == ck_id[right_idx]
  same_lo_key <- lk_id[left_idx] == lk_id[right_idx]

  adduct_keep <- which(valid_delta & same_slope & same_cl_key & same_lo_key)
  adduct_diffs <- if (length(adduct_keep) > 0L) {
    tidytable::tidytable(
      Distance = delta[adduct_keep],
      Group1 = add_v[left_idx[adduct_keep]],
      Group2 = add_v[right_idx[adduct_keep]]
    )
  } else {
    tidytable::tidytable(
      Distance = numeric(),
      Group1 = character(),
      Group2 = character()
    )
  }

  same_core <- valid_delta &
    same_slope &
    n_mer_v[left_idx] == n_mer_v[right_idx] &
    z_v[left_idx] == z_v[right_idx] &
    n_iso_v[left_idx] == n_iso_v[right_idx] &
    ca_id[left_idx] == ca_id[right_idx]

  cluster_idx <- which(same_core & same_lo_key)
  cluster_rows <- if (length(cluster_idx) > 0L) {
    cluster_labels <- .fast_single_component_increments(
      comp_list = lookup$clusters,
      from_idx = left_idx[cluster_idx],
      to_idx = right_idx[cluster_idx]
    )
    keep_labels <- which(!is.na(cluster_labels) & nzchar(cluster_labels))
    if (length(keep_labels) > 0L) {
      hit_c <- cluster_idx[keep_labels]
      tidytable::tidytable(
        cluster = cluster_labels[keep_labels],
        mass = delta[hit_c],
        adduct = add_v[left_idx[hit_c]],
        adduct_dest = add_v[right_idx[hit_c]]
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

  loss_idx <- which(same_core & same_cl_key)
  loss_rows <- if (length(loss_idx) > 0L) {
    loss_labels <- .fast_single_component_increments(
      comp_list = lookup$losses,
      from_idx = right_idx[loss_idx],
      to_idx = left_idx[loss_idx]
    )
    keep_labels <- which(!is.na(loss_labels) & nzchar(loss_labels))
    if (length(keep_labels) > 0L) {
      hit_l <- loss_idx[keep_labels]
      tidytable::tidytable(
        loss = loss_labels[keep_labels],
        mass = delta[hit_l],
        adduct = add_v[left_idx[hit_l]],
        adduct_dest = add_v[right_idx[hit_l]]
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
