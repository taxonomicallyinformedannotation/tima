#' @title Mass-pair edge inference helpers
#'
#' @description
#' Helpers that turn (feature, adduct) hypotheses into adduct- and
#' neutral-loss-edge candidates by joining RT- and m/z-tolerance windows.
#' Extracted from the original `annotate_masses.R` monolith for clarity.
#'
#' @include adduct_universe.R
#' @keywords internal
#' @name mass_pair_edges
NULL

#' Build feature pairs inside RT windows
#' @keywords internal
build_feature_pairs_within_rt <- function(
  df_rt_tol,
  df_fea_min,
  tolerance_ppm
) {
  src <- tidytable::as_tidytable(df_rt_tol)[,
    .(
      src_feature_id = feature_id,
      src_rt = rt,
      src_mz = mz,
      src_adduct = adduct,
      sample,
      rt_min,
      rt_max
    )
  ]
  dest <- tidytable::as_tidytable(df_fea_min)[,
    .(
      feature_id_dest = feature_id,
      rt_dest = rt,
      mz_dest = mz,
      adduct_dest = adduct,
      sample
    )
  ]

  matches <- dest[
    src,
    on = .(sample, rt_dest >= rt_min, rt_dest <= rt_max),
    nomatch = 0L,
    allow.cartesian = TRUE
  ][
    src_feature_id != feature_id_dest & mz_dest >= src_mz,
    .(
      feature_id = src_feature_id,
      rt = src_rt,
      mz = src_mz,
      adduct = src_adduct,
      feature_id_dest,
      mz_dest,
      adduct_dest
    )
  ]

  if (nrow(matches) == 0L) {
    return(tidytable::as_tidytable(matches)[,
      `:=`(delta = numeric(), delta_min = numeric(), delta_max = numeric())
    ])
  }

  matches <- unique(matches)
  matches[, delta := mz_dest - mz]
  matches[,
    `:=`(
      delta_min = delta - (1E-6 * tolerance_ppm * (mz + mz_dest) / 2),
      delta_max = delta + (1E-6 * tolerance_ppm * (mz + mz_dest) / 2)
    )
  ]

  tidytable::as_tidytable(matches)
}

#' Build all ordered adduct-pair differences for single-charge adducts
#' @keywords internal
build_adduct_pair_differences <- function(
  add_clu_table,
  tolerance_ppm,
  max_mz
) {
  add_src <- add_clu_table |>
    tidytable::distinct(adduct, adduct_mass) |>
    tidytable::rename(Group1 = adduct, mass1 = adduct_mass) |>
    tidytable::mutate(join = "x")

  add_dest <- add_clu_table |>
    tidytable::distinct(adduct, adduct_mass) |>
    tidytable::rename(Group2 = adduct, mass2 = adduct_mass) |>
    tidytable::mutate(join = "x")

  add_src |>
    tidytable::left_join(y = add_dest, by = "join") |>
    tidytable::filter(Group1 != Group2) |>
    tidytable::filter(mass1 < mass2) |>
    tidytable::mutate(Distance = mass2 - mass1) |>
    ## Keep all adduct mappings with matching deltas; only remove exact
    ## duplicates from repeated adduct entries.
    tidytable::distinct(Group1, Group2, Distance) |>
    ## Using max because ppm tolerance tends to be overconfident
    tidytable::filter(Distance >= tolerance_ppm * 1E-6 * max_mz) |>
    tidytable::select(Distance, Group1, Group2)
}

#' Join RT/mz couple windows with neutral losses
#' @keywords internal
join_couples_with_neutral_losses <- function(df_couples_diff, neutral_losses) {
  cd_src <- tidytable::as_tidytable(df_couples_diff)[,
    .(
      src_feature_id = feature_id,
      src_adduct = adduct,
      src_feature_id_dest = feature_id_dest,
      src_adduct_dest = adduct_dest,
      delta_min,
      delta_max
    )
  ]
  nl_win <- tidytable::as_tidytable(neutral_losses)[
    !is.na(loss),
    .(loss, loss_mass = mass, loss_mass_keep = mass)
  ]

  nl_win[
    cd_src,
    on = .(loss_mass >= delta_min, loss_mass <= delta_max),
    nomatch = 0L,
    allow.cartesian = TRUE
  ][,
    .(
      feature_id = src_feature_id,
      adduct = src_adduct,
      loss,
      mass = loss_mass_keep,
      feature_id_dest = src_feature_id_dest,
      adduct_dest = src_adduct_dest
    )
  ] |>
    unique() |>
    tidytable::as_tidytable()
}

#' Join multi-adduct candidates with add/loss inferred masses
#' @keywords internal
join_multi_with_addlossed <- function(df_multi, df_addlossed_rdy) {
  multi_src <- tidytable::as_tidytable(df_multi)[,
    .(
      feature_id,
      adduct,
      rt,
      mz,
      rt_min,
      rt_max,
      mass_min,
      mass_max
    )
  ]
  addloss_win <- tidytable::as_tidytable(df_addlossed_rdy)[,
    .(
      rt_obs = rt,
      mass_obs = mass,
      rt_obs_keep = rt,
      mass_obs_keep = mass
    )
  ]

  addloss_win[
    multi_src,
    on = .(
      rt_obs >= rt_min,
      rt_obs <= rt_max,
      mass_obs >= mass_min,
      mass_obs <= mass_max
    ),
    nomatch = 0L,
    allow.cartesian = TRUE
  ][,
    .(
      feature_id,
      rt,
      mz,
      adduct,
      mass = mass_obs_keep
    )
  ] |>
    tidytable::as_tidytable()
}
