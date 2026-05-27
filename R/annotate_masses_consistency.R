#               because tests import these by name)
# ============================================================

#' Build feature pairs inside RT windows.
#'
#' For every pair `(src, dest)` of features in the same RT tolerance window
#' (and same sample), with `mz_dest >= mz_src`, returns `delta = mz_dest -
#' mz_src` together with `delta_min/delta_max` widened by the ppm tolerance.
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
  tidytable::as_tidytable(matches) |>
    tidytable::select(-adduct, -adduct_dest)
}

#' Build all ordered adduct-pair differences for single-charge adducts.
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
    tidytable::distinct(Group1, Group2, Distance) |>
    tidytable::filter(Distance >= tolerance_ppm * 1E-6 * max_mz) |>
    tidytable::select(Distance, Group1, Group2)
}

#' Count, for each (feature, adduct), how many distinct neighbors in the
#' adduct graph independently support that label.
#' @keywords internal
compute_feature_adduct_support <- function(df_add) {
  if (nrow(df_add) == 0L) {
    return(tidytable::tidytable(
      feature_id = character(),
      adduct = character(),
      adduct_support = integer()
    ))
  }
  tidytable::bind_rows(
    df_add |>
      tidytable::transmute(
        feature_id,
        adduct,
        neighbor = feature_id_dest
      ),
    df_add |>
      tidytable::transmute(
        feature_id = feature_id_dest,
        adduct = adduct_dest,
        neighbor = feature_id
      )
  ) |>
    tidytable::distinct(feature_id, adduct, neighbor) |>
    tidytable::count(feature_id, adduct, name = "adduct_support") |>
    tidytable::mutate(adduct_support = as.integer(adduct_support))
}

#' Apply optional support-based adduct consistency filtering.
#' @keywords internal
apply_adduct_consistency_filter <- function(
  df_add,
  adduct_consistency = "conditional",
  adduct_min_support = 2L,
  adduct_consistency_min_degree = 3L
) {
  if (nrow(df_add) == 0L || adduct_consistency == "off") {
    return(df_add)
  }
  pair_hyp <- df_add |>
    tidytable::count(feature_id, feature_id_dest, name = "pair_hypotheses")
  feature_degree <- tidytable::bind_rows(
    df_add |>
      tidytable::transmute(feature_id, neighbor = feature_id_dest),
    df_add |>
      tidytable::transmute(feature_id = feature_id_dest, neighbor = feature_id)
  ) |>
    tidytable::distinct(feature_id, neighbor) |>
    tidytable::count(feature_id, name = "feature_degree")
  support <- compute_feature_adduct_support(df_add)
  scored <- df_add |>
    tidytable::left_join(
      pair_hyp,
      by = c("feature_id", "feature_id_dest")
    ) |>
    tidytable::left_join(
      support |> tidytable::rename(src_support = adduct_support),
      by = c("feature_id", "adduct")
    ) |>
    tidytable::left_join(
      support |>
        tidytable::rename(
          feature_id_dest = feature_id,
          adduct_dest = adduct,
          dest_support = adduct_support
        ),
      by = c("feature_id_dest", "adduct_dest")
    ) |>
    tidytable::left_join(
      feature_degree |> tidytable::rename(src_degree = feature_degree),
      by = "feature_id"
    ) |>
    tidytable::left_join(
      feature_degree |>
        tidytable::rename(
          feature_id_dest = feature_id,
          dest_degree = feature_degree
        ),
      by = "feature_id_dest"
    ) |>
    tidytable::mutate(
      pair_hypotheses = tidytable::if_else(
        is.na(pair_hypotheses),
        1L,
        as.integer(pair_hypotheses)
      ),
      src_support = tidytable::if_else(
        is.na(src_support),
        0L,
        as.integer(src_support)
      ),
      dest_support = tidytable::if_else(
        is.na(dest_support),
        0L,
        as.integer(dest_support)
      ),
      src_degree = tidytable::if_else(
        is.na(src_degree),
        0L,
        as.integer(src_degree)
      ),
      dest_degree = tidytable::if_else(
        is.na(dest_degree),
        0L,
        as.integer(dest_degree)
      )
    )
  if (adduct_consistency == "strict") {
    return(
      scored |>
        tidytable::filter(src_support >= adduct_min_support) |>
        tidytable::filter(dest_support >= adduct_min_support) |>
        tidytable::select(feature_id, adduct, adduct_dest, feature_id_dest) |>
        tidytable::distinct()
    )
  }
  scored |>
    tidytable::mutate(
      need_check = pair_hypotheses > 1L |
        src_degree >= adduct_consistency_min_degree |
        dest_degree >= adduct_consistency_min_degree
    ) |>
    tidytable::filter(
      !need_check |
        src_support >= adduct_min_support |
        dest_support >= adduct_min_support
    ) |>
    tidytable::select(feature_id, adduct, adduct_dest, feature_id_dest) |>
    tidytable::distinct()
}

#' Enforce graph-level adduct consistency on edge hypotheses
#' @keywords internal
enforce_graph_adduct_consistency <- function(df_add) {
  if (nrow(df_add) == 0L) {
    return(df_add)
  }
  support <- compute_feature_adduct_support(df_add)
  state_map <- build_adduct_state_key_map(unique(c(
    df_add$adduct,
    df_add$adduct_dest
  )))
  scored <- df_add |>
    tidytable::left_join(
      support |> tidytable::rename(src_support = adduct_support),
      by = c("feature_id", "adduct")
    ) |>
    tidytable::left_join(
      support |>
        tidytable::rename(
          feature_id_dest = feature_id,
          adduct_dest = adduct,
          dest_support = adduct_support
        ),
      by = c("feature_id_dest", "adduct_dest")
    ) |>
    tidytable::mutate(
      src_support = tidytable::if_else(
        is.na(src_support),
        0L,
        as.integer(src_support)
      ),
      dest_support = tidytable::if_else(
        is.na(dest_support),
        0L,
        as.integer(dest_support)
      ),
      edge_score = src_support + dest_support,
      edge_complexity = nchar(adduct) + nchar(adduct_dest)
    ) |>
    tidytable::left_join(
      state_map |> tidytable::rename(adduct_state_key = state_key),
      by = "adduct"
    ) |>
    tidytable::left_join(
      state_map |>
        tidytable::rename(
          adduct_dest = adduct,
          adduct_dest_state_key = state_key
        ),
      by = "adduct_dest"
    ) |>
    tidytable::mutate(
      adduct_state_key = tidytable::coalesce(adduct_state_key, adduct),
      adduct_dest_state_key = tidytable::coalesce(
        adduct_dest_state_key,
        adduct_dest
      )
    )

  undirected <- tidytable::bind_rows(
    scored |>
      tidytable::transmute(src = feature_id, dest = feature_id_dest),
    scored |>
      tidytable::transmute(src = feature_id_dest, dest = feature_id)
  ) |>
    tidytable::distinct()

  nodes <- unique(c(undirected$src, undirected$dest))
  neighbors <- split(undirected$dest, undirected$src)
  visited <- stats::setNames(rep(FALSE, length(nodes)), nodes)
  components <- list()
  for (node in nodes) {
    if (isTRUE(visited[[node]])) {
      next
    }
    queue <- c(node)
    visited[[node]] <- TRUE
    comp_nodes <- character()
    while (length(queue) > 0L) {
      current <- queue[[1L]]
      queue <- queue[-1L]
      comp_nodes <- c(comp_nodes, current)
      next_nodes <- neighbors[[current]]
      if (is.null(next_nodes)) {
        next
      }
      for (nxt in next_nodes) {
        if (!isTRUE(visited[[nxt]])) {
          visited[[nxt]] <- TRUE
          queue <- c(queue, nxt)
        }
      }
    }
    components[[length(components) + 1L]] <- unique(comp_nodes)
  }

  component_results <- vector("list", length(components))
  for (ci in seq_along(components)) {
    comp_nodes <- components[[ci]]
    sub <- scored |>
      tidytable::filter(
        feature_id %in% comp_nodes & feature_id_dest %in% comp_nodes
      )
    if (nrow(sub) == 0L) {
      next
    }
    node_candidates <- tidytable::bind_rows(
      sub |>
        tidytable::transmute(
          feature = feature_id,
          state_key = adduct_state_key
        ),
      sub |>
        tidytable::transmute(
          feature = feature_id_dest,
          state_key = adduct_dest_state_key
        )
    ) |>
      tidytable::distinct()
    node_priors <- tidytable::bind_rows(
      sub |>
        tidytable::transmute(
          feature = feature_id,
          state_key = adduct_state_key,
          prior = edge_score
        ),
      sub |>
        tidytable::transmute(
          feature = feature_id_dest,
          state_key = adduct_dest_state_key,
          prior = edge_score
        )
    ) |>
      tidytable::summarize(prior = sum(prior), .by = c(feature, state_key))
    assignments <- node_priors |>
      tidytable::arrange(feature, tidytable::desc(prior), nchar(state_key)) |>
      tidytable::distinct(feature, .keep_all = TRUE) |>
      tidytable::select(feature, state_key)
    assign_map <- stats::setNames(assignments$state_key, assignments$feature)
    max_iter <- 20L
    for (iter in seq_len(max_iter)) {
      changed <- FALSE
      for (f in names(assign_map)) {
        candidates_f <- node_candidates |>
          tidytable::filter(feature == f) |>
          tidytable::pull(state_key)
        if (length(candidates_f) <= 1L) {
          next
        }
        cand_scores <- rep(0, length(candidates_f))
        for (k in seq_along(candidates_f)) {
          cand <- candidates_f[[k]]
          contrib_src <- sub |>
            tidytable::filter(feature_id == f, adduct_state_key == cand)
          if (nrow(contrib_src) > 0L) {
            dest_match <- unname(assign_map[contrib_src$feature_id_dest])
            cand_scores[[k]] <- cand_scores[[k]] +
              sum(
                contrib_src$edge_score[
                  contrib_src$adduct_dest_state_key == dest_match
                ]
              )
          }
          contrib_dest <- sub |>
            tidytable::filter(
              feature_id_dest == f,
              adduct_dest_state_key == cand
            )
          if (nrow(contrib_dest) > 0L) {
            src_match <- unname(assign_map[contrib_dest$feature_id])
            cand_scores[[k]] <- cand_scores[[k]] +
              sum(
                contrib_dest$edge_score[
                  contrib_dest$adduct_state_key == src_match
                ]
              )
          }
          prior_val <- node_priors |>
            tidytable::filter(feature == f, state_key == cand) |>
            tidytable::pull(prior)
          if (length(prior_val) > 0L) {
            cand_scores[[k]] <- cand_scores[[k]] + (0.01 * prior_val[[1L]])
          }
        }
        best_idx <- which(cand_scores == max(cand_scores))
        if (length(best_idx) > 1L) {
          lens <- nchar(candidates_f[best_idx])
          best_idx <- best_idx[which.min(lens)]
        }
        best_cand <- candidates_f[[best_idx[[1L]]]]
        if (!identical(assign_map[[f]], best_cand)) {
          assign_map[[f]] <- best_cand
          changed <- TRUE
        }
      }
      if (!changed) break
    }
    kept <- sub |>
      tidytable::filter(
        adduct_state_key == unname(assign_map[feature_id]) &
          adduct_dest_state_key == unname(assign_map[feature_id_dest])
      ) |>
      tidytable::arrange(
        feature_id,
        feature_id_dest,
        tidytable::desc(edge_score),
        edge_complexity
      ) |>
      tidytable::distinct(feature_id, feature_id_dest, .keep_all = TRUE)
    component_results[[ci]] <- kept
  }
  kept_all <- tidytable::bind_rows(component_results) |>
    tidytable::select(feature_id, adduct, adduct_dest, feature_id_dest) |>
    tidytable::distinct()

  scored_min <- scored |>
    tidytable::select(
      feature_id,
      adduct,
      adduct_dest,
      feature_id_dest,
      edge_score,
      edge_complexity
    ) |>
    tidytable::distinct()
  dropped <- scored_min |>
    tidytable::anti_join(
      kept_all,
      by = c("feature_id", "adduct", "adduct_dest", "feature_id_dest")
    )
  if (nrow(dropped) > 0L) {
    pair_best <- scored_min |>
      tidytable::arrange(
        feature_id,
        feature_id_dest,
        tidytable::desc(edge_score),
        edge_complexity
      ) |>
      tidytable::distinct(feature_id, feature_id_dest, .keep_all = TRUE) |>
      tidytable::select(
        feature_id,
        feature_id_dest,
        best_adduct = adduct,
        best_adduct_dest = adduct_dest
      )
    dropped <- dropped |>
      tidytable::left_join(
        pair_best,
        by = c("feature_id", "feature_id_dest")
      ) |>
      tidytable::mutate(
        drop_reason = tidytable::if_else(
          adduct == best_adduct & adduct_dest == best_adduct_dest,
          "node_state_conflict",
          "pair_dedup_or_conflict"
        )
      )
  }
  dropped_reason_counts <- if (nrow(dropped) > 0L) {
    dropped |>
      tidytable::count(drop_reason, name = "n") |>
      tidytable::arrange(tidytable::desc(n))
  } else {
    tidytable::tidytable(drop_reason = character(), n = integer())
  }
  attr(kept_all, "consistency_audit") <- list(
    n_input = nrow(scored_min),
    n_kept = nrow(kept_all),
    n_dropped = nrow(dropped),
    dropped_reason_counts = dropped_reason_counts
  )
  kept_all
}

#' Build canonical adduct-state keys from adduct notation
#' @keywords internal
build_adduct_state_key_map <- function(adducts) {
  adducts <- unique(as.character(adducts))
  adducts <- adducts[!is.na(adducts)]
  if (length(adducts) == 0L) {
    return(tidytable::tidytable(adduct = character(), state_key = character()))
  }
  state_key <- vapply(
    X = adducts,
    FUN = adduct_to_state_key,
    FUN.VALUE = character(1)
  )
  tidytable::tidytable(adduct = adducts, state_key = state_key)
}

#' Convert adduct notation to a canonical state key
#' @keywords internal
adduct_to_state_key <- function(adduct) {
  parsed <- tryCatch(
    parse_adduct(adduct),
    error = function(.err) {
      invisible(.err)
      NULL
    }
  )
  if (!is.null(parsed) && !is_parse_failed(parsed)) {
    return(sprintf(
      "z:%d|m:%d|i:%d|d:%.6f",
      parsed[["n_charges"]],
      parsed[["n_mer"]],
      parsed[["n_iso"]],
      parsed[["los_add_clu"]]
    ))
  }
  mod_mass <- calculate_net_mod_mass_from_text(adduct)
  if (is.finite(mod_mass)) {
    return(paste0("fallback-d:", sprintf("%.6f", mod_mass)))
  }
  paste0("raw:", adduct)
}

#' Estimate net modification mass directly from adduct text
#' @keywords internal
calculate_net_mod_mass_from_text <- function(adduct) {
  adduct <- as.character(adduct)
  if (is.na(adduct) || !nzchar(adduct)) {
    return(NA_real_)
  }
  inner <- sub("^\\[(.*)\\][0-9]*[+-]+$", "\\1", adduct, perl = TRUE)
  if (!nzchar(inner) || identical(inner, adduct)) {
    return(NA_real_)
  }
  mods <- sub("^[0-9]*M[0-9]*", "", inner, perl = TRUE)
  if (!nzchar(mods)) {
    return(0)
  }
  tokens <- regmatches(mods, gregexpr("[+-][^+-]+", mods, perl = TRUE))[[1L]]
  if (length(tokens) == 0L) {
    return(NA_real_)
  }
  total <- 0
  for (tok in tokens) {
    sign <- if (startsWith(tok, "+")) 1 else -1
    body <- substr(tok, 2L, nchar(tok))
    coef_match <- regmatches(body, regexpr("^[0-9]+", body, perl = TRUE))
    coef <- if (length(coef_match) == 0L || coef_match[[1L]] == "") {
      1
    } else {
      as.numeric(coef_match[[1L]])
    }
    formula <- sub("^[0-9]+", "", body, perl = TRUE)
    if (!nzchar(formula)) {
      return(NA_real_)
    }
    mass <- suppressWarnings(MetaboCoreUtils::calculateMass(formula))
    if (!is.finite(mass)) {
      return(NA_real_)
    }
    total <- total + (sign * coef * mass)
  }
  total
}

#' Join RT/mz couple windows with neutral losses
#' @keywords internal
join_couples_with_neutral_losses <- function(df_couples_diff, neutral_losses) {
  cd_src <- tidytable::as_tidytable(df_couples_diff)[,
    .(
      src_feature_id = feature_id,
      src_feature_id_dest = feature_id_dest,
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
      loss,
      mass = loss_mass_keep,
      feature_id_dest = src_feature_id_dest
    )
  ] |>
    unique() |>
    tidytable::as_tidytable()
}

#' Join multi-adduct candidates with add/loss inferred masses (kept for
#' backward compatibility with downstream callers and tests).
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
