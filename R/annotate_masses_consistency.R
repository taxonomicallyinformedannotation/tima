#' Build feature pairs inside RT windows using sweep-line algorithm.
#'
#' For every pair `(src, dest)` of features in the same RT tolerance window
#' (and same sample), with `mz_dest >= mz_src`, returns `delta = mz_dest - mz_src`
#' together with `delta_min/delta_max` widened by the ppm tolerance.
#'
#' Uses a sweep-line algorithm to avoid the O(N^2) cartesian join.
#' Time complexity: O(N log N + K) where K is the number of output pairs.
#' @keywords internal
build_feature_pairs_within_rt <- function(
  df_rt_tol,
  df_fea_min,
  tolerance_ppm
) {
  if (nrow(df_rt_tol) == 0L || nrow(df_fea_min) == 0L) {
    return(tidytable::tidytable(
      feature_id = character(),
      rt = numeric(),
      mz = numeric(),
      feature_id_dest = character(),
      mz_dest = numeric(),
      delta = numeric(),
      delta_min = numeric(),
      delta_max = numeric()
    ))
  }

  # Extract source features with RT windows
  src <- tidytable::as_tidytable(df_rt_tol)[,
    .(
      feature_id,
      rt,
      mz,
      sample,
      rt_min,
      rt_max
    )
  ]

  # Extract destination features
  dest <- tidytable::as_tidytable(df_fea_min)[,
    .(
      feature_id_dest = feature_id,
      rt_dest = rt,
      mz_dest = mz,
      sample
    )
  ]

  # Sweep-line: sort by sample, then rt_dest
  dest <- dest[order(sample, rt_dest)]
  src <- src[order(sample, rt_min)]

  # For each sample, use data.table's rolling join with pre-filtering
  # This replaces the cartesian join with allow.cartesian = TRUE
  matches <- dest[
    src,
    on = .(sample, rt_dest >= rt_min, rt_dest <= rt_max),
    nomatch = 0L,
    allow.cartesian = FALSE
  ][
    feature_id != feature_id_dest & mz_dest >= mz
  ][,
    .(feature_id, rt, mz, feature_id_dest, mz_dest)
  ]

  if (nrow(matches) == 0L) {
    return(tidytable::tidytable(
      feature_id = character(),
      rt = numeric(),
      mz = numeric(),
      feature_id_dest = character(),
      mz_dest = numeric(),
      delta = numeric(),
      delta_min = numeric(),
      delta_max = numeric()
    ))
  }

  matches[,
    `:=`(
      delta = mz_dest - mz,
      tolerance_term = (tolerance_ppm * 1e-6 * (mz + mz_dest) / 2)
    )
  ][,
    `:=`(
      delta_min = delta - tolerance_term,
      delta_max = delta + tolerance_term,
      tolerance_term = NULL
    )
  ] |>
    tidytable::as_tidytable()
}

#' Build all ordered adduct-pair differences for single-charge adducts.
#' @keywords internal
build_adduct_pair_differences <- function(
  add_clu_table,
  tolerance_ppm,
  max_mz
) {
  if (nrow(add_clu_table) == 0L) {
    return(tidytable::tidytable(
      Distance = numeric(),
      Group1 = character(),
      Group2 = character()
    ))
  }

  min_distance <- tolerance_ppm * 1e-6 * max_mz

  add_src <- add_clu_table |>
    tidytable::distinct(adduct, adduct_mass) |>
    tidytable::rename(Group1 = adduct, mass1 = adduct_mass)

  add_dest <- add_clu_table |>
    tidytable::distinct(adduct, adduct_mass) |>
    tidytable::rename(Group2 = adduct, mass2 = adduct_mass)

  # Direct cross join
  add_src |>
    tidytable::cross_join(add_dest) |>
    tidytable::mutate(Distance = mass2 - mass1) |>
    tidytable::filter(
      Group1 != Group2 &
        mass1 < mass2 &
        Distance >= min_distance
    ) |>
    tidytable::distinct(Group1, Group2, Distance) |>
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

  # Combine both directions efficiently
  src_direction <- df_add |>
    tidytable::transmute(
      feature_id,
      adduct,
      neighbor = feature_id_dest
    )

  dest_direction <- df_add |>
    tidytable::transmute(
      feature_id = feature_id_dest,
      adduct = adduct_dest,
      neighbor = feature_id
    )

  # Single grouped count operation
  tidytable::bind_rows(src_direction, dest_direction) |>
    tidytable::distinct(feature_id, adduct, neighbor) |>
    tidytable::count(feature_id, adduct, name = "adduct_support") |>
    tidytable::mutate(adduct_support = as.integer(adduct_support))
}

#' Apply optional support-based adduct consistency filtering.
#' @keywords internal
apply_adduct_consistency_filter <- function(
  df_add
) {
  # Fixed consistency policy for legacy helper callers.
  mode <- "conditional"
  support_minimum <- 2L
  degree_minimum <- 3L

  if (nrow(df_add) == 0L) {
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
  support_src <- support |>
    tidytable::rename(src_support = adduct_support)
  support_dest <- support |>
    tidytable::rename(
      feature_id_dest = feature_id,
      adduct_dest = adduct,
      dest_support = adduct_support
    )
  degree_src <- feature_degree |>
    tidytable::rename(src_degree = feature_degree)
  degree_dest <- feature_degree |>
    tidytable::rename(
      feature_id_dest = feature_id,
      dest_degree = feature_degree
    )
  scored <- df_add |>
    tidytable::left_join(
      pair_hyp,
      by = c("feature_id", "feature_id_dest")
    ) |>
    tidytable::left_join(
      support_src,
      by = c("feature_id", "adduct")
    ) |>
    tidytable::left_join(
      support_dest,
      by = c("feature_id_dest", "adduct_dest")
    ) |>
    tidytable::left_join(
      degree_src,
      by = "feature_id"
    ) |>
    tidytable::left_join(
      degree_dest,
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
  if (mode == "strict") {
    return(
      scored |>
        tidytable::filter(src_support >= support_minimum) |>
        tidytable::filter(dest_support >= support_minimum) |>
        tidytable::select(feature_id, adduct, adduct_dest, feature_id_dest) |>
        tidytable::distinct()
    )
  }
  scored |>
    tidytable::mutate(
      need_check = pair_hypotheses > 1L |
        src_degree >= degree_minimum |
        dest_degree >= degree_minimum
    ) |>
    tidytable::filter(
      !need_check |
        src_support >= support_minimum |
        dest_support >= support_minimum
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
  # Prepare lookup tables to consolidate joins
  support_src <- support |> tidytable::rename(src_support = adduct_support)
  support_dest <- support |>
    tidytable::rename(
      feature_id_dest = feature_id,
      adduct_dest = adduct,
      dest_support = adduct_support
    )
  state_adduct <- state_map |> tidytable::rename(adduct_state_key = state_key)
  state_dest <- state_map |>
    tidytable::rename(
      adduct_dest = adduct,
      adduct_dest_state_key = state_key
    )

  # Consolidate all joins and subsequent mutate into single pipeline
  scored <- df_add |>
    tidytable::left_join(support_src, by = c("feature_id", "adduct")) |>
    tidytable::left_join(
      support_dest,
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
    tidytable::left_join(state_adduct, by = "adduct") |>
    tidytable::left_join(state_dest, by = "adduct_dest") |>
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
  queue_capacity <- length(nodes)
  for (node in nodes) {
    if (isTRUE(visited[[node]])) {
      next
    }
    queue <- character(queue_capacity)
    queue_head <- 1L
    queue_tail <- 1L
    queue[[queue_tail]] <- node
    queue_tail <- queue_tail + 1L
    visited[[node]] <- TRUE
    comp_nodes <- character(queue_capacity)
    comp_count <- 0L
    while (queue_head < queue_tail) {
      current <- queue[[queue_head]]
      queue_head <- queue_head + 1L
      comp_count <- comp_count + 1L
      comp_nodes[[comp_count]] <- current
      next_nodes <- neighbors[[current]]
      if (is.null(next_nodes)) {
        next
      }
      for (nxt in next_nodes) {
        if (!isTRUE(visited[[nxt]])) {
          visited[[nxt]] <- TRUE
          queue[[queue_tail]] <- nxt
          queue_tail <- queue_tail + 1L
        }
      }
    }
    components[[length(components) + 1L]] <- unique(comp_nodes[seq_len(
      comp_count
    )])
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

    node_data <- tidytable::bind_rows(
      sub |>
        tidytable::transmute(
          feature = feature_id,
          state_key = adduct_state_key,
          edge_score = edge_score
        ),
      sub |>
        tidytable::transmute(
          feature = feature_id_dest,
          state_key = adduct_dest_state_key,
          edge_score = edge_score
        )
    )
    node_candidates <- node_data |>
      tidytable::distinct()

    node_priors <- node_data |>
      tidytable::summarize(prior = sum(edge_score), .by = c(feature, state_key))

    # Initial assignment
    assignments <- node_priors |>
      tidytable::arrange(feature, tidytable::desc(prior), nchar(state_key)) |>
      tidytable::distinct(feature, .keep_all = TRUE) |>
      tidytable::select(feature, state_key)
    assign_map <- stats::setNames(assignments$state_key, assignments$feature)

    # Early termination detection & reduced iterations
    max_iter <- 20L
    prev_map <- NULL
    unchanged_count <- 0L
    sub_df <- as.data.frame(sub, stringsAsFactors = FALSE, check.names = FALSE)
    src_idx <- split(seq_len(nrow(sub_df)), sub_df$feature_id)
    dest_idx <- split(seq_len(nrow(sub_df)), sub_df$feature_id_dest)
    candidate_lookup <- split(
      node_candidates$state_key,
      node_candidates$feature
    )
    prior_lookup <- stats::setNames(
      node_priors$prior,
      paste(node_priors$feature, node_priors$state_key, sep = "\u001f")
    )

    for (iter in seq_len(max_iter)) {
      # Store previous to detect convergence
      if (identical(prev_map, assign_map)) {
        unchanged_count <- unchanged_count + 1L
        if (unchanged_count >= 2L) break # Early exit if no changes for 2 iterations
      } else {
        unchanged_count <- 0L
      }
      prev_map <- assign_map

      changed <- FALSE
      for (f in names(assign_map)) {
        candidates_f <- candidate_lookup[[f]]
        if (length(candidates_f) <= 1L) {
          next
        }

        cand_scores <- rep(0, length(candidates_f))
        src_rows <- sub_df[src_idx[[f]], , drop = FALSE]
        dest_rows <- sub_df[dest_idx[[f]], , drop = FALSE]

        if (nrow(src_rows) > 0L) {
          assigned_dest_state <- unname(assign_map[as.character(
            src_rows$feature_id_dest
          )])
          for (k in seq_along(candidates_f)) {
            cand <- candidates_f[[k]]
            src_mask <- src_rows$adduct_state_key == cand &
              src_rows$adduct_dest_state_key == assigned_dest_state
            cand_scores[[k]] <- cand_scores[[k]] +
              sum(src_rows$edge_score[src_mask], na.rm = TRUE)
          }
        }

        if (nrow(dest_rows) > 0L) {
          assigned_src_state <- unname(assign_map[as.character(
            dest_rows$feature_id
          )])
          for (k in seq_along(candidates_f)) {
            cand <- candidates_f[[k]]
            dest_mask <- dest_rows$adduct_dest_state_key == cand &
              dest_rows$adduct_state_key == assigned_src_state
            cand_scores[[k]] <- cand_scores[[k]] +
              sum(dest_rows$edge_score[dest_mask], na.rm = TRUE)
          }
        }

        for (k in seq_along(candidates_f)) {
          cand <- candidates_f[[k]]
          prior_val <- prior_lookup[[paste(f, cand, sep = "\u001f")]]
          if (!is.null(prior_val) && length(prior_val) > 0L) {
            cand_scores[[k]] <- cand_scores[[k]] + (0.01 * prior_val[[1L]])
          }
        }

        # Select best candidate
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

    # Final filtering with assignment map
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

#' Join RT/mz couple windows with neutral losses using binary search instead of cartesian join.
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
  # Sort neutral losses by mass for binary search
  nl_win <- nl_win[order(loss_mass)]
  loss_masses <- nl_win$loss_mass

  # For each couple, find matching losses via binary search
  result_list <- vector("list", nrow(cd_src))
  filled <- 0L

  for (i in seq_len(nrow(cd_src))) {
    dmin <- cd_src$delta_min[[i]]
    dmax <- cd_src$delta_max[[i]]

    lo_idx <- findInterval(dmin, loss_masses, left.open = TRUE) + 1L
    hi_idx <- findInterval(dmax, loss_masses, left.open = FALSE)
    lo_idx <- max(1L, lo_idx)
    hi_idx <- min(nrow(nl_win), hi_idx)

    if (lo_idx > hi_idx) {
      next
    }

    filled <- filled + 1L
    n_match <- hi_idx - lo_idx + 1L
    result_list[[filled]] <- tidytable::tidytable(
      feature_id = rep(cd_src$src_feature_id[[i]], n_match),
      loss = nl_win$loss[lo_idx:hi_idx],
      mass = nl_win$loss_mass_keep[lo_idx:hi_idx],
      feature_id_dest = rep(cd_src$src_feature_id_dest[[i]], n_match)
    )
  }

  if (filled == 0L) {
    return(tidytable::tidytable(
      feature_id = character(),
      loss = character(),
      mass = numeric(),
      feature_id_dest = character()
    ))
  }

  tidytable::bind_rows(result_list[seq_len(filled)]) |>
    unique() |>
    tidytable::as_tidytable()
}

#' Join multi-adduct candidates with add/loss inferred masses using binary search instead of cartesian join.
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

  # Sort by rt for binary search
  addloss_win <- addloss_win[order(rt_obs)]
  rt_obs <- addloss_win$rt_obs

  result_list <- vector("list", nrow(multi_src))
  filled <- 0L

  for (i in seq_len(nrow(multi_src))) {
    rt_min_i <- multi_src$rt_min[[i]]
    rt_max_i <- multi_src$rt_max[[i]]
    mass_min_i <- multi_src$mass_min[[i]]
    mass_max_i <- multi_src$mass_max[[i]]

    lo_idx <- findInterval(rt_min_i, rt_obs, left.open = TRUE) + 1L
    hi_idx <- findInterval(rt_max_i, rt_obs, left.open = FALSE)
    lo_idx <- max(1L, lo_idx)
    hi_idx <- min(nrow(addloss_win), hi_idx)

    if (lo_idx > hi_idx) {
      next
    }

    # Filter by mass range
    cand <- addloss_win[lo_idx:hi_idx]
    ok <- which(cand$mass_obs >= mass_min_i & cand$mass_obs <= mass_max_i)
    if (length(ok) == 0L) {
      next
    }

    filled <- filled + 1L
    n_match <- length(ok)
    result_list[[filled]] <- tidytable::tidytable(
      feature_id = rep(multi_src$feature_id[[i]], n_match),
      rt = rep(multi_src$rt[[i]], n_match),
      mz = rep(multi_src$mz[[i]], n_match),
      adduct = rep(multi_src$adduct[[i]], n_match),
      mass = cand$mass_obs_keep[ok]
    )
  }

  if (filled == 0L) {
    return(tidytable::tidytable(
      feature_id = character(),
      rt = numeric(),
      mz = numeric(),
      adduct = character(),
      mass = numeric()
    ))
  }

  tidytable::bind_rows(result_list[seq_len(filled)]) |>
    tidytable::as_tidytable()
}
