#' Decorate annotation rows with structural / taxonomic metadata
#' @keywords internal
enrich_with_structure_metadata <- function(combined, structure_table) {
  if (nrow(combined) == 0L) {
    return(
      tidytable::tidytable(
        feature_id = character(),
        candidate_structure_error_mz = numeric(),
        candidate_structure_name = character(),
        candidate_structure_inchikey_connectivity_layer = character(),
        candidate_structure_smiles_no_stereo = character(),
        candidate_library = character(),
        candidate_adduct = character()
      )
    )
  }
  struct_cols <- c(
    "structure_name",
    "structure_inchikey_connectivity_layer",
    "structure_inchikey_no_stereo",
    "structure_smiles_no_stereo",
    "structure_molecular_formula",
    "structure_exact_mass",
    "structure_xlogp"
  )
  struct_unique <- structure_table |>
    tidytable::distinct(tidyselect::any_of(struct_cols)) |>
    tidytable::distinct(
      tidyselect::any_of(c(
        "structure_inchikey_connectivity_layer",
        "structure_molecular_formula",
        "structure_exact_mass"
      )),
      .keep_all = TRUE
    ) |>
    tidytable::mutate(tidytable::across(
      .cols = tidyselect::where(is.numeric),
      .fns = as.character
    ))

  enriched <- combined |>
    tidytable::mutate(
      structure_exact_mass = as.character(structure_exact_mass)
    )
  join_keys <- intersect(colnames(enriched), colnames(struct_unique))
  if (length(join_keys) > 0L) {
    enriched <- enriched |>
      tidytable::left_join(struct_unique, by = join_keys)
  }

  tax_cols <- c(
    "structure_inchikey_connectivity_layer",
    "structure_smiles_no_stereo",
    "structure_tax_npc_01pat",
    "structure_tax_npc_02sup",
    "structure_tax_npc_03cla",
    "structure_tax_cla_chemontid",
    "structure_tax_cla_01kin",
    "structure_tax_cla_02sup",
    "structure_tax_cla_03cla",
    "structure_tax_cla_04dirpar"
  )
  if (any(tax_cols %in% colnames(structure_table))) {
    taxonomy_table <- structure_table |>
      tidytable::distinct(tidyselect::any_of(tax_cols))
    tax_keys <- intersect(colnames(enriched), colnames(taxonomy_table))
    if (length(tax_keys) > 0L) {
      enriched <- enriched |>
        tidytable::left_join(taxonomy_table, by = tax_keys)
    }
  }

  # canonical_adduct <- vapply(
  #   X = enriched$adduct,
  #   FUN = function(a) {
  #     if (is.na(a)) NA_character_ else canonicalize_adduct_notation(a)
  #   },
  #   FUN.VALUE = character(1L),
  #   USE.NAMES = FALSE
  # )

  enriched |>
    tidytable::mutate(
      # candidate_adduct = canonical_adduct,
      candidate_library = "TIMA MS1",
      candidate_structure_error_mz = as.numeric(error_mz)
    ) |>
    tidytable::rename(
      candidate_structure_name = tidyselect::any_of("structure_name"),
      candidate_structure_inchikey_connectivity_layer = tidyselect::any_of(
        "structure_inchikey_connectivity_layer"
      ),
      candidate_structure_inchikey_no_stereo = tidyselect::any_of(
        "structure_inchikey_no_stereo"
      ),
      candidate_structure_smiles_no_stereo = tidyselect::any_of(
        "structure_smiles_no_stereo"
      ),
      candidate_structure_molecular_formula = tidyselect::any_of(
        "structure_molecular_formula"
      ),
      candidate_structure_xlogp = tidyselect::any_of("structure_xlogp"),
      candidate_structure_tax_npc_01pat = tidyselect::any_of(
        "structure_tax_npc_01pat"
      ),
      candidate_structure_tax_npc_02sup = tidyselect::any_of(
        "structure_tax_npc_02sup"
      ),
      candidate_structure_tax_npc_03cla = tidyselect::any_of(
        "structure_tax_npc_03cla"
      ),
      candidate_structure_tax_cla_chemontid = tidyselect::any_of(
        "structure_tax_cla_chemontid"
      ),
      candidate_structure_tax_cla_01kin = tidyselect::any_of(
        "structure_tax_cla_01kin"
      ),
      candidate_structure_tax_cla_02sup = tidyselect::any_of(
        "structure_tax_cla_02sup"
      ),
      candidate_structure_tax_cla_03cla = tidyselect::any_of(
        "structure_tax_cla_03cla"
      ),
      candidate_structure_tax_cla_04dirpar = tidyselect::any_of(
        "structure_tax_cla_04dirpar"
      )
    ) |>
    tidytable::distinct()
}

#' Extract per-element atom requirements implied by a loss formula token
#'
#' Parses loss tokens (e.g. `H2O`, `2C6H10O5`, `C6H10O5 (hexose)`) and returns
#' a named integer vector with required atom counts.
#' @keywords internal
loss_term_atom_requirements <- function(loss_term) {
  if (is.null(loss_term) || is.na(loss_term) || !nzchar(loss_term)) {
    return(integer())
  }
  req <- integer()
  loss_term <- gsub("\n", " ", as.character(loss_term))
  loss_term <- trimws(sub(" .*", "", loss_term))
  coef_match <- regmatches(
    loss_term,
    regexpr("^[0-9]+", loss_term, perl = TRUE)
  )
  coef <- if (length(coef_match) == 0L || !nzchar(coef_match)) {
    1L
  } else {
    as.integer(coef_match)
  }
  formula <- sub("^[0-9]+", "", loss_term, perl = TRUE)
  parsed <- tryCatch(
    parse_atomic_formula(formula),
    error = function(.err) {
      invisible(.err)
      NULL
    }
  )
  if (is.null(parsed) || length(parsed) == 0L) {
    return(integer())
  }
  for (el in names(parsed)) {
    req[el] <- (if (is.na(req[el])) 0L else req[el]) +
      as.integer(parsed[[el]] * coef)
  }
  req
}

#' Backwards-compatible alias for older tests / callers
#' @keywords internal
adduct_loss_atom_requirements <- function(adduct) {
  if (is.null(adduct) || is.na(adduct) || !nzchar(adduct)) {
    return(integer())
  }
  adduct_norm <- tryCatch(
    normalize_adduct_string(adduct),
    error = function(.err) {
      invisible(.err)
      as.character(adduct)
    }
  )
  inner <- sub("^\\[(.*)\\][0-9]*[+-]+$", "\\1", adduct_norm, perl = TRUE)
  if (!nzchar(inner) || identical(inner, adduct_norm)) {
    return(loss_term_atom_requirements(adduct_norm))
  }
  toks <- regmatches(
    inner,
    gregexpr("[+-][0-9]*[A-Za-z][A-Za-z0-9]*", inner, perl = TRUE)
  )[[1L]]
  toks <- toks[nzchar(toks)]
  if (length(toks) == 0L) {
    return(integer())
  }
  req <- integer()
  for (tok in toks) {
    if (startsWith(tok, "-")) {
      tok_req <- loss_term_atom_requirements(substring(tok, 2L))
      for (el in names(tok_req)) {
        req[el] <- (if (is.na(req[el])) 0L else req[el]) + tok_req[[el]]
      }
    }
  }
  req
}

#' Check whether formula contains all atoms required by adduct losses
#' @keywords internal
formula_satisfies_loss_requirements <- function(formula, loss_requirements) {
  if (length(loss_requirements) == 0L) {
    return(TRUE)
  }
  if (is.null(formula) || is.na(formula) || !nzchar(formula)) {
    return(NA)
  }
  parsed <- tryCatch(
    parse_atomic_formula(formula),
    error = function(.err) {
      invisible(.err)
      NULL
    }
  )
  if (is.null(parsed) || length(parsed) == 0L) {
    return(NA)
  }
  for (el in names(loss_requirements)) {
    if (!el %in% names(parsed) || parsed[[el]] < loss_requirements[[el]]) {
      return(FALSE)
    }
  }
  TRUE
}

#' Demote structural matches whose adduct-loss atoms are absent from formula
#'
#' Rows that fail this check keep the adduct hypothesis but lose structure-level
#' fields (treated as unmatched).
#' @keywords internal
enforce_loss_formula_compatibility <- function(annotations) {
  if (nrow(annotations) == 0L) {
    return(annotations)
  }
  required_cols <- c(
    "adduct",
    "candidate_structure_molecular_formula",
    "candidate_structure_error_mz"
  )
  if (!all(required_cols %in% colnames(annotations))) {
    return(annotations)
  }

  has_source <- "source" %in% colnames(annotations)
  has_loss_term <- "loss_term" %in% colnames(annotations)
  idx_check <- which(
    !is.na(annotations$candidate_structure_error_mz) &
      !is.na(annotations$adduct) &
      ((has_source & annotations$source == "loss") |
        (has_loss_term & !is.na(annotations$loss_term)))
  )
  if (length(idx_check) == 0L) {
    return(annotations)
  }

  keep_struct <- vapply(
    X = idx_check,
    FUN = function(i) {
      req <- if (has_loss_term && !is.na(annotations$loss_term[[i]])) {
        loss_term_atom_requirements(annotations$loss_term[[i]])
      } else {
        adduct_loss_atom_requirements(annotations$adduct[[i]])
      }
      fs <- formula_satisfies_loss_requirements(
        formula = annotations$candidate_structure_molecular_formula[[i]],
        loss_requirements = req
      )
      if (is.na(fs)) TRUE else fs
    },
    FUN.VALUE = logical(1L)
  )
  idx_drop <- idx_check[!keep_struct]
  if (length(idx_drop) == 0L) {
    return(annotations)
  }

  out <- tidytable::as_tidytable(annotations)
  struct_cols <- unique(c(
    grep("^candidate_structure_", colnames(out), value = TRUE),
    intersect(
      colnames(out),
      c("candidate_library", "structure_exact_mass", "error_mz")
    )
  ))
  for (cc in struct_cols) {
    out[idx_drop, (cc) := NA]
  }
  out <- out |> tidytable::distinct()

  dropped <- annotations[idx_drop, , drop = FALSE]
  dropped_bd <- dropped |>
    tidytable::distinct(feature_id, adduct) |>
    tidytable::count(adduct, name = "N") |>
    tidytable::arrange(tidytable::desc(N))
  log_info(
    paste0(
      "Loss/formula filter demoted %d structural match row(s) where the actual ",
      "loss-edge term was not contained in the candidate formula."
    ),
    length(idx_drop)
  )
  out
}

#' Keep only annotation adducts that agree with edge evidence
#'
#' Agreement is evaluated per feature. For pair-derived adducts,
#' `(feature_id, adduct)` must be present in the adduct-edge endpoint set.
#' Loss/cluster-derived hypotheses are considered edge-backed by construction
#' (their `source` originates from those edge tables). Explicit user-guided
#' hypotheses are also preserved (`baseline`, `preassigned`,
#' `preassigned_propagated`).
#' @keywords internal
enforce_annotation_edge_adduct_agreement <- function(
  annotations,
  adduct_edges,
  baseline_adduct = NULL
) {
  if (nrow(annotations) == 0L) {
    return(annotations)
  }
  required <- c("feature_id", "adduct")
  if (!all(required %in% colnames(annotations))) {
    return(annotations)
  }

  adduct_key_map <- build_adduct_state_key_map(unique(c(
    annotations$adduct,
    adduct_edges$adduct,
    adduct_edges$adduct_dest
  )))
  if (!"adduct" %in% colnames(adduct_key_map)) {
    adduct_key_map$adduct <- character(nrow(adduct_key_map))
  }
  if (!"state_key" %in% colnames(adduct_key_map)) {
    adduct_key_map$state_key <- adduct_key_map$adduct
  }
  adduct_key_map <- adduct_key_map |>
    tidytable::mutate(adduct_state_key = state_key) |>
    tidytable::select(adduct, adduct_state_key)
  key_lookup <- stats::setNames(
    object = adduct_key_map$adduct_state_key,
    nm = adduct_key_map$adduct
  )

  edge_supported <- if (nrow(adduct_edges) > 0L) {
    tidytable::bind_rows(
      adduct_edges |>
        tidytable::transmute(feature_id, adduct),
      adduct_edges |>
        tidytable::transmute(feature_id = feature_id_dest, adduct = adduct_dest)
    ) |>
      tidytable::mutate(adduct_state_key = unname(key_lookup[adduct])) |>
      tidytable::filter(!is.na(adduct_state_key)) |>
      tidytable::distinct(feature_id, adduct_state_key)
  } else {
    tidytable::tidytable(
      feature_id = character(),
      adduct_state_key = character()
    )
  }

  has_source <- "source" %in% colnames(annotations)
  has_is_preassigned <- "is_preassigned" %in% colnames(annotations)
  has_origin <- "candidate_adduct_origin" %in% colnames(annotations)
  preassigned_safe <- if (has_is_preassigned) {
    annotations$is_preassigned
  } else {
    rep(FALSE, nrow(annotations))
  }
  origin_safe <- if (has_origin) {
    annotations$candidate_adduct_origin %in% c("enforced", "preassigned")
  } else {
    rep(FALSE, nrow(annotations))
  }
  baseline_safe <- if (!is.null(baseline_adduct)) {
    annotations$adduct == baseline_adduct
  } else {
    rep(FALSE, nrow(annotations))
  }

  out <- annotations |>
    tidytable::mutate(adduct_state_key = unname(key_lookup[adduct])) |>
    tidytable::left_join(
      edge_supported |>
        tidytable::mutate(edge_supported = TRUE),
      by = c("feature_id", "adduct_state_key")
    ) |>
    tidytable::mutate(
      edge_supported = tidytable::if_else(
        is.na(edge_supported),
        FALSE,
        edge_supported
      )
    ) |>
    tidytable::group_by(feature_id) |>
    tidytable::mutate(feature_has_edge_support = any(edge_supported)) |>
    tidytable::ungroup() |>
    tidytable::mutate(
      keep = edge_supported |
        (!feature_has_edge_support & !(has_source & source == "multi")) |
        (has_source & source %in% c("loss", "cluster")) |
        preassigned_safe |
        origin_safe |
        baseline_safe |
        (has_source &
          source %in%
            c(
              "baseline",
              "preassigned",
              "preassigned_propagated"
            ))
    )

  dropped <- out |> tidytable::filter(!keep)
  if (nrow(dropped) > 0L) {
    if ("adduct" %in% colnames(dropped)) {
      dropped_n <- nrow(dropped |> tidytable::distinct(feature_id, adduct))
      dropped_bd <- dropped |>
        tidytable::distinct(feature_id, adduct) |>
        tidytable::count(adduct, name = "N") |>
        tidytable::arrange(tidytable::desc(N))
    } else {
      dropped_n <- nrow(dropped |> tidytable::distinct(feature_id))
      dropped_bd <- dropped |>
        tidytable::count(name = "N")
    }
    log_info(
      paste0(
        "Annotation/edge adduct agreement removed %d unsupported ",
        "(feature, adduct) assignment(s)."
      ),
      dropped_n
    )
  }

  out |>
    tidytable::filter(keep) |>
    tidytable::select(
      -tidyselect::any_of(c(
        "adduct_state_key",
        "edge_supported",
        "feature_has_edge_support",
        "keep"
      ))
    )
}

#' Build the output edges table from the three edge sets
#' @keywords internal
build_output_edges <- function(
  adduct_edges,
  loss_edges,
  cluster_edges,
  covariance_edges = NULL,
  features_table,
  name_source,
  name_target
) {
  parts <- list()
  if (nrow(adduct_edges) > 0L) {
    parts[[length(parts) + 1L]] <- adduct_edges |>
      tidytable::mutate(
        label = paste0(adduct, " _ ", adduct_dest),
        correlation = NA_real_,
        p_value = NA_real_
      ) |>
      tidytable::select(
        !!as.name(name_source) := feature_id,
        !!as.name(name_target) := feature_id_dest,
        label,
        correlation,
        p_value
      ) |>
      tidytable::distinct()
  }
  if (nrow(loss_edges) > 0L) {
    loss_edges <- loss_edges |>
      tidytable::mutate(
        .drop_redundant = vapply(
          seq_len(nrow(loss_edges)),
          function(i) {
            if (!all(c("adduct", "adduct_dest") %in% colnames(adduct_edges))) {
              return(FALSE)
            }
            same_pair <- adduct_edges |>
              tidytable::filter(
                feature_id == loss_edges$feature_id[[i]] &
                  feature_id_dest == loss_edges$feature_id_dest[[i]]
              )
            if (nrow(same_pair) == 0L) {
              return(FALSE)
            }
            loss_formula <- loss_edges$loss[[i]]
            any(vapply(
              seq_len(nrow(same_pair)),
              function(j) {
                adduct_has_explicit_loss(same_pair$adduct[[j]], loss_formula) &&
                  adduct_has_explicit_loss(
                    same_pair$adduct_dest[[j]],
                    loss_formula
                  )
              },
              logical(1L)
            ))
          },
          logical(1L)
        )
      ) |>
      tidytable::filter(!.drop_redundant) |>
      tidytable::select(-.drop_redundant)
  }
  if (nrow(loss_edges) > 0L) {
    parts[[length(parts) + 1L]] <- loss_edges |>
      tidytable::mutate(
        label = paste0(loss, " loss"),
        correlation = NA_real_,
        p_value = NA_real_
      ) |>
      tidytable::select(
        # NL convention: precursor (higher mz, feature_id_dest) -> product
        !!as.name(name_source) := feature_id_dest,
        !!as.name(name_target) := feature_id,
        label,
        correlation,
        p_value
      ) |>
      tidytable::distinct()
  }
  if (nrow(cluster_edges) > 0L) {
    parts[[length(parts) + 1L]] <- cluster_edges |>
      tidytable::mutate(
        label = paste0("+", cluster, " cluster"),
        correlation = NA_real_,
        p_value = NA_real_
      ) |>
      tidytable::select(
        # cluster: lower mz -> higher mz (which carries the cluster)
        !!as.name(name_source) := feature_id,
        !!as.name(name_target) := feature_id_dest,
        label,
        correlation,
        p_value
      ) |>
      tidytable::distinct()
  }

  if (!is.null(covariance_edges) && nrow(covariance_edges) > 0L) {
    parts[[length(parts) + 1L]] <- covariance_edges |>
      tidytable::mutate(label = "covariance") |>
      tidytable::select(
        !!as.name(name_source) := feature_id,
        !!as.name(name_target) := feature_id_dest,
        label,
        correlation,
        p_value
      ) |>
      tidytable::distinct()
  }

  edges <- if (length(parts) > 0L) {
    tidytable::bind_rows(parts)
  } else {
    tidytable::tidytable(
      !!as.name(name_source) := character(),
      !!as.name(name_target) := character(),
      label = character(),
      correlation = numeric(),
      p_value = numeric()
    )
  }

  # Self-loops for isolated features so downstream graph processing keeps them
  isolated <- features_table |>
    tidytable::filter(
      !feature_id %in% edges[[name_source]] &
        !feature_id %in% edges[[name_target]]
    ) |>
    tidytable::mutate(!!as.name(name_target) := feature_id) |>
    tidytable::rename(!!as.name(name_source) := feature_id) |>
    tidytable::distinct(!!as.name(name_source), !!as.name(name_target))

  tidytable::bind_rows(edges, isolated)
}

#' Log the consistency-audit attribute attached by
#' `enforce_graph_adduct_consistency()`.
#' @keywords internal
log_consistency_audit <- function(audit) {
  if (is.null(audit)) {
    return(invisible(NULL))
  }
  log_debug(
    "Adduct consistency audit: kept=%d, dropped=%d",
    audit$n_kept,
    audit$n_dropped
  )
  invisible(NULL)
}

# ============================================================
#                LOWER-LEVEL HELPERS (kept stable
