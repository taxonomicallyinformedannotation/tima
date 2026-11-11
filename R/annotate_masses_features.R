#' Coerce, harmonize, and prepare the features table for downstream steps
#' @keywords internal
prepare_features_table <- function(features_table, tolerance_rt) {
  if (!"adduct" %in% colnames(features_table)) {
    features_table$adduct <- NA_character_
  }
  if ("candidate_adduct" %in% colnames(features_table)) {
    features_table$adduct <- ifelse(
      is.na(features_table$adduct) | !nzchar(features_table$adduct),
      features_table$candidate_adduct,
      features_table$adduct
    )
  }
  features_table <- features_table |>
    harmonize_adducts(adducts_translations = adducts_translations)

  if (!"sample" %in% colnames(features_table)) {
    log_debug("No 'sample' column; using 'all'")
    features_table$sample <- "all"
  }
  if (!"rt" %in% colnames(features_table)) {
    log_warn("No 'rt' column; using sequential numbering as RT proxy")
    features_table$rt <- seq_len(nrow(features_table))
  }

  features_table |>
    tidytable::mutate(
      mz = as.numeric(mz),
      rt = as.numeric(rt)
    ) |>
    tidytable::distinct(feature_id, sample, .keep_all = TRUE) |>
    tidytable::mutate(
      rt_min = rt - tolerance_rt,
      rt_max = rt + tolerance_rt
    )
}

#' Extract any non-NA pre-assigned adducts as explicit hypotheses
#' @keywords internal
extract_preassigned_adducts <- function(features_table) {
  if (
    !"adduct" %in% colnames(features_table) &&
      !"candidate_adduct" %in% colnames(features_table)
  ) {
    return(tidytable::tidytable(feature_id = character(), adduct = character()))
  }
  if (!"adduct" %in% colnames(features_table)) {
    features_table$adduct <- NA_character_
  }
  if (!"candidate_adduct" %in% colnames(features_table)) {
    features_table$candidate_adduct <- NA_character_
  }
  pre_raw <- features_table |>
    tidytable::mutate(
      adduct = tidytable::if_else(
        is.na(adduct) | !nzchar(adduct),
        candidate_adduct,
        adduct
      )
    ) |>
    tidytable::distinct(feature_id, adduct)

  adduct_chr <- as.character(pre_raw$adduct)
  adduct_chr[is.na(adduct_chr)] <- ""
  parts <- strsplit(adduct_chr, "[|/]", perl = TRUE)
  lens <- lengths(parts)
  if (sum(lens) == 0L) {
    return(tidytable::tidytable(feature_id = character(), adduct = character()))
  }

  expanded <- tidytable::tidytable(
    feature_id = rep(pre_raw$feature_id, lens),
    adduct = trimws(unlist(parts, use.names = FALSE))
  )

  expanded |>
    tidytable::filter(!is.na(adduct) & nzchar(adduct)) |>
    tidytable::distinct(feature_id, adduct) |>
    harmonize_adducts(adducts_translations = adducts_translations)
}

#' Propagate pre-assigned adduct labels through adduct edges in both directions
#'
#' If an edge hypothesis says `(feature_id, adduct) -> (feature_id_dest,
#' adduct_dest)`, and one side is pre-assigned upstream, the opposite side gets
#' the corresponding propagated pre-assignment candidate.
#' @keywords internal
propagate_preassigned_over_adduct_edges <- function(adduct_edges, preassigned) {
  if (nrow(adduct_edges) == 0L || nrow(preassigned) == 0L) {
    return(tidytable::tidytable(feature_id = character(), adduct = character()))
  }
  pre <- preassigned |>
    tidytable::distinct(feature_id, adduct) |>
    tidytable::filter(!is.na(adduct) & nzchar(adduct))
  if (nrow(pre) == 0L) {
    return(tidytable::tidytable(feature_id = character(), adduct = character()))
  }

  forward <- adduct_edges |>
    tidytable::inner_join(pre, by = c("feature_id", "adduct")) |>
    tidytable::transmute(feature_id = feature_id_dest, adduct = adduct_dest)

  reverse <- adduct_edges |>
    tidytable::inner_join(
      pre,
      by = c("feature_id_dest" = "feature_id", "adduct_dest" = "adduct")
    ) |>
    tidytable::transmute(feature_id, adduct)

  tidytable::bind_rows(forward, reverse) |>
    tidytable::distinct() |>
    harmonize_adducts(adducts_translations = adducts_translations)
}

#' Load the structural library, join supplementary tables, and build the
#' ppm-window table for neutral-mass lookups
#' @keywords internal
load_structural_library <- function(
  library,
  str_stereo,
  str_met,
  str_nam = NULL,
  str_tax_cla,
  str_tax_npc,
  tolerance_ppm,
  tolerance_dalton = NULL
) {
  library_table <- safe_fread(
    file = library,
    file_type = "structure library",
    na.strings = c("", "NA"),
    colClasses = "character"
  )
  supp_files <- list(str_stereo, str_met)
  supp_names <- c("stereochemistry", "metadata")
  if (!is.null(str_nam)) {
    supp_files <- c(supp_files, list(str_nam))
    supp_names <- c(supp_names, "names")
  }
  supp_files <- c(supp_files, list(str_tax_cla, str_tax_npc))
  supp_names <- c(supp_names, "ClassyFire taxonomy", "NPClassifier taxonomy")
  supp_tables <- Map(
    function(x, y) {
      safe_fread(
        file = x,
        file_type = y,
        na.strings = c("", "NA"),
        colClasses = "character"
      )
    },
    supp_files,
    supp_names
  )
  joined <- Reduce(
    function(acc, st) {
      by_cols <- intersect(names(acc), names(st))
      if (
        length(by_cols) == 1L &&
          anyDuplicated(st[[by_cols]]) == 0L &&
          !anyNA(st[[by_cols]])
      ) {
        key_col <- by_cols[[1L]]
        m_idx <- fastmatch::fmatch(acc[[key_col]], st[[key_col]])
        new_cols <- setdiff(names(st), key_col)
        if (length(new_cols) > 0L) {
          data.table::set(
            acc,
            j = new_cols,
            value = lapply(as.list(st)[new_cols], `[`, m_idx)
          )
        }
        acc
      } else if (length(by_cols) > 0L) {
        tidytable::left_join(acc, st, by = by_cols)
      } else {
        acc
      }
    },
    supp_tables,
    init = tidytable::as_tidytable(library_table)
  )
  rm(library_table, supp_tables)

  structures <- joined[!is.na(joined$structure_exact_mass), ]
  rm(joined)
  em_col <- structures$structure_exact_mass
  if (!is.numeric(em_col)) {
    u_em_chr <- unique(em_col)
    u_em_num <- as.numeric(u_em_chr)
    em_col <- u_em_num[fastmatch::fmatch(em_col, u_em_chr)]
    data.table::set(structures, j = "structure_exact_mass", value = em_col)
  }
  if (!"structure_inchikey_connectivity_layer" %in% colnames(structures)) {
    u_ik <- unique(structures$structure_inchikey)
    u_ik14 <- stri_sub(str = u_ik, from = 1L, to = 14L)
    data.table::set(
      structures,
      j = "structure_inchikey_connectivity_layer",
      value = u_ik14[fastmatch::fmatch(structures$structure_inchikey, u_ik)]
    )
  }
  structures <- round_reals(structures)

  u_em <- unique(structures$structure_exact_mass)
  u_em <- u_em[!is.na(u_em) & u_em > 0]
  tol_da <- if (is.null(tolerance_dalton)) 0 else as.numeric(tolerance_dalton)
  mass_tol <- pmax(1E-6 * tolerance_ppm * u_em, tol_da)
  em_windows <- tidytable::tidytable(
    exact_mass = u_em,
    mass_tol = mass_tol,
    value_min = u_em - mass_tol,
    value_max = u_em + mass_tol
  )

  list(structures = structures, em_windows = em_windows)
}


#' Log the per-adduct annotation breakdown table
#'
#' Produces the classic "Breakdown of the annotated adduct species" table
#' (N_features / N_annotations / Pct) that was present in the original
#' monolithic \code{annotate_masses()} implementation.
#'
#' @include logs_utils.R
#' @include safe_fread.R
#' @include round_reals.R
#' @include harmonize_adducts.R
#' @param annotations Data frame returned by \code{enrich_with_structure_metadata()}.
#'   Expected to have at minimum \code{feature_id} and \code{adduct} columns.
#'   When a \code{source} column is present the "baseline / enforced" fallback
#'   rows are reported separately.
#' @keywords internal
log_adduct_breakdown <- function(annotations) {
  if (nrow(annotations) == 0L || !"adduct" %in% colnames(annotations)) {
    return(invisible(NULL))
  }

  # ---- library-matched adduct breakdown (main breakdown) ------------------
  # Only count rows that have an actual library hit (error_mz not NA).
  # Unmatched hypotheses retained for downstream use are excluded here to
  # avoid inflating counts with speculative cross-join candidates.
  has_error_col <- "candidate_structure_error_mz" %in% colnames(annotations)
  add_col <- annotations[["adduct"]]
  f_col <- annotations[["feature_id"]]
  keep_m <- if (has_error_col) {
    which(
      !is.na(add_col) & !is.na(annotations[["candidate_structure_error_mz"]])
    )
  } else {
    which(!is.na(add_col))
  }

  if (length(keep_m) > 0L) {
    a_m <- add_col[keep_m]
    f_m <- f_col[keep_m]
    u_a <- unique(a_m)
    u_f <- unique(f_m)
    a_id <- fastmatch::fmatch(a_m, u_a)
    f_id <- fastmatch::fmatch(f_m, u_f)
    n_a <- length(u_a)
    n_ann_v <- tabulate(a_id, nbins = n_a)
    fa_key <- as.double(f_id - 1L) * n_a + a_id
    n_feat_v <- tabulate(a_id[!duplicated(fa_key)], nbins = n_a)
    adduct_bd <- tidytable::tidytable(
      adduct = u_a,
      N_features = n_feat_v,
      N_annotations = n_ann_v
    ) |>
      tidytable::arrange(
        tidytable::desc(N_features),
        tidytable::desc(N_annotations)
      )
    adduct_bd <- add_percentage_column(
      adduct_bd,
      count_col = "N_features",
      out_col = "Pct_features"
    )
    adduct_bd <- add_percentage_column(
      adduct_bd,
      count_col = "N_annotations",
      out_col = "Pct_annotations"
    )
    log_info(
      "Breakdown of the (top 64) annotated adduct species (library-matched):"
    )
    log_info(
      "\n%s",
      paste(
        utils::capture.output(
          print.data.frame(
            x = adduct_bd |>
              tidytable::slice_head(n = 64L),
            row.names = FALSE
          )
        ),
        collapse = "\n"
      )
    )
  }

  # ---- retained-but-unmatched adduct hypotheses --------------------------
  # These are adducts kept in the output even though no library structure
  # matched; they still carry adduct-network evidence and are useful for
  # downstream tools.  Log a compact summary grouped by source.
  if (has_error_col && "source" %in% colnames(annotations)) {
    keep_u <- which(
      !is.na(add_col) & is.na(annotations[["candidate_structure_error_mz"]])
    )
    if (length(keep_u) > 0L) {
      unmatched_bd <- tidytable::tidytable(
        feature_id = f_col[keep_u],
        adduct = add_col[keep_u],
        source = annotations[["source"]][keep_u]
      ) |>
        tidytable::summarise(
          N_features = tidytable::n_distinct(feature_id),
          N_adduct_types = tidytable::n_distinct(adduct),
          .by = source
        ) |>
        tidytable::arrange(tidytable::desc(N_features))
      log_info(
        "Adduct hypotheses retained without library match (by source):"
      )
      log_info(
        "\n%s",
        paste(
          utils::capture.output(
            print.data.frame(x = unmatched_bd, row.names = FALSE)
          ),
          collapse = "\n"
        )
      )
    }
  }

  invisible(NULL)
}
