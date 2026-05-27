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
  str_tax_cla,
  str_tax_npc,
  tolerance_ppm
) {
  library_table <- safe_fread(
    file = library,
    file_type = "structure library",
    na.strings = c("", "NA"),
    colClasses = "character"
  )
  supp_files <- list(str_stereo, str_met, str_tax_cla, str_tax_npc)
  supp_names <- c(
    "stereochemistry",
    "metadata",
    "ClassyFire taxonomy",
    "NPClassifier taxonomy"
  )
  supp_tables <- purrr::map2(
    .x = supp_files,
    .y = supp_names,
    .f = ~ safe_fread(
      file = .x,
      file_type = .y,
      na.strings = c("", "NA"),
      colClasses = "character"
    )
  )
  joined <- purrr::reduce(
    .x = c(list(library_table), supp_tables),
    .f = tidytable::left_join
  )

  structures <- joined |>
    tidytable::filter(!is.na(structure_exact_mass)) |>
    tidytable::mutate(
      structure_exact_mass = as.numeric(structure_exact_mass)
    )
  if (!"structure_inchikey_connectivity_layer" %in% colnames(structures)) {
    structures <- structures |>
      tidytable::mutate(
        structure_inchikey_connectivity_layer = stringi::stri_sub(
          str = structure_inchikey,
          from = 1L,
          to = 14L
        )
      )
  }
  structures <- round_reals(structures)

  em_windows <- structures |>
    tidytable::distinct(exact_mass = structure_exact_mass) |>
    tidytable::filter(!is.na(exact_mass) & exact_mass > 0) |>
    tidytable::mutate(
      value_min = exact_mass - (1E-6 * tolerance_ppm * exact_mass),
      value_max = exact_mass + (1E-6 * tolerance_ppm * exact_mass)
    )

  list(structures = structures, em_windows = em_windows)
}

#' Log the top observed pair-delta bins for QC
#' @keywords internal
log_top_pair_deltas <- function(pairs) {
  if (nrow(pairs) == 0L) {
    return(invisible(NULL))
  }
  bins <- pairs[, .N, by = .(bin = cut(delta, breaks = 10000L))] |>
    tidytable::arrange(tidytable::desc(N)) |>
    tidytable::slice_head(n = 10L)
  bins <- add_percentage_column(bins, count_col = "N", out_col = "Pct")
  log_info(
    "Here are the top 10 observed m/z differences inside the RT windows:"
  )
  log_info(
    "\n%s",
    paste(
      utils::capture.output(print.data.frame(bins, row.names = FALSE)),
      collapse = "\n"
    )
  )
  invisible(NULL)
}

#' Log the per-adduct annotation breakdown table
#'
#' Produces the classic "Breakdown of the annotated adduct species" table
#' (N_features / N_annotations / Pct) that was present in the original
#' monolithic \code{annotate_masses()} implementation.
#'
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
  matched_ann <- if (has_error_col) {
    annotations |> tidytable::filter(!is.na(candidate_structure_error_mz))
  } else {
    annotations |> tidytable::filter(!is.na(adduct))
  }

  if (nrow(matched_ann) > 0L) {
    adduct_bd <- matched_ann |>
      tidytable::filter(!is.na(adduct)) |>
      tidytable::summarise(
        N_features = tidytable::n_distinct(feature_id),
        N_annotations = .N,
        .by = adduct
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
    log_info("Breakdown of the annotated adduct species (library-matched):")
    log_info(
      "\n%s",
      paste(
        utils::capture.output(
          print.data.frame(x = adduct_bd, row.names = FALSE)
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
    unmatched_ann <- annotations |>
      tidytable::filter(
        !is.na(adduct) & is.na(candidate_structure_error_mz)
      )
    if (nrow(unmatched_ann) > 0L) {
      unmatched_bd <- unmatched_ann |>
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
