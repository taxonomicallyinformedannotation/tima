require(package = dplyr, quietly = TRUE)
require(package = splitstackshape, quietly = TRUE)

#' Title
#'
#' @param dataframe
#' @param pathway
#'
#' @return
#' @export
#'
#' @examples
prepare_hierarchy_2 <- function(dataframe, pathway) {
  ms1_best_candidate <- dataframe |>
    dplyr::mutate_all(list(~ gsub(
      pattern = "\\|.*",
      replacement = "",
      x = .
    ))) |>
    splitstackshape::cSplit("best_candidate", sep = "§") |>
    dplyr::distinct(
      feature_id,
      smiles_2D,
      inchikey_2D,
      score_biological,
      score_chemical,
      score_final,
      # best_candidate_organism,
      # consensus_1 = consensus_pat,
      # consensus_2 = consensus_sup,
      # consensus_3 = consensus_cla,
      # consistency_1 = consistency_pat,
      # consistency_2 = consistency_sup,
      # consistency_3 = consistency_cla,
      best_candidate_1,
      best_candidate_2,
      best_candidate_3
    ) |>
    dplyr::mutate_all(list(~ y_as_na(x = ., y = ""))) |>
    dplyr::mutate(
      best_candidate_1 = if_else(
        condition = is.na(smiles_2D),
        true = "notAnnotated",
        false = best_candidate_1
      ),
      best_candidate_2 = if_else(
        condition = is.na(smiles_2D),
        true = paste(best_candidate_1, "notAnnotated"),
        false = best_candidate_2
      ),
      best_candidate_3 = if_else(
        condition = is.na(smiles_2D),
        true = paste(best_candidate_2, "notAnnotated"),
        false = best_candidate_3
      ),
      best_candidate_1 = if_else(
        condition = !is.na(smiles_2D) &
          is.na(best_candidate_1),
        true = "notClassified",
        false = best_candidate_1
      ),
      best_candidate_2 = if_else(
        condition = !is.na(smiles_2D) &
          is.na(best_candidate_2),
        true = paste(best_candidate_1, "notClassified"),
        false = best_candidate_2
      ),
      best_candidate_3 = if_else(
        condition = !is.na(smiles_2D) &
          is.na(best_candidate_3),
        true = paste(best_candidate_2, "notClassified"),
        false = best_candidate_3
      )
    )

  ms1_multiple <- ms1_best_candidate |>
    dplyr::left_join(top_m) |>
    ## add this step
    dplyr::filter(best_candidate_1 == pathway) |>
    dplyr::filter(!is.na(species)) |>
    dplyr::filter(intensity != 0)

  parents <- ms1_multiple |>
    dplyr::distinct(best_candidate_1, best_candidate_2) |>
    dplyr::mutate(ids = paste(best_candidate_1, best_candidate_2, sep = "-")) |>
    dplyr::distinct(labels = best_candidate_2, ids) |>
    dplyr::mutate(parents = gsub(
      pattern = "-.*",
      replacement = "",
      x = ids
    )) |>
    dplyr::mutate(
      labels = gsub(
        pattern = "notAnnotated notAnnotated",
        replacement = "notAnnotated",
        x = labels,
        fixed = TRUE
      ),
      ids = gsub(
        pattern = "notAnnotated-notAnnotated notAnnotated",
        replacement = "Other-notAnnotated",
        x = ids,
        fixed = TRUE
      )
    ) |>
    dplyr::mutate(
      labels = gsub(
        pattern = "notClassified notClassified$",
        replacement = "notClassified",
        x = labels,
        fixed = TRUE
      ),
      ids = gsub(
        pattern = "notClassified-notClassified notClassified",
        replacement = "Other-notClassified",
        x = ids,
        fixed = TRUE
      )
    ) |>
    dplyr::mutate(parents = gsub(
      pattern = "^notAnnotated$",
      replacement = "Other",
      x = parents
    )) |>
    dplyr::mutate(parents = gsub(
      pattern = "^notClassified$",
      replacement = "Other",
      x = parents
    )) |>
    dplyr::mutate(
      parents = gsub(
        pattern = pathway,
        replacement = "",
        x = parents
      ),
      ids = gsub(
        pattern = paste0(pathway, "-"),
        replacement = "",
        x = ids
      )
    ) |>
    dplyr::distinct()

  children_1 <- ms1_multiple |>
    dplyr::distinct(best_candidate_2, best_candidate_3) |>
    dplyr::mutate(ids = paste(best_candidate_2, best_candidate_3, sep = "-")) |>
    dplyr::distinct(labels = best_candidate_3, ids, join = best_candidate_2) |>
    dplyr::mutate(
      labels = gsub(
        pattern = "notAnnotated notAnnotated notAnnotated",
        replacement = "notAnnotated notAnnotated",
        x = labels,
        fixed = TRUE
      ),
      ids = gsub(
        pattern = "notAnnotated notAnnotated-notAnnotated notAnnotated notAnnotated",
        replacement = "notAnnotated-notAnnotated notAnnotated",
        x = ids,
        fixed = TRUE
      ),
      join = gsub(
        pattern = "notAnnotated notAnnotated",
        replacement = "notAnnotated",
        x = join,
        fixed = TRUE
      )
    ) |>
    dplyr::mutate(
      labels = gsub(
        pattern = "notClassified notClassified notClassified",
        replacement = "notClassified notClassified",
        x = labels,
        fixed = TRUE
      ),
      ids = gsub(
        pattern = "notClassified notClassified-notClassified notClassified notClassified",
        replacement = "notClassified-notClassified notClassified",
        x = ids,
        fixed = TRUE
      ),
      join = gsub(
        pattern = "notClassified notClassified",
        replacement = "notClassified",
        x = join,
        fixed = TRUE
      )
    ) |>
    dplyr::mutate(
      join = gsub(
        pattern = "([a-zA-Z]|\\))(-)([a-zA-Z]|[0-9])(.*)",
        replacement = "\\1",
        x = join
      )
    ) |>
    dplyr::full_join(parents, by = c("join" = "labels")) |>
    dplyr::distinct(ids = ids.x, labels, parents = ids.y) |>
    dplyr::filter(!is.na(parents))

  genealogy <- rbind(parents, children_1) |>
    dplyr::ungroup() |>
    dplyr::select(parents, ids, labels) |>
    dplyr::distinct()

  table <- ms1_multiple |>
    dplyr::mutate(
      best_candidate_3 = gsub(
        pattern = "notAnnotated notAnnotated notAnnotated",
        replacement = "notAnnotated notAnnotated",
        x = best_candidate_3
      )
    ) |>
    dplyr::mutate(
      best_candidate_3 = gsub(
        pattern = "notClassified notClassified notClassified",
        replacement = "notClassified notClassified",
        x = best_candidate_3
      )
    ) |>
    dplyr::full_join(genealogy, by = c("best_candidate_3" = "labels")) |>
    dplyr::distinct(
      feature_id,
      smiles_2D,
      inchikey_2D,
      score_biological,
      score_chemical,
      score_final,
      # best_candidate_organism,
      labels = best_candidate_3,
      ids,
      parents,
      sample,
      intensity,
      species
    )

  table_1 <- table |>
    dplyr::group_by(labels, sample) |>
    dplyr::add_count(name = "values") |>
    dplyr::ungroup() |>
    dplyr::distinct(ids, labels, parents, values, sample, intensity, species) |>
    dplyr::select(parents, ids, labels, values, sample, intensity, species)

  table_1_1 <- table_1 |>
    dplyr::group_by(parents, sample) |>
    dplyr::add_count(name = "sum") |>
    dplyr::ungroup()

  top_parents_table <- table_1_1 |>
    dplyr::group_by(parents) |>
    dplyr::mutate(values_3 = sum(sum)) |>
    dplyr::filter(parents != "") |>
    dplyr::distinct(parents, values_3) |>
    dplyr::ungroup() |>
    dplyr::top_n(n = 8) |>
    dplyr::select(-values_3) |>
    dplyr::mutate(ids = parents, labels = parents) |>
    dplyr::mutate(parents = "", new_labels = labels)

  top_parents <- top_parents_table$labels

  table_3 <-
    dplyr::left_join(table_1_1, children_1, by = c("parents" = "parents")) |>
    dplyr::group_by(labels.x) |>
    dplyr::mutate(n = sum(sum)) |>
    dplyr::mutate(n = if_else(
      condition = parents == "Other",
      true = n + 666,
      false = n + 0
    )) |>
    dplyr::select(parents, ids = ids.x, labels = labels.x, sum, n) |>
    dplyr::ungroup() |>
    dplyr::distinct()

  table_3_1 <- table_3 |>
    dplyr::distinct(parents, n, .keep_all = TRUE) |>
    dplyr::slice_max(n, n = 4, with_ties = FALSE) |>
    dplyr::select(
      parents,
      ids,
      labels
    )

  top_medium_table <- table_3 |>
    dplyr::filter(parents %in% top_parents) |>
    dplyr::group_by(parents) |>
    dplyr::distinct(n, .keep_all = TRUE) |>
    dplyr::slice_max(n, n = 4, with_ties = FALSE) |>
    dplyr::select(
      parents,
      ids,
      labels
    ) |>
    dplyr::mutate(new_labels = labels)

  top_medium <- top_medium_table$ids

  low_medium_table <-
    dplyr::anti_join(table_3, dplyr::bind_rows(table_3_1, top_medium_table)) |>
    dplyr::select(
      parents,
      ids,
      labels
    ) |>
    dplyr::filter(parents != "") |>
    dplyr::filter(!is.na(parents)) |>
    dplyr::distinct()

  low_medium_table_1_a <- low_medium_table |>
    dplyr::filter(parents %in% top_parents) |>
    dplyr::mutate(new_labels = paste(parents, "Other")) |>
    dplyr::mutate(ids = paste(parents, new_labels, sep = "-")) |>
    dplyr::mutate(labels = new_labels) |>
    dplyr::distinct()

  low_medium_table_1_b <- low_medium_table |>
    dplyr::filter(parents %in% top_parents) |>
    dplyr::mutate(new_labels = paste(parents, "Other")) |>
    dplyr::mutate(
      parents = paste(parents, new_labels, sep = "-"),
      ids = paste(new_labels, labels, sep = "-")
    ) |>
    dplyr::mutate(new_labels = labels) |>
    dplyr::distinct()

  low_medium_table_1 <-
    dplyr::bind_rows(low_medium_table_1_a, low_medium_table_1_b) |>
    dplyr::distinct()

  low_medium_table_2 <- low_medium_table |>
    dplyr::filter(!parents %in% top_parents) |>
    dplyr::mutate(new_labels = paste("Other", parents)) |>
    dplyr::mutate(parents = "Other") |>
    dplyr::mutate(ids = paste(parents, new_labels, sep = "-")) |>
    dplyr::distinct()

  genealogy_new_med <-
    dplyr::bind_rows(
      top_parents_table,
      top_medium_table,
      # low_medium_table_2,
      low_medium_table_1
    ) |>
    dplyr::distinct()

  table_next <- genealogy |>
    dplyr::filter(!labels %in% genealogy_new_med$labels) |>
    dplyr::filter(parents %in% top_medium_table$ids) |>
    dplyr::filter(parents != "") |>
    dplyr::mutate(new_labels = paste(
      gsub(
        pattern = "^([^-]*)-(.*)",
        replacement = "",
        x = parents
      ),
      labels
    ) |>
      trimws()) |>
    dplyr::mutate(ids = paste(
      gsub(
        pattern = "^([^-]*)-(.*)",
        replacement = "\\2",
        x = parents
      ),
      new_labels,
      sep = "-"
    )) |>
    dplyr::mutate(new_labels = if_else(
      condition = grepl(pattern = "^Other", x = parents),
      true = labels,
      false = new_labels
    ))

  genealogy_new_med_2 <-
    dplyr::bind_rows(genealogy_new_med, table_next) |>
    dplyr::ungroup() |>
    dplyr::distinct()

  table_new <- ms1_multiple |>
    dplyr::filter(!is.na(species)) |>
    dplyr::mutate(
      best_candidate_3 = gsub(
        pattern = "notAnnotated notAnnotated notAnnotated",
        replacement = "notAnnotated notAnnotated",
        x = best_candidate_3
      )
    ) |>
    dplyr::mutate(
      best_candidate_3 = gsub(
        pattern = "notClassified notClassified notClassified",
        replacement = "notClassified notClassified",
        x = best_candidate_3
      )
    ) |>
    dplyr::full_join(genealogy_new_med_2, by = c("best_candidate_3" = "labels")) |>
    dplyr::distinct(
      feature_id,
      smiles_2D,
      inchikey_2D,
      score_biological,
      score_chemical,
      score_final,
      # best_candidate_organism,
      labels = new_labels,
      ids,
      parents,
      sample,
      intensity,
      species
    )

  table_1_new <- table_new |>
    dplyr::group_by(labels, sample, intensity) |>
    dplyr::add_count(name = "values") |>
    dplyr::select(parents, ids, labels, values, sample, intensity, species) |>
    dplyr::distinct() |>
    dplyr::filter(!is.na(ids))

  additional_row <- table_1_new |>
    dplyr::filter(labels == "Other other") |>
    dplyr::mutate(
      parents = "Other-Other other",
      ids = "Other other - Other other other",
      labels = "Other other other"
    )

  table_1_1_new <- rbind(table_1_new, additional_row) |>
    mutate(values = sum(intensity)) |>
    dplyr::ungroup()

  final_parents_table <- table_1_1_new |>
    dplyr::filter(parents == "") |>
    dplyr::select(-values, -sample, -intensity, -species) |>
    dplyr::left_join(
      table_1_1_new |>
        dplyr::select(parents, values, sample, species),
      by = c("ids" = "parents")
    ) |>
    dplyr::group_by(parents, ids, labels, sample, species) |>
    dplyr::summarise(values = sum(values)) |>
    dplyr::ungroup()

  final_table <-
    dplyr::bind_rows(
      final_parents_table,
      table_1_1_new
    ) |>
    dplyr::filter(!is.na(sample))

  return(final_table)
}
