require(package = dplyr, quietly = TRUE, warn.conflicts = FALSE)
require(package = splitstackshape, quietly = TRUE)
require(package = tidyr, quietly = TRUE)

#' Title
#'
#' @param dfsel
#'
#' @return
#' @export
#'
#' @examples
manipulating_taxo_otl <- function(dfsel) {
  ## selecting and splitting taxonomy and ranks
  df1 <- dfsel |>
    dplyr::select(organismCleaned,
      name = organismCleaned_dbTaxoTaxonomy,
      rank = organismCleaned_dbTaxoTaxonRanks,
      organismDbTaxo
    ) |>
    dplyr::filter(organismDbTaxo == "Open Tree of Life") |>
    dplyr::distinct(organismCleaned,
      .keep_all = TRUE
    ) |>
    splitstackshape::cSplit(
      splitCols = "name",
      sep = "|"
    ) |>
    splitstackshape::cSplit(
      splitCols = "rank",
      sep = "|"
    ) |>
    dplyr::mutate_all(as.character)

  # manipulating taxa
  df2 <- df1 |>
    tidyr::pivot_longer(
      cols = 2:ncol(df1),
      names_to = c(".value", "level"),
      names_sep = "_",
      values_to = "taxonomy",
      values_drop_na = TRUE
    ) |>
    dplyr::distinct(organismCleaned,
      level,
      .keep_all = TRUE
    )

  df2$rank <- ifelse(test = is.na(df2$rank),
    yes = "NA",
    no = df2$rank
  )

  df2_a <- df2 |>
    dplyr::filter(!is.na(rank)) |>
    dplyr::filter(rank != "") |>
    dplyr::arrange(desc(level)) |>
    dplyr::select(
      taxon_rank = rank,
      dplyr::everything(),
      -level
    ) |>
    dplyr::distinct()

  # manipulating taxa
  df3 <- df2 |>
    dplyr::filter(
      rank == "domain" |
        rank == "kingdom" |
        rank == "phylum" |
        rank == "class" |
        rank == "order" |
        rank == "infraorder" |
        rank == "family" |
        rank == "subfamily" |
        rank == "tribe" |
        rank == "subtribe" |
        rank == "genus" |
        rank == "subgenus" |
        rank == "species" |
        rank == "subspecies" |
        rank == "variety"
    ) |>
    tidyr::pivot_wider(
      names_from = rank,
      values_from = name
    ) |>
    dplyr::arrange(desc(level)) |>
    ## because of Open Tree of Life taxonomy
    dplyr::select(-level)

  if (nrow(df3) != 0) {
    df4 <- df3 |>
      tidyr::pivot_longer(
        cols = 2:ncol(df3),
        names_to = "rank",
        values_to = "name",
        values_drop_na = TRUE
      ) |>
      dplyr::distinct(organismCleaned,
        rank,
        .keep_all = TRUE
      ) ## because of Open Tree of Life taxonomy
  }

  # pivoting (wide)
  if (nrow(df3) != 0) {
    df5 <- df4 %>%
      dplyr::group_by(organismCleaned) %>%
      dplyr::distinct(rank,
        name,
        .keep_all = TRUE
      ) %>%
      tidyr::pivot_wider(
        names_from = rank,
        values_from = name
      ) %>%
      dplyr::ungroup() %>%
      dplyr::select_if(
        names(.) %in%
          c(
            "organismCleaned",
            "domain",
            "kingdom",
            "phylum",
            "class",
            "order",
            "infraorder",
            "family",
            "subfamily",
            "tribe",
            "subtribe",
            "genus",
            "subgenus",
            "species",
            "subspecies",
            "variety"
          )
      )
  }

  if (nrow(df3) != 0) {
    df5[setdiff(
      x = c(
        "organismCleaned",
        "domain",
        "kingdom",
        "phylum",
        "class",
        "order",
        "infraorder",
        "family",
        "subfamily",
        "tribe",
        "subtribe",
        "genus",
        "subgenus",
        "species",
        "subspecies",
        "variety"
      ),
      y = names(df5)
    )] <- NA
  }

  # adding taxa to initial df
  if (nrow(df3) != 0) {
    df6 <- dplyr::left_join(dfsel, df5)

    df6 <- dplyr::left_join(df6, df2_a) |>
      dplyr::select(
        organismCleaned,
        organism_01_domain = domain,
        organism_02_kingdom = kingdom,
        organism_03_phylum = phylum,
        organism_04_class = class,
        organism_05_order = order,
        organism_05_1_infraorder = infraorder,
        organism_06_family = family,
        organism_06_1_subfamily = subfamily,
        organism_07_tribe = tribe,
        organism_07_1_subtribe = subtribe,
        organism_08_genus = genus,
        organism_08_1_subgenus = subgenus,
        organism_09_species = species,
        organism_09_1_subspecies = subspecies,
        organism_10_variety = variety
      ) |>
      dplyr::distinct()
  }

  if (nrow(df3) == 0) {
    df6 <- data.frame() |>
      dplyr::mutate(
        organismCleaned = NA,
        organism_01_domain = NA,
        organism_02_kingdom = NA,
        organism_03_phylum = NA,
        organism_04_class = NA,
        organism_05_order = NA,
        organism_05_1_infraorder = NA,
        organism_06_family = NA,
        organism_06_1_subfamily = NA,
        organism_07_tribe = NA,
        organism_07_1_subtribe = NA,
        organism_08_genus = NA,
        organism_08_1_subgenus = NA,
        organism_09_species = NA,
        organism_09_1_subspecies = NA,
        organism_10_variety = NA
      )
  }

  return(df6)
}
