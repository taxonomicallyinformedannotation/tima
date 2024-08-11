import::from(tidytable, across, .into = environment())
import::from(tidytable, add_count, .into = environment())
import::from(tidytable, distinct, .into = environment())
import::from(tidytable, filter, .into = environment())
import::from(tidytable, group_by, .into = environment())
import::from(tidytable, replace_na, .into = environment())
import::from(tidytable, select, .into = environment())
import::from(tidytable, ungroup, .into = environment())
import::from(tidytable, where, .into = environment())

#' @title Split Structure Organism Pairs table
#'
#' @description This function splits the structure organism table.
#'
#' @importFrom tidytable across
#' @importFrom tidytable add_count
#' @importFrom tidytable distinct
#' @importFrom tidytable filter
#' @importFrom tidytable group_by
#' @importFrom tidytable replace_na
#' @importFrom tidytable select
#' @importFrom tidytable ungroup
#' @importFrom tidytable where
#'
#' @include clean_collapse.R
#'
#' @noRd
#'
#' @param table Table to split
#'
#' @return NULL
#'
#' @examples NULL
split_tables_sop <- function(table) {
  log_debug(x = "Splitting the concatenated library into smaller pieces")

  table_keys <- table |>
    filter(!is.na(structure_inchikey)) |>
    filter(!is.na(structure_smiles)) |>
    filter(!is.na(organism_name)) |>
    select(
      structure_inchikey,
      structure_smiles,
      organism_name,
      reference_doi
    ) |>
    distinct() |>
    group_by(structure_inchikey, structure_smiles, organism_name) |>
    add_count() |>
    ungroup() |>
    filter(!is.na(reference_doi) | n == 1) |>
    select(-n)
  log_debug(x = "Led to", nrow(table_keys), "referenced structure-organism pairs")

  table_structures_stereo <- table |>
    filter(!is.na(structure_inchikey)) |>
    filter(!is.na(structure_smiles)) |>
    filter(!is.na(structure_inchikey_no_stereo)) |>
    filter(!is.na(structure_smiles_no_stereo)) |>
    select(
      structure_inchikey,
      structure_smiles,
      structure_inchikey_no_stereo,
      structure_smiles_no_stereo
    ) |>
    distinct()
  log_debug(
    x = "Corresponding to",
    nrow(table_structures_stereo),
    "unique structures with stereo..."
  )
  log_debug(
    x = "and",
    nrow(
      table_structures_stereo |>
        distinct(structure_inchikey_no_stereo)
    ),
    "unique structures without stereo"
  )

  table_structures_metadata <- table |>
    filter(!is.na(structure_inchikey)) |>
    filter(!is.na(structure_smiles)) |>
    filter(!is.na(structure_molecular_formula)) |>
    filter(!is.na(structure_exact_mass)) |>
    filter(!is.na(structure_xlogp)) |>
    select(
      structure_inchikey,
      structure_smiles,
      structure_molecular_formula,
      structure_exact_mass,
      structure_xlogp
    ) |>
    distinct()

  table_structures_names <- table |>
    filter(!is.na(structure_inchikey)) |>
    filter(!is.na(structure_smiles)) |>
    filter(!is.na(structure_name)) |>
    select(structure_inchikey, structure_smiles, structure_name) |>
    distinct() |>
    group_by(structure_inchikey, structure_smiles) |>
    clean_collapse(cols = c("structure_name"))

  table_structures_taxonomy_npc <- table |>
    filter(!is.na(structure_smiles_no_stereo)) |>
    filter(
      !is.na(structure_tax_npc_01pat) |
        !is.na(structure_tax_npc_02sup) |
        !is.na(structure_tax_npc_03cla)
    ) |>
    select(
      structure_smiles_no_stereo,
      structure_tax_npc_01pat,
      structure_tax_npc_02sup,
      structure_tax_npc_03cla
    ) |>
    distinct() |>
    group_by(structure_smiles_no_stereo) |>
    clean_collapse(
      cols = c(
        "structure_tax_npc_01pat",
        "structure_tax_npc_02sup",
        "structure_tax_npc_03cla"
      )
    ) |>
    mutate(across(
      .cols = where(is.character),
      .fns = function(x) {
        replace_na(x, "notClassified")
      }
    ))

  table_structures_taxonomy_cla <- table |>
    filter(!is.na(structure_inchikey_no_stereo)) |>
    filter(!is.na(structure_tax_cla_chemontid)) |>
    select(
      structure_inchikey_no_stereo,
      structure_tax_cla_chemontid,
      structure_tax_cla_01kin,
      structure_tax_cla_02sup,
      structure_tax_cla_03cla,
      structure_tax_cla_04dirpar
    ) |>
    distinct() |>
    group_by(structure_inchikey_no_stereo) |>
    clean_collapse(
      cols = c(
        "structure_tax_cla_chemontid",
        "structure_tax_cla_01kin",
        "structure_tax_cla_02sup",
        "structure_tax_cla_03cla",
        "structure_tax_cla_04dirpar"
      )
    ) |>
    mutate(across(
      .cols = where(is.character),
      .fns = function(x) {
        replace_na(x, "notClassified")
      }
    ))

  table_organisms_names <- table |>
    filter(!is.na(organism_name)) |>
    select(organism_name) |>
    distinct()

  log_debug(x = "among", nrow(table_organisms_names), "unique organisms")

  table_org_tax_ott <- table |>
    filter(!is.na(organism_name)) |>
    filter(!is.na(organism_taxonomy_ottid)) |>
    select(
      organism_name,
      organism_taxonomy_ottid,
      organism_taxonomy_01domain,
      organism_taxonomy_02kingdom,
      organism_taxonomy_03phylum,
      organism_taxonomy_04class,
      organism_taxonomy_05order,
      organism_taxonomy_06family,
      organism_taxonomy_07tribe,
      organism_taxonomy_08genus,
      organism_taxonomy_09species,
      organism_taxonomy_10varietas
    ) |>
    distinct() |>
    mutate(across(
      .cols = where(is.character),
      .fns = function(x) {
        replace_na(x, "notClassified")
      }
    ))
  rm(table)

  tables <-
    list(
      "key" = table_keys,
      # "org_nam" = table_organisms_names,
      "org_tax_ott" = table_org_tax_ott,
      "str_stereo" = table_structures_stereo,
      "str_met" = table_structures_metadata,
      "str_nam" = table_structures_names,
      "str_tax_cla" = table_structures_taxonomy_cla,
      "str_tax_npc" = table_structures_taxonomy_npc
    )

  rm(
    table_keys,
    # table_organisms_names,
    table_org_tax_ott,
    table_structures_stereo,
    table_structures_metadata,
    table_structures_names,
    table_structures_taxonomy_cla,
    table_structures_taxonomy_npc
  )
  return(tables)
}
