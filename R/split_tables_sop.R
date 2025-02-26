#' @title Split Structure Organism Pairs table
#'
#' @description This function splits the structure organism table.
#'
#' @include clean_collapse.R
#'
#' @param table Table to split
#'
#' @return A list of tables from the structure organism pairs tables
#'
#' @examples NULL
split_tables_sop <- function(table) {
  log_debug(x = "Splitting the concatenated library into smaller pieces")

  table_keys <- table |>
    tidytable::filter(!is.na(structure_inchikey)) |>
    tidytable::filter(!is.na(structure_smiles)) |>
    tidytable::filter(!is.na(organism_name)) |>
    tidytable::select(
      structure_inchikey,
      structure_smiles,
      organism_name,
      reference_doi
    ) |>
    tidytable::distinct() |>
    tidytable::group_by(structure_inchikey, structure_smiles, organism_name) |>
    tidytable::add_count() |>
    tidytable::ungroup() |>
    tidytable::filter(!is.na(reference_doi) | n == 1) |>
    tidytable::select(-n)
  log_debug(x = "Led to", nrow(table_keys), "referenced structure-organism pairs")

  table_structures_stereo <- table |>
    tidytable::filter(!is.na(structure_inchikey)) |>
    tidytable::filter(!is.na(structure_smiles) |
      !is.na(structure_smiles_no_stereo)) |>
    tidytable::filter(!is.na(structure_inchikey_no_stereo)) |>
    tidytable::filter() |>
    tidytable::select(
      structure_inchikey,
      structure_smiles,
      structure_inchikey_no_stereo,
      structure_smiles_no_stereo
    ) |>
    tidytable::distinct() |>
    tidytable::group_by(structure_inchikey) |>
    tidytable::fill(structure_smiles, .direction = "downup") |>
    tidytable::ungroup() |>
    tidytable::distinct() |>
    tidytable::group_by(structure_inchikey_no_stereo) |>
    tidytable::fill(structure_smiles_no_stereo, .direction = "downup") |>
    tidytable::ungroup() |>
    tidytable::distinct()
  log_debug(
    x = "Corresponding to",
    nrow(
      table_structures_stereo |>
        tidytable::filter(
          !grepl(
            pattern = "-UHFFFAOYSA-",
            x = structure_inchikey,
            fixed = TRUE
          )
        ) |>
        tidytable::distinct(structure_inchikey)
    ),
    "unique stereoisomers (excluding structures without stereochemistry)..."
  )

  log_debug(
    x = "... and",
    nrow(
      table_structures_stereo |>
        tidytable::filter(
          grepl(
            pattern = "-UHFFFAOYSA-",
            x = structure_inchikey,
            fixed = TRUE
          )
        ) |>
        tidytable::distinct(structure_inchikey)
    ),
    "unique structures without stereochemistry..."
  )

  log_debug(
    x = "or",
    nrow(
      table_structures_stereo |>
        tidytable::distinct(structure_inchikey_no_stereo)
    ),
    "unique constitutional isomers (ignoring stereochemistry)"
  )

  table_structures_metadata <- table |>
    tidytable::filter(!is.na(structure_inchikey)) |>
    tidytable::filter(!is.na(structure_molecular_formula)) |>
    tidytable::filter(!is.na(structure_exact_mass)) |>
    # tidytable::filter(!is.na(structure_xlogp)) |>
    tidytable::select(
      structure_inchikey,
      structure_molecular_formula,
      structure_exact_mass,
      structure_xlogp
    ) |>
    tidytable::distinct() |>
    tidytable::group_by(structure_inchikey) |>
    tidytable::fill(structure_molecular_formula,
      structure_exact_mass,
      structure_xlogp,
      .direction = "downup"
    ) |>
    tidytable::ungroup() |>
    tidytable::distinct() |>
    # COMMENT else duplicate enties
    tidytable::group_by(structure_inchikey, structure_molecular_formula) |>
    tidytable::mutate(
      structure_exact_mass = mean(as.numeric(structure_exact_mass), na.rm = TRUE),
      structure_xlogp = mean(as.numeric(structure_xlogp), na.rm = TRUE)
    ) |>
    tidytable::ungroup() |>
    tidytable::distinct()

  table_structures_names <- table |>
    tidytable::filter(!is.na(structure_inchikey)) |>
    tidytable::filter(!is.na(structure_name)) |>
    tidytable::select(structure_inchikey, structure_name) |>
    tidytable::distinct() |>
    tidytable::group_by(structure_inchikey) |>
    clean_collapse(cols = c("structure_name"))

  table_structures_taxonomy_npc <- table |>
    tidytable::filter(!is.na(structure_smiles_no_stereo)) |>
    tidytable::filter(
      !is.na(structure_tax_npc_01pat) |
        !is.na(structure_tax_npc_02sup) |
        !is.na(structure_tax_npc_03cla)
    ) |>
    tidytable::select(
      structure_smiles_no_stereo,
      structure_tax_npc_01pat,
      structure_tax_npc_02sup,
      structure_tax_npc_03cla
    ) |>
    tidytable::distinct() |>
    tidytable::group_by(structure_smiles_no_stereo) |>
    clean_collapse(
      cols = c(
        "structure_tax_npc_01pat",
        "structure_tax_npc_02sup",
        "structure_tax_npc_03cla"
      )
    ) |>
    tidytable::mutate(tidytable::across(
      .cols = tidyselect::where(is.character),
      .fns = function(x) {
        tidytable::replace_na(x, "notClassified")
      }
    ))

  table_structures_taxonomy_cla <- table |>
    tidytable::filter(!is.na(structure_inchikey_no_stereo)) |>
    tidytable::filter(!is.na(structure_tax_cla_chemontid)) |>
    tidytable::select(
      structure_inchikey_no_stereo,
      structure_tax_cla_chemontid,
      structure_tax_cla_01kin,
      structure_tax_cla_02sup,
      structure_tax_cla_03cla,
      structure_tax_cla_04dirpar
    ) |>
    tidytable::distinct() |>
    tidytable::group_by(structure_inchikey_no_stereo) |>
    clean_collapse(
      cols = c(
        "structure_tax_cla_chemontid",
        "structure_tax_cla_01kin",
        "structure_tax_cla_02sup",
        "structure_tax_cla_03cla",
        "structure_tax_cla_04dirpar"
      )
    ) |>
    tidytable::mutate(tidytable::across(
      .cols = tidyselect::where(is.character),
      .fns = function(x) {
        tidytable::replace_na(x, "notClassified")
      }
    ))

  table_organisms_names <- table |>
    tidytable::filter(!is.na(organism_name)) |>
    tidytable::select(organism_name) |>
    tidytable::distinct()

  log_debug(x = "among", nrow(table_organisms_names), "unique organisms")

  table_org_tax_ott <- table |>
    tidytable::filter(!is.na(organism_name)) |>
    tidytable::filter(!is.na(organism_taxonomy_ottid)) |>
    tidytable::select(
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
    tidytable::distinct() |>
    tidytable::mutate(tidytable::across(
      .cols = tidyselect::where(is.character),
      .fns = function(x) {
        tidytable::replace_na(x, "notClassified")
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
