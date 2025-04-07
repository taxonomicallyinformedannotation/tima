#' @title Split Structure Organism Pairs table
#'
#' @description This function splits the structure organism table.
#'
#' @include clean_collapse.R
#' @include process_smiles.R
#'
#' @param table Table to split
#' @param cache Cache where already processed SMILES are located
#'
#' @return A list of tables from the structure organism pairs tables
#'
#' @examples NULL
split_tables_sop <- function(table, cache) {
  logger::log_trace(
    "Splitting the concatenated library into smaller standardized pieces"
  )

  table <- table |>
    tidytable::mutate(
      structure_smiles_initial = tidytable::coalesce(
        structure_smiles,
        structure_smiles_no_stereo
      )
    )
  table_structural_initial <- table |>
    tidytable::select(
      tidyselect::all_of(
        tidyselect::contains("structure")
      )
    ) |>
    tidytable::distinct()

  table_organisms <- table |>
    tidytable::select(
      tidyselect::contains("organism")
    ) |>
    tidytable::filter(!is.na(organism_name)) |>
    tidytable::distinct()

  logger::log_trace("Sanitizing structures")
  table_structural_standardized <- table_structural_initial |>
    process_smiles(cache = cache)

  table_structural <- table_structural_initial |>
    tidytable::select(
      structure_smiles_initial,
      structure_name,
      tidyselect::contains("_tax")
    ) |>
    tidytable::distinct() |>
    tidytable::inner_join(table_structural_standardized) |>
    tidytable::distinct()
  rm(table_structural_initial, table_structural_standardized)

  table <- table |>
    tidytable::select(
      structure_smiles_initial,
      tidyselect::contains("organism"),
      tidyselect::contains("reference")
    ) |>
    tidytable::distinct() |>
    tidytable::inner_join(table_structural) |>
    tidytable::distinct()

  table_keys <- table |>
    tidytable::filter(!is.na(structure_inchikey)) |>
    tidytable::filter(!is.na(structure_smiles_no_stereo)) |>
    tidytable::filter(!is.na(organism_name)) |>
    tidytable::select(
      structure_inchikey,
      structure_smiles_no_stereo,
      organism_name,
      reference_doi
    ) |>
    tidytable::distinct() |>
    tidytable::group_by(structure_inchikey, organism_name) |>
    tidytable::add_count() |>
    tidytable::ungroup() |>
    tidytable::filter(!is.na(reference_doi) | n == 1) |>
    tidytable::select(-n)
  rm(table)
  logger::log_info(
    "Led to {nrow(table_keys)} referenced structure-organism pairs"
  )

  table_structures_stereo <- table_structural |>
    tidytable::filter(!is.na(structure_inchikey)) |>
    tidytable::filter(
      !is.na(structure_smiles) |
        !is.na(structure_smiles_no_stereo)
    ) |>
    tidytable::filter(!is.na(structure_inchikey_connectivity_layer)) |>
    tidytable::filter() |>
    tidytable::select(
      structure_inchikey,
      structure_smiles,
      structure_inchikey_connectivity_layer,
      structure_smiles_no_stereo
    ) |>
    tidytable::distinct()
  logger::log_info(
    "Corresponding to ",
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
    " unique stereoisomers (excluding structures without stereochemistry)..."
  )

  logger::log_info(
    "... and ",
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
    " unique structures without stereochemistry..."
  )

  logger::log_info(
    "... or ",
    nrow(
      table_structures_stereo |>
        tidytable::distinct(structure_inchikey_connectivity_layer)
    ),
    " unique constitutional isomers (ignoring stereochemistry)"
  )

  table_structures_metadata <- table_structural |>
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
    tidytable::distinct()

  table_structures_names <- table_structural |>
    tidytable::filter(!is.na(structure_inchikey)) |>
    tidytable::filter(!is.na(structure_name)) |>
    tidytable::select(structure_inchikey, structure_name) |>
    tidytable::distinct() |>
    tidytable::group_by(structure_inchikey) |>
    clean_collapse(cols = c("structure_name"))

  table_structures_taxonomy_npc <- table_structural |>
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

  table_structures_taxonomy_cla <- table_structural |>
    tidytable::filter(!is.na(structure_inchikey_connectivity_layer)) |>
    tidytable::filter(!is.na(structure_tax_cla_chemontid)) |>
    tidytable::select(
      structure_inchikey_connectivity_layer,
      structure_tax_cla_chemontid,
      structure_tax_cla_01kin,
      structure_tax_cla_02sup,
      structure_tax_cla_03cla,
      structure_tax_cla_04dirpar
    ) |>
    tidytable::distinct() |>
    tidytable::group_by(structure_inchikey_connectivity_layer) |>
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

  table_organisms_names <- table_organisms |>
    tidytable::filter(!is.na(organism_name)) |>
    tidytable::select(organism_name) |>
    tidytable::distinct()

  logger::log_info(
    "... among ",
    nrow(table_organisms_names),
    " unique organisms"
  )

  table_org_tax_ott <- table_organisms |>
    tidytable::filter(!is.na(organism_taxonomy_ottid)) |>
    tidytable::distinct() |>
    tidytable::mutate(tidytable::across(
      .cols = tidyselect::where(is.character),
      .fns = function(x) {
        tidytable::replace_na(x, "notClassified")
      }
    ))
  rm(table_organisms)

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
