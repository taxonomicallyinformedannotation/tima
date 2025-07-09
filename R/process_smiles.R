#' @title Process SMILES
#'
#' @description This function processes SMILES
#'
#' @param df Dataframe to use
#' @param smiles_colname Column name of the SMILES variable
#' @param cache Cache where already processed SMILES are located
#'
#' @return NULL
#'
#' @examples
#' data.frame(
#'   "structure_smiles_initial" = "C[C@@H]1C=C(C(=O)[C@]2([C@H]1C[C@@H]3[C@@]4([C@@H]2C(=O)C(=C([C@@H]4CC(=O)O3)C)OC)C)C)OC"
#' ) |>
#'   process_smiles()
process_smiles <- function(
    df,
    smiles_colname = "structure_smiles_initial",
    cache = NULL) {
  logger::log_trace("Processing SMILES")
  reticulate::source_python(
    file = system.file("python/process_smiles.py", package = "tima")
  )

  table_smiles <- df |>
    tidytable::filter(!is.na(!!as.name(smiles_colname))) |>
    # tidytable::slice_sample(10000L) |>
    tidytable::distinct(!!as.name(smiles_colname))

  if (
    cache |>
      is.null()
  ) {
    table_processed_1 <- tidytable::tidytable(
      !!as.name(smiles_colname) := NA_character_,
      "structure_smiles" = NA_character_,
      "structure_inchikey" = NA_character_,
      "structure_molecular_formula" = NA_character_,
      "structure_exact_mass" = NA_real_,
      "structure_smiles_no_stereo" = NA_character_,
      "structure_xlogp" = NA_real_
    )
  } else {
    table_processed_1 <- cache |>
      tidytable::fread()
  }

  table_smiles_to_process <- table_smiles |>
    tidytable::anti_join(table_processed_1)

  if (nrow(table_smiles_to_process) == 0) {
    logger::log_info("No new SMILES to process. Returning cached results.")
    return(
      table_processed_1 |>
        tidytable::mutate(
          structure_inchikey_connectivity_layer = structure_inchikey |>
            stringi::stri_sub(from = 1L, to = 14L)
        )
    )
  }

  input_smi_file <- tempfile(fileext = ".smi")
  output_csv_file <- tempfile(fileext = ".csv.gz")

  logger::log_info(
    "Passing ",
    nrow(table_smiles_to_process),
    " SMILES to RDKit"
  )

  tidytable::fwrite(x = table_smiles_to_process, input_smi_file)

  # Pass to Python
  reticulate::py$process_smiles(input_smi_file, output_csv_file)

  table_processed_2 <- output_csv_file |>
    tidytable::fread() |>
    tidytable::rename(!!as.name(smiles_colname) := "structure_smiles_initial")

  table_final <- table_smiles |>
    tidytable::inner_join(
      table_processed_1 |>
        tidytable::bind_rows(table_processed_2) |>
        tidytable::filter(!is.na(smiles_colname))
    ) |>
    tidytable::mutate(
      structure_inchikey_connectivity_layer = structure_inchikey |>
        stringi::stri_sub(from = 1L, to = 14L)
    )

  return(table_final)
}
