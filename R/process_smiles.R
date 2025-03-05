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
#' @examples NULL
process_smiles <- function(df,
                           smiles_colname = "structure_smiles_initial",
                           cache) {
  log_debug(x = "Processing SMILES...")
  reticulate::source_python(file = system.file("python/process_smiles.py", package = "tima"))
  table_smiles <- df |>
    tidytable::filter(!is.na(!!as.name(smiles_colname))) |>
    # tidytable::slice_sample(10000L) |>
    tidytable::distinct(!!as.name(smiles_colname))

  table_processed_1 <- cache |>
    tidytable::fread()

  table_smiles_to_process <- table_smiles |>
    tidytable::anti_join(table_processed_1)

  input_smi_file <- tempfile(fileext = ".smi")
  output_csv_file <- tempfile(fileext = ".csv.gz")

  log_debug(x = "Passing ", nrow(table_smiles_to_process), " SMILES to RDKit")
  tidytable::fwrite(x = table_smiles_to_process, input_smi_file)

  # Pass to Python
  reticulate::py$process_smiles(input_smi_file, output_csv_file)

  table_processed_2 <- output_csv_file |>
    tidytable::fread()

  table_final <- table_smiles |>
    tidytable::inner_join(table_processed_1 |> tidytable::bind_rows(table_processed_2)) |>
    tidytable::mutate(structure_inchikey_connectivity_layer = structure_inchikey |>
      stringi::stri_sub(from = 1, to = 14))

  return(table_final)
}
