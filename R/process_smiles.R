#' @title Process SMILES
#'
#' @description This function processes SMILES strings using RDKit (via Python)
#'     to standardize structures, generate InChIKeys, calculate molecular properties,
#'     and extract 2D representations. Results are cached to avoid reprocessing.
#'
#' @param df Data frame containing SMILES strings to process
#' @param smiles_colname Character string name of the column containing SMILES
#'     (default: "structure_smiles_initial")
#' @param cache Character string path to cached processed SMILES file, or NULL
#'     to skip caching (default: NULL)
#'
#' @return Data frame with processed SMILES including InChIKey, molecular formula,
#'     exact mass, 2D SMILES, xLogP, and connectivity layer
#'
#' @export
#'
#' @examples
#' \dontrun{
#' smiles <- "C=C[C@H]1[C@@H](OC=C2C1=CCOC2=O)O[C@H]3[C@@H]([C@H]([C@@H]([C@H](O3)CO)O)O)O"
#' data.frame(
#'   "structure_smiles_initial" = smiles
#' ) |>
#'   process_smiles()
#' }
process_smiles <- function(
  df,
  smiles_colname = "structure_smiles_initial",
  cache = NULL
) {
  # Validate inputs
  if (!is.data.frame(df) && !inherits(df, "tbl")) {
    stop("Input 'df' must be a data frame or tibble")
  }

  if (!smiles_colname %in% names(df)) {
    stop("Column '", smiles_colname, "' not found in data frame")
  }

  logger::log_trace("Processing SMILES strings with RDKit")

  # Load Python script for SMILES processing
  tryCatch(
    {
      reticulate::source_python(
        file = system.file("python/process_smiles.py", package = "tima")
      )
    },
    error = function(e) {
      stop("Failed to load Python SMILES processor: ", conditionMessage(e))
    }
  )

  # Extract unique SMILES for processing
  table_smiles <- df |>
    tidytable::filter(!is.na(!!as.name(smiles_colname))) |>
    tidytable::distinct(!!as.name(smiles_colname))

  logger::log_debug("Found ", nrow(table_smiles), " unique SMILES to check")

  # Load cached results if available
  if (is.null(cache)) {
    logger::log_trace("No cache provided, processing all SMILES")
    table_processed_1 <- tidytable::tidytable(
      !!as.name(smiles_colname) := NA_character_,
      structure_smiles = NA_character_,
      structure_inchikey = NA_character_,
      structure_molecular_formula = NA_character_,
      structure_exact_mass = NA_real_,
      structure_smiles_no_stereo = NA_character_,
      structure_xlogp = NA_real_
    )
  } else {
    logger::log_trace("Loading cached SMILES from: ", cache)
    tryCatch(
      {
        table_processed_1 <- tidytable::fread(cache)
      },
      error = function(e) {
        logger::log_warn(
          "Failed to load cache, processing all SMILES: ",
          conditionMessage(e)
        )
        table_processed_1 <- tidytable::tidytable(
          !!as.name(smiles_colname) := NA_character_,
          structure_smiles = NA_character_,
          structure_inchikey = NA_character_,
          structure_molecular_formula = NA_character_,
          structure_exact_mass = NA_real_,
          structure_smiles_no_stereo = NA_character_,
          structure_xlogp = NA_real_
        )
      }
    )
  }

  # Identify SMILES not yet in cache
  table_smiles_to_process <- table_smiles |>
    tidytable::anti_join(table_processed_1)

  if (nrow(table_smiles_to_process) == 0L) {
    logger::log_info("All SMILES already cached, returning existing results")
    return(
      table_processed_1 |>
        tidytable::mutate(
          structure_inchikey_connectivity_layer = stringi::stri_sub(
            str = structure_inchikey,
            from = 1L,
            to = 14L
          )
        )
    )
  }

  # Create temporary files for Python processing
  input_smi_file <- tempfile(fileext = ".smi")
  output_csv_file <- tempfile(fileext = ".csv.gz")

  logger::log_info(
    "Processing ",
    nrow(table_smiles_to_process),
    " new SMILES with RDKit"
  )

  # Write SMILES to temporary file
  tidytable::fwrite(x = table_smiles_to_process, file = input_smi_file)

  # Process SMILES using Python/RDKit
  tryCatch(
    {
      reticulate::py$process_smiles(input_smi_file, output_csv_file)
    },
    error = function(e) {
      stop("RDKit SMILES processing failed: ", conditionMessage(e))
    }
  )

  # Read processed results
  table_processed_2 <- tidytable::fread(output_csv_file) |>
    tidytable::rename(!!as.name(smiles_colname) := "structure_smiles_initial")

  logger::log_info(
    "Successfully processed ",
    nrow(table_processed_2),
    " SMILES"
  )

  # Combine cached and new results
  table_final <- table_smiles |>
    tidytable::inner_join(
      table_processed_1 |>
        tidytable::bind_rows(table_processed_2) |>
        tidytable::filter(!is.na(!!as.name(smiles_colname)))
    ) |>
    tidytable::mutate(
      structure_inchikey_connectivity_layer = stringi::stri_sub(
        str = structure_inchikey,
        from = 1L,
        to = 14L
      )
    )

  return(table_final)
}
