#' @title Process SMILES strings
#'
#' @description Processes SMILES using RDKit (via Python) to standardize
#'     structures, generate InChIKeys, calculate molecular properties,
#'     and extract 2D representations. Results are cached to avoid reprocessing.
#'
#' @include validations_utils.R
#'
#' @param df Data frame containing SMILES strings
#' @param smiles_colname Column name containing SMILES
#'     (default: "structure_smiles_initial")
#' @param cache Path to cached processed SMILES file, or NULL to skip caching
#'
#' @return Data frame with processed SMILES including InChIKey, molecular formula
#'     (with isotopes shown), exact mass (with isotope contributions), 2D SMILES,
#'     xLogP, and connectivity layer
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Natural compound
#' df <- data.frame(
#'   structure_smiles_initial = "OC[C@H]1OC(O)[C@H](O)[C@@H](O)[C@@H]1O"
#' )
#' result <- process_smiles(df)
#' # Formula: C6H12O6, Mass: 180.063 Da
#'
#' # Isotope-labeled compound (4× 13C)
#' df_labeled <- data.frame(
#'   structure_smiles_initial = "OC[13C@H]1OC(O)[13C@H](O)[13C@@H](O)[13C@@H]1O"
#' )
#' result_labeled <- process_smiles(df_labeled)
#' # Formula: C2[13C]4H12O6 (isotopes shown separately)
#' # Mass: 184.077 Da (difference of ~4.013 Da from natural)
#' # SMILES preserves [13C] notation
#' # InChIKey differs from natural glucose
#' }
process_smiles <- function(
  df,
  smiles_colname = "structure_smiles_initial",
  cache = NULL
) {
  # Input Validation ----
  validate_dataframe(df, param_name = "df")
  validate_character(
    smiles_colname,
    param_name = "smiles_colname",
    allow_empty = FALSE
  )

  # Validate column exists
  if (!smiles_colname %in% names(df)) {
    stop(
      "Column '",
      smiles_colname,
      "' not found in data frame",
      call. = FALSE
    )
  }

  log_info("Processing SMILES with RDKit")

  # Load Python Processor ----
  load_python_smiles_processor()

  # Extract Unique SMILES ----
  table_smiles <- extract_unique_smiles(df, smiles_colname)

  if (nrow(table_smiles) == 0L) {
    log_warn("No valid SMILES to process")
    return(df)
  }

  log_debug("Processing %d unique SMILES", nrow(table_smiles))

  # Load or Initialize Cache ----
  table_processed_1 <- load_smiles_cache(cache, smiles_colname)

  # Identify SMILES not yet in cache
  table_smiles_to_process <- table_smiles |>
    tidytable::anti_join(y = table_processed_1)

  n_to_process <- nrow(table_smiles_to_process)

  if (n_to_process == 0L) {
    log_info("All SMILES already in cache, no processing needed")
    log_info("All SMILES already cached, returning existing results")
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

  log_info(
    "Processing %d new SMILES with RDKit",
    nrow(table_smiles_to_process)
  )

  # Write SMILES to temporary file
  tidytable::fwrite(
    x = table_smiles_to_process,
    file = input_smi_file
  )

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
    tidytable::rename(
      !!as.name(smiles_colname) := "structure_smiles_initial"
    )

  log_info(
    "Successfully processed %d SMILES",
    nrow(table_processed_2)
  )

  # Combine cached and new results
  table_final <- table_smiles |>
    tidytable::inner_join(
      y = table_processed_1 |>
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

# Helper Functions ----

#' Load Python SMILES processor
#' @keywords internal
load_python_smiles_processor <- function() {
  tryCatch(
    {
      py_script <- system.file("python/process_smiles.py", package = "tima")
      reticulate::source_python(file = py_script)
    },
    error = function(e) {
      log_error(
        "Failed to load Python processor: %s",
        conditionMessage(e)
      )
      stop(
        "Cannot load Python SMILES processor: ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )
}

#' Extract unique SMILES from dataframe
#' @keywords internal
extract_unique_smiles <- function(df, smiles_colname) {
  df |>
    tidytable::filter(!is.na(!!as.name(smiles_colname))) |>
    tidytable::distinct(!!as.name(smiles_colname))
}

#' Load SMILES cache or create empty template
#' @keywords internal
load_smiles_cache <- function(cache, smiles_colname) {
  if (is.null(cache)) {
    return(create_empty_smiles_template(smiles_colname))
  }

  tryCatch(
    {
      cached <- tidytable::fread(cache)
      log_debug("Loaded %d cached SMILES", nrow(cached))
      cached
    },
    error = function(e) {
      log_warn("Cache load failed: %s", conditionMessage(e))
      create_empty_smiles_template(smiles_colname)
    }
  )
}

#' Create empty SMILES result template
#' @keywords internal
create_empty_smiles_template <- function(smiles_colname) {
  tidytable::tidytable(
    !!as.name(smiles_colname) := NA_character_,
    structure_smiles = NA_character_,
    structure_inchikey = NA_character_,
    structure_molecular_formula = NA_character_,
    structure_exact_mass = NA_real_,
    structure_smiles_no_stereo = NA_character_,
    structure_xlogp = NA_real_
  )
}
