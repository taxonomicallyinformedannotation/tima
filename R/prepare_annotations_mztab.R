#' @title Prepare annotations from mzTab-M
#'
#' @description This function prepares compound annotations from mzTab-M files
#'     by standardizing column names, integrating structure metadata, and
#'     formatting for downstream TIMA annotation workflows. Handles both
#'     database matches and manual annotations in the Small Molecule table.
#'
#' @include mztab_utils.R
#' @include columns_utils.R
#' @include get_params.R
#' @include select_annotations_columns.R
#'
#' @param input Character string path to mzTab-M file or prepared feature table
#'     from read_mztab()
#' @param output Character string path for prepared mzTab annotations output
#' @param str_stereo Character string path to structures stereochemistry file
#' @param str_met Character string path to structures metadata file
#' @param str_nam Character string path to structures names file
#' @param str_tax_cla Character string path to ClassyFire taxonomy file
#' @param str_tax_npc Character string path to NPClassifier taxonomy file
#'
#' @return Character string path to prepared mzTab annotations
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Prepare mzTab-M annotations for TIMA
#' prepare_annotations_mztab(
#'   input = "data/source/experiment.mztab",
#'   output = "data/interim/annotations/mztab_prepared.tsv"
#' )
#' }
prepare_annotations_mztab <- function(
  input,
  output,
  str_stereo = get_params(
    step = "prepare_annotations_mztab"
  )$files$libraries$sop$merged$structures$stereo,
  str_met = get_params(
    step = "prepare_annotations_mztab"
  )$files$libraries$sop$merged$structures$metadata,
  str_nam = get_params(
    step = "prepare_annotations_mztab"
  )$files$libraries$sop$merged$structures$names,
  str_tax_cla = get_params(
    step = "prepare_annotations_mztab"
  )$files$libraries$sop$merged$structures$taxonomies$cla,
  str_tax_npc = get_params(
    step = "prepare_annotations_mztab"
  )$files$libraries$sop$merged$structures$taxonomies$npc
) {
  # Start logging ----
  ctx <- log_operation("prepare_annotations_mztab", input = input)

  # Input Validation ----
  validate_character(input, param_name = "input")
  validate_character(output, param_name = "output")

  if (!file.exists(input)) {
    stop(
      "✗ Input file not found: ",
      input,
      "\n\n",
      "Fix: Verify the file path is correct",
      call. = FALSE
    )
  }

  validate_file_existence(
    file_list = list(
      str_stereo = str_stereo,
      str_met = str_met,
      str_nam = str_nam,
      str_tax_cla = str_tax_cla,
      str_tax_npc = str_tax_npc
    ),
    allow_null = FALSE
  )

  # Load and Parse mzTab-M File ----
  log_metadata(ctx, phase = "loading")

  mztab_obj <- tryCatch(
    {
      # Use custom line-by-line parser (same approach as read_mztab)
      lines <- readLines(input)

      # Parse metadata
      mtd_lines <- grep("^MTD", lines, value = TRUE)
      mtd_data <- if (length(mtd_lines) > 0) {
        parts_list <- stringi::stri_split_fixed(mtd_lines, "\t")
        data.frame(
          key = vapply(parts_list, function(p) if (length(p) >= 2) p[2] else NA_character_, character(1)),
          value = vapply(parts_list, function(p) if (length(p) >= 3) p[3] else NA_character_, character(1)),
          stringsAsFactors = FALSE
        )
      } else {
        data.frame(key = character(), value = character())
      }

      # Parse small molecule table (SMH/SML)
      smh_line <- grep("^SMH", lines, value = TRUE)
      sml_lines <- grep("^SML", lines, value = TRUE)
      sml_data <- if (length(smh_line) > 0 && length(sml_lines) > 0) {
        col_names <- stringi::stri_split_fixed(smh_line[1], "\t")[[1]][-1]
        row_list <- lapply(sml_lines, function(line) {
          parts <- stringi::stri_split_fixed(line, "\t")[[1]][-1]
          length(parts) <- length(col_names)
          parts
        })
        mat <- do.call(rbind, row_list)
        df <- as.data.frame(mat, stringsAsFactors = FALSE)
        colnames(df) <- col_names
        df
      } else {
        NULL
      }

      list(
        Metadata = mtd_data,
        Small_Molecule = sml_data
      )
    },
    error = function(e) {
      stop(
        "✗ Failed to parse mzTab file\n\n",
        "Error: ",
        conditionMessage(e),
        "\n\n",
        "Fix: Ensure file follows mzTab-M 2.0 specification",
        call. = FALSE
      )
    }
  )

  # Validate structure
  .validate_mztab_structure(mztab_obj)

  # Extract Small Molecule Table ----
  log_metadata(ctx, phase = "extracting_annotations")

  sml_data <- mztab_obj$Small_Molecule
  log_debug("Processing %d small molecules from mzTab", nrow(sml_data))

  # Map mzTab-M columns to TIMA annotation schema
  table <- .map_mztab_to_annotations(sml_data)

  if (nrow(table) == 0) {
    log_warn("No valid annotations found in mzTab file")
    table <- fake_annotations_columns()
  } else {
    log_debug("Mapped %d annotations to TIMA format", nrow(table))

    # Integrate structure metadata
    log_metadata(ctx, phase = "integrating_metadata")

    table <- select_annotations_columns(
      table,
      str_stereo = str_stereo,
      str_met = str_met,
      str_nam = str_nam,
      str_tax_cla = str_tax_cla,
      str_tax_npc = str_tax_npc
    )
  }

  # Export Results ----
  log_complete(ctx, n_annotations = nrow(table))

  export_output(x = table, file = output)

  return(output)
}

#' Map mzTab-M Small Molecule table to TIMA annotation schema
#'
#' @description Internal function to convert mzTab-M SML table columns to
#'     TIMA's internal annotation format with standardized column names.
#'
#' @param sml_data Data frame with mzTab-M Small Molecule table
#'
#' @return Data frame with TIMA annotation schema
#'
#' @keywords internal
.map_mztab_to_annotations <- function(sml_data) {
  # mzTab-M SML columns of interest:
  # - SML_ID: feature/molecule identifier
  # - database_identifier: external database ID (e.g., HMDB00001)
  # - chemical_formula: molecular formula
  # - chemical_name: compound name
  # - smiles: SMILES string
  # - inchi: InChI string
  # - inchikey: InChI Key
  # - adduct_ions: adduct notation
  # - reliability: confidence level
  # - search_engine_score: various scores
  # - exp_mass_to_charge: experimental m/z
  # - theoretical_neutral_mass: theoretical mass

  # Initialize result with empty TIMA annotation columns
  result <- data.frame(
    feature_id = character(),
    candidate_structure_inchikey = character(),
    candidate_structure_inchi = character(),
    candidate_structure_smiles_no_stereo = character(),
    candidate_structure_molecular_formula = character(),
    candidate_structure_name = character(),
    candidate_structure_exact_mass = numeric(),
    candidate_adduct = character(),
    candidate_library = character(),
    candidate_score_similarity = numeric(),
    stringsAsFactors = FALSE
  )

  if (nrow(sml_data) == 0) {
    return(result)
  }

  # Map feature ID
  result$feature_id <- if ("SML_ID" %in% names(sml_data)) {
    as.character(sml_data$SML_ID)
  } else {
    as.character(seq_len(nrow(sml_data)))
  }

  # Map structural identifiers
  if ("database_identifier" %in% names(sml_data)) {
    result$candidate_database_id <- as.character(sml_data$database_identifier)
  }

  if ("chemical_name" %in% names(sml_data)) {
    result$candidate_structure_name <- as.character(sml_data$chemical_name)
  }

  if ("smiles" %in% names(sml_data)) {
    result$candidate_structure_smiles_no_stereo <- as.character(sml_data$smiles)
  }

  if ("inchi" %in% names(sml_data)) {
    result$candidate_structure_inchi <- as.character(sml_data$inchi)
  }

  if ("inchikey" %in% names(sml_data)) {
    inchikey <- as.character(sml_data$inchikey)
    # Handle "null" values
    inchikey[inchikey == "null" | is.na(inchikey)] <- NA_character_
    result$candidate_structure_inchikey <- inchikey

    # Extract connectivity layer (first 14 characters)
    result$candidate_structure_inchikey_connectivity_layer <- ifelse(
      !is.na(inchikey) & nchar(inchikey) >= 14,
      substr(inchikey, 1, 14),
      NA_character_
    )
  }

  if ("chemical_formula" %in% names(sml_data)) {
    result$candidate_structure_molecular_formula <- as.character(
      sml_data$chemical_formula
    )
  }

  if ("theoretical_neutral_mass" %in% names(sml_data)) {
    mass <- as.numeric(sml_data$theoretical_neutral_mass)
    mass[is.na(mass) | mass == 0] <- NA_real_
    result$candidate_structure_exact_mass <- mass
  }

  # Map adduct
  if ("adduct_ions" %in% names(sml_data)) {
    result$candidate_adduct <- .parse_mztab_adduct(sml_data$adduct_ions)
  }

  # Map reliability/confidence as score
  if ("reliability" %in% names(sml_data)) {
    # mzTab-M reliability: 1 (highest) to 4 (lowest)
    # Convert to 0-1 scale: 1→1.0, 2→0.75, 3→0.5, 4→0.25
    reliability <- as.numeric(gsub(
      "[^0-9]",
      "",
      as.character(sml_data$reliability)
    ))
    result$candidate_score_reliability <- ifelse(
      !is.na(reliability) & reliability >= 1 & reliability <= 4,
      (5 - reliability) / 4,
      NA_real_
    )
  }

  # Map search engine scores if available
  score_cols <- grep("^search_engine_score", names(sml_data), value = TRUE)
  if (length(score_cols) > 0) {
    # Use first score column as similarity score
    scores <- as.numeric(sml_data[[score_cols[1]]])
    result$candidate_score_similarity <- ifelse(
      !is.na(scores),
      scores,
      NA_real_
    )
  }

  # Map library/database source
  if ("database" %in% names(sml_data)) {
    result$candidate_library <- as.character(sml_data$database)
  } else {
    result$candidate_library <- "mzTab-M"
  }

  # Filter out rows with no structural information
  has_structure <- !is.na(result$candidate_structure_inchikey) |
    !is.na(result$candidate_structure_smiles_no_stereo) |
    !is.na(result$candidate_structure_inchi)

  result <- result[has_structure, , drop = FALSE]

  if (nrow(result) < nrow(sml_data)) {
    n_removed <- nrow(sml_data) - nrow(result)
    log_debug("Filtered %d molecules without structural identifiers", n_removed)
  }

  # Add placeholder columns for taxonomy (will be filled by select_annotations_columns)
  result$candidate_structure_tax_npc_01pat <- NA_character_
  result$candidate_structure_tax_npc_02sup <- NA_character_
  result$candidate_structure_tax_npc_03cla <- NA_character_
  result$candidate_structure_tax_cla_chemontid <- NA_character_
  result$candidate_structure_tax_cla_01kin <- NA_character_
  result$candidate_structure_tax_cla_02sup <- NA_character_
  result$candidate_structure_tax_cla_03cla <- NA_character_
  result$candidate_structure_tax_cla_04dirpar <- NA_character_
  result$candidate_structure_xlogp <- NA_real_

  return(result)
}
