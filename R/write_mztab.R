#' @title Write TIMA Annotations to mzTab-M Format
#'
#' @description Takes TIMA annotation results and writes them back into mzTab-M
#'     format, either updating an existing mzTab file or creating a new one.
#'     Annotations are added as additional columns to the Small Molecule table
#'     with TIMA scores, structural identifiers, and taxonomic classifications.
#'
#' @include mztab_utils.R
#' @include logs_utils.R
#' @include validations_utils.R
#'
#' @param annotations Character string path to TIMA annotation results (TSV/CSV)
#'     Typically the output from weight_annotations()
#' @param original_mztab Character string path to original mzTab-M file.
#'     If NULL, creates a new mzTab from annotations.
#' @param output Character string path for output mzTab-M file
#' @param top_n Integer number of top annotations per feature to include
#'     (default: 3). Set to NULL for all annotations.
#' @param score_threshold Numeric minimum score threshold for inclusion
#'     (default: 0). Only annotations with score >= threshold are included.
#'
#' @return Character string path to written mzTab-M file (invisibly)
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Write TIMA annotations back to mzTab-M
#' write_mztab(
#'   annotations = "data/output/tima_annotations.tsv",
#'   original_mztab = "data/source/experiment.mztab",
#'   output = "data/output/experiment_annotated.mztab",
#'   top_n = 3,
#'   score_threshold = 0.5
#' )
#' }
write_mztab <- function(
  annotations,
  original_mztab = NULL,
  output,
  top_n = 3,
  score_threshold = 0
) {
  # Start logging ----
  ctx <- log_operation(
    "write_mztab",
    input = annotations,
    has_template = !is.null(original_mztab),
    top_n = top_n,
    score_threshold = score_threshold
  )

  # Input Validation ----
  log_metadata(ctx, phase = "validation")

  validate_character(annotations, param_name = "annotations")
  validate_character(output, param_name = "output")

  if (!is.null(original_mztab)) {
    validate_character(original_mztab, param_name = "original_mztab")
  }

  if (!is.null(top_n)) {
    validate_numeric_range(
      top_n,
      min = 1,
      param_name = "top_n"
    )
  }

  validate_numeric_range(
    score_threshold,
    min = 0,
    max = 1,
    param_name = "score_threshold"
  )

  if (!file.exists(annotations)) {
    stop(
      "✗ Annotations file not found: ",
      annotations,
      "\n\n",
      "Fix: Verify the file path is correct",
      call. = FALSE
    )
  }

  # Load TIMA Annotations ----
  log_metadata(ctx, phase = "loading_annotations")

  ann_data <- safe_fread(
    file = annotations,
    file_type = "TIMA annotations",
    na.strings = c("", "NA")
  )

  log_debug("Loaded %d TIMA annotations", nrow(ann_data))

  # Filter and prepare annotations
  ann_filtered <- .filter_top_annotations(
    ann_data,
    top_n = top_n,
    score_threshold = score_threshold
  )

  log_debug(
    "Filtered to %d annotations (top_n=%s, threshold=%f)",
    nrow(ann_filtered),
    ifelse(is.null(top_n), "all", as.character(top_n)),
    score_threshold
  )

  # Load or Create mzTab Template ----
  log_metadata(ctx, phase = "preparing_mztab")

  if (!is.null(original_mztab) && file.exists(original_mztab)) {
    log_debug(
      "Loading original mzTab as template: %s",
      basename(original_mztab)
    )

    mztab_obj <- tryCatch(
      .parse_mztab_for_write(original_mztab),
      error = function(e) {
        log_warn(
          "Failed to load original mzTab, creating new: %s",
          conditionMessage(e)
        )
        NULL
      }
    )
  } else {
    log_debug("Creating new mzTab from annotations")
    mztab_obj <- NULL
  }

  # Merge Annotations into mzTab ----
  log_metadata(ctx, phase = "merging_annotations")

  mztab_updated <- .merge_tima_annotations_to_mztab(
    mztab_obj = mztab_obj,
    tima_annotations = ann_filtered
  )

  # Write mzTab File ----
  log_metadata(ctx, phase = "writing")

  .write_mztab_file(
    mztab_obj = mztab_updated,
    output_file = output
  )

  log_info("Wrote annotated mzTab file: %s", basename(output))

  # Complete ----
  log_complete(
    ctx,
    n_features_annotated = length(unique(ann_filtered$feature_id)),
    n_annotations = nrow(ann_filtered)
  )

  invisible(output)
}

#' Filter annotations to top N per feature
#'
#' @description Internal function to filter TIMA annotations to top N
#'     candidates per feature based on score.
#'
#' @param ann_data Data frame with TIMA annotations
#' @param top_n Integer or NULL for top N per feature
#' @param score_threshold Numeric minimum score threshold
#'
#' @return Filtered data frame
#'
#' @keywords internal
.filter_top_annotations <- function(ann_data, top_n, score_threshold) {
  # Identify score column - could be different names
  score_col <- NULL
  possible_score_cols <- c(
    "candidate_score_final",
    "score_final",
    "candidate_score_combined",
    "score_combined",
    "candidate_score_similarity"
  )

  for (col in possible_score_cols) {
    if (col %in% names(ann_data)) {
      score_col <- col
      break
    }
  }

  if (is.null(score_col)) {
    log_warn("No score column found, using all annotations")
    return(ann_data)
  }

  # Filter by threshold
  ann_filtered <- ann_data[
    !is.na(ann_data[[score_col]]) &
      ann_data[[score_col]] >= score_threshold,
  ]

  # Select top N per feature if requested
  if (!is.null(top_n)) {
    ann_filtered <- ann_filtered |>
      tidytable::arrange(feature_id, tidytable::desc(.data[[score_col]])) |>
      tidytable::group_by(feature_id) |>
      tidytable::slice_head(n = top_n) |>
      tidytable::ungroup()
  }

  return(ann_filtered)
}

#' Merge TIMA annotations into mzTab object
#'
#' @description Internal function to merge TIMA annotation results into
#'     mzTab-M Small Molecule table structure.
#'
#' @param mztab_obj mzTab object from RmzTabM or NULL to create new
#' @param tima_annotations Data frame with filtered TIMA annotations
#'
#' @return Updated mzTab object
#'
#' @keywords internal
.merge_tima_annotations_to_mztab <- function(mztab_obj, tima_annotations) {
  # Map TIMA columns to mzTab-M fields
  sml_new <- data.frame(
    SML_ID = as.character(tima_annotations$feature_id),
    stringsAsFactors = FALSE
  )

  # Add structural identifiers
  if ("candidate_structure_inchikey" %in% names(tima_annotations)) {
    sml_new$inchikey <- tima_annotations$candidate_structure_inchikey
  }

  if ("candidate_structure_inchi" %in% names(tima_annotations)) {
    sml_new$inchi <- tima_annotations$candidate_structure_inchi
  }

  if ("candidate_structure_smiles_no_stereo" %in% names(tima_annotations)) {
    sml_new$smiles <- tima_annotations$candidate_structure_smiles_no_stereo
  }

  if ("candidate_structure_molecular_formula" %in% names(tima_annotations)) {
    sml_new$chemical_formula <- tima_annotations$candidate_structure_molecular_formula
  }

  if ("candidate_structure_name" %in% names(tima_annotations)) {
    sml_new$chemical_name <- tima_annotations$candidate_structure_name
  }

  if ("candidate_structure_exact_mass" %in% names(tima_annotations)) {
    sml_new$theoretical_neutral_mass <- tima_annotations$candidate_structure_exact_mass
  }

  if ("candidate_adduct" %in% names(tima_annotations)) {
    # Convert TIMA adduct back to mzTab format
    sml_new$adduct_ions <- .tima_adduct_to_mztab(
      tima_annotations$candidate_adduct
    )
  }

  # Add TIMA scores as custom fields
  score_cols <- grep(
    "^(candidate_score_|score_)",
    names(tima_annotations),
    value = TRUE
  )

  for (col in score_cols) {
    new_col <- paste0("opt_tima_", col)
    sml_new[[new_col]] <- tima_annotations[[col]]
  }

  # Add TIMA taxonomy as custom fields
  tax_cols <- grep(
    "^candidate_structure_tax_",
    names(tima_annotations),
    value = TRUE
  )

  for (col in tax_cols) {
    new_col <- paste0("opt_tima_", col)
    sml_new[[new_col]] <- tima_annotations[[col]]
  }

  # Add reliability level based on TIMA score
  if ("candidate_score_final" %in% names(tima_annotations)) {
    score <- tima_annotations$candidate_score_final
    # Map score to reliability: >0.75→2a, >0.5→2b, >0.25→3, else→4
    sml_new$reliability <- ifelse(
      score >= 0.75,
      "2a",
      ifelse(score >= 0.5, "2b", ifelse(score >= 0.25, "3", "4"))
    )
  }

  # If we have original mzTab, merge with it
  if (!is.null(mztab_obj) && !is.null(mztab_obj$Small_Molecule)) {
    log_debug("Merging with existing mzTab Small Molecule table")

    # Keep original data that doesn't have TIMA annotations
    original_sml <- mztab_obj$Small_Molecule
    original_ids <- setdiff(
      unique(original_sml$SML_ID),
      unique(sml_new$SML_ID)
    )

    if (length(original_ids) > 0) {
      original_keep <- original_sml[original_sml$SML_ID %in% original_ids, ]
      sml_new <- tidytable::bind_rows(sml_new, original_keep)
    }

    mztab_obj$Small_Molecule <- sml_new
  } else {
    # Create minimal mzTab object
    log_debug("Creating new mzTab object with TIMA annotations")

    mztab_obj <- list(
      Metadata = data.frame(
        key = c("mzTab-version", "mzTab-id", "description"),
        value = c("2.0.0-M", "TIMA_export", "TIMA annotations"),
        stringsAsFactors = FALSE
      ),
      Small_Molecule = sml_new
    )
  }

  return(mztab_obj)
}

#' Convert TIMA adduct notation to mzTab-M format
#'
#' @description Converts TIMA adduct format to mzTab-M standard notation.
#'     TIMA format: "\[M+H\]+" → mzTab format: "\[M+H\]1+"
#'
#' @param adduct_string Character vector of TIMA adduct notations
#'
#' @return Character vector with mzTab-M adduct notations
#'
#' @keywords internal
.tima_adduct_to_mztab <- function(adduct_string) {
  if (is.null(adduct_string)) {
    return(NA_character_)
  }

  result <- vapply(
    adduct_string,
    function(x) {
      if (is.na(x) || nchar(x) == 0) {
        return(NA_character_)
      }

      # TIMA format: "[M+H]+" → add charge number: "[M+H]1+"
      x <- stringi::stri_replace_all_regex(
        x,
        pattern = "(\\])([+-])$",
        replacement = "$11$2"
      )

      return(as.character(x))
    },
    character(1),
    USE.NAMES = FALSE
  )

  return(result)
}

#' Parse mzTab-M file for use as template in write_mztab
#'
#' @description Uses the same custom line-by-line parsing approach as read_mztab,
#'     avoiding the RmzTabM dependency. Returns a structured list compatible
#'     with the rest of write_mztab.
#'
#' @param path Character string path to mzTab-M file
#'
#' @return List with Metadata, Small_Molecule, Small_Molecule_Evidence
#'
#' @keywords internal
.parse_mztab_for_write <- function(path) {
  lines <- readLines(path)

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
    data.frame(key = character(), value = character(), stringsAsFactors = FALSE)
  }

  # Parse small molecule table
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
    Small_Molecule = sml_data,
    Small_Molecule_Evidence = NULL
  )
}

#' Write mzTab object to file
#'
#' @description Internal function to write mzTab object to file in
#'     mzTab-M format.
#'
#' @param mztab_obj mzTab object
#' @param output_file Character string path for output
#'
#' @return Invisible NULL
#'
#' @keywords internal
.write_mztab_file <- function(mztab_obj, output_file) {
  # Create output directory if needed
  create_dir(export = output_file)

  tryCatch(
    {
      # Write using our custom TSV format implementation
      # (RmzTabM doesn't currently export writeMzTab function)
      .write_mztab_fallback(mztab_obj, output_file)
    },
    error = function(e) {
      log_error("Failed to write mzTab file: %s", conditionMessage(e))
      stop(
        "✗ Failed to write mzTab file\n\n",
        "Error: ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )

  invisible(NULL)
}

#' Fallback method to write mzTab as TSV
#'
#' @description Simple fallback to write mzTab structure as TSV format
#'     following mzTab-M specification.
#'
#' @param mztab_obj mzTab object
#' @param output_file Character string path for output
#'
#' @return Invisible NULL
#'
#' @keywords internal
.write_mztab_fallback <- function(mztab_obj, output_file) {
  lines <- character()

  # Comment header
  lines <- c(lines, "COM\tGenerated by TIMA", "")

  # Write metadata section (MTD) - vectorized
  if (!is.null(mztab_obj$Metadata) && nrow(mztab_obj$Metadata) > 0) {
    mtd <- mztab_obj$Metadata
    mtd_lines <- paste("MTD", mtd[[1]], mtd[[2]], sep = "\t")
    lines <- c(lines, mtd_lines, "")
  }

  # Write Small Molecule table (SML) - vectorized
  if (!is.null(mztab_obj$Small_Molecule) && nrow(mztab_obj$Small_Molecule) > 0) {
    sml <- mztab_obj$Small_Molecule

    # Header
    lines <- c(lines, paste("SMH", paste(names(sml), collapse = "\t"), sep = "\t"))

    # Data rows - vectorized: build all rows at once
    sml_mat <- as.matrix(sml)
    sml_mat[is.na(sml_mat)] <- "null"
    row_strings <- apply(sml_mat, 1, paste, collapse = "\t")
    sml_lines <- paste("SML", row_strings, sep = "\t")
    lines <- c(lines, sml_lines)
  }

  # Write all lines at once
  writeLines(lines, output_file)

  invisible(NULL)
}
