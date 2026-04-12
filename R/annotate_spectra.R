#' @title Annotate spectra
#'
#' @description Annotates MS/MS query spectra against one or more spectral
#'     libraries, computing similarity scores and returning best candidate
#'     annotations above a similarity threshold.
#'
#' @details This is an orchestration wrapper that performs:
#'   1. Input validation & normalization (query + libraries, numeric params).
#'   2. Query spectra import & light preprocessing (intensity cutoff).
#'   3. Library spectra import, cleaning of empty peak lists, optional polarity
#'      filtering, optional precursor-based library size reduction (when
#'      `approx = FALSE`).
#'   4. Similarity computation via `calculate_entropy_and_similarity()`.
#'   5. Candidate metadata extraction (formula, name, etc.).
#'   6. Result shaping: derive error (mz), select canonical output columns,
#'      threshold filtering, keep best per (feature_id, library,
#'      connectivity layer).
#'   7. Export of parameters & results to the configured output path.
#'
#' If no annotations are produced (empty inputs or below threshold), a
#' standardized empty template (see `fake_annotations_columns()`) is exported
#' to ensure downstream code receives expected columns.
#'
#' @section Robustness:
#'   The function performs strict validation and logs informative messages.
#'   File existence is checked early; similarity computation is wrapped in a
#'   `tryCatch` to surface errors without leaving partially allocated objects.
#'
#' @section Performance:
#'   Library precursor reduction (when `approx = FALSE`) limits similarity
#'   computation to precursor-tolerant spectra, reducing complexity for large
#'   libraries. Repeated metadata extraction uses a single vectorized helper.
#'
#' @include adducts_utils.R
#' @include columns_utils.R
#' @include logs_utils.R
#' @include validations_utils.R
#'
#' @param input [character] Vector or list of query spectral file paths (.mgf).
#' @param libraries [character] Vector or list of library spectral file paths
#'   (.mgf / Spectra-supported). Must contain at least one path.
#' @param polarity [character] MS polarity; one of `VALID_MS_MODES` ("pos", "neg").
#' @param output [character] Output file path (the function writes a tabular file here).
#' @param method [character] Similarity method; one of `VALID_SIMILARITY_METHODS`.
#' @param threshold [numeric] Minimal similarity score to retain candidates (0-1).
#' @param ppm [numeric] Relative mass tolerance (ppm) for MS/MS matching.
#' @param dalton [numeric] Absolute mass tolerance (Daltons) for MS/MS matching.
#' @param cutoff [numeric] Intensity cutoff under which MS2 fragments are removed.
#'     Non-negative numeric or NULL for dynamic thresholding.
#' @param qutoff `r lifecycle::badge("deprecated")` Use `cutoff` instead.
#' @param approx [logical] If TRUE perform matching ignoring precursor masses
#'     (broader, slower); if FALSE restrict library to precursor-tolerant
#'     spectra first.
#'
#' @return Character scalar: the output file path (invisible). Side effect:
#'     writes the annotations table to `output`.
#'
#' @family annotation
#'
#' @export
#'
#' @examples
#' \dontrun{
#' copy_backbone()
#' go_to_cache()
#' get_file(
#'   url = get_default_paths()$urls$examples$spectra_mini,
#'   export = get_params(step = "annotate_spectra")$files$spectral$raw
#' )
#' get_file(
#'   url = get_default_paths()$urls$examples$spectral_lib_mini$with_rt,
#'   export = get_default_paths()$data$source$libraries$spectra$exp$with_rt
#' )
#' annotate_spectra(
#'   libraries = get_default_paths()$data$source$libraries$spectra$exp$with_rt
#' )
#' unlink("data", recursive = TRUE)
#' }
annotate_spectra <- function(
  input = get_params(step = "annotate_spectra")$files$spectral$raw,
  libraries = get_params(step = "annotate_spectra")$files$libraries$spectral,
  polarity = get_params(step = "annotate_spectra")$ms$polarity,
  output = get_params(
    step = "annotate_spectra"
  )$files$annotations$raw$spectral$spectral,
  method = get_params(
    step = "annotate_spectra"
  )$similarities$methods$annotations,
  threshold = get_params(
    step = "annotate_spectra"
  )$similarities$thresholds$annotations,
  ppm = get_params(step = "annotate_spectra")$ms$tolerances$mass$ppm$ms2,
  dalton = get_params(step = "annotate_spectra")$ms$tolerances$mass$dalton$ms2,
  cutoff = get_params(step = "annotate_spectra")$ms$thresholds$ms2$intensity,
  approx = get_params(step = "annotate_spectra")$annotations$ms2approx,
  qutoff = deprecated()
) {
  # Handle deprecated qutoff parameter
  if (lifecycle::is_present(qutoff)) {
    lifecycle::deprecate_warn(
      "2.13.0",
      "annotate_spectra(qutoff)",
      "annotate_spectra(cutoff)"
    )
    cutoff <- qutoff
  }

  params <- get_params(step = "annotate_spectra")
  output_path <- resolve_annotation_output(output)

  assert_choice(polarity, VALID_MS_MODES, "polarity")
  assert_choice(method, VALID_SIMILARITY_METHODS, "method")
  assert_flag(approx, "approx")
  assert_scalar_numeric(threshold, "threshold", min = 0, max = 1)
  assert_scalar_numeric(ppm, "ppm", min = 0)
  assert_scalar_numeric(dalton, "dalton", min = 0)
  if (!is.null(cutoff) && (!is.numeric(cutoff) || cutoff < 0)) {
    cli::cli_abort(
      "{.arg cutoff} must be non-negative or NULL, got {.val {cutoff}}",
      class = "tima_validation_error"
    )
  }

  input_vec <- normalize_input_files(input, "Input")
  libs_vec <- normalize_input_files(libraries, "Library")
  if (length(libs_vec) == 0L) {
    cli::cli_abort(
      "at least one library must be provided",
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }

  # Pre-flight data sanitizing for MGF files
  log_debug("Running pre-flight checks on input MGF files...")
  invisible(vapply(
    X = input_vec,
    FUN = function(mgf) {
      sanitize_all_inputs(mgf_file = mgf)
      TRUE
    },
    FUN.VALUE = logical(1L)
  ))

  missing_in <- input_vec[!file.exists(input_vec)]
  if (length(missing_in)) {
    cli::cli_abort(
      c(
        "input file(s) not found",
        "x" = paste(missing_in, collapse = ", ")
      ),
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }
  missing_lib <- libs_vec[!file.exists(libs_vec)]
  if (length(missing_lib)) {
    cli::cli_abort(
      c(
        "library file(s) not found",
        "x" = paste(missing_lib, collapse = ", ")
      ),
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }

  log_info("Starting spectral annotation in %s mode", polarity)
  log_debug(
    "Method: %s, Threshold: %.2f, PPM: %.2f, Dalton: %.2f",
    method,
    threshold,
    ppm,
    dalton
  )

  query_sp <- import_spectra(
    input_vec,
    cutoff = cutoff,
    dalton = dalton,
    polarity = polarity,
    ppm = ppm
  )
  if (length(query_sp) == 0L) {
    return(
      annotate_spectra_handle_empty_result(
        output_path,
        params,
        "No query spectra loaded"
      )
    )
  }

  libs_vec <- filter_library_paths_by_polarity(libs_vec, polarity)
  if (length(libs_vec) == 0L) {
    return(
      annotate_spectra_handle_empty_result(
        output_path,
        params,
        "No libraries remain after polarity filtering"
      )
    )
  }

  spectral_library <- import_and_clean_library_collection(
    libs_vec,
    dalton = dalton,
    polarity = polarity,
    ppm = ppm
  )
  if (length(spectral_library) == 0L) {
    return(
      annotate_spectra_handle_empty_result(
        output_path,
        params,
        "No spectra left in library after cleaning"
      )
    )
  }

  log_library_stats(spectral_library)

  query_precursors <- get_precursors(query_sp)
  if (!approx) {
    spectral_library <- reduce_library_by_precursor(
      lib_sp = spectral_library,
      query_precursors = query_precursors,
      dalton = dalton,
      ppm = ppm
    )
    if (length(spectral_library) == 0L) {
      return(
        annotate_spectra_handle_empty_result(
          output_path,
          params,
          "No spectra remain after precursor-based reduction"
        )
      )
    }
  }

  log_debug(
    "Annotating %d query spectra against %d library references",
    length(query_sp@backend@peaksData),
    length(spectral_library)
  )

  sim_raw <- compute_similarity_safe(
    lib_sp = spectral_library,
    query_sp = query_sp,
    method = method,
    dalton = dalton,
    ppm = ppm,
    threshold = threshold,
    approx = approx
  )
  if (nrow(sim_raw) == 0L) {
    return(
      annotate_spectra_handle_empty_result(
        output_path,
        params,
        "Similarity computation returned no candidates"
      )
    )
  }

  lib_precursors <- get_precursors(spectral_library)
  meta <- build_library_metadata(spectral_library, lib_precursors)
  df_final <- finalize_results(
    df_sim = tidytable::as_tidytable(x = sim_raw),
    meta = meta,
    threshold = threshold
  )
  if (nrow(df_final) == 0L) {
    return(
      annotate_spectra_handle_empty_result(
        output_path,
        params,
        sprintf(
          "All candidates fell below threshold (%s) or were duplicates",
          threshold
        )
      )
    )
  }

  # Log distribution of annotation scores by 0.1 bins before export (0-0.1, ..., 0.9-1.0)
  if (
    nrow(df_final) > 0L && "candidate_score_similarity" %in% names(df_final)
  ) {
    log_similarity_distribution(
      scores = df_final$candidate_score_similarity,
      title = "Here is the distribution of annotation similarity scores (0.1 bins):"
    )
  }

  log_info(
    "%d Candidates annotated on %d features (threshold >= %s).",
    nrow(
      df_final |>
        tidytable::distinct(
          candidate_structure_smiles_no_stereo
        )
    ),
    nrow(df_final |> tidytable::distinct(feature_id)),
    threshold
  )

  persist_annotation_parameters(params)
  export_output(x = df_final, file = output_path)
  invisible(output_path)
}

#' @keywords internal
resolve_annotation_output <- function(output) {
  if (!is.character(output)) {
    cli::cli_abort(
      "output path must be a character string",
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }
  if (length(output) == 0L) {
    cli::cli_abort(
      "output must contain at least one file path",
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }
  output[[1L]]
}

#' @keywords internal
normalize_input_files <- function(x, label) {
  if (is.list(x)) {
    x <- unlist(x, use.names = FALSE)
  }
  if (!is.character(x)) {
    cli::cli_abort(
      paste0(label, " elements must be character strings"),
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }
  x
}

#' @keywords internal
annotate_spectra_return_empty_template <- function(path) {
  log_warn("Returning empty annotation template")
  empty <- fake_annotations_columns()
  export_output(x = empty, file = path)
  invisible(path)
}

#' @keywords internal
persist_annotation_parameters <- function(params) {
  export_params(parameters = params, step = "annotate_spectra")
  invisible(NULL)
}

#' @keywords internal
annotate_spectra_handle_empty_result <- function(path, params, message = NULL) {
  if (!is.null(message)) {
    log_warn(message)
  }
  persist_annotation_parameters(params)
  annotate_spectra_return_empty_template(path)
}

#' @keywords internal
filter_library_paths_by_polarity <- function(paths, polarity) {
  if (length(paths) <= 1L) {
    return(paths)
  }
  filtered <- paths[grepl(polarity, paths, fixed = TRUE)]
  if (length(filtered) < length(paths)) {
    log_debug(
      "Filtered libraries by polarity: %d -> %d",
      length(paths),
      length(filtered)
    )
  }
  filtered
}

#' @keywords internal
import_and_clean_library_collection <- function(paths, dalton, polarity, ppm) {
  paths |>
    purrr::map(
      import_spectra,
      cutoff = 0,
      dalton = dalton,
      polarity = polarity,
      ppm = ppm,
      sanitize = FALSE,
      combine = FALSE
    ) |>
    purrr::map(
      Spectra::applyProcessing,
      BPPARAM = BiocParallel::SerialParam()
    ) |>
    purrr::map(.f = remove_empty_peak_spectra) |>
    Spectra::concatenateSpectra()
}

remove_empty_peak_spectra <- function(sp) {
  has_peaks <- !vapply(
    sp@backend@peaksData,
    is.null,
    logical(1)
  )
  sp[has_peaks]
}

extract_vector <- function(obj, field, len, fill = NA) {
  spectra_data <- obj@backend@spectraData
  if (!length(field)) {
    return(rep(fill, len))
  }

  # Try exact names first (in priority order), then case-insensitive aliases.
  idx_exact <- match(field, names(spectra_data), nomatch = 0L)
  idx_exact <- idx_exact[idx_exact > 0L]
  if (!length(idx_exact)) {
    idx_ci <- match(tolower(field), tolower(names(spectra_data)), nomatch = 0L)
    idx_ci <- idx_ci[idx_ci > 0L]
    if (!length(idx_ci)) {
      return(rep(fill, len))
    }
    idx <- idx_ci[[1L]]
  } else {
    idx <- idx_exact[[1L]]
  }

  v <- spectra_data[[idx]]
  if (is.null(v)) {
    return(rep(fill, len))
  }
  if (length(v) == len) {
    return(v)
  }
  if (length(v) == 1L) {
    return(rep(v, len))
  }
  if (length(v) < len) {
    return(c(v, rep(fill, len - length(v))))
  }
  v[seq_len(len)]
}

#' @keywords internal
build_library_metadata <- function(lib_sp, lib_precursors) {
  n <- length(lib_sp)
  tidytable::tidytable(
    target_id = seq_len(n),
    target_adduct = extract_vector(
      lib_sp,
      c("adduct", "precursor_type"),
      n,
      NA_character_
    ),
    target_spectrum_id = extract_vector(
      lib_sp,
      c("spectrum_id", "spectrumid", "id"),
      n,
      NA_character_
    ),
    target_smiles = extract_vector(
      lib_sp,
      c("smiles", "structure_smiles", "SMILES"),
      n,
      NA_character_
    ),
    target_smiles_no_stereo = extract_vector(
      lib_sp,
      c(
        "smiles_no_stereo",
        "structure_smiles_no_stereo",
        "smiles_2D",
        "smiles2D",
        "structure_smiles_2D"
      ),
      n,
      NA_character_
    ),
    target_library = extract_vector(
      lib_sp,
      c("library", "source_library", "database"),
      n,
      NA_character_
    ),
    target_name = extract_vector(
      lib_sp,
      c("name", "structure_name", "compound_name", "compoundName", "NAME"),
      n,
      NA_character_
    ),
    target_tag = extract_vector(
      lib_sp,
      c("tag", "structure_tag"),
      n,
      NA_character_
    ),
    target_precursorMz = lib_precursors
  ) |>
    harmonize_adducts(
      adducts_colname = "target_adduct",
      adducts_translations = adducts_translations
    )
}

#' @keywords internal
get_precursors <- function(sp) {
  sp@backend@spectraData |>
    tidytable::transmute(
      precursor = tidytable::coalesce(
        tidytable::across(
          .cols = tidyselect::any_of(
            x = c(
              "precursorMz",
              "precursor_mz"
            )
          )
        )
      )
    ) |>
    tidytable::pull()
}

#' @keywords internal
reduce_library_by_precursor <- function(
  lib_sp,
  query_precursors,
  dalton,
  ppm
) {
  lib_prec <- get_precursors(lib_sp)
  minimal <- pmin(lib_prec - dalton, lib_prec * (1 - (1E-6 * ppm)))
  maximal <- pmax(lib_prec + dalton, lib_prec * (1 + (1E-6 * ppm)))
  df_idx <- dplyr::inner_join(
    x = tidytable::tidytable(minimal, maximal, lib_precursors = lib_prec),
    y = tidytable::tidytable(val = unique(query_precursors)),
    by = dplyr::join_by(minimal <= val, maximal >= val)
  ) |>
    tidytable::distinct(lib_precursors, .keep_all = TRUE)
  lib_sp[lib_prec %in% df_idx$lib_precursors]
}

#' @keywords internal
compute_similarity_safe <- function(
  lib_sp,
  query_sp,
  method,
  dalton,
  ppm,
  threshold,
  approx
) {
  query_prec <- query_sp@backend@spectraData$precursorMz
  lib_prec <- get_precursors(lib_sp)
  tryCatch(
    calculate_entropy_and_similarity(
      lib_ids = seq_along(lib_sp),
      lib_precursors = lib_prec,
      lib_spectra = lib_sp@backend@peaksData,
      query_ids = get_spectra_ids(query_sp),
      query_precursors = query_prec,
      query_spectra = query_sp@backend@peaksData,
      method = method,
      dalton = dalton,
      ppm = ppm,
      threshold = threshold,
      approx = approx
    ),
    error = function(e) {
      log_error("Similarity computation failed: %s", conditionMessage(e))
      tidytable::tidytable()
    }
  )
}

#' @keywords internal
finalize_results <- function(
  df_sim,
  meta,
  threshold
) {
  if (nrow(df_sim) == 0) {
    return(tidytable::tidytable())
  }
  df_sim$candidate_spectrum_entropy <- as.numeric(
    df_sim$candidate_spectrum_entropy
  )
  df_sim$candidate_score_similarity <- as.numeric(
    df_sim$candidate_score_similarity
  )
  df_sim$candidate_count_similarity_peaks_matched <- as.integer(
    df_sim$candidate_count_similarity_peaks_matched
  )
  df_final <- df_sim |>
    tidytable::left_join(y = meta, by = "target_id") |>
    tidytable::select(-target_id) |>
    tidytable::mutate(
      candidate_structure_error_mz = target_precursorMz - precursorMz,
      ## SMILES is the single source of truth for structure identity.
      ## All structural identifiers (InChIKey, formula, mass, xlogp)
      ## are strictly recomputed from SMILES via process_smiles()
      ## downstream in select_annotations_columns().
      candidate_structure_smiles_no_stereo = tidytable::coalesce(
        target_smiles_no_stereo,
        target_smiles
      )
    ) |>
    tidytable::select(
      tidyselect::any_of(
        x = c(
          "feature_id",
          "candidate_adduct" = "target_adduct",
          "candidate_library" = "target_library",
          "candidate_spectrum_id" = "target_spectrum_id",
          "candidate_structure_error_mz",
          "candidate_structure_name" = "target_name",
          "candidate_structure_smiles_no_stereo",
          "candidate_spectrum_entropy",
          "candidate_score_similarity",
          "candidate_count_similarity_peaks_matched"
        )
      )
    ) |>
    tidytable::arrange(tidytable::desc(x = candidate_score_similarity)) |>
    tidytable::distinct(
      feature_id,
      candidate_library,
      candidate_structure_smiles_no_stereo,
      .keep_all = TRUE
    )
  df_final
}

log_library_stats <- function(lib_sp) {
  # Use backend spectraData directly to avoid materializing peak data
  sd <- lib_sp@backend@spectraData
  if (!"library" %in% names(sd)) {
    return(invisible(NULL))
  }
  df <- sd |>
    data.frame() |>
    tidytable::filter(!is.na(library))

  # Build a reliable structure identity column using coalesce
  id_cols <- intersect(
    c("smiles_no_stereo", "smiles", "inchikey", "compound_id"),
    names(df)
  )
  if (length(id_cols) > 0L) {
    df <- df |>
      tidytable::mutate(
        .structure_id = tidytable::coalesce(
          !!!rlang::syms(id_cols)
        )
      )
  } else {
    df$.structure_id <- NA_character_
  }

  stats <- df |>
    tidytable::group_by(library) |>
    tidytable::summarise(
      spectra = tidytable::n(),
      unique_structures = tidytable::n_distinct(
        .structure_id,
        na.rm = TRUE
      )
    ) |>
    tidytable::arrange(tidytable::desc(x = spectra))
  log_info(
    "\n%s",
    paste(
      utils::capture.output(print.data.frame(x = stats, row.names = FALSE)),
      collapse = "\n"
    )
  )
  invisible(NULL)
}
