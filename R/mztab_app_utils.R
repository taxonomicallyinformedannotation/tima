#' mzTab app save utilities
#'
#' @description Internal helpers used by `inst/app.R` to decide whether an
#' uploaded mzTab should be copied, reused, or expanded into TIMA source files.
#' @include logs_utils.R
#' @keywords internal
#' @name mztab_app_utils
NULL

#' Plan mzTab-derived save outputs for the app.
#'
#' @param mztab_path `character(1)` Path to the copied mzTab file in
#'   `data/source`.
#' @param data_source_path `character(1)` TIMA source directory.
#' @param features_path `character(1) | NULL` Existing feature file path, if any.
#' @param spectra_path `character(1) | NULL` Existing spectra file path, if any.
#' @param metadata_path `character(1) | NULL` Existing metadata file path, if any.
#'
#' @return A list with:
#'   - `resolved`: final paths to use, preferring explicit files, then cached
#'     mzTab-derived files if present.
#'   - `write`: paths that still need to be generated from the mzTab.
#'   - `needs_read`: whether `read_mztab()` must run.
#'   - `all_cached`: whether all missing outputs already exist on disk.
#' @keywords internal
plan_mztab_app_outputs <- function(
  mztab_path,
  data_source_path,
  features_path = NULL,
  spectra_path = NULL,
  metadata_path = NULL
) {
  if (is.null(mztab_path) || !nzchar(mztab_path)) {
    return(list(
      resolved = list(
        features = features_path,
        spectra = spectra_path,
        metadata = metadata_path
      ),
      write = list(features = NULL, spectra = NULL, metadata = NULL),
      needs_read = FALSE,
      all_cached = FALSE,
      outputs = list(features = NULL, spectra = NULL, metadata = NULL)
    ))
  }

  stem <- tools::file_path_sans_ext(basename(mztab_path))
  outputs <- list(
    features = file.path(data_source_path, paste0(stem, "_features.tsv")),
    spectra = file.path(data_source_path, paste0(stem, "_spectra.mgf")),
    metadata = file.path(data_source_path, paste0(stem, "_metadata.tsv"))
  )

  resolved <- list(
    features = features_path %||%
      if (file.exists(outputs$features)) {
        outputs$features
      } else {
        NULL
      },
    spectra = spectra_path %||%
      if (file.exists(outputs$spectra)) {
        outputs$spectra
      } else {
        NULL
      },
    metadata = metadata_path %||%
      if (file.exists(outputs$metadata)) {
        outputs$metadata
      } else {
        NULL
      }
  )

  write <- list(
    features = if (is.null(features_path) && !file.exists(outputs$features)) {
      outputs$features
    } else {
      NULL
    },
    spectra = if (is.null(spectra_path) && !file.exists(outputs$spectra)) {
      outputs$spectra
    } else {
      NULL
    },
    metadata = if (is.null(metadata_path) && !file.exists(outputs$metadata)) {
      outputs$metadata
    } else {
      NULL
    }
  )

  needs_read <- any(vapply(write, Negate(is.null), logical(1L)))
  all_cached <- !needs_read && !is.null(mztab_path)

  list(
    resolved = resolved,
    write = write,
    needs_read = needs_read,
    all_cached = all_cached,
    outputs = outputs
  )
}
