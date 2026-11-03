##' @description
##'
##' Extract **all** fields from the MGF eventually renaming the field names to
##' the spectra variable names specified with `mapping`.
##'
#' @include logs_utils.R
#' @param mgf [character] `character()` of lines defining a spectrum in mgf
#'     format.
##'
##' @author Laurent Gatto, Johannes Rainer
##'
##' @noRd
.extract_mgf_spectrum <- function(mgf) {
  r <- regexpr("=", mgf, fixed = TRUE)
  desc.idx <- which(r > 0L) # nolint: object_name_linter.
  desc <- mgf[desc.idx]
  peak_lines <- if (length(desc.idx)) mgf[-desc.idx] else mgf
  if (!length(peak_lines)) {
    return(NULL)
  }

  spec <- strsplit(peak_lines, "[[:space:]]+", perl = TRUE)
  # Remove empty entries from blank lines (strsplit("") returns character(0))
  spec <- spec[lengths(spec) > 0L]
  if (!length(spec) || length(spec[[1L]]) == 1L) {
    return(NULL)
  }
  ms <- matrix(
    as.double(unlist(spec, use.names = FALSE, recursive = FALSE)),
    ncol = length(spec[[1L]]),
    byrow = TRUE
  )

  if (.is_unsorted(ms)) {
    ms <- ms[order(ms[, 1L]), , drop = FALSE]
  }

  r_desc <- r[desc.idx]
  desc <- stats::setNames(
    substring(desc, r_desc + 1L),
    substring(desc, 1L, r_desc - 1L)
  )

  desc[c("PEPMASS", "PEPMASSINT")] <-
    strsplit(desc["PEPMASS"], "[[:space:]]+", perl = TRUE)[[1L]][c(1L, 2L)]

  list(
    desc = desc,
    mz = ms[, 1L],
    intensity = ms[, 2L]
  )
}

.is_unsorted <- function(x) {
  nrow(x) && is.unsorted(x[, 1L])
}

#' Format MGF charge string into an integer compatible format.
#'
#' Handles both the trailing-sign convention used by Mascot (`"1+"`, `"1-"`)
#' and the leading-sign convention used by some tools (`"-1"`, `"1"`).
#'
#' @param x [character] `character`
#' @return `character`, charge without +/- at the end but - as prefix if needed
#' @noRd
.format_charge <- function(x) {
  # Detect negative before stripping: trailing "-" OR leading "-"
  negs <- which(endsWith(x, "-") | startsWith(x, "-"))
  # Strip any leading or trailing +/-
  res <- gsub("(^[+-])|([+-]$)", "", x, perl = TRUE)
  if (length(negs)) {
    res[negs] <- paste0("-", res[negs])
  }
  res
}

#' @title Read MGF opti
#'
#' @description This function reads a Mascot Generic Format (MGF) file using
#'     a fast, vectorized approach. It mimics the `MsBackendMgf` implementation
#'     while avoiding per-spectrum `data.frame` allocations and quadratic
#'     row-binding overhead.
#'
#' @details The function extracts all MGF fields and maps them to standard
#'     spectra variable names.
#'
#' @param f [character] Character string specifying the path to a single MGF
#'     file
#' @param msLevel [integer] Integer MS level to assign to spectra (default: 2L
#'     for MS2)
#' @param mapping [character] Named character vector mapping MGF field names to
#'     standard spectra variable names.
#'
#' @return A DataFrame containing the parsed spectra data with standardized
#'     variable names
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # Read MGF file with memory-efficient approach
#' library(Spectra)
#'
#' spectra_data <- read_mgf_opti(
#'   f = "path/to/spectra.mgf",
#'   msLevel = 2L
#' )
#'
#' # Create Spectra object from result
#' sps <- Spectra(spectra_data)
#' }
read_mgf_opti <- function(
  f,
  msLevel = 2L, # nolint: object_name_linter. Bioconductor convention.
  mapping = c(
    rtime = "RTINSECONDS",
    acquisitionNum = "SCANS",
    precursorMz = "PEPMASS",
    precursorIntensity = "PEPMASSINT",
    precursorCharge = "CHARGE"
  )
) {
  # Validate inputs
  if (missing(f) || is.null(f) || length(f) == 0L) {
    cli::cli_abort(
      "file path 'f' must be provided",
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }

  if (length(f) != 1L) {
    cli::cli_abort(
      "please provide a single MGF file. for multiple files, call this function separately for each.",
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }

  if (!file.exists(f)) {
    cli::cli_abort(
      c(
        "MGF file not found",
        "x" = f
      ),
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }

  if (!is.numeric(msLevel) || msLevel < 1L) {
    cli::cli_abort(
      c(
        "msLevel must be a positive integer",
        "x" = as.character(msLevel)
      ),
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }

  # Get file size for progress reporting
  file_size_mb <- file.info(f)$size / 1024^2
  log_info(
    "Reading MGF file (%s MB) with optimized parser: %s",
    round(file_size_mb, 2),
    f
  )
  log_debug("Using MS level: %d", msLevel)

  lines <- readLines(f, warn = FALSE)
  begins <- which(lines == "BEGIN IONS")
  ends <- which(lines == "END IONS")
  n_blocks <- min(length(begins), length(ends))

  log_info("Total spectra read: %s", n_blocks)

  sp_list <- vector("list", n_blocks)
  for (i in seq_len(n_blocks)) {
    s <- begins[i] + 1L
    e <- ends[i] - 1L
    if (s <= e) {
      sp_list[[i]] <- .extract_mgf_spectrum(lines[s:e])
    }
  }
  rm(lines)
  sp_list <- Filter(Negate(is.null), sp_list)

  if (length(sp_list) == 0L) {
    log_warn(
      "No spectra with fragment peaks found in file; returning empty result"
    )
    empty <- methods::as(
      data.frame(
        TITLE = character(0L),
        precursorMz = numeric(0L),
        precursorIntensity = numeric(0L),
        stringsAsFactors = FALSE
      ),
      Class = "DataFrame"
    )
    empty$mz <- methods::as(list(), Class = "NumericList")
    empty$intensity <- methods::as(list(), Class = "NumericList")
    empty$msLevel <- integer(0L)
    empty$dataOrigin <- character(0L)
    return(empty)
  }

  desc_list <- lapply(sp_list, `[[`, "desc")
  desc_lens <- lengths(desc_list)
  all_vals <- unlist(desc_list, use.names = FALSE)
  all_nms <- unlist(lapply(desc_list, names), use.names = FALSE)
  all_keys <- unique(all_nms)
  n_sp <- length(sp_list)
  n_keys <- length(all_keys)
  if (n_keys > 0L) {
    spec_idx <- rep.int(seq_len(n_sp), desc_lens)
    key_idx <- fastmatch::fmatch(all_nms, all_keys)
    flat_pos <- spec_idx + (key_idx - 1L) * n_sp
    first_occ <- !duplicated(flat_pos)
    mat <- matrix(NA_character_, nrow = n_sp, ncol = n_keys)
    mat[flat_pos[first_occ]] <- all_vals[first_occ]
    colnames(mat) <- all_keys
    res <- as.data.frame(mat, stringsAsFactors = FALSE, check.names = FALSE)
  } else {
    res <- data.frame(row.names = seq_len(n_sp))
  }
  res$mz <- lapply(sp_list, `[[`, "mz")
  res$intensity <- lapply(sp_list, `[[`, "intensity")
  rm(sp_list, desc_list)

  # Format charge field if present
  if ("CHARGE" %in% colnames(res)) {
    res$CHARGE <- .format_charge(res$CHARGE)
  }

  # Map MGF field names to standard spectra variable names
  idx <- match(colnames(res), mapping)
  not_na <- !is.na(idx)
  if (any(not_na)) {
    colnames(res)[not_na] <- names(mapping)[idx][not_na]
  }

  # Convert column types to match core spectra variables
  spv <- Spectra::coreSpectraVariables()
  spv <- spv[!names(spv) %in% c("mz", "intensity")]

  col_names <- colnames(res)
  spv_names <- names(spv)

  col_matches <- match(col_names, spv_names, nomatch = 0L)
  needs_conversion <- col_matches > 0L
  if (any(needs_conversion)) {
    res[needs_conversion] <- Map(
      function(col, type_class) methods::as(object = col, Class = type_class),
      res[needs_conversion],
      spv[col_matches[needs_conversion]]
    )
  }

  # Convert to DataFrame and set up peak lists
  res <- methods::as(object = res, Class = "DataFrame")
  res$mz <- methods::as(object = res$mz, Class = "NumericList")
  res$intensity <- methods::as(object = res$intensity, Class = "NumericList")
  res$dataOrigin <- f

  if (!"msLevel" %in% colnames(res)) {
    res$msLevel <- as.integer(msLevel)
  }

  res
}

#' Read the metadata of an MGF file
#'
#' @description Reads only the `KEY=VALUE` fields of each `BEGIN IONS` /
#'     `END IONS` block. Peaks are not parsed and no spectrum is dropped, so
#'     entries without fragments or without `PEPMASS` (common in retention
#'     time libraries) are kept.
#'
#' @param f [character] Path to a single MGF file.
#'
#' @return A tidytable with one row per block and one character column per
#'     key. Keys are upper-cased; the first value wins when a key repeats.
#'
#' @keywords internal
read_mgf_metadata <- function(f) {
  if (length(f) != 1L || !file.exists(f)) {
    cli::cli_abort(
      c("MGF file not found", "x" = as.character(f)),
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }

  lines <- trimws(readLines(f, warn = FALSE))
  is_begin <- lines == "BEGIN IONS"
  is_end <- lines == "END IONS"
  block <- cumsum(is_begin)
  inside <- block > cumsum(is_end)
  is_field <- inside & !is_begin & grepl("=", lines, fixed = TRUE)

  if (!any(is_field)) {
    return(tidytable::tidytable())
  }

  fields <- lines[is_field]
  eq <- regexpr("=", fields, fixed = TRUE)
  tidytable::tidytable(
    .block = block[is_field],
    key = toupper(trimws(substring(fields, 1L, eq - 1L))),
    value = trimws(substring(fields, eq + 1L))
  ) |>
    tidytable::distinct(.block, key, .keep_all = TRUE) |>
    tidytable::pivot_wider(names_from = key, values_from = value) |>
    tidytable::select(-.block)
}
