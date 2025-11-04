##' @description
##'
##' Extract **all** fields from the MGF eventually renaming the field names to
##' the spectra variable names specified with `mapping`.
##'
##' @param mgf `character()` of lines defining a spectrum in mgf
##'     format.
##'
##' @author Laurent Gatto, Johannes Rainer
##'
##' @noRd
.extract_mgf_spectrum <- function(mgf) {
  ## grep description
  desc.idx <- grep("=", mgf, fixed = TRUE)
  desc <- mgf[desc.idx]

  spec <- strsplit(mgf[-desc.idx], "[[:space:]]+", perl = TRUE)
  if (!length(spec) || length(spec[[1L]]) == 1L) {
    ms <- matrix(numeric(), ncol = 2L)
  } else {
    ms <- matrix(
      as.double(unlist(spec, use.names = FALSE, recursive = FALSE)),
      ncol = length(spec[[1L]]),
      byrow = TRUE
    )
  }

  if (.is_unsorted(ms)) {
    ms <- ms[order(ms[, 1L]), , drop = FALSE]
  }

  r <- regexpr("=", desc, fixed = TRUE)
  desc <- stats::setNames(
    substring(desc, r + 1L, nchar(desc)),
    substring(desc, 1L, r - 1L)
  )

  desc[c("PEPMASS", "PEPMASSINT")] <-
    strsplit(desc["PEPMASS"], "[[:space:]]+", perl = TRUE)[[1L]][c(1L, 2L)]

  res <- as.data.frame.matrix(matrix(
    desc,
    nrow = 1,
    dimnames = list(NULL, names(desc))
  ))
  res$mz <- list(ms[, 1L])
  res$intensity <- list(ms[, 2L])
  res
}

.is_unsorted <- function(x) {
  nrow(x) && is.unsorted(x[, 1L])
}

#' Format MGF charge string into an integer compatible format.
#'
#' @param x `character`
#' @return `character`, charge without +/- at the end but - as prefix if needed
#' @noRd
.format_charge <- function(x) {
  res <- sub("[+-]", "", x, perl = TRUE)
  negs <- which(endsWith(x, "-"))
  if (length(negs)) {
    res[negs] <- paste0("-", res[negs])
  }
  res
}

#' @title Read MGF opti
#'
#' @description This function reads a Mascot Generic Format (MGF) file using an
#'     optimized, memory-efficient approach. It mimics the `MsBackendMgf`
#'     implementation but uses significantly lower memory, making it suitable
#'     for processing large MGF files that might otherwise cause memory issues.
#'
#' @details The function processes spectra in batches to minimize memory usage
#'     and avoid loading the entire file into memory at once. It extracts all
#'     MGF fields and maps them to standard spectra variable names.
#'
#' @param f Character string specifying the path to a single MGF file
#' @param msLevel Integer MS level to assign to spectra (default: 2L for MS2)
#' @param mapping Named character vector mapping MGF field names to standard
#'     spectra variable names. Default uses the mapping from MsBackendMgf.
#'
#' @return A DataFrame containing the parsed spectra data with standardized
#'     variable names
#'
#' @examples NULL
#' @export
read_mgf_opti <- function(
  f,
  msLevel = 2L,
  mapping = Spectra::spectraVariableMapping(MsBackendMgf::MsBackendMgf())
) {
  # Validate inputs
  if (missing(f) || is.null(f) || length(f) == 0L) {
    stop("File path 'f' must be provided")
  }

  if (length(f) != 1L) {
    stop(
      "Please provide a single MGF file. For multiple files, call this function separately for each."
    )
  }

  if (!file.exists(f)) {
    stop("MGF file not found: ", f)
  }

  if (!is.numeric(msLevel) || msLevel < 1L) {
    stop("msLevel must be a positive integer")
  }

  if (!requireNamespace("MsBackendMgf", quietly = TRUE)) {
    stop("Package 'MsBackendMgf' is required but not installed")
  }

  logger::log_info(
    "Reading MGF file with optimized memory-efficient parser: ",
    f
  )

  sp_list <- list()
  current_spectrum <- list()
  inside_spectrum <- FALSE
  total_processed <- 0
  batch_size <- 10000
  chunk_id <- 1
  line_counter <- 0

  con <- file(f, "r")
  on.exit(close(con), add = TRUE) # Ensure the file is closed on exit

  while (TRUE) {
    line <- readLines(con, n = 1, warn = FALSE)

    if (length(line) == 0) {
      break
    }

    if (line == "BEGIN IONS") {
      inside_spectrum <- TRUE
      current_spectrum <- list()
      line_counter <- 0
      next
    }

    if (line == "END IONS") {
      inside_spectrum <- FALSE

      if (length(current_spectrum) > 0) {
        spectrum_data <- unlist(current_spectrum)
        sp_list[[chunk_id]] <- .extract_mgf_spectrum(spectrum_data)
        chunk_id <- chunk_id + 1
      }

      total_processed <- total_processed + 1

      if (total_processed %% batch_size == 0) {
        logger::log_info(sprintf("Read %d spectra...", total_processed))
      }

      current_spectrum <- list()
      next
    }

    if (inside_spectrum) {
      line_counter <- line_counter + 1
      current_spectrum[[line_counter]] <- line
    }
  }

  res <- MsCoreUtils::rbindFill(sp_list)

  if ("CHARGE" %in% colnames(res)) {
    res$CHARGE <- .format_charge(res$CHARGE)
  }

  idx <- match(colnames(res), mapping)
  not_na <- !is.na(idx)
  if (any(not_na)) {
    colnames(res)[not_na] <- names(mapping)[idx][not_na]
  }

  spv <- Spectra::coreSpectraVariables()
  spv <- spv[!names(spv) %in% c("mz", "intensity")]
  for (i in seq_along(res)) {
    if (all(lengths(res[[i]]) == 1)) {
      res[[i]] <- unlist(res[[i]])
    }
    if (any(col <- names(spv) == colnames(res)[i])) {
      res[[i]] <- methods::as(res[[i]], spv[col][1])
    }
  }

  res <- methods::as(res, "DataFrame")
  res$mz <- IRanges::NumericList(res$mz, compress = FALSE)
  res$intensity <- IRanges::NumericList(res$intensity, compress = FALSE)
  res$dataOrigin <- f
  if (!"msLevel" %in% colnames(res)) {
    res$msLevel <- as.integer(msLevel)
  }

  res
}
