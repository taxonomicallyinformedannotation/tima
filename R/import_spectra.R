#' @title Import spectra
#'
#' @description This function imports spectra from a file (.mgf or .sqlite)
#'
#' @include sanitize_spectra.R
#'
#' @param file File path of the spectrum file to be imported
#' @param cutoff Absolute minimal intensity
#' @param dalton Dalton tolerance
#' @param polarity Polarity
#' @param ppm PPM tolerance
#' @param sanitize Flag indicating whether to sanitize. Default TRUE
#'
#' @return Spectra object containing the imported spectra
#'
#' @export
#'
#' @examples
#' \dontrun{
#' get_file(
#'   url = get_default_paths()$urls$examples$spectra_mini,
#'   export = get_default_paths()$data$source$spectra
#' )
#' import_spectra(file = get_default_paths()$data$source$spectra)
#' import_spectra(
#'   file = get_default_paths()$data$source$spectra,
#'   sanitize = FALSE
#' )
#' }
import_spectra <- function(file,
                           cutoff = 0,
                           dalton = 0.01,
                           polarity = NA,
                           ppm = 10,
                           sanitize = TRUE) {
  file_ext <-
    stringi::stri_replace_all_regex(
      str = file,
      pattern = ".*\\.",
      replacement = "",
      vectorize_all = FALSE
    )

  ## Temporary for large MGFs
  ## Mimicks the `MsBackendMgf` implementation but much lower memory for large files
  read_mgf_opti <- function(f,
                            msLevel = 2L,
                            mapping = Spectra::spectraVariableMapping(MsBackendMgf::MsBackendMgf())) {
    requireNamespace("MsBackendMgf", quietly = TRUE)
    if (length(f) != 1L) {
      stop("Please provide a single MGF file.")
    }

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
          sp_list[[chunk_id]] <- MsBackendMgf:::.extract_mgf_spectrum(spectrum_data)
          chunk_id <- chunk_id + 1
        }

        total_processed <- total_processed + 1

        if (total_processed %% batch_size == 0) {
          message(sprintf("Read %d spectra...", total_processed))
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
      res$CHARGE <- MsBackendMgf:::.format_charge(res$CHARGE)
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
        res[[i]] <- as(res[[i]], spv[col][1])
      }
    }

    res <- as(res, "DataFrame")
    res$mz <- IRanges::NumericList(res$mz, compress = FALSE)
    res$intensity <- IRanges::NumericList(res$intensity, compress = FALSE)
    res$dataOrigin <- f
    if (!"msLevel" %in% colnames(res)) {
      res$msLevel <- as.integer(msLevel)
    }

    res
  }

  spectra <- switch(
    EXPR = file_ext,
    "mgf" = {
      read_mgf_opti(f = file) |>
        # TODO Change as soon as R 4.4.0 becomes oldrel
        # MsBackendMgf::readMgfSplit(f = file) |>
        Spectra::Spectra()
    },
    "msp" = {
      MsBackendMsp::readMsp(f = file) |>
        Spectra::Spectra()
    },
    # "sqlite" = {
    #   CompDb(x = file) |>
    #     Spectra::Spectra() |>
    #     setBackend(MsBackendMemory())
    # },
    "rds" = {
      readRDS(file = file)
    }
  )
  if (sanitize) {
    spectra <- spectra |>
      sanitize_spectra(
        cutoff = cutoff,
        dalton = dalton,
        polarity = polarity,
        ppm = ppm
      )
  }
  return(spectra)
}
