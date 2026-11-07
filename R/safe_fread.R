#' Safe Data Loading with Error Messages
#'
#' @description Wrapper around tidytable::fread with error messages
#'     that provide actionable guidance when data loading fails.
#'
#' @include errors_utils.R
#' @include validations_utils.R
#' @include validations_params.R
#'
#' @param file [character] Path to file
#' @param file_type [character] Description of file type for error
#'     messages
#' @param required_cols [character] Vector of required column names
#'     (optional)
#' @param .reader [function] Function used to perform the actual read, called
#'     as `.reader(file = file, ...)`. Defaults to `tidytable::fread()`.
#'     Mainly useful for testing.
#' @param ... Additional arguments passed to tidytable::fread
#'
#' @return Data frame/tidytable
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' df <- safe_fread(
#'   file = "data/features.tsv",
#'   file_type = "features table",
#'   required_cols = c("feature_id", "mz", "rt")
#' )
#' }
safe_fread <- function(
  file,
  file_type = "data file",
  required_cols = NULL,
  .reader = .safe_fread_read,
  ...
) {
  validate_file_exists(
    path = file,
    file_type = file_type,
    param_name = "file"
  )

  # Try to read the file
  df <- tryCatch(
    {
      .reader(file = file, ...)
    },
    error = function(e) {
      error_msg <- conditionMessage(e)

      # Provide specific guidance based on error type
      if (grepl("could not open file", error_msg, ignore.case = TRUE)) {
        msg <- format_error(
          problem = paste0("Cannot open ", file_type),
          expected = "Readable file",
          received = "Permission denied or file locked",
          location = file,
          context = "File may be open in another program or permissions are incorrect",
          fix = paste0(
            "1. Close the file if it's open in Excel or another program\n",
            "2. Check file permissions (should be readable)\n",
            "3. Verify you have access rights to the directory"
          )
        )
      } else if (
        grepl("line.*expected.*fields", error_msg, ignore.case = TRUE)
      ) {
        msg <- format_error(
          problem = paste0("Malformed ", file_type),
          expected = "Consistent number of columns per row",
          received = "Rows with different column counts",
          location = file,
          context = paste0(
            "File has inconsistent formatting. Common causes:\n",
            "  - Extra delimiters in data values\n",
            "  - Unquoted values containing delimiters\n",
            "  - Corrupted file"
          ),
          fix = paste0(
            "1. Open file in text editor to inspect formatting\n",
            "2. Check for unquoted commas/tabs in data values\n",
            "3. Verify file wasn't corrupted during download\n",
            "4. Try specifying quote='\"' if values contain delimiters"
          )
        )
      } else if (grepl("empty", error_msg, ignore.case = TRUE)) {
        msg <- format_error(
          problem = paste0(file_type, " is empty"),
          expected = "File with data rows",
          received = "Empty file or only headers",
          location = file,
          context = "File has no data rows to process",
          fix = paste0(
            "1. Verify the file was created correctly\n",
            "2. Check if upstream processing completed\n",
            "3. Ensure data export succeeded"
          )
        )
      } else {
        msg <- format_error(
          problem = paste0("Failed to read ", file_type),
          expected = "Valid tabular data file",
          received = error_msg,
          location = file,
          fix = paste0(
            "1. Verify file format is correct (TSV, CSV, etc.)\n",
            "2. Check for file corruption\n",
            "3. Try opening file manually to inspect\n",
            "4. Check encoding (should be UTF-8)"
          )
        )
      }

      cli::cli_abort(
        msg,
        class = c("tima_runtime_error", "tima_error"),
        call = NULL
      )
    }
  )

  if (!is.null(required_cols)) {
    validate_dataframe(
      df = df,
      required_cols = required_cols
    )
  }

  df
}

# Internal wrapper to make error branches easy to test without brittle FS setup.
# Decompresses .gz/.bgz/.bz2 with Base R connections so data.table::fread does
# not require the R.utils package at runtime (and avoids full-file decompression
# when nrows = 0L is used to inspect headers).
.safe_fread_read <- function(file, ...) {
  if (
    is.character(file) &&
      length(file) == 1L &&
      file.exists(file) &&
      grepl("\\.(gz|bgz|bz2)$", file, ignore.case = TRUE)
  ) {
    dots <- list(...)
    is_bz2 <- grepl("\\.bz2$", file, ignore.case = TRUE)
    open_con <- if (is_bz2) bzfile else gzfile

    if (identical(as.integer(dots$nrows), 0L)) {
      con <- open_con(file, open = "rt")
      on.exit(close(con), add = TRUE)
      header_line <- readLines(con, n = 1L, warn = FALSE)
      if (length(header_line) == 0L) {
        return(tidytable::tidytable())
      }
      dots$nrows <- NULL
      return(do.call(
        tidytable::fread,
        c(list(text = paste0(header_line, "\n"), nrows = 0L), dots)
      ))
    }

    tmp_file <- tempfile(fileext = ".tsv")
    on.exit(unlink(tmp_file), add = TRUE)
    in_con <- open_con(file, open = "rb")
    out_con <- file(tmp_file, open = "wb")
    tryCatch(
      {
        repeat {
          buf <- readBin(in_con, what = "raw", n = 16777216L)
          if (length(buf) == 0L) {
            break
          }
          writeBin(buf, out_con)
        }
      },
      finally = {
        close(in_con)
        close(out_con)
      }
    )
    return(tidytable::fread(file = tmp_file, ...))
  }

  tidytable::fread(file = file, ...)
}
