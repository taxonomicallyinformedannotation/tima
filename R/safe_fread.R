#' Safe Data Loading with Error Messages
#'
#' @description Wrapper around tidytable::fread with error messages
#'     that provide actionable guidance when data loading fails.
#'
#' @include errors_utils.R
#' @include validations_utils.R
#'
#' @param file Character path to file
#' @param file_type Character description of file type for error messages
#' @param required_cols Character vector of required column names (optional)
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
  ...
) {
  # Validate file exists first
  validate_file_exists(
    path = file,
    file_type = file_type,
    param_name = "file"
  )

  # Try to read the file
  df <- tryCatch(
    {
      tidytable::fread(file = file, ...)
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

      stop(msg, call. = FALSE)
    }
  )

  # Validate columns if specified
  if (!is.null(required_cols)) {
    validate_dataframe(
      df = df,
      required_cols = required_cols
    )
  }

  return(df)
}
