#' Archive Log File with Timestamp
#'
#' @description Internal helper to archive log files with timestamps.
#'
#' @param log_file Character path to log file
#' @param timestamp POSIXct timestamp for filename
#'
#' @return Logical TRUE if successful, FALSE otherwise
#' @keywords internal
archive_log_file <- function(log_file, timestamp) {
  if (!file.exists(log_file)) {
    log_debug("No log file to archive: %s", log_file)
    return(FALSE)
  }

  # Create output directory if needed
  output_dir <- "data/processed"
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }

  output_log <- file.path(
    output_dir,
    sprintf(
      "%s_%s",
      format(timestamp, format = "%Y%m%d_%H%M%S"),
      basename(log_file)
    )
  )

  log_debug("Archiving log to: %s", output_log)

  success <- tryCatch(
    {
      file.copy(from = log_file, to = output_log, overwrite = TRUE)
    },
    error = function(e) {
      log_warn("Failed to archive log: %s", conditionMessage(e))
      return(FALSE)
    }
  )

  if (isTRUE(success)) {
    file.remove(log_file)
    log_info("Log archived: %s", output_log)
  }

  return(success)
}

#' Execute TIMA Targets Pipeline
#'
#' @description Internal helper to run the targets workflow with error handling.
#'
#' @param target_pattern Character regex pattern to match target names
#'
#' @return Invisible NULL
#' @keywords internal
execute_targets_pipeline <- function(target_pattern = "^ann_wei$") {
  log_info("Executing targets pipeline (pattern: %s)...", target_pattern)

  tryCatch(
    {
      targets::tar_make(names = tidyselect::matches(match = target_pattern))
      log_success("Targets pipeline completed successfully")
    },
    error = function(e) {
      log_error("Pipeline execution failed: %s", conditionMessage(e))
      stop(
        "TIMA workflow pipeline failed. ",
        "Check logs for details: ",
        conditionMessage(e),
        call. = FALSE
      )
    },
    warning = function(w) {
      log_warn("Pipeline warning: %s", conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  invisible(NULL)
}

#' @title Run Complete TIMA Workflow
#'
#' @description Executes the full Taxonomically Informed Metabolite Annotation (TIMA)
#'     workflow from start to finish. This includes data preparation, library loading,
#'     annotation, weighting, and output generation. The function runs the targets
#'     pipeline and archives logs with timestamps for reproducibility.
#'
#' @details
#' The workflow performs the following steps:
#' \itemize{
#'   \item Initializes logging and timing
#'   \item Navigates to cache directory
#'   \item Executes the targets pipeline (annotation preparation)
#'   \item Archives timestamped logs to data/processed/
#' }
#'
#' @include go_to_cache.R
#' @include logs_utils.R
#'
#' @param target_pattern Character. Regex pattern for target selection.
#'     Default: "^ann_wei$" (annotation preparation target)
#' @param log_file Character. Path to log file. Default: "tima.log"
#' @param clean_old_logs Logical. Remove old log file before starting.
#'     Default: TRUE
#' @param log_level Character or numeric. Logging verbosity level.
#'     Can be one of: "trace", "debug", "info", "warn", "error", "fatal"
#'     or numeric values: TRACE=600, DEBUG=500, INFO=400, WARN=300, ERROR=200, FATAL=100.
#'     Default: "info" (400). Use "debug" for detailed troubleshooting.
#'
#' @return Invisible NULL. Executes workflow as side effect and creates
#'     timestamped log files in data/processed/
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Run full workflow with defaults (INFO level)
#' run_tima()
#'
#' # Run with debug logging for troubleshooting
#' run_tima(log_level = "debug")
#'
#' # Run with minimal logging (warnings and errors only)
#' run_tima(log_level = "warn")
#'
#' # Run with custom target pattern
#' run_tima(target_pattern = "^prepare_")
#'
#' # Preserve existing logs
#' run_tima(clean_old_logs = FALSE)
#'
#' # Combine multiple options
#' run_tima(
#'   target_pattern = "^ann_",
#'   log_level = "debug",
#'   clean_old_logs = FALSE
#' )
#' }
run_tima <- function(
  target_pattern = "^ann_wei$",
  log_file = "tima.log",
  clean_old_logs = TRUE,
  log_level = "info"
) {
  # Input Validation ----
  if (!is.character(target_pattern) || length(target_pattern) != 1L) {
    stop("target_pattern must be a single character string", call. = FALSE)
  }

  if (!is.character(log_file) || length(log_file) != 1L) {
    stop("log_file must be a single character string", call. = FALSE)
  }

  if (!is.logical(clean_old_logs) || length(clean_old_logs) != 1L) {
    stop("clean_old_logs must be a single logical value", call. = FALSE)
  }

  # Validate and convert log_level
  if (is.character(log_level)) {
    if (length(log_level) != 1L) {
      stop("log_level must be a single value", call. = FALSE)
    }

    valid_levels <- c("trace", "debug", "info", "warn", "error", "fatal")
    log_level_lower <- tolower(log_level)

    if (!log_level_lower %in% valid_levels) {
      stop(
        sprintf(
          "log_level must be one of: %s (got '%s')",
          paste(valid_levels, collapse = ", "),
          log_level
        ),
        call. = FALSE
      )
    }

    # Convert to numeric threshold for lgr
    level_map <- list(
      trace = 600,
      debug = 500,
      info = 400,
      warn = 300,
      error = 200,
      fatal = 100
    )
    log_threshold <- level_map[[log_level_lower]]
  } else if (is.numeric(log_level)) {
    if (length(log_level) != 1L) {
      stop("log_level must be a single value", call. = FALSE)
    }

    valid_numeric <- c(600, 500, 400, 300, 200, 100)
    if (!log_level %in% valid_numeric) {
      stop(
        sprintf(
          "log_level must be one of: %s (got %g)",
          paste(valid_numeric, collapse = ", "),
          log_level
        ),
        call. = FALSE
      )
    }
    log_threshold <- as.integer(log_level)
  } else {
    stop(
      "log_level must be character (e.g., 'info') or numeric (e.g., 400)",
      call. = FALSE
    )
  }

  # Initialize ----
  start_time <- Sys.time()

  # Setup logger with specified threshold
  setup_logger(filename = log_file, threshold = log_threshold)

  # Clean up previous log file if requested
  if (isTRUE(clean_old_logs) && file.exists(log_file)) {
    log_debug("Removing old log file: %s", log_file)
    file.remove(log_file)
  }

  log_info("=" |> rep(60) |> paste(collapse = ""))
  log_info("Starting Complete TIMA Annotation Workflow")
  log_info("=" |> rep(60) |> paste(collapse = ""))
  log_info("Start time: %s", format(start_time, '%Y-%m-%d %H:%M:%S'))
  log_info("Authors: Adriano Rutz (AR)")
  log_info("Contributors: Pierre-Marie Allard (PMA)")
  log_info("=" |> rep(60) |> paste(collapse = ""))

  # Setup Environment ----
  tryCatch(
    {
      go_to_cache()
      log_debug("Working directory: %s", getwd())
    },
    error = function(e) {
      stop(
        "Failed to navigate to cache directory: ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )

  # Execute Workflow ----
  execute_targets_pipeline(target_pattern = target_pattern)

  # Finalize ----
  end_time <- Sys.time()
  elapsed_time <- end_time - start_time
  elapsed_formatted <- format(
    round(elapsed_time, digits = 2),
    units = "auto"
  )

  log_info("=" |> rep(60) |> paste(collapse = ""))
  log_success(
    "Complete TIMA workflow finished successfully in %s",
    elapsed_formatted
  )
  log_info("End time: %s", format(end_time, '%Y-%m-%d %H:%M:%S'))
  log_info("=" |> rep(60) |> paste(collapse = ""))

  # Archive Logs ----
  archive_log_file(log_file = log_file, timestamp = end_time)

  invisible(NULL)
}

#' @title Run Complete TIMA Workflow (DEPRECATED)
#'
#' @description **DEPRECATED:** This function has been renamed to \code{\link{run_tima}}.
#'     Please use \code{run_tima()} instead. \code{tima_full()} will be removed in a
#'     future version.
#'
#' @details
#' This function is deprecated as of TIMA version 2.12.0 (November 2025).
#' It now simply calls \code{\link{run_tima}} with all arguments passed through,
#' but issues a deprecation warning.
#'
#' \strong{Migration Guide:}
#' \itemize{
#'   \item Old: \code{tima_full(target_pattern = "^ann_wei$")}
#'   \item New: \code{run_tima(target_pattern = "^ann_wei$")}
#' }
#'
#' All parameters and behavior are identical between the two functions.
#'
#' @param target_pattern Character. Regex pattern for target selection.
#'     Default: "^ann_wei$"
#' @param log_file Character. Path to log file. Default: "tima.log"
#' @param clean_old_logs Logical. Remove old log file before starting.
#'     Default: TRUE
#' @param log_level Character or numeric. Logging verbosity level.
#'     Default: "info"
#'
#' @return Invisible NULL (same as \code{\link{run_tima}})
#'
#' @export
#'
#' @seealso \code{\link{run_tima}} for the current function
#'
#' @examples
#' \dontrun{
#' # DEPRECATED - Use run_tima() instead
#' # tima_full()
#'
#' # RECOMMENDED
#' run_tima()
#' }
tima_full <- function(
  target_pattern = "^ann_wei$",
  log_file = "tima.log",
  clean_old_logs = TRUE,
  log_level = "info"
) {
  # Issue deprecation warning
  .Deprecated(
    new = "run_tima",
    package = "tima",
    msg = paste0(
      "tima_full() is deprecated and will be removed in a future version.\n",
      "Please use run_tima() instead.\n",
      "All parameters work exactly the same way.\n",
      "Update your code: tima_full(...) -> run_tima(...)"
    )
  )

  # Call the new function with all arguments
  run_tima(
    target_pattern = target_pattern,
    log_file = log_file,
    clean_old_logs = clean_old_logs,
    log_level = log_level
  )
}
