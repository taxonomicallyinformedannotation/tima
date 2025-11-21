#' Archive Log File with Timestamp
#'
#' @description Internal helper to archive log files with timestamps.
#'     Implements Single Responsibility Principle.
#'
#' @param log_file Character path to log file
#' @param timestamp POSIXct timestamp for filename
#'
#' @return Logical TRUE if successful, FALSE otherwise
#' @keywords internal
archive_log_file <- function(log_file, timestamp) {
  if (!file.exists(log_file)) {
    logger::log_debug("No log file to archive: {log_file}")
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

  logger::log_debug("Archiving log to: {output_log}")

  success <- tryCatch(
    {
      file.copy(from = log_file, to = output_log, overwrite = TRUE)
    },
    error = function(e) {
      logger::log_warn("Failed to archive log: {conditionMessage(e)}")
      return(FALSE)
    }
  )

  if (isTRUE(success)) {
    file.remove(log_file)
    logger::log_info("Log archived: {output_log}")
  }

  return(success)
}

#' Execute TIMA Targets Pipeline
#'
#' @description Internal helper to run the targets workflow with error handling.
#'     Implements Single Responsibility Principle.
#'
#' @param target_pattern Character regex pattern to match target names
#'
#' @return Invisible NULL
#' @keywords internal
execute_targets_pipeline <- function(target_pattern = "^ann_pre$") {
  logger::log_info("Executing targets pipeline (pattern: {target_pattern})...")

  tryCatch(
    {
      targets::tar_make(names = tidyselect::matches(match = target_pattern))
      logger::log_success("Targets pipeline completed successfully")
    },
    error = function(e) {
      logger::log_error("Pipeline execution failed: {conditionMessage(e)}")
      stop(
        "TIMA workflow pipeline failed. ",
        "Check logs for details: ",
        conditionMessage(e),
        call. = FALSE
      )
    },
    warning = function(w) {
      logger::log_warn("Pipeline warning: {conditionMessage(w)}")
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
#'
#' @param target_pattern Character. Regex pattern for target selection.
#'     Default: "^ann_pre$" (annotation preparation target)
#' @param log_file Character. Path to log file. Default: "tima.log"
#' @param clean_old_logs Logical. Remove old log file before starting.
#'     Default: TRUE
#'
#' @return Invisible NULL. Executes workflow as side effect and creates
#'     timestamped log files in data/processed/
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Run full workflow with defaults
#' tima_full()
#'
#' # Run with custom target pattern
#' tima_full(target_pattern = "^prepare_")
#'
#' # Preserve existing logs
#' tima_full(clean_old_logs = FALSE)
#' }
tima_full <- function(
  target_pattern = "^ann_pre$",
  log_file = "tima.log",
  clean_old_logs = TRUE
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

  # Initialize ----
  start_time <- Sys.time()

  # Clean up previous log file if requested
  if (isTRUE(clean_old_logs) && file.exists(log_file)) {
    logger::log_debug("Removing old log file: {log_file}")
    file.remove(log_file)
  }

  logger::log_info("=" |> rep(60) |> paste(collapse = ""))
  logger::log_info("Starting Complete TIMA Annotation Workflow")
  logger::log_info("=" |> rep(60) |> paste(collapse = ""))
  logger::log_info("Start time: {format(start_time, '%Y-%m-%d %H:%M:%S')}")
  logger::log_info("Authors: Adriano Rutz (AR)")
  logger::log_info("Contributors: Pierre-Marie Allard (PMA)")
  logger::log_info("=" |> rep(60) |> paste(collapse = ""))

  # Setup Environment ----
  tryCatch(
    {
      go_to_cache()
      logger::log_debug("Working directory: {getwd()}")
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

  logger::log_info("=" |> rep(60) |> paste(collapse = ""))
  logger::log_success(
    "Complete TIMA workflow finished successfully in {elapsed_formatted}"
  )
  logger::log_info("End time: {format(end_time, '%Y-%m-%d %H:%M:%S')}")
  logger::log_info("=" |> rep(60) |> paste(collapse = ""))

  # Archive Logs ----
  archive_log_file(log_file = log_file, timestamp = end_time)

  invisible(NULL)
}
