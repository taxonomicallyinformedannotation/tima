#' Retry Pattern with Exponential Backoff
#'
#' @description Executes an expression with automatic retry logic and exponential
#'     backoff. Useful for API calls, network operations, and external tool calls
#'     that may fail transiently.
#'
#' @include errors_utils.R
#' @include logs_utils.R
#'
#' @param expr Expression to evaluate
#' @param max_attempts Integer maximum number of attempts (default: 3)
#' @param backoff Numeric base for exponential backoff in seconds (default: 2)
#' @param operation_name Character name of operation for logging
#' @param silent Logical whether to suppress retry warnings (default: FALSE)
#'
#' @return Result of expr if successful
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # Retry API call
#' result <- with_retry(
#'   httr2::request("https://api.example.com") |> httr2::req_perform(),
#'   max_attempts = 5,
#'   operation_name = "API call"
#' )
#'
#' # Retry file download
#' with_retry(
#'   download.file(url, dest),
#'   max_attempts = 3,
#'   operation_name = "file download"
#' )
#' }
with_retry <- function(
  expr,
  max_attempts = 3L,
  backoff = 2,
  operation_name = "operation",
  silent = FALSE
) {
  # Validate inputs
  assert_positive_integer(max_attempts, "max_attempts")
  assert_scalar_numeric(backoff, "backoff", min = 0)

  # Capture the expression for repeated evaluation
  expr_captured <- substitute(expr)
  parent_env <- parent.frame()

  for (attempt in seq_len(max_attempts)) {
    result <- tryCatch(
      eval(expr_captured, envir = parent_env),
      error = function(e) e
    )

    # Success - return result
    if (!inherits(result, "error")) {
      if (attempt > 1 && !silent) {
        log_debug("Retry succeeded on attempt %d/%d", attempt, max_attempts)
      }
      return(result)
    }

    # Failed - decide whether to retry
    if (attempt < max_attempts) {
      wait_time <- backoff^(attempt - 1)

      if (!silent) {
        log_warn(
          "%s failed (attempt %d/%d), retrying in %ds: %s",
          operation_name,
          attempt,
          max_attempts,
          wait_time,
          conditionMessage(result)
        )
      }

      Sys.sleep(wait_time)
    } else {
      # Final attempt failed - give up with error
      msg <- format_error(
        problem = paste0(operation_name, " failed after retries"),
        expected = "Successful operation",
        received = conditionMessage(result),
        context = sprintf(
          "Tried %d times with exponential backoff",
          max_attempts
        ),
        fix = paste0(
          "Possible solutions:\n",
          "  1. Check network connection\n",
          "  2. Verify server/service is available\n",
          "  3. Check authentication credentials\n",
          "  4. Try again later if service is down\n",
          "  5. Increase max_attempts if transient failures are common"
        )
      )
      stop(msg, call. = FALSE)
    }
  }
}

#' Retry Wrapper for HTTP Requests
#'
#' @description Specialized retry wrapper for HTTP requests with smart error
#'     detection for transient vs permanent failures.
#'
#' @param request httr2 request object or expression that performs request
#' @param max_attempts Integer maximum retry attempts (default: 3)
#' @param backoff Numeric backoff multiplier (default: 2)
#'
#' @return Response object
#'
#' @keywords internal
with_http_retry <- function(request, max_attempts = 3L, backoff = 2) {
  with_retry(
    expr = request,
    max_attempts = max_attempts,
    backoff = backoff,
    operation_name = "HTTP request"
  )
}

#' Check if Error is Retryable
#'
#' @description Determines if an error should trigger a retry based on the
#'     error message or type.
#'
#' @param error Error condition object
#'
#' @return Logical TRUE if error appears transient/retryable
#'
#' @keywords internal
is_retryable_error <- function(error) {
  error_msg <- conditionMessage(error)

  # Network/connection errors (typically transient)
  retryable_patterns <- c(
    "timeout",
    "timed out",
    "connection.*failed",
    "connection.*refused",
    "connection.*reset",
    "network.*unreachable",
    "temporary failure",
    "503", # Service Unavailable
    "502", # Bad Gateway
    "504", # Gateway Timeout
    "429" # Too Many Requests
  )

  any(vapply(
    retryable_patterns,
    function(pattern) grepl(pattern, error_msg, ignore.case = TRUE),
    logical(1)
  ))
}

#' Smart Retry with Error Type Detection
#'
#' @description Retry with automatic detection of transient vs permanent errors.
#'     Only retries if error appears transient.
#'
#' @param expr Expression to evaluate
#' @param max_attempts Integer maximum attempts (default: 3)
#' @param backoff Numeric backoff multiplier (default: 2)
#' @param operation_name Character operation name for logging
#'
#' @return Result of expr if successful
#'
#' @keywords internal
with_smart_retry <- function(
  expr,
  max_attempts = 3L,
  backoff = 2,
  operation_name = "operation"
) {
  for (attempt in seq_len(max_attempts)) {
    result <- tryCatch(
      expr,
      error = function(e) e
    )

    if (!inherits(result, "error")) {
      return(result)
    }

    # Check if error is worth retrying
    if (!is_retryable_error(result)) {
      log_debug(
        "Error appears permanent, not retrying: %s",
        conditionMessage(result)
      )
      stop(result)
    }

    # Retry transient error
    if (attempt < max_attempts) {
      wait_time <- backoff^(attempt - 1)
      log_warn(
        "Transient error in %s (attempt %d/%d), retrying in %ds",
        operation_name,
        attempt,
        max_attempts,
        wait_time
      )
      Sys.sleep(wait_time)
    } else {
      stop(result)
    }
  }
}
