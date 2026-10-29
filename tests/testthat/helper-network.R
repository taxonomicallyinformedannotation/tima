# Network-dependent tests ----
#
# Tests that reach the internet call skip_if_no_network() with the host they
# need. They are skipped when that host cannot be reached (offline, behind a
# blocking proxy, sandboxed) or when TIMA_TESTS_OFFLINE is "true", so that the
# suite gives the same result everywhere. Set TIMA_TESTS_OFFLINE=true to skip
# them on purpose.

.network_cache <- new.env(parent = emptyenv())

#' Whether `url` answers at all (any HTTP status counts as reachable)
#' @keywords internal
.is_reachable <- function(url, timeout_s = 5) {
  host <- sub("^https?://([^/]+).*$", "\\1", url)
  if (!exists(host, envir = .network_cache, inherits = FALSE)) {
    ok <- tryCatch(
      {
        httr2::request(url) |>
          httr2::req_method("HEAD") |>
          httr2::req_timeout(timeout_s) |>
          httr2::req_error(is_error = function(resp) FALSE) |>
          httr2::req_perform()
        TRUE
      },
      error = function(e) FALSE
    )
    assign(host, ok, envir = .network_cache)
  }
  get(host, envir = .network_cache, inherits = FALSE)
}

#' Skip a test that needs `url`'s host to be reachable
#' @keywords internal
skip_if_no_network <- function(url = "https://github.com") {
  testthat::skip_on_cran()
  if (identical(tolower(Sys.getenv("TIMA_TESTS_OFFLINE")), "true")) {
    testthat::skip("Network tests disabled (TIMA_TESTS_OFFLINE=true)")
  }
  if (!.is_reachable(url)) {
    testthat::skip(paste("Network unavailable:", url))
  }
  invisible(TRUE)
}
