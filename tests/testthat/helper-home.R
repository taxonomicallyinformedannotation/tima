# Temporary home directory ----

#' Point the home directory to a temporary one for the calling scope
#'
#' @description Functions that default to the tima cache (`~/.tima`, see
#'     go_to_cache()) must not read or write the user's real one. The
#'     temporary cache is seeded with the parameter files an installation
#'     provides. Called at the top level of a test file, it lasts until the
#'     file completes.
#'
#' @param .local_envir Scope to restore the environment variables in.
#'
#' @return The temporary home path, invisibly
local_test_home <- function(.local_envir = parent.frame()) {
  home <- withr::local_tempdir(pattern = "home_", .local_envir = .local_envir)
  params <- file.path(home, ".tima", "params")
  dir.create(params, recursive = TRUE, showWarnings = FALSE)
  file.copy(
    from = list.files(
      system.file("params", package = "tima"),
      full.names = TRUE
    ),
    to = params,
    recursive = TRUE
  )
  withr::local_envvar(
    HOME = home,
    R_USER = home,
    USERPROFILE = home,
    .local_envir = .local_envir
  )
  invisible(home)
}
