#' @title Replace ID in file paths
#'
#' @description This function replaces the default ID
#'    in the example by a user-specified one
#'
#' @include get_default_paths.R
#' @include get_params.R
#'
#' @param x a character string containing the default ID
#' @param user_filename a user-specified value for a file name job ID
#' @param user_gnps a user-specified value for a GNPS job ID
#' @param example_gnps an example value for a GNPS job ID
#'
#' @return Character string with the GNPS job ID modified according
#'    to the rules specified in the function
#'
#' @examples
#' \dontrun{
#' replace_id(
#'   x = "example/123456_features.tsv",
#'   user_gnps = NULL,
#'   user_filename = "Foo"
#' )
#' }
replace_id <-
  function(x,
           user_filename = get_params(step = "prepare_params")$files$pattern,
           user_gnps = get_params(step = "prepare_params")$gnps$id,
           example_gnps = get_default_paths()$gnps$example) {
    if (length(user_gnps) != 0) {
      if (user_gnps == "") {
        user_gnps <- NULL
      }
    }
    if (!is.null(user_gnps)) {
      if (user_gnps == example_gnps) {
        user_gnps <- "example"
      }
    }

    path <- x |>
      stringi::stri_replace_all_regex(
        pattern = "/[^/]*$",
        replacement = "",
        vectorize_all = FALSE
      )

    file <- x |>
      stringi::stri_replace_all_regex(
        pattern = ".*/",
        replacement = "",
        vectorize_all = FALSE
      )

    old <- file |>
      stringi::stri_replace_all_regex(
        pattern = "(.*)(_)(.*)",
        replacement = "$1",
        vectorize_all = FALSE
      )

    new <- file |>
      stringi::stri_replace_all_regex(
        pattern = old,
        replacement = ifelse(
          test = !is.null(user_gnps),
          yes = user_gnps,
          no = user_filename
        ),
        vectorize_all = FALSE
      )

    final <- ifelse(test = path != file,
      yes = file.path(path, new),
      no = new
    )

    return(final)
  }
