#' @title Follow next
#'
#' @noRd
#'
#' @param session TODO
#' @param text TODO
#' @param ...
#'
#' @return TODO
#'
#' @export
#'
#' @importFrom rvest html_attr html_element session_jump_to
#'
#' @examples TODO
follow_next <- function(session,
                        text = "Next",
                        ...) {
  link <- rvest::html_element(
    x = session,
    xpath = sprintf("//*[text()[contains(.,'%s')]]", text)
  )

  url <- rvest::html_attr(link, "href") |>
    trimws() |>
    gsub(pattern = "^\\.{1}/", replacement = "")

  message("Navigating to ", url)

  rvest::session_jump_to(session, url, ...)
}
