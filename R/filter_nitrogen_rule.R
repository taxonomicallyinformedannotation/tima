#' @title Filter nitrogen rule
#'
#' @description This function filters annotations according to nitrogen rule
#'
#' @param df_annotated_final Table containing your MS1 annotations
#' @param features_table Feature table containing your mzs
#' @param filter_nitro Filter according to Nitrogen rule. Boolean
#'
#' @return A table containing filtered MS1 annotations
#'
#' @export
#'
#' @examples NULL
filter_nitrogen_rule <-
  function(df_annotated_final, features_table, filter_nitro) {
    count_n <- function(vec, element) {
      return(vec |>
        stringi::stri_count(regex = paste0(element, "(?![a-z])")))
    }
    multiply_n <- function(vec, element) {
      return(
        vec |>
          stringi::stri_extract_all_regex(pattern = paste0(element, "[0-9]{1,3}")) |>
          as.character() |>
          stringi::stri_replace_all_fixed(pattern = element, replacement = "") |>
          tidytable::replace_na("1") |>
          as.numeric()
      )
    }
    count_element <- function(vec, elem) {
      return(count_n(vec, elem) * multiply_n(vec, elem))
    }
    add_element_count <- function(df, element) {
      formula <- df$candidate_structure_molecular_formula |>
        count_element(element)
      adduct <- df$candidate_library1 |>
        count_element(element)
      loss <- df$candidate_library2 |>
        gsub(pattern = "\\(.*", replacement = "") |>
        count_element(element)
      return(formula + adduct - loss)
    }

    df_1 <- df_annotated_final |>
      tidytable::left_join(features_table) |>
      tidytable::separate_wider_delim(candidate_library,
        delim = " - ",
        cols_remove = FALSE
      )

    df_1$c <- add_element_count(df_1, element = "C")
    df_1$h <- add_element_count(df_1, element = "H")
    df_1$n <- add_element_count(df_1, element = "N")
    df_1$o <- add_element_count(df_1, element = "O")
    df_1$p <- add_element_count(df_1, element = "P")
    df_1$s <- add_element_count(df_1, element = "S")

    df_2 <- df_1 |>
      tidytable::filter(c >= 0 | is.na(c)) |>
      tidytable::filter(h >= 0 | is.na(h)) |>
      tidytable::filter(n >= 0 | is.na(n)) |>
      tidytable::filter(o >= 0 | is.na(o)) |>
      tidytable::filter(p >= 0 | is.na(p)) |>
      tidytable::filter(s >= 0 | is.na(s))

    log_debug(
      "Removed",
      nrow(df_1) - nrow(df_2),
      "non-sensical adducts"
    )

    if (filter_nitro == TRUE) {
      df_3 <- df_2 |>
        tidytable::filter(mz |> as.integer() %% 2 == n |> as.integer() %% 2 |
          n == 0 | is.na(n))
      log_debug(
        "Removed other",
        nrow(df_2) - nrow(df_3),
        "adducts based on Nitrogen rule"
      )
    } else {
      df_3 <- df_2
      log_debug("Skipping Nitrogen rule")
    }
    df_3 <- df_3 |>
      tidytable::select(colnames(df_annotated_final))

    return(df_3)
  }
