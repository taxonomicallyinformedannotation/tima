#' Title
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
dirty_trick <-
  function(...) {
    if (grepl(pattern = "tests/testthat", x = getwd())) {
      setwd("../../")
      cat(getwd())
    }
  }

test_that(
  desc = "copy params",
  code = {
    dirty_trick()
    expect_error(
      object = system(command = "cp -R config/default config/params"),
      NA
    )
  }
)

test_that(
  desc = "get lotus",
  code = {
    dirty_trick()
    expect_error(
      object = system(command = "bash inst/scripts/get_lotus.sh"),
      NA
    )
  }
)

test_that(
  desc = "prepare lotus",
  code = {
    dirty_trick()
    expect_error(
      object = source(file = "inst/scripts/prepare_lotus.R"),
      NA
    )
  }
)

test_that(
  desc = "prepare library",
  code = {
    dirty_trick()
    expect_error(
      object = source(file = "inst/scripts/prepare_library.R"),
      NA
    )
  }
)

test_that(
  desc = "prepare adducts",
  code = {
    dirty_trick()
    expect_error(
      object = source(file = "inst/scripts/prepare_adducts.R"),
      NA
    )
  }
)

test_that(
  desc = "get isdb example",
  code = {
    dirty_trick()
    expect_error(
      object = system(
        command =
          "bash inst/scripts/get_example_isdb.sh"
      ),
      NA
    )
  }
)

test_that(
  desc = "get gnverifier",
  code = {
    dirty_trick()
    expect_error(
      object = system(
        command =
          "bash inst/scripts/get_gnverifier.sh"
      ),
      NA
    )
  }
)

test_that(
  desc = "prepare gnps",
  code = {
    dirty_trick()
    expect_error(
      object = source(file = "inst/scripts/prepare_gnps.R"),
      NA
    )
  }
)

test_that(
  desc = "prepare isdb",
  code = {
    dirty_trick()
    expect_error(
      object = source(file = "inst/scripts/prepare_isdb.R"),
      NA
    )
  }
)

test_that(
  desc = "prepare edges",
  code = {
    dirty_trick()
    expect_error(
      object = source(file = "inst/scripts/prepare_edges.R"),
      NA
    )
  }
)

test_that(
  desc = "prepare components",
  code = {
    dirty_trick()
    expect_error(
      object = source(file = "inst/scripts/prepare_features_components.R"),
      NA
    )
  }
)

test_that(
  desc = "prepare classification",
  code = {
    dirty_trick()
    expect_error(
      object = source(file = "inst/scripts/prepare_features_classification.R"),
      NA
    )
  }
)

test_that(
  desc = "prepare taxa",
  code = {
    dirty_trick()
    expect_error(
      object = source(file = "inst/scripts/prepare_taxa.R"),
      NA
    )
  }
)

test_that(
  desc = "process annotations",
  code = {
    dirty_trick()
    expect_error(
      object = source(file = "inst/scripts/process_annotations.R"),
      NA
    )
  }
)
