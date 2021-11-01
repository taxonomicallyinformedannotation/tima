library(testthat)

test_that(desc = "copy params",
          code = {
            expect_error(object = system(command = "cp -R config/default config/params"),
                         NA)
          })

test_that(desc = "get lotus",
          code = {
            expect_error(object = system(command = paste0(
              "bash ", file.path(dirname(dirname(test_path(

              ))), "src/get_lotus.sh")
            )),
            NA)
          })

test_that(desc = "prepare lotus",
          code = {
            expect_error(object = source(file = file.path(
              dirname(dirname(test_path())), "src/prepare_lotus.R"
            )),
            NA)
          })

test_that(desc = "prepare library",
          code = {
            expect_error(object = source(file = file.path(
              dirname(dirname(test_path())), "src/prepare_library.R"
            )),
            NA)
          })

test_that(desc = "prepare adducts",
          code = {
            expect_error(object = source(file = file.path(
              dirname(dirname(test_path())), "src/prepare_adducts.R"
            )),
            NA)
          })

test_that(desc = "get isdb example",
          code = {
            expect_error(object = system(command = paste0(
              "bash ", file.path(dirname(dirname(test_path(

              ))), "src/get_example_isdb.sh")
            )),
            NA)
          })

test_that(desc = "get gnverifier",
          code = {
            expect_error(object = system(command = paste0(
              "bash ", file.path(dirname(dirname(test_path(

              ))), "src/get_gnverifier.sh")
            )),
            NA)
          })

test_that(desc = "prepare gnps",
          code = {
            expect_error(object = source(file = file.path(
              dirname(dirname(test_path())), "src/prepare_gnps.R"
            )),
            NA)
          })

test_that(desc = "prepare isdb",
          code = {
            expect_error(object = source(file = file.path(
              dirname(dirname(test_path())), "src/prepare_isdb.R"
            )),
            NA)
          })

test_that(desc = "prepare edges",
          code = {
            expect_error(object = source(file = file.path(
              dirname(dirname(test_path())), "src/prepare_edges.R"
            )),
            NA)
          })

test_that(desc = "prepare components",
          code = {
            expect_error(object = source(file = file.path(
              dirname(dirname(test_path())), "src/prepare_features_components.R"
            )),
            NA)
          })

test_that(desc = "prepare classification",
          code = {
            expect_error(object = source(file = file.path(
              dirname(dirname(test_path())),
              "src/prepare_features_classification.R"
            )),
            NA)
          })

test_that(desc = "prepare taxa",
          code = {
            expect_error(object = source(file = file.path(
              dirname(dirname(test_path())), "src/prepare_taxa.R"
            )),
            NA)
          })

test_that(desc = "process annotations",
          code = {
            expect_error(object = source(file = file.path(
              dirname(dirname(test_path())), "src/process_annotations.R"
            )),
            NA)
          })
