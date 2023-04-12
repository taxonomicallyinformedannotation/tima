library(shinyvalidate)
source(file = "R/save_input.R")

options(shiny.maxRequestSize = 1000 * 1024^2)

server <- function(input, output, session) {
  # Observe helpers
  shinyhelper::observe_helpers()

  # Enable the Submit button when all mandatory fields are filled out
  shiny::observe(x = {
    mandatory_filled <-
      vapply(
        X = fields_mandatory,
        FUN = function(x) {
          ## TODO improve
          suppressWarnings(!is.null(input[[x]]) &&
            input[[x]] != "")
        },
        FUN.VALUE = logical(1)
      )
    mandatory_filled <- all(mandatory_filled)

    shinyjs::toggleState(id = "save", condition = mandatory_filled)
    shinyjs::toggleState(id = "launch", condition = input$save >= 1)
  })

  # Special check for taxon name
  iv <- InputValidator$new()
  iv$add_rule("org_tax", function(taxon) {
    if (!grepl(pattern = "^[[:upper:]]", x = taxon)) {
      "Please provide your taxon name with capital letter"
    }
  })
  iv$add_rule("org_tax", function(taxon) {
    if (is.na(rotl::tnrs_match_names(
      names = taxon,
      do_approximate_matching = FALSE
    )$ott_id)) {
      "Taxon not found in Open Tree of Life"
    }
  })
  iv$enable()

  # When the Save button is clicked, save the response
  shiny::observeEvent(
    eventExpr = input$save,
    handlerExpr = {
      # User-experience stuff
      shinyjs::show("save_msg")
      shinyjs::enable("launch")
      shinyjs::hide("error")

      # Save the data (show an error message in case of error)
      tryCatch(
        expr = {
          save_input(input = input)
          shinyjs::show("thankyou_msg")
        },
        error = function(err) {
          shinyjs::html("error_msg", err$message)
          shinyjs::show(
            id = "error",
            anim = TRUE,
            animType = "fade"
          )
        },
        finally = {
          shinyjs::enable("save")
          shinyjs::enable("launch")
          shinyjs::hide("save_msg")
          shinyjs::hide("error")
        }
      )
    }
  )

  shiny::observeEvent(
    eventExpr = input$launch,
    handlerExpr = {
      shinyjs::show("job_msg")
      shinyjs::hide("error")
      shinyjs::hide("params")
      shinyjs::hide("form")
      shinyjs::show("tar_watch")
      tryCatch(expr = {
        setwd("../..")
        targets::tar_watch_server(id = "tar_watch")
        targets::tar_watch(
          port = 3839,
          display = "graph",
          displays = c("summary", "graph"),
          degree_from = 10,
          # level_separation = 500,
          outdated = TRUE,
          # targets_only = TRUE,
          supervise = TRUE,
          verbose = TRUE,
          exclude = c(
            "yaml_paths",
            "paths",
            "par_def_ann_mas",
            "par_def_ann_spe",
            "par_def_cre_com",
            "par_def_cre_edg_spe",
            "par_def_pre_ann_gnp",
            "par_def_pre_ann_sir",
            "par_def_pre_ann_spe",
            "par_def_pre_fea_com",
            "par_def_pre_fea_edg",
            "par_def_pre_fea_tab",
            "par_def_pre_lib_add",
            "par_def_pre_lib_sop_clo",
            "par_def_pre_lib_sop_ecm",
            "par_def_pre_lib_sop_lot",
            "par_def_pre_lib_sop_mer",
            "par_def_pre_lib_spe",
            "par_def_pre_tax",
            "par_def_wei_ann",
            "par_fin_par",
            "par_pre_par",
            "par_usr_ann_mas",
            "par_usr_ann_spe",
            "par_usr_cre_com",
            "par_usr_cre_edg_spe",
            "par_usr_pre_ann_gnp",
            "par_usr_pre_ann_sir",
            "par_usr_pre_ann_spe",
            "par_usr_pre_fea_com",
            "par_usr_pre_fea_edg",
            "par_usr_pre_fea_tab",
            "par_usr_pre_lib_add",
            "par_usr_pre_lib_sop_clo",
            "par_usr_pre_lib_sop_ecm",
            "par_usr_pre_lib_sop_lot",
            "par_usr_pre_lib_sop_mer",
            "par_usr_pre_lib_spe",
            "par_usr_pre_tax",
            "par_usr_wei_ann",
            ".Random.seed"
          )
        )
        targets::tar_make(
          names = matches("annotations_prepared$"),
          reporter = "verbose_positives"
        )
      }, finally = {
        shiny::stopApp()
      })
      process <-
        shiny::reactiveValues(status = targets::tar_active())
      shiny::observe({
        shiny::invalidateLater(millis = 5000)
        process$status <- targets::tar_active()
      })

      shiny::observeEvent(
        eventExpr = process$status,
        handlerExpr = {
          message("Job finished")
          shiny::stopApp()
        }
      )
    }
  )
}
