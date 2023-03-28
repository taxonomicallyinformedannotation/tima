source(file = "R/save_input.R")

options(shiny.maxRequestSize = 1000 * 1024^2)

server <- function(input, output, session) {
  # Observe helpers
  shinyhelper::observe_helpers()

  # Enable the Submit button when all mandatory fields are filled out
  observe(x = {
    mandatoryFilled <-
      vapply(
        X = fieldsMandatory,
        FUN = function(x) {
          ## TODO improve
          suppressWarnings(!is.null(input[[x]]) &&
            input[[x]] != "")
        },
        FUN.VALUE = logical(1)
      )
    mandatoryFilled <- all(mandatoryFilled)

    shinyjs::toggleState(id = "save", condition = mandatoryFilled)
    shinyjs::toggleState(id = "launch", condition = input$save >= 1)
  })

  # When the Save button is clicked, save the response
  observeEvent(
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

  observeEvent(
    eventExpr = input$launch,
    handlerExpr = {
      shinyjs::show("job_msg")
      shinyjs::hide("error")
      shinyjs::hide("params")
      shinyjs::hide("form")
      shinyjs::show("tar_watch")
      targets::tar_watch_server(id = "tar_watch")
      targets::tar_watch()
      targets::tar_make(
        names = targets::matches("annotations_prepared$"),
        reporter = "verbose_positives"
      )
    }
  )
}
