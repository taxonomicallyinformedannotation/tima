library(shiny)
library(timaR)

source(file = "ui.R")
source(file = "server.R")

source(file = "R/fields_mandatory.R")
source(file = "R/label_mandatory.R")
source(file = "R/save_input.R")
source(file = "R/redirect_console.R")

shinyApp(
  ui = ui,
  server = server
)
