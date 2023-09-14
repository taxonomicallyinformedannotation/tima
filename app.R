library(shiny)
library(timaR)

source(file = "inst/app/ui.R")
source(file = "inst/app/server.R")

shinyApp(
  ui = ui,
  server = server
)
