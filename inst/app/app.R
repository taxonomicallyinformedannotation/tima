library(shiny)
library(timaR)

source(file = "R/ui.R")
source(file = "R/server.R")

options(shiny.host = "0.0.0.0")

shinyApp(
  ui = ui,
  server = server
)
