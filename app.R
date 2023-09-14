library(shiny)
library(timaR)

source(file = "R/ui.R")
source(file = "R/server.R")

shinyApp(
  ui = ui,
  server = server
)
