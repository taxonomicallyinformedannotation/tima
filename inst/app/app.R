library(shiny)
library(timaR)

source(file = "R/ui.R")
source(file = "R/server.R")

options(shiny.port = 3838)

shinyApp(
  ui = ui,
  server = server
)
