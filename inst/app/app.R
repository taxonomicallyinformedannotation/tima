library(shiny)
library(timaR)

setwd(dir = "inst/app")

source(file = "ui.R")
source(file = "server.R")

shinyApp(
  ui = ui,
  server = server
)
