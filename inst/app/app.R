library(shiny)
library(timaR)

if (grepl(pattern = "tima-r$", x = getwd())) {
  setwd(dir = "inst/app")
}

source(file = "ui.R")
source(file = "server.R")

shinyApp(
  ui = ui,
  server = server
)
