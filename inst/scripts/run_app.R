options(shiny.host="0.0.0.0")

shiny::runApp(
  appDir = "inst/app",
  port = 3838
)
