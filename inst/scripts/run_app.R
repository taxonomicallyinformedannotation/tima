options(shiny.host="0.0.0.0")
options(shiny.port = 3838)

shiny::runApp(
  appDir = "inst/app"
)
