# Check if runs in Docker environment or not
i_am_a_whale <- file.exists("/.dockerenv")
if (i_am_a_whale) {
  options(shiny.host = "0.0.0.0")
} else {
  options(shiny.host = "127.0.0.1")
}
options(shiny.port = 3838)
options(shiny.maxRequestSize = 2000 * 1024^2)

ui <- source(system.file("ui.R", package = "tima"))
server <- source(system.file("server.R", package = "tima"))

url <- "<http://127.0.0.1:3838>"
message("Please, open:", url, "on your favorite browser, but not Edge.")
shiny::shinyApp(
  ui = ui,
  server = server,
  onStart = function() {
    if (i_am_a_whale) {
      message("I'm inside the matrix ;(")
      setwd(dir = "..")
    } else {
      tima:::copy_backbone()
      tima:::go_to_cache()
    }
  }
)
