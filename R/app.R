#' Run shiny application
#' @import shiny
#' @import bslib
#' @param browser logical launch in browser
#' @export
run_app <- function(browser = TRUE)
{
  shiny::addResourcePath("www", system.file("app/www/", package = "RPBCapp"))
  shiny::shinyApp(ui = ui, server = server, options = list(launch.browser = browser))
}
