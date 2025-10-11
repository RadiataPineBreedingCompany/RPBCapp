#' Run shiny application
#' @import shiny
#' @import bslib
#' @export
run_app <- function()
{
  shiny::addResourcePath("www", system.file("app/www/", package = "RPBCapp"))
  shiny::shinyApp(ui = ui, server = server)
}
