#' Run shiny application
#' @import shiny
#' @import bslib
#' @param browser logical launch in browser
#' @export
run_app <- function(browser = TRUE)
{
  shiny::addResourcePath("www", system.file("app/www/", package = "RPBCapp"))

  # create the app
  app <- shiny::shinyApp(ui = ui, server = server)

  # run it and capture the stopApp() return value
  ans <- shiny::runApp(app, launch.browser = browser)

  if (identical(ans, "upgrade")) {
    message("Updating RPBCapp from GitHub...")
    remotes::install_github("RadiataPineBreedingCompany/RPBCapp", upgrade = "never")
  }

  invisible()
}
