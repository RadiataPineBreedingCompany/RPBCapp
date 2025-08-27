#' @export
run_myapp <- function() {
  app_dir <- system.file("app", package = "RPBCapp")
  shiny::runApp(app_dir, display.mode = "normal")
}
