#' Run Glassdoor App
#'
#' @return Data in shiny, translated to .Rmarkdown document
#' @export
#'
#' @examples
#' \dontrun{
#' runApp()
#' }
runApp <- function(){
  appDir <- system.file("shiny-app", package = "GSPglass")
  if (appDir == "") {
    stop("Could not find directory. Try reinstalling `GSPglass`", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}
