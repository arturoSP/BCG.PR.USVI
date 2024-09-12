#' Function to call the BCG do-it-all formatter
#'
#' @return Calls the app for using the BCG Formatter
#'
#' @import shiny
#' @export
#'

BCG_Formatter <- function(){
  shiny::runApp(system.file("Format", package = "BCG.PR.USVI"))
}
