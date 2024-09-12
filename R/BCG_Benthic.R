#' Function to call the Benthic BCG model
#'
#' @return Calls the app for using the Benthic model
#'
#' @import shiny
#' @export
#'
#'

BCG_Benthic <- function(){
  shiny::runApp(system.file("Benthic", package = "BCG.PR.USVI"))
}
