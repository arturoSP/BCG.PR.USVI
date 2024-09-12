#' Function to call the Fish BCG model
#'
#' @return Calls the app for using the Fish model
#'
#' @import shiny
#' @export
#'

BCG_Fish <- function(){
  shiny::runApp(system.file("Fish", package = "BCG.PR.USVI"))
}
