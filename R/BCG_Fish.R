#' Function to call the BCG Fish model
#'
#' Executes the shinyapp for the BCG Fish model. The app will take your data,
#' asses its quality, and then calculate the BCG level for the temporal and
#' spatial scales that were selected by the user. Additional metrics include
#' endangered and invasive species.
#'
#' @return Executes the app for using the BCG Fish model.
#'
#' @author Arturo Sanchez-Porras
#'
#' @seealso
#' [BCG_Benthic()]
#' [BCG_Formatter()]
#'
#' @aliases BCGFish
#'
#' @export
#' @import DT
#' @import shiny
#' @import tidyr
#' @import dplyr
#' @import stringr
#' @import shinycssloaders
#' @import leaflet
#' @import rio
#' @import capture
#' @import plotly
#' @import rfishbase
#' @import worrms
#' @import bslib
#' @import shinyFeedback
#' @import waiter
#'
#' @examples
#' if(interactive()) {
#'   BCG_Fish()
#' }
#'
#'

BCG_Fish <- function(){
  shiny::runApp(system.file("Fish", package = "BCG.PR.USVI"))
}
