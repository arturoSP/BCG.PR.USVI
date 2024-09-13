#' Function to call the BCG Benthic model
#'
#' Executes the shinyapp for the BCG Benthic model. The app will take your data,
#' asses its quality, and then calculate the BCG level for the temporal and
#' spatial scales that were selected by the user. Additional metrics include
#' endangered and invasive species.
#'
#' @return Executes the app for using the BCG Benthic model.
#'
#' @author Arturo Sanchez-Porras
#'
#' @seealso
#' [BCG_Fish()]
#' [BCG_Formatter()]
#'
#' @aliases BCGBenthic
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
#' @import worrms
#'
#' @examples
#' if(interactive()){
#'   BCG_Benthic()
#' }
#'
#'

BCG_Benthic <- function(){
  shiny::runApp(system.file("Benthic", package = "BCG.PR.USVI"))
}
