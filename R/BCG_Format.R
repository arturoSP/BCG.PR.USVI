#' Function to call the BCG do-it-all formatter
#'
#' Executes the shinyapp for the BCG formatter. The app will try to adapt your
#' data from LPI, BSAT and Fish surveys to the templates that are used by the
#' models apps. It takes the user step by step in the consolidation of data and
#' then retuns an \code{.xlsx} file that can be readily used.
#'
#' @return Executes the app for using the BCG Formatter.
#'
#' @author Arturo Sanchez-Porras
#'
#' @seealso
#' [BCG_Benthic()]
#' [BCG_Fish()]
#'
#' @aliases BCGFormatter
#'
#' @export
#' @import DT
#' @import shiny
#' @import rfishbase
#' @import dplyr
#' @import shinycssloaders
#' @importFrom purrr map2
#'
#' @examples
#' if(interactive()){
#'   BCG_Formatter()
#' }
#'
#'

BCG_Formatter <- function(){
  shiny::runApp(system.file("Format", package = "BCG.PR.USVI"))
}
