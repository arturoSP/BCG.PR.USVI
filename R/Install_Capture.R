#' Install Capture
#'
#' @return Installs the package `capture` from GitHub
#'
#' @importFrom remotes install_github
#' @importFrom utils install.packages
#' @keywords internal
#'

install_capture <- function(){
  if (!requireNamespace("remotes", quietly = TRUE)) {
    install.packages("remotes")
  }
  if (!requireNamespace("capture", quietly = TRUE)) {
    remotes::install_github("dreamRs/capture")
  }
}
