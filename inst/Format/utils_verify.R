#' verify
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

f_VerifyColumns <- function(varData, type){
  nC <- length(varData)

  # check that at least one variable is selected
  if(nC == 0 & type != "coordinates"){
    showModal(modalDialog(
      title = "Error in selection",
      "You did not make any selections. Please select at least one variable.",
      easyClose = TRUE,
      footer = NULL
    ))
    verified <- "Please check your selection again."
  } else {

    # check for temporal variables, print the number
    verified <- if(type == "temporal"){
      if(nC == 1){
        paste0("There is 1 temporal variable.")
      } else {
        paste0("There are ", nC, " temporal variables.")
      }

      # check for spatial variables, print the number
    } else if(type == "spatial"){
      if(nC == 1){
        paste0("There is 1 spatial variable.")
      } else {
        paste0("There are ", nC, " spatial variables.")
      }

      # check for transect variable. It must be only one, otherwise it gives a warning
    } else if(type == "transect"){
      if(nC > 1){
        showModal(modalDialog(
          title = "Error in transect variable",
          "You selected more than one variable. Please select only one.",
          easyClose = TRUE,
          footer = NULL
        ))
      } else {
        paste0("Your transect variable is: ", varData, ".")
      }

      # check for coordinates variables. They must be two, otherwise it gives a warning
    } else if(type == "coordinates"){
      if(nC == 2){
        paste0("Your selections for latitude and longited are ",
               varData[1], " and ", varData[2], ", respectively.")
      } else if(nC >0) {
        showModal(modalDialog(
          title = "Error in coordinates variables",
          "You selected either one or more than two variables to represent the
          coordinates. Please adjust your selection so that there are two
          variables selected.",
          easyClose = TRUE,
          footer = NULL
        ))
        paste0("Please check your selection again.")
      } else if(nC == 0){
        paste0("Your data does not seem to include coordinates.")
      }
    }
  }

  return(verified)
}
