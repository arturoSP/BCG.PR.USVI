#' DataUpload UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_DataUpload_ui <- function(id){
  ns <- NS(id)
  tagList(
    fileInput(ns("GeneralInput"),
              label = "Upload your data:",
              multiple = FALSE,
              accept = c(".csv", ".xlsx", ".xls")),
    uiOutput(ns("checkSheet")),
    tableOutput(ns("hojas")),
    fluidRow(withSpinner(DTOutput(ns("InputTable"), width = "100%"),
                         type = 4))
    )
}

#' DataUpload Server Functions
#'
#' @noRd
mod_DataUpload_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    ## Reactive for file type ----
    fileType <- reactive({
      req(input$GeneralInput)
      tools::file_ext(input$GeneralInput$name)
    })

    ## Reactive for sheet names ----
    sheetNames <- reactive({
      req(input$GeneralInput,
          fileType() %in% c("xlsx", "xls"))
      readxl::excel_sheets(input$GeneralInput$datapath)
    })

    ## UI for selecting sheet ----
    output$checkSheet <- renderUI({
      if(fileType() %in% c("xlsx", "xls")) {
        selectInput(ns("checkSheet"),
                    "Choose a sheet to read:",
                    choices = sheetNames(),
                    selected = NULL)
      }
    })

    ## observer to get selected sheet to NULL when the file changes ----
    observeEvent(input$GeneralInput, {
      updateSelectInput(session, ns("checkSheet"),
                        choices = sheetNames(),
                        selected = NULL)
    })

    ## Reactive for selected sheet ----
    SSheet <- reactive({
      req(input$GeneralInput)
      input$checkSheet
    })

    ## Reactive for reading data ----
    InputDT <- reactive({
      req(input$GeneralInput, input$checkSheet)
      if(fileType() == "csv") {
        readr::read_csv(input$GeneralInput$datapath,
                        show_col_types = FALSE,
                        name_repair = "universal") %>%
          `colnames<-`(stringr::str_to_upper(colnames(.)))
      } else if(fileType() %in% c("xlsx", "xls")) {
        req(SSheet())
        readxl::read_excel(input$GeneralInput$datapath,
                           sheet = SSheet(),
                           .name_repair = "universal") %>%
          `colnames<-`(stringr::str_to_upper(colnames(.)))
      }
    })

    output$InputTable <- renderDT(head(InputDT()))

    ## get the names of the columns in the selected sheet ----
    columnNames <- reactive({
      req(input$GeneralInput)
      colnames(InputDT())
    })

  return(list(
    sheetNames = sheetNames,
    InputDT = InputDT,
    columnNames = columnNames
    ))
  })
}

## To be copied in the UI
# mod_DataUpload_ui("DataUpload_1")

## To be copied in the server
# mod_DataUpload_server("DataUpload_1")
