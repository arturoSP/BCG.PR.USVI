#' DataUploadSmall UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_DataUploadSmall_ui <- function(id){
  ns <- NS(id)
  tagList(
    fileInput(ns("GeneralInput"),
              label = "Upload your data:",
              multiple = FALSE),
    uiOutput(ns("checkSheet")),
    uiOutput(ns("wideFish1")),
    uiOutput(ns("wideFish2"))
  )
}

#' DataUploadSmall Server Functions
#'
#' @noRd
mod_DataUploadSmall_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    ## Reactive for sheet names ----
    sheetNames <- reactive({
      req(input$GeneralInput)
      readxl::excel_sheets(input$GeneralInput$datapath)
    })

    ## UI for selecting sheet ----
    output$checkSheet <- renderUI({
      selectInput(ns("selectedSheet"),
                  "Choose a sheet to read:",
                  choices = sheetNames(),
                  selected = NULL)
    })

    ## Reactive for selected sheet ----
    SSheet <- reactive({
      req(input$selectedSheet)
      input$selectedSheet
    })

    ## Reactive for reading data ----
    InputDT <- reactive({
      req(input$GeneralInput, SSheet())
      readxl::read_excel(input$GeneralInput$datapath,
                         sheet = SSheet(),
                         .name_repair = "universal") %>%
        `colnames<-`(stringr::str_to_upper(colnames(.)))
    })

    # output$InputTable <- renderDT(InputDT())

    # get the names of the columns in the selected sheet ----
    columnNames <- reactive({
      req(input$GeneralInput, SSheet())
      colnames(InputDT())
    })

    output$wideFish1 <- renderUI({
      selectInput(ns("wideFish1"),
                  "Registry begins at:",
                  choices = columnNames(),
                  selected = NULL)
    })
    output$wideFish2 <- renderUI({
      selectInput(ns("wideFish2"),
                  "Registry ends at:",
                  choices = columnNames(),
                  selected = NULL)
    })

    wideStart <- reactive({
      req(input$wideFish1)
        input$wideFish1
    })
    wideEnd <- reactive({
      req(input$wideFish2)
        input$wideFish2
    })

    return(list(
      selectedSheet = SSheet,
      InputDT = InputDT,
      wideStart = wideStart,
      wideEnd = wideEnd,
      columnNames = columnNames
    ))
  })
}

## To be copied in the UI
# mod_DataUploadSmall_ui("DataUploadSmall_1")

## To be copied in the server
# mod_DataUploadSmall_server("DataUploadSmall_1")
