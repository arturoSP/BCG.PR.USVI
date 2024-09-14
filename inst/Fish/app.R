if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}
if (!requireNamespace("capture", quietly = TRUE)) {
  remotes::install_github("dreamRs/capture")
}

# Load libraries and helper files

library(shiny)
library(dplyr)
library(tidyr)
library(stringr)
library(shinycssloaders)
library(leaflet)
library(rio)
library(DT)
library(capture)
library(plotly)
library(rfishbase)
library(worrms)

source("./Fish_utils.R")
source("./Fish_QualityControl.R")
source("./Fish_Model.R")

# User interface ----
ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "litera"),
  shinyFeedback::useShinyFeedback(),
  waiter::use_waiter(),

  navlistPanel(
    id = "tabset",
    selected = "Preface and guide of use",
    well = TRUE,
    fluid = TRUE,
    widths = c(3,9),
    "BCG Fish App Control Panel",
    tabPanel("Preface and guide of use",
             fluidRow(tags$iframe(src = "./BCG_Coral_Reef_App.html",
                                  width = "100%", height = "900px",
                                  scrolling = "yes")
                      )
             ),
    tabPanel("Upload your data",
             fluidRow(br()),

             fluidRow(column(4,
                             fileInput("WorkFile",
                                       "Load the data",
                                       buttonLabel = "Browse",
                                       accept = c(".xls", ".xlsx", ".csv"),
                                       multiple = FALSE)
                             )
                      ),

             fluidRow(hr()),

             fluidRow(column(12,
                             h4("Data"),
                             DT::dataTableOutput("headData")
                             )
                      ),
             fluidRow(hr()),

             fluidRow(column(4,
                             uiOutput("inputYear")
                             ),
                      column(4,
                             uiOutput("inputSite")
                             ),
                      column(4,
                             uiOutput("inputTransect"),
                             h5(textOutput("nYear")),
                             h5(textOutput("nSite")),
                             h5(textOutput("nTransect"))
                             )
                      ),
             ),

    tabPanel("Quality control",
             fluidRow(br()),
             fluidRow(column(12,
                             br(),
                             h4("Data quality control test"))
                      ),
             fluidRow(column(8,
                             actionButton("quality", "Go!")),
                      column(4,
                             downloadButton("DownloadQuality",
                                            "Download data"))
                      ),
             fluidRow(br(),
                      column(12,
                             withSpinner(DT::dataTableOutput("Misspell"),
                                                         type = 4))
                      ),
             fluidRow(hr()),

             fluidRow(column(12,
                             textOutput("NotRecognized"),
                             textOutput("NumericDate"),
                             textOutput("EmptySpecies")
                             )
                      )
             ),

    tabPanel("BCG Results",
             fluidRow(br()),
             fluidRow(column(8,
                             h4("BCG levels"),
                             withSpinner(DT::dataTableOutput("Metrics"),
                                         type = 4)),
                      column(4,
                             br(),
                             downloadButton("Download",
                                            "Download metrics"),
                             br(), br(),
                             downloadButton("DownloadResults",
                                            "Download results"),
                             br(), br(),
                             capture::capture(selector = "#map1",
                                              filename = paste0("Map_", Sys.Date(), ".png"),
                                              icon("download"),
                                              "Download map")
                             )
                      ),

             fluidRow(column(12,
                             br(),
                             div(id = "map1",
                                 withSpinner(leafletOutput("map",
                                                           width = "100%"),
                                             type = 4)
                                 )
                             )
                      )
             ),
    tabPanel("Other indicators",
             fluidRow(br()),
             fluidRow(column(4,
                             h4("Vulnerable species"),
                             withSpinner(DT::dataTableOutput("ThreatSp"),
                                         type = 4)),
                      column(4,
                             h4("Invasive species"),
                             withSpinner(DT::dataTableOutput("InvasiveSp"),
                                         type = 0)),
                      column(4,
                             downloadButton("DownloadVulnerable",
                                            "Download vulnerable species list"),
                             br(), br(),
                             downloadButton("DownloadInvasive",
                                            "Download invasive species list")),
                      column(4,
                             uiOutput("histSp")),
                      column(4,
                             uiOutput("histTime")),
                      column(4,
                             uiOutput("histSpatial"))
                      ),
             fluidRow(#uiOutput("histSp"),
                      withSpinner(plotlyOutput("LenWei"),
                                  type = 0))
             )
  )
)

# Server logic ----
server <- function(input, output, session) {

  # Get the data ---------
  # Store the data to show it to the user
  MetricData <- reactive({
    req(input$WorkFile)
    import(input$WorkFile$datapath)
  })

  cNames <- reactive({
    req(input$WorkFile)
    colnames(MetricData())
  })

  output$headData <- DT::renderDataTable(MetricData(),
                              options = list(pageLength = 10,
                                             lengthMenu = c(5, 10, 25),
                                             scrollX = TRUE,
                                             searching = FALSE),
                              rownames = FALSE,
                              style = "default"
                              )

  # Ask for the names of columns containing temporal and spatial dimensions
  output$inputYear <- renderUI({
    req(input$WorkFile)
    selectInput("InputYear", "Temporal factor", character(), choices = NULL)
  })
  output$inputSite <- renderUI({
    req(input$WorkFile)
    selectInput("InputSite", "Spatial factor", character(), choices = NULL)
  })

  observeEvent(MetricData(), {
    updateSelectInput(session,
                      "InputYear",
                      choices = temporalValues[temporalValues %in% cNames()]
    )

    updateSelectInput(session,
                      "InputSite",
                      choices = spatialValues[spatialValues %in% cNames()]
    )

  })

  # Read the number of years in the data
  numYear <- reactive({
    req(input$InputYear)
    unique(MetricData()[[input$InputYear]])
  })

  output$nYear <- renderText({
    if(is.numeric(numYear())){
      paste("Number of temporal units:", "\n", length(numYear()))
    } else {
      paste("Error:\nTemporal factor must be numeric")
    }
  })

  colYear <- reactive({
    input$InputYear
  })

  # Read the number of sites in the data
  numSite <- reactive({
    req(input$InputSite)
    unique(MetricData()[[input$InputSite]])
  })

  output$nSite <- renderText({
    paste("Number of spatial units:", "\n", length(numSite()))
  })

  colSite <- reactive({
    input$InputSite
  })

  # Read the number of transects in the data
  numTransect <- reactive({
    req(input$WorkFile)
    unique(MetricData()$PRIMARY_SAMPLE_UNIT)
  })

  output$nTransect <- renderText({
    paste("Number of sampling units:", "\n", length(numTransect()))
  })

  colTransect <- reactive({
    #input$InputTransect
    "PRIMARY_SAMPLE_UNIT"
  })

  # Quality control ------------------------------
  # print a table with the rownames and data that is not registered in the MasterList
  Misspelling <- eventReactive(input$quality, {
    F_Misspelling_2(F_Misspelling_1(MetricData()))
  })

  output$Misspell <- DT::renderDataTable(Misspelling(),
                              options = list(pageLength = 5,
                                             lengthMenu = c(5, 10, 25),
                                             scrollX = T,
                                             searching = F),
                              rownames = F,
                              style = "default")

  output$DownloadQuality <- downloadHandler(
    filename = function() {
      paste0("BCGFish_Quality_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(Misspelling(), file, row.names = F)
    }
  )

  ## Warnings for the user ----
  # number of species that are not in the  master list
  NotRecognized <- eventReactive(req(input$quality),
                                 label = "SpeciesNotInList",
                                 {
                                   F_NotRecognized(Misspelling())
                                 }
  )
  output$NotRecognized <- renderText(NotRecognized())

  NumericDate <- eventReactive(req(input$quality),
                               label = "DateNotNumeric",
                               {
                                 F_NumericDate(MetricData(), colYear())
                               }
  )
  output$NumericDate <- renderText(NumericDate())

  EmptySpecies <- eventReactive(req(input$quality),
                                label = "EmptySpeciesName",
                                {
                                  F_EmptySpecies(MetricData())
                                }
  )
  output$EmptySpecies <- renderText(EmptySpecies())

  # Compute the results ----
  mData <- reactive({
    req(input$WorkFile)
    F_tSamp(MetricData())
  })

  M_Result <- reactive({
    req(input$WorkFile)
    M_Fish_1(mData(), colTransect(), colSite(), colYear())
  })

  DefinLevel <- reactive({
    req(input$WorkFile)
    M_Fish_2(M_Result(), MetricData(), colSite(), colYear())
  })

  output$Metrics <- DT::renderDataTable(
    tryCatch({
      DefinLevel()  |>
        `colnames<-`(c(str_to_sentence(colSite()), str_to_sentence(colYear()), "Level"))
    },
    error = function(e){
      return(tribble(~'WARNING', "Verify temporal and/or spatial factors."))
      message("Verify temporal and/or spatial factors.")
    }),
    options = list(pageLength = 5,
                   lengthMenu = c(5, 10, 25),
                   scrollX = T,
                   searching = F),
    rownames = F,
    style = "default"
  )

  output$Download <- downloadHandler(
    filename = function() {
      paste0("BCGFish_Metrics_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(M_Result() |>
                  `colnames<-`(c(str_to_sentence(colSite()), str_to_sentence(colYear()),
                                 "Model", "Metric_name", "MCalc", "Numeric_rule", "Symbol",
                                 "Lower", "Upper", "Level", "Description", "Units", "MemberValue",
                                 "Membership")),
                file, row.names = F)
    }
  )
  output$DownloadResults <- downloadHandler(
    filename = function() {
      paste0("BCGFish_Results_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(DefinLevel() |>
                  `colnames<-`(c(str_to_sentence(colSite()), str_to_sentence(colYear()), "Level")),
                file, row.names = F)
    }
  )

  # Print a map with the location of the sampling sites ----
  coord <- reactive({
    F_map(MetricData(), colSite(), colYear(), DefinLevel())
  })

  mapa1 <- reactive({
    tryCatch({
      F_MapParam(coord())
    },
    error = function(e){
      message("There was an error, contact admin.")
    })
  })
  output$map <- renderLeaflet(mapa1())

  userMap <- reactive({
    mapa1() %>%
      setView(lng = input$map_center$lng,
              lat = input$map_center$lat,
              zoom = input$map_zoom)
  })

  # Other indicators ----
  ThreatSp <- reactive({
    F_threatSp(MetricData(), colSite(), colYear())
  })

  output$ThreatSp <- DT::renderDataTable(tryCatch({
    ThreatSp()
  },
  error = function(e){
    return(tibble())
    message("Select temporal and/or spatial factors.")
  },
  warning = function(w){
    return(tibble())
    message("Select temporal and/or spatial factors.")
  }),
  options = list(pageLength = 5,
                 lengthMenu = c(5, 10, 25),
                 scrollX = T,
                 searching = F),
  rownames = F,
  style = "default")

  Invasive <- reactive({
      F_invasiveSp(MetricData(), colSite(), colYear())
    })
  output$InvasiveSp <- DT::renderDataTable(tryCatch({
    Invasive()},
    error = function(e){
      return(NULL)
      message("Select temporal and/or spatial factors.")
    },
    warning = function(w){
      return(NULL)
      message("Select temporal and/or spatial factors.")
    }
  ),
  options = list(pageLength = 5,
                 lengthMenu = c(5, 10, 25),
                 scrollX = T,
                 searching = F),
  rownames = F,
  style = "default")

  VulnerableLabel <- reactive({
    tryCatch({
      ifelse(is.null(ThreatSp()), "", "Vulnerable species")
    },
    error = function(e){
      return(paste(" "))
      message("Select temporal and/or spatial factors.")
    })
  })
  output$vulnerableDATA <- renderText({VulnerableLabel()})

  InvasiveLabel <- reactive({
    tryCatch({
      ifelse(is.null(Invasive()), "", "Invasive species")
    },
    error = function(e){
      return(paste(" "))
      message("Select temporal and/or spatial factors.")
    })

  })
  output$invasiveDATA <- renderText({InvasiveLabel()})

  output$DownloadVulnerable <- downloadHandler(
    filename = function() {
      paste0("BCGFish_VulnerableSpp_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(ThreatSp(), file, row.names = F)
    }
  )

  output$DownloadInvasive <- downloadHandler(
    filename = function() {
      paste0("BCGFish_InvasiveSpp_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(Invasive(), file, row.names = F)
    }
  )

  LenWei <- reactive({
    req(input$WorkFile)
    F_LenWei(mData(), colSite(), colYear())
  })

  AvailableSp <- reactive({
    req(input$WorkFile)
    F_PlotParams(LenWei(), "Species")
  })
  output$histSp <- renderUI({
    req(input$WorkFile)
    selectInput("histSp", "Species available to plot", choices = AvailableSp())
  })

  AvailableTime <- reactive({
    req(input$WorkFile)
    F_PlotParams(LenWei(), "Temporal")
  })
  output$histTime <- renderUI({
    req(input$WorkFile)
    selectInput("histTime", "Times available to plot", choices = AvailableTime())
  })

  AvailableSpatial <- reactive({
    req(input$WorkFile)
    F_PlotParams(LenWei(), "Spatial")
  })
  output$histSpatial <- renderUI({
    req(input$WorkFile)
    selectInput("histSpatial", "Sites available to plot", choices = AvailableSpatial())
  })

  selSp <- reactive({
    input$histSp
  })

  selTm <- reactive(
    input$histTime
  )

  selSpt <- reactive(
    input$histSpatial
  )

  p1 <- reactive({
    p1 <- pl_hist(LenWei(), selSp(), selTm(), selSpt())

    return(p1)
  })

  output$LenWei <- renderPlotly(
    tryCatch({p1()},
             error = function(e){
               return(plot_ly(
                 x = c(0.5), y = c(0.5), text = "Not enough data",
                 type = 'scatter', mode = 'text',
                 textfont = list(size = 20, color = 'red'))%>%
                   layout(
                     xaxis = list(showticklabels = FALSE, zeroline = FALSE),
                     yaxis = list(showticklabels = FALSE, zeroline = FALSE),
                     showlegend = FALSE
                   ))
               message("Not enough data.")
             })
    )


}

# Run the app ----
shinyApp(ui = ui, server = server)
