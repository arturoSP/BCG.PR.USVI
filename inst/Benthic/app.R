library(rio)
library(shiny)
library(dplyr)
library(tidyr)
library(stringr)
library(worrms)
library(shinycssloaders)
library(leaflet)
library(DT)
library(capture)
source("utils.R")
source("QualityControl.R")
source("BSAT_Model.R")
source("BCG_Model.R")

# Test material, do not delete ----
#
# tempTemplateLPI3 <- import("./CR_BCG_Data/tests/TestTemplate01.xlsx", sheet = "LPI")
# tempTemplateMOB3 <- import("./CR_BCG_Data/tests/TestTemplate01.xlsx", sheet = "MOBILE_FAUNA")
# tempTemplateDEM3 <- import("./CR_BCG_Data/tests/TestTemplate01.xlsx", sheet = "DEMO")
# colYear = "YEAR"
# colSite = "SITE"
# colTransect = "PRIMARY_SAMPLE_UNIT"
# Samp <- F_SampBCG_Long(F_Template_BCG(tempTemplateLPI3, tempTemplateMOB3, tempTemplateDEM3), colTransect, colSite, colYear)
# Samp <- F_SampBSAT_Long(F_Template_BSAT(tempTemplateLPI3, tempTemplateMOB3), colTransect, colSite, colYear)

# User interface ----
ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "litera"),
  shinyFeedback::useShinyFeedback(),
  waiter::use_waiter(),

  navlistPanel(
    id = "tabset",
    selected = "Preface and guide of use",
    well = T,
    fluid = T,
    widths = c(3,9),
    "BCG App Control Panel",
    tabPanel("Preface and guide of use",
             fluidRow(tags$iframe(src = "./BCG_Coral.html",
                                  width = "100%", height = "900px",
                                  scrolling = "yes"))
             ),
    tabPanel("Upload your data",
             fluidRow(br()),
             fluidRow(column(4, fileInput("WorkFile", "Load the data",
                                                  buttonLabel = "Browse...",
                                                  accept = c(".xls", ".xlsx", ".csv"),
                                                  multiple = F)
                             ),
                      column(4, selectInput("ModelSel", "Select a model",
                                            choices = c("BCG (Demo + LPI)", "BSAT (LPI)"),
                                            selected = "BSAT (LPI)")
                             )
                      ),
             fluidRow(hr()),

             fluidRow(column(12,
                             h4(textOutput("lpiDATA")),
                             dataTableOutput("headLPI"),
                             h4(textOutput("demoDATA")),
                             dataTableOutput("headDEMO"),
                             h4(textOutput("mobDATA")),
                             dataTableOutput("headMOB")
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
                             h4("Data quality control test"))),
             fluidRow(column(8,
                             actionButton("quality", "Go!")),
                      column(4,
                             downloadButton("DownloadQuality", "Download data"))),
             fluidRow(br(),
                      column(12,
                             withSpinner(dataTableOutput("Misspell"), type = 4))),
             fluidRow(hr()),

             fluidRow(column(12,
                             textOutput("OrbAcro"),
                             textOutput("NotRecognized"),
                             textOutput("NumericDate"),
                             textOutput("EmptySpecies"),
                             hr(),
                             textOutput("EmptyTransect"),
                             textOutput("LPI100"),
                             dataTableOutput("Not100")
                             )
                      )
            ),

    tabPanel("BCG results",
             fluidRow(br()),
             fluidRow(column(8,
                             h4("BCG/BSAT levels"),
                             withSpinner(dataTableOutput("Metrics"), type = 4)),
                      column(4,
                             br(),
                             downloadButton("Download", "Download metrics"),
                             br(), br(),
                             downloadButton("DownloadResults", "Download results"),
                             br(), br(),
                             capture::capture(selector = "#map1",
                                              filename = paste0("Map_",Sys.Date(),".png"),
                                              icon("download"), "Download map"))),
             fluidRow(column(12,
                             br(),
                             div(
                               id = "map1",
                               withSpinner(leafletOutput("map", width = "100%"), type = 0)
                             )
                             )
                      )
             ),

    tabPanel("Other indicators",
             fluidRow(br()),
             fluidRow(column(12,
                             h4("Other ecological indicators"),
                             withSpinner(dataTableOutput("OtherInd"), type = 4))
                      ),
             fluidRow(hr()),

             fluidRow(column(4,
                             h4(textOutput("vulnerableDATA")),
                             withSpinner(dataTableOutput("ThreatSp2"), type = 0)),
                      column(4,
                             h4(textOutput("invasiveDATA")),
                             withSpinner(dataTableOutput("InvasiveSp2"), type = 0)),
                      column(4,
                             downloadButton("DownloadOther", "Download additional metrics"),
                             br(),br(),
                             downloadButton("DownloadVulnerable", "Download vulnerable species list"),
                             br(),br(),
                             downloadButton("DownloadInvasive", "Download invasive species list"))
             )
    )
))

server <- function(input, output, session) {
# Start here --------------------------------------------------------------
  # read the file with data, ask for it to be either excel or csv file
  MetricData <- reactive ({
    req(input$WorkFile)
      if(ModSel() == "BSAT (LPI)") {
        sheetLPI <- import(input$WorkFile$datapath, sheet = "LPI")
        sheetMOB <- import(input$WorkFile$datapath, sheet = "MOBILE_FAUNA")
        F_Template_BSAT(sheetLPI, sheetMOB)
      } else {
        sheetLPI <- import(input$WorkFile$datapath, sheet = "LPI")
        sheetMOB <- import(input$WorkFile$datapath, sheet = "MOBILE_FAUNA")
        sheetDEMO <- import(input$WorkFile$datapath, sheet = "DEMO")
        F_Template_BCG(sheetLPI, sheetMOB, sheetDEMO)
      }
  })

  TableList <- reactive({
    req(input$WorkFile)
    if(ModSel() == "BSAT (LPI)") {
      sheetLPI <- import(input$WorkFile$datapath, sheet = "LPI")
      sheetMOB <- import(input$WorkFile$datapath, sheet = "MOBILE_FAUNA")
      list(sheetLPI, sheetMOB)
    } else {
      sheetLPI <- import(input$WorkFile$datapath, sheet = "LPI")
      sheetMOB <- import(input$WorkFile$datapath, sheet = "MOBILE_FAUNA")
      sheetDEMO <- import(input$WorkFile$datapath, sheet = "DEMO")
      list(sheetLPI, sheetMOB, sheetDEMO)
    }
  })

  # publish the first lines of the table to confirm the information is correct.
  DemoData <- reactive({
    if(ModSel() != "BSAT (LPI)") {
      TableList()[[3]]
      } else {NULL}
    }
  )
  LPILabel <- reactive({
    req(input$WorkFile)
    "LPI data"
  })
  DemoLabel <- reactive({
    req(input$WorkFile)
    ifelse(is.null(DemoData()) == T, "", "DEMO data")
  })
  MobLabel <- reactive({
    req(input$WorkFile)
    "Mobile fauna data"
  })

  output$lpiDATA <- renderText({LPILabel()})
  output$demoDATA <- renderText({DemoLabel()})
  output$mobDATA <- renderText({MobLabel()})


  output$headLPI <- renderDT(TableList()[[1]],
                           options = list(pageLength = 5,
                                          lengthMenu = c(5, 10, 25),
                                          scrollX = T,
                                          searching = F),
                           rownames = F,
                           style = "default"
                           )
  output$headDEMO <- renderDT(DemoData(),
                              options = list(pageLength = 5,
                                             lengthMenu = c(5, 10, 25),
                                             scrollX = T,
                                             searching = F),
                              rownames = F,
                              style = "default"
                              )
  output$headMOB <- renderDT(TableList()[[2]],
                             options = list(pageLength = 5,
                                            lengthMenu = c(5, 10, 25),
                                            scrollX = T,
                                            searching = F),
                             rownames = F,
                             style = "default"
                             )

  # Ask for the names of the columns containing years, sites and transects
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
                      choices = c("YEAR", "MONTH", "DAY"), selected = "")

    updateSelectInput(session,
                      "InputSite",
                      choices = c("REGION", "SUB_REGION", "SITE", "LOCATION", "HABITAT", "PRIMARY_SAMPLE_UNIT"), selected = "")

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

  # Visualise results ------------------------------
  # Selection of model, either reduced or full, then choose the adequate rules
  ModSel <- reactive(input$ModelSel)

  Samp <- reactive({
        if(ModSel() == "BSAT (LPI)") {
        F_SampBSAT_Long(MetricData(), colTransect(), colSite(), colYear())
      } else if(ModSel() == "BCG (Demo + LPI)") {
        F_SampBCG_Long(MetricData(), colTransect(), colSite(), colYear())
      }
    })

  M_Result <- reactive({
    if(ModSel() == "BSAT (LPI)") {
      M_BSAT_1(Samp(), colTransect(), colSite(), colYear())
      }
    else {
      M_BCG_1(Samp(), colTransect(), colSite(), colYear())
    }
  })

  DefinLevel <- reactive({
    if(ModSel() == "BSAT (LPI)") {
      M_BSAT_2(M_Result())
    }
    else {
      M_BCG_2(M_Result())
    }
  })
  output$Metrics <- renderDT(
    tryCatch({
      DefinLevel() %>%
        `colnames<-`(c(str_to_sentence(colSite()), str_to_sentence(colYear()), "Level"))
    },
    error = function(e){
      return(tribble(~'WARNING', "Select temporal and/or spatial factors."))
      message("Select temporal and/or spatial factors.")
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
      paste0(str_replace_all(ModSel(), " ", ""), "_Metrics_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(M_Result() %>%
                  `colnames<-`(c(str_to_sentence(colSite()), str_to_sentence(colYear()),
                                 "Model", "Metric_name", "MCalc", "Numeric_rule", "Symbol",
                                 "Lower", "Upper", "Level", "Description", "Units", "MemberValue",
                                 "Membership")),
                file, row.names = F)
    }
  )
  output$DownloadResults <- downloadHandler(
    filename = function() {
      paste0(str_replace_all(ModSel(), " ", ""), "_Results_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(DefinLevel() %>%
                  `colnames<-`(c(str_to_sentence(colSite()), str_to_sentence(colYear()), "Level")),
                file, row.names = F)
    }
  )

  # Data for the "Other indicators" panel ------------------------------
  otherInd <- reactive({
    if(ModSel() == "BSAT (LPI)"){
      F_otherInd_1(Samp(), colTransect(), colSite(), colYear())
    }
    else {
    F_otherInd_2(Samp(), colTransect(), colSite(), colYear())
    }
  })

  output$OtherInd <- renderDT(tryCatch({
    otherInd()
  },
  error = function(e){
    return(tribble(~'WARNING', "Select temporal and/or spatial factors."))
    message("Select temporal and/or spatial factors.")
  },
  warning = function(w){
    return(tibble())
    message("Select temporal and/or spatial factors.")
  }
  ),
                                     options = list(pageLength = 5,
                                                    lengthMenu = c(5, 10, 25),
                                                    scrollX = T,
                                                    searching = F),
                              rownames = F,
                              style = "default")

  ThreatSp2 <- reactive({
    if(ModSel() == "BSAT (LPI)"){
      F_threatSp(Samp(), colSite(), colYear())
    }
    else {
      F_threatSp_2(Samp(), colSite(), colYear())
    }
  })

  output$ThreatSp2 <- renderDT(tryCatch({
    ThreatSp2()
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

  Invasive2 <- reactive({
    if(ModSel() == "BSAT (LPI)"){
      F_invasiveSp(Samp(), colSite(), colYear())
    }
    else {
      F_invasiveSp_2(Samp(), colSite(), colYear())
    }
  })
  output$InvasiveSp2 <- renderDT(tryCatch({
    Invasive2()},
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
      ifelse(is.null(ThreatSp2()) == T, "", "Vulnerable species")
    },
    error = function(e){
      return(paste(" "))
      message("Select temporal and/or spatial factors.")
    })
  })
  output$vulnerableDATA <- renderText({VulnerableLabel()})

  InvasiveLabel <- reactive({
    tryCatch({
      ifelse(is.null(Invasive2()) == T, "", "Invasive species")
    },
    error = function(e){
      return(paste(" "))
      message("Select temporal and/or spatial factors.")
    })

  })
  output$invasiveDATA <- renderText({InvasiveLabel()})

  output$DownloadOther <- downloadHandler(
    filename = function() {
      paste0(str_replace_all(ModSel(), " ", ""), "_AdditionalMetrics_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(otherInd(), file, row.names = F)
    }
  )

  output$DownloadVulnerable <- downloadHandler(
    filename = function() {
      paste0(str_replace_all(ModSel(), " ", ""), "_VulnerableSpp_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(ThreatSp2(), file, row.names = F)
    }
  )

  output$DownloadInvasive <- downloadHandler(
    filename = function() {
      paste0(str_replace_all(ModSel(), " ", ""), "_InvasiveSpp_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(Invasive2(), file, row.names = F)
    }
  )

  # Quality control ------------------------------
  # print a table with the rownames and data that is not registered in the MasterList
  Misspelling <- eventReactive(input$quality, {
    F_Misspelling_2(F_Misspelling_1(Samp()))
  })

  output$Misspell <- renderDT(Misspelling(),
                                     options = list(pageLength = 5,
                                                    lengthMenu = c(5, 10, 25),
                                                    scrollX = T,
                                                    searching = F),
                              rownames = F,
                              style = "default")

  output$DownloadQuality <- downloadHandler(
    filename = function() {
      paste0(str_replace_all(ModSel(), " ", ""), "_Quality_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(Misspelling(), file, row.names = F)
    }
  )

  # List of warnings that the user may receive ----
  OrbAcro <- eventReactive(req(input$quality),
                           label = "OrbicellaAcoporaPresence",
                           {
                             if(ModSel() == "BSAT (LPI)") {
                               F_OrbAcro(Samp())
                               } else {
                                 F_OrbAcro2(Samp())
                               }
                             }
                           )
  output$OrbAcro <- renderText(OrbAcro())

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
                                 F_EmptySpecies(Samp())
                               }
  )
  output$EmptySpecies <- renderText(EmptySpecies())

  # table with data from the incorrect transects
  Not100 <- eventReactive(req(input$quality),
                          label = "Not100Transect",
                          {
                            tempA <- F_Not100(Samp(), colTransect(), colSite(), colYear())
                            return(tempA)
                            }
                          )
  output$Not100 <- renderDataTable(
    tryCatch({
      Not100() %>%
        transmute(Year = as.integer(Year),
                  Site = Site,
                  `Sampling unit` = as.character(Transect),
                  `Recorded points` = as.integer(RecordPoints)) %>%
        arrange(Year, Site, `Sampling unit`)},
      error = function(e){
        return(tibble())
        message("Select temporal and/or spatial factors.")
      },
      warning = function(w){
        return(tibble())
        message("Select temporal and/or spatial factors.")
      }
      ),
      options = list(pageLength = 5,
                     lengthMenu = c(5, 10, 25),
                     scrollX = T),
      rownames = F,
      style = "default",
      )

  # number of transects that sum 0
  EmptyTransect <- eventReactive(req(input$quality),
                                 label = "EmptyTransect", {
                                   tryCatch({
                                     tempA <- Not100() %>%
                                       filter(RecordPoints == 0) %>%
                                       nrow()

                                     tempB <- if(tempA == 1) {paste("WARNING: There is", tempA, "sampling unit that is empty.")
                                     } else {
                                       if(tempA > 1) {paste("WARNING: There are", tempA, "sampling units that are empty.")
                                       }
                                       else{""}
                                     }
                                     return(tempB)
                                   },
                                   error = function(e){
                                     message("Select temporal and/or spatial factors.")
                                   },
                                   warning = function(w){
                                     message("Select temporal and/or spatial factors.")
                                   }
                                   )
                                 }
                                 )
  output$EmptyTransect <- renderText(EmptyTransect())

  # number of lpi transects that are not 100
  LPI100 <- eventReactive(req(input$quality),
                          label = "LPINotCompletedTransect", {
                            tryCatch({
                              tempA <- Not100() %>%
                                filter(RecordPoints > 0) %>%
                                nrow()
                              tempB <- if(tempA == 1) {paste("WARNING: There is", tempA, "sampling unit that includes data, but the sum of points is different from 100.\nConfirm your information with the next chart:")}

                              else {
                                if(tempA > 1) {paste("WARNING: There are", tempA, "sampling units that include data, but the sum of points is different from 100.\nConfirm your information with the next chart:")}
                                else {""}
                              }
                              return(tempB)
                            },
                            error = function(e){
                              message("Select temporal and/or spatial factors.")
                            },
                            warning = function(w){
                              message("Select temporal and/or spatial factors.")
                              }
                            )
                          }
                          )
  output$LPI100 <- renderText(LPI100())

  # number of repeated transects
  DoubleTransect <- eventReactive(req(input$quality),
                                  label = "RepeatedTransect",
                                  {F_DoubleTransect(MetricData(), colTransect())
                                    }
                                  )
  output$DoubleTransect <- renderText(DoubleTransect())

  # Print a map with the location of the sampling sites ----
  coord <- reactive({
    F_map(Samp(), colSite(), colYear(), DefinLevel())
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

}

# Run the application ----
shinyApp(ui = ui, server = server)
