# Load packages and helper files ----
library(DT)
library(rfishbase)
library(dplyr)
library(shinycssloaders)

source("./mod_DataUpload.R")
source("./mod_DataUpload2.R")
source("./mod_DataUploadSmall.R")
source("./utils_helpers.R")
source("./utils_LongColumns.R")
source("./utils_name_mapping.R")
source("./utils_verify.R")
source("./utils_WideColumns.R")

# # Testing data, do not delete
# inputDT <- readxl::read_xlsx("./data/Test_Format_Fish_Wide.xlsx", sheet = 5)
# columnNames <- names(inputDT)
# year <- "Year"
# month <- "MONTH"
# region <- "Site"
# subregion <- "sub_region"
# location <- NULL
# habitat <- NULL
# primsampleu <- "PRIMARY_SAMPLE_UNIT"
# latitude <- "Lat_degrees"
# longitude <- "LON_degrees"
# wideLPIMetersCR <- "METERS_COMPLETED"
# wideLPIT1R <- "c0"
# wideLPIT2R <- "c5"
# timeVar <- c(year, month)
# spatialVar <- c(region, subregion, location, habitat)
# transectVar <- primsampleu
# coordinateVar <- c(latitude, longitude)
# specVar <- "SCIENTIFIC_NAME"
# lenVar <- NULL
# datashape <- "Wide"
# selected_values <- c(year, month, region, subregion, location, habitat, primsampleu, latitude, longitude)

# User interface ---
ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "litera"),
  titlePanel("DO-IT-ALL BCG Formatter"),
  tabsetPanel(
    id = "tabset",
    type = "hidden",

    # Start here ----
    tabPanel(
      "page_Ini",
      fluidRow(
        column(
          12,
          h3("Select a model", ),
          radioButtons(
            "SelectModel",
            label = NULL,
            choices = c("BCG Coral (Demo + LPI)", "BSAT (LPI)", "BCG Fish"),
            selected = "BCG Fish"
          )
        ),
        mod_DataUpload2_ui("Intro"),
      ),
      actionButton("page_Ini_SCol", "Continue")
    ),

    # Selection of experiment variables ----
    tabPanel(
      "page_SCol",
      h3("Choose your sampling parameters"),
      fluidRow(
        column(
          4,
          h4("Time scale"),
          selectInput("year", "Year:", choices = NULL, selected = NULL),
          selectInput("month", "Month:", choices = NULL, selected = NULL),
          selectInput("day", "Day:", choices = NULL, selected = NULL)
        ),
        column(
          4,
          h4("Spatial scale"),
          selectInput("region", "Region:", choices = NULL, selected = NULL),
          selectInput("subregion", "Subregion:", choices = NULL, selected = NULL),
          selectInput("site", "Site:", choices = NULL, selected = NULL),
          selectInput("location", "Location:", choices = NULL, selected = NULL),
          selectInput("habitat", "Habitat:", choices = NULL, selected = NULL)
        ),
        column(
          4,
          h4("Sampling unit"),
          selectInput("primsampleu", "Primary sample unit:", choices = NULL, selected = NULL),
          h4("Coordinates"),
          selectInput("latitude", "Latitude:", choices = NULL, selected = NULL),
          selectInput("longitude", "Longitude", choices = NULL, selected = NULL)
        ),
        span("Lets review your selections."),
        span("Click the button when you are ready."),
        actionButton("verifyVariables", "Check", width = "100%"),
        textOutput("timeColumns"),
        textOutput("spatialColumns"),
        textOutput("transectColumn"),
        textOutput("coordinateColumns"),
        hr(),
        span(
          "If you agree with these parameters, click the \"Save\"
                        button here:"
        ),
        actionButton("saveVariables", "Save", width = "100%"),
        # textOutput("vectorSelected"), # aquí podría quedar un mensaje flotante en vez de este texto
        # textOutput("coordinatedFlag")
      ),
      h3("Format selection"),
      fluidRow(column(
        8,
        span("The next step depends on how your data was arranged."),
        br(),
        span(
          "If your data for different species is distributed
              in several columns, click on the \"Wide\" button.
              Otherwise, if you have a single column for listing
              species, then click on the \"Long\" button."
        )
      ), column(
        4,
        radioButtons(
          "dataShape",
          "Pick the one that describes your data:",
          choices = c("Wide", "Long"),
          selected = NULL
        )
      )),
      br(),
      actionButton("page_SCol_Ini", "Back"),
      actionButton("page_SCol_xFish", "Continue")
    ),

    # Options for WideFish data ----
    tabPanel(
      "page_SWFish",
      fluidRow(
        h3("Specifics of the model"),
        radioButtons(
          "FWideOptions",
          "Select the option that best represents
                                your data:",
          choiceNames = list(
            "Length data only.",
            "Length and biomass data in one sheet.",
            "Length and biomass data in separate sheets.",
            "Coded length (i.e. c0, c1, c2, ...)."
          ),
          choiceValues = list(
            "WFLength", "WFOneSheet",
            "WFTwoSheets", "WFCoded"
          ),
          selected = NULL,
          width = "100%"
        ),
        column(6, uiOutput("wideFishT1"), uiOutput("wideFishB1")),
        column(6, uiOutput("wideFishT2"), uiOutput("wideFishB2")),
        column(6, uiOutput("wideFishSpp"))
      ),
      uiOutput("TwoSheetData"),
      br(),
      br(),
      span("When you are ready, hit the \"Save\" button:"),
      actionButton("wideFishSave", "Save", width = "100%"),
      h3("This is your data:"),
      withSpinner(DT::dataTableOutput("wideFishFinal"), type = 4),
      textOutput("wideFishNA"),
      br(),
      actionButton("page_SWFish_SCol", "Back"),
      actionButton("page_SWFish_DlBCG", "Continue")
    ),

    # WideFish data, for the case when you have data in separate sheets ----
    tabPanel(
      "page_WFTwoSheets",
      h3("Upload your files here:"),
      fluidRow(
        column(
          6,
          h4("Length data:"),
          mod_DataUploadSmall_ui("WFLength"),
          uiOutput("wideFishT12"),
          uiOutput("wideFishT22")
        ),
        column(
          6,
          h4("Biomass data:"),
          mod_DataUploadSmall_ui("WFBiomass"),
          uiOutput("wideFishB12"),
          uiOutput("wideFishB22")
        )
      ),
      br(),
      br(),
      span("When you are ready, hit the \"Save\" button:"),
      actionButton("wideFishSaveTwoSheets", "Save", width = "100%"),
      h3("This is your data:"),
      withSpinner(DT::dataTableOutput("wideFishFinalTwoSheets"), type = 4),
      br(),
      actionButton("page_WFTwoSheets_SCol", "Back"),
      actionButton("page_WFTwoSheets_DlBCG", "Continue")
    ),

    # LongFish data, either to calculate biomass or not ----
    tabPanel(
      "page_LFish",
      fluidRow(
        h3("Long Fish data"),
        column(
          4,
          radioButtons(
            "CalcBiomass",
            "Indicate if you need to calculate the fish
                biomass by selecting the corresponding option.",
            choices = c("Yes", "No"),
            selected = NULL
          )
        ),
        column(
          4,
          radioButtons(
            "CodedLongLength",
            "Indicate if your length data is coded (i.e. c0, c1, c2, ...).",
            choices = c("Yes (e.g. c0, c1, c2, ...)" = "Yes",
                        "No" = "No"),
            selected = "No"
          )
        )
      ),
      fluidRow(
        column(4, uiOutput("longFishName")),
        column(4, uiOutput("longFishLen")),
        column(4, uiOutput("longFishBiomass")),
        br(),
        span("When you are ready, hit the \"Save\" button:"),
        actionButton("longFishSave", "Save", width = "100%"),
        h3("This is your data:"),
        withSpinner(DT::dataTableOutput("longFishFinal"), type = 4) # ,
      ),
      br(),
      actionButton("page_LFish_SCol", "Back"),
      actionButton("page_LFish_DlBCG", "Continue"),
      actionButton("page_LFish_Ini", "Start over")
    ),

    # Mobile fauna data for Corals model ----
    tabPanel(
      "page_SMob",
      fluidRow(
        h3("Mobile species data"),
        mod_DataUpload_ui("Mobile")
      ),
      actionButton("page_SMob_SCol", "Back"),
      actionButton("page_SMob_xMob", "Continue"),
      actionButton("page_SMob_skip", "Skip this step")
    ),

    # Wide mobile fauna results ----
    tabPanel(
      "page_WMob",
      fluidRow(
        # mod_Wide_ui("Wide_Mobile")
        h3("Wide Mobile Species data"),
        span(
          "Please prepare your data so that you have consolidated
                        blocks to present the counts on observed mobile species.
                        You will use this information to make some selections in
                        here."
        ),
        span("1. Select the first column for the count of observed
                        species"),
        span("2. Select the last column for the count of observed
                        species."),
        column(4, uiOutput("wideMobMetersC")),
        column(4, uiOutput("wideMobT1")),
        column(4, uiOutput("wideMobT2")),
        br(),
        br(),
        span("When you are ready, hit the \"Save\" button:"),
        actionButton("wideMobSave", "Save", width = "100%"),
        h3("This is your data:"),
        withSpinner(DT::dataTableOutput("wideMobFinal"), type = 4)
      ),
      br(),
      actionButton("page_WMob_SMob", "Back"),
      actionButton("page_WMob_SLPI", "Continue")
    ),

    # Long mobile fauna results ----
    tabPanel(
      "page_LMob",
      fluidRow(
        h3("Long Mobile species data"),
        span("Select the name of your variables."),
        column(4, uiOutput("longMobMetersC")),
        column(4, uiOutput("longMobSpecies")),
        column(4, uiOutput("longMobCounts")),
        br(),
        br(),
        span("When you are ready, hit the \"Save\" button:"),
        actionButton("longMobSave", "Save", width = "100%"),
        h3("This is your data:"),
        withSpinner(DT::dataTableOutput("longMobFinal"), type = 4)
      ),
      br(),
      actionButton("page_LMob_SMob", "Back"),
      actionButton("page_LMob_SLPI", "Continue")
    ),

    # LPI data for corals model ----
    tabPanel(
      "page_SLPI",
      fluidRow(
        h3("LPI records"),
        mod_DataUpload_ui("LPI")
      ),
      br(),
      actionButton("page_SLPI_SMob", "Back"),
      actionButton("page_SLPI_xLPI", "Continue"),
      actionButton("page_SLPI_skip", "Skip this step")
    ),

    # Wide LPI data for corals ----
    tabPanel(
      "page_WLPI",
      fluidRow(
        h3("Wide LPI data"),
        span(
          "Please prepare your data so that you have consolidated
                        blocks to present the counts on observed mobile species.
                        You will use this information to make some selections in
                        here."
        ),
        span("1. Select the first column for the count of observed
                        species"),
        span("2. Select the last column for the count of observed
                        species."),
        column(4, uiOutput("wideLPIMetersC")),
        column(4, uiOutput("wideLPIT1")),
        column(4, uiOutput("wideLPIT2")),
        br(),
        br(),
        span("When you are ready, hit the \"Save\" button:"),
        actionButton("wideLPISave", "Save", width = "100%"),
        h3("This is your data:"),
        withSpinner(DT::dataTableOutput("wideLPIFinal"), type = 4)
      ),
      br(),
      actionButton("page_WLPI_SLPI", "Back"),
      actionButton("page_WLPI_SDEMO", "Continue")
    ),

    # Long LPI data for corals ----
    tabPanel(
      "page_LLPI",
      fluidRow(
        h3("Long LPI data"),
        span("Select the name of your variables."),
        column(4, uiOutput("longLPIMetersC")),
        column(4, uiOutput("longLPISpecies")),
        column(4, uiOutput("longLPICover")),
        br(),
        br(),
        span("When you are ready, hit the \"Save\" button:"),
        actionButton("longLPISave", "Save", width = "100%"),
        h3("This is your data:"),
        withSpinner(DT::dataTableOutput("longLPIFinal"), type = 4)
      ),
      br(),
      actionButton("page_LLPI_SLPI", "Back"),
      actionButton("page_LLPI_SDEMO", "Continue")
    ),

    # DEMO data for Corals model ----
    tabPanel(
      "page_SDEMO",
      fluidRow(
        h3("DEMO records"),
        mod_DataUpload_ui("DEMO")
      ),
      br(),
      actionButton("page_SDEMO_SLPI", "Back"),
      actionButton("page_SDEMO_xDEMO", "Continue"),
      actionButton("page_SDEMO_skip", "Skip this step")
    ),

    # Wide DEMO data ----
    tabPanel(
      "page_WDEMO",
      fluidRow(
        h3("Wide DEMO data"),
        span(
          "Please prepare your data so that you have consolidated
                        blocks to present the counts on observed mobile species.
                        You will use this information to make some selections in
                        here."
        ),
        span("1. Select the first column for the count of observed
                        species"),
        span("2. Select the last column for the count of observed
                        species."),
        column(4, uiOutput("wideDEMOMetersC")),
        column(
          4,
          textInput("wideDEMOMaxDiam", "Maximum diameter", "MAX_DIAMETER")
        ),
        column(
          4,
          textInput(
            "wideDEMOPerpDiam",
            "Perpendicular diameter",
            "PERP_DIAMETER"
          )
        ),
        column(4, textInput("wideDEMOHeight", "Height", "HEIGHT")),
        column(4, textInput(
          "wideDEMOOldMort", "Old mortality", "OLD_MORT"
        )),
        column(
          4,
          textInput("wideDEMORecentMort", "Recent mortality", "RECENT_MORT")
        ),
        column(
          4,
          textInput(
            "wideDEMOBleach",
            "Presence of bleaching",
            "BLEACH_CONDITION"
          )
        ),
        column(
          4,
          textInput("wideDEMODisease", "Presence of diseases", "DISEASE")
        ),
        br(),
        br(),
        span("When you are ready, hit the \"Save\" button:"),
        actionButton("wideDEMOSave", "Save", width = "100%"),
        h3("This is your data:"),
        withSpinner(DT::dataTableOutput("wideDEMOFinal"), type = 4)
      ),
      br(),
      actionButton("page_WDEMO_SDEMO", "Back"),
      actionButton("page_WDEMO_DlBCG", "Continue"),
      actionButton("page_WDEMO_Ini", "Start over")
    ),

    # Long DEMO data ----
    tabPanel(
      "page_LDEMO",
      fluidRow(
        h3("Long DEMO data"),
        span("Select the name of your variables."),
        column(4, uiOutput("longDEMOMetersC")),
        column(4, uiOutput("longDEMOSpecies")),
        column(4, uiOutput("longDEMONCol")),
        column(4, uiOutput("longDEMOMaxDiam")),
        column(4, uiOutput("longDEMOPerpDiam")),
        column(4, uiOutput("longDEMOHeight")),
        column(4, uiOutput("longDEMOOldMort")),
        column(4, uiOutput("longDEMORecentMort")),
        column(4, uiOutput("longDEMOBleach")),
        column(4, uiOutput("longDEMODisease")),
        br(),
        br(),
        span("When you are ready, hit the \"Save\" button:"),
        actionButton("longDEMOSave", "Save", width = "100%"),
        h3("This is your data:"),
        withSpinner(DT::dataTableOutput("longDEMOFinal"), type = 4)
      ),
      br(),
      actionButton("page_LDEMO_SDEMO", "Back"),
      actionButton("page_LDEMO_DlBCG", "Continue"),
      actionButton("page_LDEMO_Ini", "Start over")
    ),

    # Download the results ----
    tabPanel(
      "page_DlBCG",
      fluidRow(
        h3("Download your data"),
        downloadButton("downloadBCG", "Download the formatted data")
      ),
      actionButton("page_DlBCG_Ini", "Start over")
    )
  )
)

# Server logic ----
server <- function(input, output, session) {
  options(shiny.maxRequestSize = 20*1024^2) #set upload limit to 10MB

  # Pagination buttons ----
  switch_page <- function(i) {
    updateTabsetPanel(
      inputId = "tabset",
      selected = paste0("page_", i)
    )
  }

  observeEvent(input$page_Ini_SCol, switch_page("SCol"))
  observeEvent(input$page_SCol_xFish, {
    if (Model() == "BCG Fish") {
      if (dataShape() == "Wide") {
        switch_page("SWFish")
      } else {
        switch_page("LFish")
      }
    } else {
      switch_page("SMob")
    }
  })
  observeEvent(input$page_SCoord_SCol, switch_page("SCol"))
  observeEvent(input$page_SCoord_xFish, {
    if (Model() == "BCG Fish") {
      if (dataShape() == "Wide") {
        switch_page("SWFish")
      } else {
        switch_page("LFish")
      }
    } else {
      switch_page("SMob")
    }
  })
  observeEvent(input$page_SCol_Ini, switch_page("Ini"))
  observeEvent(input$page_SWFish_SCol, switch_page("SCol"))
  observeEvent(input$page_SWFish_DlBCG, switch_page("DlBCG"))
  observeEvent(input$TwoSheetData, switch_page("WFTwoSheets"))
  observeEvent(input$FreqCoded, switch_page("WFFreqCoded"))
  observeEvent(input$page_WFTwoSheets_SCol, switch_page("SCol"))
  observeEvent(input$page_WFTwoSheets_DlBCG, switch_page("DlBCG"))
  observeEvent(input$page_WFFreqCoded_SCol, switch_page("SCol"))
  observeEvent(input$page_WFFreqCoded_DlBCG, switch_page("DlBCG"))
  observeEvent(input$page_LFish_SCol, switch_page("SCol"))
  observeEvent(input$page_LFish_DlBCG, switch_page("DlBCG"))
  observeEvent(input$page_LFish_Ini, switch_page("Ini"))
  observeEvent(input$page_SMob_xMob, {
    if (dataShape() == "Wide") {
      switch_page("WMob")
    } else {
      switch_page("LMob")
    }
  })
  observeEvent(input$page_SMob_SCol, switch_page("SCol"))
  observeEvent(input$page_SMob_skip, switch_page("SLPI"))
  observeEvent(input$page_WMob_SLPI, switch_page("SLPI"))
  observeEvent(input$page_WMob_SMob, switch_page("SMob"))
  observeEvent(input$page_LMob_SLPI, switch_page("SLPI"))
  observeEvent(input$page_LMob_SMob, switch_page("SMob"))
  observeEvent(input$page_SLPI_xLPI, {
    if (dataShape() == "Wide") {
      switch_page("WLPI")
    } else {
      switch_page("LLPI")
    }
  })
  observeEvent(input$page_SLPI_SMob, switch_page("SMob"))
  observeEvent(input$page_SLPI_skip, {
    if (Model() == "BCG Coral (Demo + LPI)") {
      switch_page("SDEMO")
    } else {
      switch_page("DlBCG")
    }
  })
  observeEvent(input$page_WLPI_SDEMO, {
    if (Model() == "BCG Coral (Demo + LPI)") {
      switch_page("SDEMO")
    } else {
      switch_page("DlBCG")
    }
  })
  observeEvent(input$page_WLPI_SLPI, switch_page("SLPI"))
  observeEvent(input$page_LLPI_SDEMO, {
    if (Model() == "BCG Coral (Demo + LPI)") {
      switch_page("SDEMO")
    } else {
      switch_page("DlBCG")
    }
  })
  observeEvent(input$page_LLPI_SLPI, switch_page("SLPI"))
  observeEvent(input$page_SDEMO_xDEMO, {
    if (dataShape() == "Wide") {
      switch_page("WDEMO")
    } else {
      switch_page("LDEMO")
    }
  })
  observeEvent(input$page_SDEMO_SLPI, switch_page("SLPI"))
  observeEvent(input$page_SDEMO_skip, switch_page("DlBCG"))
  observeEvent(input$page_WDEMO_SDEMO, switch_page("SDEMO"))
  observeEvent(input$page_WDEMO_DlBCG, switch_page("DlBCG"))
  observeEvent(input$page_WDEMO_Ini, switch_page("Ini"))
  observeEvent(input$page_LDEMO_SDEMO, switch_page("SDEMO"))
  observeEvent(input$page_LDEMO_DlBCG, switch_page("DlBCG"))
  observeEvent(input$page_LDEMO_Ini, switch_page("Ini"))
  observeEvent(input$page_DlBCG_Ini, switch_page("Ini"))

  # Show a logo ----
  output$logo <- renderImage(
    {
      filename <- "./inst/app/www/hexlogo.png"
      list(
        src = filename,
        width = "86px",
        height = "100px"
      )
    },
    deleteFile = FALSE
  )

  # Select the model to work with ----
  Model <- reactive({
    req(input$SelectModel)

    selected <- input$SelectModel
  })

  Intro <- mod_DataUpload2_server("Intro")

  # Ask the user to select the variables to work with ----
  # initial_columns <- reactive(colnames(Intro$InputDT()))
  initial_columns <- reactive(Intro$columnNames())

  # Regresar a la versión inicializada de los nombres de variables cuando se carga un nuevo archivo
  observeEvent(Intro$InputDT(), {
    initial_columns <- reactive(colnames(Intro$InputDT()))

    rv <- reactiveValues(
      available = initial_columns,
      selected = list(
        year = NULL,
        month = NULL,
        day = NULL,
        region = NULL,
        subregion = NULL,
        site = NULL,
        location = NULL,
        primsampleu = NULL,
        latitude = NULL,
        longitude = NULL
      )
    )
  })

  # Almacenar opciones disponibles y seleccionadas
  rv <- reactiveValues(
    available = initial_columns,
    selected = list(
      year = NULL,
      month = NULL,
      day = NULL,
      region = NULL,
      subregion = NULL,
      site = NULL,
      location = NULL,
      habitat = NULL,
      primsampleu = NULL,
      latitude = NULL,
      longitude = NULL
    )
  )

  # Function to update the selection options
  update_choices <- function() {
    selected_values <- unlist(rv$selected)

    updateSelectInput(session,
      "year",
      choices = f_CheckColumns(initial_columns(), "temporal"), # initial_columns(),
      selected = rv$selected$year
    )
    updateSelectInput(session,
      "month",
      choices = f_CheckColumns(initial_columns(), "temporal"),
      selected = rv$selected$month
    )
    updateSelectInput(session,
      "day",
      choices = f_CheckColumns(initial_columns(), "temporal"),
      selected = rv$selected$day
    )
    updateSelectInput(session,
      "region",
      choices = f_CheckColumns(initial_columns(), "spatial"),
      selected = rv$selected$region
    )
    updateSelectInput(session,
      "subregion",
      choices = f_CheckColumns(initial_columns(), "spatial"),
      selected = rv$selected$subregion
    )
    updateSelectInput(session,
      "site",
      choices = f_CheckColumns(initial_columns(), "spatial"),
      selected = rv$selected$site
    )
    updateSelectInput(session,
      "location",
      choices = f_CheckColumns(initial_columns(), "spatial"),
      selected = rv$selected$location
    )
    updateSelectInput(session,
      "habitat",
      choices = f_CheckColumns(initial_columns(), "spatial"),
      selected = rv$selected$habitat
    )
    updateSelectInput(session,
      "primsampleu",
      choices = f_CheckColumns(initial_columns(), "transect"),
      selected = rv$selected$primsampleu
    )
    updateSelectInput(session,
      "latitude",
      choices = f_CheckColumns(initial_columns(), "coordinates"),
      selected = rv$selected$latitude
    )
    updateSelectInput(session,
      "longitude",
      choices = f_CheckColumns(initial_columns(), "coordinates"),
      selected = rv$selected$longitude
    )
  }

  # Observers to update the selected variables
  observeEvent(input$year, {
    rv$selected$year <- input$year
  })
  observeEvent(input$month, {
    rv$selected$month <- input$month
  })
  observeEvent(input$day, {
    rv$selected$day <- input$day
  })
  observeEvent(input$region, {
    rv$selected$region <- input$region
  })
  observeEvent(input$subregion, {
    rv$selected$subregion <- input$subregion
  })
  observeEvent(input$site, {
    rv$selected$site <- input$site
  })
  observeEvent(input$location, {
    rv$selected$location <- input$location
  })
  observeEvent(input$habitat, {
    rv$selected$habitat <- input$habitat
  })
  observeEvent(input$primsampleu, {
    rv$selected$primsampleu <- input$primsampleu
  })
  observeEvent(input$latitude, {
    rv$selected$latitude <- input$latitude
  })
  observeEvent(input$longitude, {
    rv$selected$longitude <- input$longitude
  })

  # Inicializar las opciones de selección al inicio
  observeEvent(input$page_Ini_SCol, {
    update_choices()
  })

  # Show the user a summary of her selections ----
  ## List of time variables ----
  timeVar <- eventReactive(
    input$verifyVariables,
    {
      vect <- c(rv$selected$year, rv$selected$month, rv$selected$day) |>
        unique()
    }
  )

  output$timeColumns <- renderText({
    varSummary <- f_VerifyColumns(timeVar(), "temporal")
    return(paste0("Temporal: ", varSummary))
  })

  ## List of place variables ----
  spatialVar <- eventReactive(
    input$verifyVariables,
    {
      vect <- c(
        rv$selected$region, rv$selected$subregion,
        rv$selected$site, rv$selected$location,
        rv$selected$habitat
      ) |>
        unique()
    }
  )

  output$spatialColumns <- renderText({
    varSummary <- f_VerifyColumns(spatialVar(), "spatial")
    return(paste0("Spatial: ", varSummary))
  })

  ## Show transect variable ----
  transectVar <- eventReactive(
    input$verifyVariables,
    {
      vect <- c(rv$selected$primsampleu)
    }
  )

  output$transectColumn <- renderText({
    varSummary <- f_VerifyColumns(transectVar(), "transect")
    return(paste0("Transect: ", varSummary))
  })

  ## List of coordinate variables ----
  coordinateVar <- eventReactive(
    input$verifyVariables,
    {
      vect <- c(rv$selected$latitude, rv$selected$longitude) |>
        unique()
    }
  )

  output$coordinateColumns <- renderText({
    varSummary <- f_VerifyColumns(coordinateVar(), "coordinates")
    return(paste0("Coordinates: ", varSummary))
  })

  # Once finished selecting, the user can create the variable vector ----

  vectorSel <- eventReactive(
    input$saveVariables,
    {
      c(timeVar(), spatialVar(), transectVar(), coordinateVar())
    }
  )

  vectorSaved <- eventReactive(
    input$saveVariables,
    {
      req(input$checkTemporal, input$checkSpatial, input$checkTransect)
      "Your selection was saved."
    },
    ignoreNULL = FALSE
  )

  output$vectorSelected <- renderText(vectorSel())

  ## Ask the user what shape does the data have ----
  dataShape <- eventReactive(
    input$dataShape,
    {
      input$dataShape
    }
  )

  ## Ask the user how does the data look like for Wide Fish ----
  FWideOptions <- eventReactive(
    input$FWideOptions,
    {
      input$FWideOptions
    }
  )

  # Series of UI elements to select and prepare the data ----

  ## Wide fish all-in-one ----
  ### Get the blocks of columns for each case ----

  colNamesWT <- eventReactive(
    input$page_SCol_xFish,
    {
      f_WideColumns(Intro$columnNames(), vectorSel(), "counts")
    }
  )

  colNamesWB <- eventReactive(
    input$page_SCol_xFish,
    {
      f_WideColumns(Intro$columnNames(), vectorSel(), "weigths")
    }
  )

  ### Wide fish length start 1 ----
  output$wideFishT1 <- renderUI({
    if (FWideOptions() != "WFTwoSheets") {
      selectInput("wideFishT1",
        "Length registry begins at:",
        choices = colNamesWT()
      )
    }
  })

  wideFishT1R <- eventReactive(
    input$wideFishSave,
    {
      input$wideFishT1
    }
  )

  ### Wide fish length end 2 ----
  output$wideFishT2 <- renderUI({
    if (FWideOptions() != "WFTwoSheets") {
      selectInput("wideFishT2",
        "Length registry ends at:",
        choices = colNamesWT()
      )
    }
  })

  wideFishT2R <- eventReactive(
    input$wideFishSave,
    {
      input$wideFishT2
    }
  )

  ### Wide fish biomass start ----
  output$wideFishB1 <- renderUI({
    if (FWideOptions() == "WFOneSheet") {
      selectInput("wideFishB1",
        "Biomass registry begins at:",
        choices = colNamesWB()
      )
    }
  })

  wideFishB1R <- eventReactive(
    input$wideFishSave,
    {
      input$wideFishB1
    }
  )


  ### Wide fish biomass last ----
  output$wideFishB2 <- renderUI({
    if (FWideOptions() == "WFOneSheet") {
      selectInput("wideFishB2",
        "Biomass registry ends at:",
        choices = colNamesWB()
      )
    }
  })

  wideFishB2R <- eventReactive(
    input$wideFishSave,
    {
      input$wideFishB2
    }
  )

  ## Wide fish species name ----
  output$wideFishSpp <- renderUI({
    if(FWideOptions() == "WFCoded") {
      selectInput("wideFishSpp",
                  "Column for species name:",
                  choices = colNamesWT()
      )
    }
  })

  wideFishSppR <- eventReactive(
    input$wideFishSave,
    {
      input$wideFishSpp
    }
  )

  ### Finish by presenting the table ----
  wideFishTable <- eventReactive(
    input$wideFishSave,
    {
      fishTable <- if (FWideOptions() == "WFOneSheet") {
        f_W2LFish(Intro$InputDT(), vectorSel(),
          wideFishT1R(), wideFishT2R(),
          wideFishB1R(), wideFishB2R(),
          timeVar(), spatialVar(),
          coordinateVar(), transectVar(),
          specV = "SPECIES_NAME", type = "WFOneSheet"
        )
      } else if (FWideOptions() == "WFLength") {
        f_W2LFish(Intro$InputDT(), vectorSel(),
          wideFishT1R(), wideFishT2R(),
          BT1 = NULL, BT2 = NULL,
          timeVar(), spatialVar(),
          coordinateVar(), transectVar(),
          specV = "SPECIES_NAME", type = "WFLength"
        )
      } else if (FWideOptions() == "WFCoded") {
        f_C2LFish(Intro$InputDT(), vectorSel(),
          timeVar(), spatialVar(),
          coordinateVar(), transectVar(),
          specV = wideFishSppR(),
          FT1 = wideFishT1R(), FT2 = wideFishT2R(),
          type = "Wide"
        )
      }

      return(fishTable)
    }
  )

  output$wideFishFinal <- DT::renderDataTable(wideFishTable())

  ### Present a warning if the selections didn't match ----
  wFishNA <- eventReactive(
    input$wideFishSave,
    {
      wFNA <- sum(is.na(wideFishTable()$TOTAL_COUNT) |
        is.na(wideFishTable()$TOTAL_BIOMASS))
      wfNAWarning <- if (wFNA > 0) {
        paste0("WARNING: NAs were created during the operation, you might want
        to check your data or selected parameters for incomplete information.")
      } else {
        paste0("")
      }
      return(wfNAWarning)
    }
  )

  output$wideFishNA <- renderText(wFishNA())

  ### download Wide Fish button ----
  output$DownloadWideFish <- downloadHandler(
    filename = function() {
      paste0("FishData_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      writexl::write_xlsx(wideFishTable(), file)
    }
  )

  output$TwoSheetData <- renderUI({
    if (FWideOptions() == "WFTwoSheets") {
      actionButton("TwoSheetData",
        "For separate sheets, click here",
        width = "100%"
      )
    }
  })

  ### Wide fish, data comes from separate excel sheets ----
  WFLength <- mod_DataUploadSmall_server("WFLength")
  WFBiomass <- mod_DataUploadSmall_server("WFBiomass")

  wideFishTableTwoSheets <- eventReactive(
    input$wideFishSaveTwoSheets,
    {
      fishTable <- if (FWideOptions() == "WFTwoSheets") {
        f_W2LFish(WFLength$InputDT(), vectorSel(),
          WFLength$wideStart(), WFLength$wideEnd(),
          WFBiomass$wideStart(), WFBiomass$wideEnd(),
          timeVar(), spatialVar(),
          coordinateVar(), transectVar(),
          specV = "SPECIES_NAME", type = "WFTwoSheets",
          WFBiomass$InputDT()
        )
      }
    }
  )

  output$wideFishFinalTwoSheets <- DT::renderDataTable(wideFishTableTwoSheets())

  ## Long fish ----
  ### Find the length column ----
  calcBiomass <- reactive({
    req(input$CalcBiomass)
    signal <- input$CalcBiomass
  })

  codedLength <- reactive({
    req(input$CodedLongLength)
    signal <- input$CodedLongLength
  })

  output$longFishLen <- renderUI({
    colNames <- f_CheckColumns(Intro$columnNames(), "length")

    selectInput("longFishLen",
      "Confirm your length variable:",
      choices = colNames
    )
  })

  output$longFishName <- renderUI({
    colNames <- f_CheckColumns(Intro$columnNames(), "names")

    selectInput("longFishName",
      "Confirm your scientific names variable:",
      choices = colNames
    )
  })

  output$longFishBiomass <- renderUI({
    if (calcBiomass() == "No") {
      colNames <- f_CheckColumns(Intro$columnNames(), "biomass")

      selectInput("longFishBiomass",
        "Confirm your total biomass variable:",
        choices = colNames
      )
    }
  })

  longFishLength <- eventReactive(
    input$longFishLen,
    {
      input$longFishLen
    }
  )

  longFishNames <- eventReactive(
    input$longFishName,
    {
      input$longFishName
    }
  )

  longFishBiomass <- eventReactive(
    input$longFishBiomass,
    {
      input$longFishBiomass
    }
  )

  ### Calculate biomass ----
  longFishTable <- eventReactive(
    input$longFishSave,
    {
      if (calcBiomass() == "Yes") {
        if (codedLength() == "No") {
          options(warn = -1)
          fishTable <- f_cBiomass(
            Intro$InputDT(), longFishLength(),
            longFishNames()
          ) |>
            f_L2LFish(timeVar(), spatialVar(),
              coordinateVar(), transectVar(),
              type = "calc"
            )
          options(warn = 0)

          return(fishTable)
        } else if (codedLength() == "Yes") {
          fishTable <- f_C2LFish(Intro$InputDT(), vectorSel(),
            timeVar(), spatialVar(),
            coordinateVar(), transectVar(),
            longFishNames(),
            longFishLength(),
            type = "LongCalc"
          )

          return(fishTable)
        }
      } else if (calcBiomass() == "No") {
        if (codedLength() == "No") {
          fishTable <- f_L2LFish(Intro$InputDT(), timeVar(), spatialVar(),
            coordinateVar(), transectVar(),
            longFishNames(), longFishLength(),
            longFishBiomass(),
            type = "simple"
          )

          return(fishTable)
        } else if (codedLength() == "Yes") {
          fishTable <- f_C2LFish(Intro$InputDT(), vectorSel(),
            timeVar(), spatialVar(),
            coordinateVar(), transectVar(),
            longFishNames(),
            longFishLength(),
            longFishBiomass(),
            type = "LongSimple"
          )

          return(fishTable)
        }
      }
    }
  )

  output$longFishFinal <- DT::renderDataTable(longFishTable())

  ### download Long Fish button ----
  output$DownloadLongFish <- downloadHandler(
    filename = function() {
      paste0("FishData_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      writexl::write_xlsx(longFishTable(), file)
    }
  )

  # Mobile species in BCG corals ----
  ## Upload the data and check it ----
  Mobile <- mod_DataUpload_server("Mobile")

  ## Skip mobile step ----
  skipMobFlag <- reactiveVal(FALSE)
  observeEvent(input$page_SMob_skip, {
    skipMobFlag(TRUE)
  })
  observeEvent(input$page_SLPI_SMob, {
    skipMobFlag(FALSE)
  })
  observeEvent(input$page_WMob_SMob, {
    skipMobFlag(FALSE)
  })
  observeEvent(input$page_LMob_SMob, {
    skipMobFlag(FALSE)
  })
  observeEvent(input$page_SCol_xFish, {
    skipMobFlag(FALSE)
  })
  observeEvent(input$longMobSave, {
    skipMobFlag(FALSE)
  })
  observeEvent(input$wideMobSave, {
    skipMobFlag(FALSE)
  })


  ## Wide mobile ----
  ### Get the blocks of columns for each case ----

  colNamesWMob <- eventReactive(
    input$page_SMob_xMob,
    {
      f_WideColumns(Mobile$columnNames(), vectorSel(), "bcg_counts")
    }
  )

  ### Wide mobile sp1 ----
  output$wideMobT1 <- renderUI({
    selectInput("wideMobT1",
      "Observation registry begins at:",
      choices = colNamesWMob()
    )
  })

  wideMobT1R <- eventReactive(
    input$wideMobSave,
    {
      input$wideMobT1
    }
  )

  ### Wide mobile t2 ----
  output$wideMobT2 <- renderUI({
    selectInput("wideMobT2",
      "Observation registry ends at:",
      choices = colNamesWMob()
    )
  })

  wideMobT2R <- eventReactive(
    input$wideMobSave,
    {
      input$wideMobT2
    }
  )

  ### Meters completed ----
  output$wideMobMetersC <- renderUI({
    selectInput("wideMobMetersC",
      "Select a variable for transect length:",
      choices = c("None", colNamesWMob())
    )
  })

  wideMobMetersCR <- eventReactive(
    input$wideMobSave,
    {
      input$wideMobMetersC
    }
  )

  ### Finish by presenting the table ----
  wideMobTable <- eventReactive(
    input$wideMobSave,
    {
      f_W2LBCGcount(Mobile$InputDT(), vectorSel(),
        wideMobT1R(), wideMobT2R(),
        timeVar(), spatialVar(),
        coordinateVar(), wideMobMetersCR(),
        transectVar(),
        type = "Mobile"
      )
    }
  )

  output$wideMobFinal <- DT::renderDataTable(wideMobTable())

  ## Long format ----
  ### Meters completed ----
  output$longMobMetersC <- renderUI({
    selectInput("longMobMetersC",
      "Select a variable for transect length:",
      choices = c("None", colNamesWMob())
    )
  })
  longMobMetersCR <- eventReactive(
    input$longMobSave,
    {
      input$longMobMetersC
    }
  )

  output$longMobSpecies <- renderUI({
    selectInput("longMobSpecies",
      "Species name:",
      choices = colNamesWMob()
    )
  })
  longMobSpecies <- eventReactive(
    input$longMobSpecies,
    {
      input$longMobSpecies
    }
  )

  output$longMobCounts <- renderUI({
    selectInput("longMobCounts",
      "Counts:",
      choices = colNamesWMob()
    )
  })
  longMobCounts <- eventReactive(
    input$longMobCounts,
    {
      input$longMobCounts
    }
  )

  ### Finish by presenting the table ----
  longMobTable <- eventReactive(
    input$longMobSave,
    {
      f_L2LBCGSimple(Mobile$InputDT(),
        timeVar(), spatialVar(),
        coordinateVar(), longMobMetersCR(),
        transectVar(), longMobSpecies(),
        longMobCounts(),
        type = "Mobile"
      )
    }
  )

  output$longMobFinal <- DT::renderDataTable(longMobTable())

  # LPI species in BCG corals ----
  ## Upload the data and check it ----
  LPI <- mod_DataUpload_server("LPI")

  ## Skip LPI step ----
  skipLPIFlag <- reactiveVal(FALSE)
  observeEvent(input$page_SLPI_skip, {
    skipLPIFlag(TRUE)
  })
  observeEvent(input$page_SDEMO_SLPI, {
    skipLPIFlag(FALSE)
  })
  observeEvent(input$page_WMob_SLPI, {
    skipLPIFlag(FALSE)
  })
  observeEvent(input$page_LMob_SLPI, {
    skipLPIFlag(FALSE)
  })
  observeEvent(input$page_SMob_skip, {
    skipLPIFlag(FALSE)
  })
  observeEvent(input$page_WLPI_SLPI, {
    skipLPIFlag(FALSE)
  })
  observeEvent(input$page_LLPI_SLPI, {
    skipLPIFlag(FALSE)
  })
  observeEvent(input$longLPISave, {
    skipLPIFlag(FALSE)
  })
  observeEvent(input$wideLPISave, {
    skipLPIFlag(FALSE)
  })

  ### Get the blocks of columns for each case ----

  colNamesWLPI <- eventReactive(
    input$page_SLPI_xLPI,
    {
      f_WideColumns(LPI$columnNames(), vectorSel(), "bcg_counts")
    }
  )

  ### Wide LPI sp1 ----
  output$wideLPIT1 <- renderUI({
    selectInput("wideLPIT1",
      "Observation registry begins at:",
      choices = colNamesWLPI()
    )
  })

  wideLPIT1R <- eventReactive(
    input$wideLPISave,
    {
      input$wideLPIT1
    }
  )

  ### Wide LPI t2 ----
  output$wideLPIT2 <- renderUI({
    selectInput("wideLPIT2",
      "Observation registry ends at:",
      choices = colNamesWLPI()
    )
  })

  wideLPIT2R <- eventReactive(
    input$wideLPISave,
    {
      input$wideLPIT2
    }
  )

  ### Meters completed ----
  output$wideLPIMetersC <- renderUI({
    selectInput("wideLPIMetersC",
      "Select a variable for transect length:",
      choices = c("None", colNamesWLPI())
    )
  })

  wideLPIMetersCR <- eventReactive(
    input$wideLPISave,
    {
      input$wideLPIMetersC
    }
  )

  ### Finish by presenting the table ----
  wideLPITable <- eventReactive(
    input$wideLPISave,
    {
      f_W2LBCGcount(LPI$InputDT(), vectorSel(),
        wideLPIT1R(), wideLPIT2R(),
        timeVar(), spatialVar(),
        coordinateVar(), wideLPIMetersCR(),
        transectVar(),
        type = "LPI"
      )
    }
  )

  output$wideLPIFinal <- DT::renderDataTable(wideLPITable())

  ## Long format ----
  ### Meters completed ----
  output$longLPIMetersC <- renderUI({
    selectInput("longLPIMetersC",
      "Select a variable for transect length:",
      choices = c("None", colNamesWLPI())
    )
  })
  longLPIMetersCR <- eventReactive(
    input$longLPISave,
    {
      input$longLPIMetersC
    }
  )

  output$longLPISpecies <- renderUI({
    selectInput("longLPISpecies",
      "Species name:",
      choices = colNamesWLPI()
    )
  })
  longLPISpecies <- eventReactive(
    input$longLPISpecies,
    {
      input$longLPISpecies
    }
  )

  output$longLPICover <- renderUI({
    selectInput("longLPICover",
      "Cover percentage:",
      choices = colNamesWLPI()
    )
  })
  longLPICover <- eventReactive(
    input$longLPICover,
    {
      input$longLPICover
    }
  )

  ### Finish by presenting the table ----
  longLPITable <- eventReactive(
    input$longLPISave,
    {
      f_L2LBCGSimple(LPI$InputDT(),
        timeVar(), spatialVar(),
        coordinateVar(), longLPIMetersCR(),
        transectVar(), longLPISpecies(),
        longLPICover(),
        type = "LPI"
      )
    }
  )

  output$longLPIFinal <- DT::renderDataTable(longLPITable())

  # DEMO species in BCG corals ----
  ## Upload the data and check it ----
  DEMO <- mod_DataUpload_server("DEMO")

  ## Skip DEMO step ----
  skipDEMOFlag <- reactiveVal(FALSE)
  observeEvent(input$page_SDEMO_skip, {
    skipDEMOFlag(TRUE)
  })
  observeEvent(input$page_DlBCG_Ini, {
    skipDEMOFlag(FALSE)
  })
  observeEvent(input$page_SLPI_SDEMO, {
    skipDEMOFlag(FALSE)
  })
  observeEvent(input$page_WLPI_SDEMO, {
    skipDEMOFlag(FALSE)
  })
  observeEvent(input$page_LLPI_SDEMO, {
    skipDEMOFlag(FALSE)
  })
  observeEvent(input$page_WDEMO_SDEMO, {
    skipDEMOFlag(FALSE)
  })
  observeEvent(input$page_LDEMO_SDEMO, {
    skipDEMOFlag(FALSE)
  })
  observeEvent(input$longDEMOSave, {
    skipDEMOFlag(FALSE)
  })
  observeEvent(input$wideDEMOSave, {
    skipDEMOFlag(FALSE)
  })

  ### Get the blocks of columns for each case ----

  colNamesWDEMO <- eventReactive(
    input$page_SDEMO_xDEMO,
    {
      f_WideColumns(DEMO$columnNames(), vectorSel(), "bcg_counts")
    }
  )

  ## Wide format ----
  ### Meters completed ----
  output$wideDEMOMetersC <- renderUI({
    selectInput("wideDEMOMetersC",
      "Select a variable for transect length:",
      choices = c("None", colNamesWDEMO())
    )
  })

  wideDEMOMetersCR <- eventReactive(
    input$wideDEMOSave,
    {
      input$wideDEMOMetersC
    }
  )

  prefMaxDiam <- eventReactive(
    input$wideDEMOSave,
    {
      input$wideDEMOMaxDiam
    }
  )

  prefPerpDiam <- eventReactive(
    input$wideDEMOSave,
    {
      input$wideDEMOPerpDiam
    }
  )

  prefHeight <- eventReactive(
    input$wideDEMOSave,
    {
      input$wideDEMOHeight
    }
  )

  prefOldMort <- eventReactive(
    input$wideDEMOSave,
    {
      input$wideDEMOOldMort
    }
  )

  prefRecentMort <- eventReactive(
    input$wideDEMOSave,
    {
      input$wideDEMORecentMort
    }
  )

  prefBleach <- eventReactive(
    input$wideDEMOSave,
    {
      input$wideDEMOBleach
    }
  )

  prefDisease <- eventReactive(
    input$wideDEMOSave,
    {
      input$wideDEMODisease
    }
  )

  ### Finish by presenting the table ----
  wideDEMOTable <- eventReactive(
    input$wideDEMOSave,
    {
      f_W2LDEMO(
        DEMO$InputDT(), vectorSel(),
        timeVar(), spatialVar(),
        coordinateVar(), wideDEMOMetersCR(),
        transectVar(),
        prefMaxDiam(), prefPerpDiam(), prefHeight(),
        prefOldMort(), prefRecentMort(),
        prefBleach(), prefDisease()
      )
    }
  )

  output$wideDEMOFinal <- DT::renderDataTable(wideDEMOTable())

  ## Long format ----
  ### Meters completed ----
  output$longDEMOMetersC <- renderUI({
    selectInput("longDEMOMetersC",
      "Select a variable for transect length:",
      choices = c("None", colNamesWDEMO())
    )
  })

  longDEMOMetersCR <- eventReactive(
    input$longDEMOSave,
    {
      input$longDEMOMetersC
    }
  )

  output$longDEMOSpecies <- renderUI({
    selectInput("longDEMOSpecies",
      "Species name:",
      choices = colNamesWDEMO()
    )
  })
  longDEMOSpecies <- eventReactive(
    input$longDEMOSpecies,
    {
      input$longDEMOSpecies
    }
  )

  output$longDEMONCol <- renderUI({
    selectInput("longDEMONCol",
      "Number of colonies:",
      choices = colNamesWDEMO()
    )
  })
  longDEMONCol <- eventReactive(
    input$longDEMONCol,
    {
      input$longDEMONCol
    }
  )

  output$longDEMOMaxDiam <- renderUI({
    selectInput("longDEMOMaxDiam",
      "Maximum diameter:",
      choices = colNamesWDEMO()
    )
  })
  longDEMOMaxDiam <- eventReactive(
    input$longDEMOMaxDiam,
    {
      input$longDEMOMaxDiam
    }
  )

  output$longDEMOPerpDiam <- renderUI({
    selectInput("longDEMOPerpDiam",
      "Perpendicular diameter:",
      choices = colNamesWDEMO()
    )
  })
  longDEMOPerpDiam <- eventReactive(
    input$longDEMOPerpDiam,
    {
      input$longDEMOPerpDiam
    }
  )

  output$longDEMOHeight <- renderUI({
    selectInput("longDEMOHeight",
      "Height:",
      choices = colNamesWDEMO()
    )
  })
  longDEMOHeight <- eventReactive(
    input$longDEMOHeight,
    {
      input$longDEMOHeight
    }
  )

  output$longDEMOOldMort <- renderUI({
    selectInput("longDEMOOldMort",
      "Old mortality:",
      choices = colNamesWDEMO()
    )
  })
  longDEMOOldMort <- eventReactive(
    input$longDEMOOldMort,
    {
      input$longDEMOOldMort
    }
  )

  output$longDEMORecentMort <- renderUI({
    selectInput("longDEMORecentMort",
      "Recent mortality:",
      choices = colNamesWDEMO()
    )
  })
  longDEMORecentMort <- eventReactive(
    input$longDEMORecentMort,
    {
      input$longDEMORecentMort
    }
  )

  output$longDEMOBleach <- renderUI({
    selectInput("longDEMOBleach",
      "Presence of bleaching:",
      choices = colNamesWDEMO()
    )
  })
  longDEMOBleach <- eventReactive(
    input$longDEMOBleach,
    {
      input$longDEMOBleach
    }
  )

  output$longDEMODisease <- renderUI({
    selectInput("longDEMODisease",
      "Presence of diseases:",
      choices = colNamesWDEMO()
    )
  })
  longDEMODisease <- eventReactive(
    input$longDEMODisease,
    {
      input$longDEMODisease
    }
  )

  ### Finish by presenting the table ----
  longDEMOTable <- eventReactive(
    input$longDEMOSave,
    {
      f_L2LDEMOSimple(DEMO$InputDT(),
        timeVar(), spatialVar(),
        coordinateVar(), longDEMOMetersCR(),
        transectVar(), longDEMOSpecies(),
        longDEMONCol(),
        longDEMOMaxDiam(), longDEMOPerpDiam(),
        longDEMOHeight(), longDEMOOldMort(), longDEMORecentMort(),
        longDEMOBleach(), longDEMODisease(),
        type = "long"
      )
    }
  )

  output$longDEMOFinal <- DT::renderDataTable(longDEMOTable())


  # Download final file for BCG ----
  # function to select the adequate table
  select_table <- function(skip_flag, table, skip_table) {
    if (!skip_flag) {
      table()
    } else {
      skip_table
    }
  }

  DLData <- reactive({
    if (Model() == "BCG Fish") { # Model is Fish
      if (dataShape() == "Wide") { # Fish is wide
        if (FWideOptions() != "WFTwoSheets") { # Wide Fish is One-Sheet
          list(Fish = wideFishTable())
        } else { # Wide Fish is Two-Sheet
          list(Fish = wideFishTableTwoSheets())
        }
      } else { # Fish is long
        list(Fish = longFishTable())
      }
    } else if (Model() == "BSAT (LPI)") { # Model is BSAT
      if (dataShape() == "Wide") { # BSAT is wide
        list(
          LPI = if (!skipLPIFlag()) { # not skip Wide BSAT
            wideLPITable()
          } else { # skip Wide BSAT
            templateLPI
          },
          MOBILE_FAUNA = if (!skipMobFlag()) { # not skip Wide Mob
            wideMobTable()
          } else { # skip Wide Mob
            templateMOB
          },
          DEMO = templateDEMO
        )
      } else { # BSAT is Long
        list(
          LPI = if (!skipLPIFlag()) { # not skip long LPI
            longLPITable()
          } else {
            templateLPI # skip long LPI
          },
          MOBILE_FAUNA = if (!skipMobFlag()) { # not skip Long Mob
            longMobTable()
          } else { # skip long Mob
            templateMOB
          },
          DEMO = templateDEMO
        )
      }
    } else { # Model is BCG full
      if (dataShape() == "Wide") { # BCG is wide
        list(
          LPI = if (!skipLPIFlag()) { # not skip Wide BSAT
            wideLPITable()
          } else { # skip Wide BSAT
            templateLPI
          },
          MOBILE_FAUNA = if (!skipMobFlag()) { # not skip Wide Mob
            wideMobTable()
          } else { # skip Wide Mob
            templateMOB
          },
          DEMO = if (!skipDEMOFlag()) { # not skip Wide DEMO
            wideDEMOTable()
          } else { # skip Wide DEMO
            templateDEMO
          }
        )
      } else { # BCG is Long
        list(
          LPI = if (!skipLPIFlag()) { # not skip Wide BSAT
            longLPITable()
          } else { # skip Wide BSAT
            templateLPI
          },
          MOBILE_FAUNA = if (!skipMobFlag()) { # not skip Wide Mob
            longMobTable()
          } else { # skip Wide Mob
            templateMOB
          },
          DEMO = if (!skipDEMOFlag()) { # not skip Wide DEMO
            longDEMOTable()
          } else { # skip Wide DEMO
            templateDEMO
          }
        )
      }
    }
  })

  output$downloadBCG <- downloadHandler(
    filename = function() {
      paste0(Intro$fileName(), "_formatted.xlsx")
    },
    content = function(file) {
      writexl::write_xlsx(DLData(), file)
    }
  )
}


# Run the app ----
shinyApp(ui = ui, server = server)
