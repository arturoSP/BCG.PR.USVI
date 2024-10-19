# Functions that help the world go around

# Rules file ----
Rls <- import("./www/Rules.csv")

# Invasive species list ----
InvSpp <- import("./www/invasive_spp.csv") |>
  transmute(FinalID = Species, Invasive = TRUE)

# Masterlist ----
MstLst <- import("./www/FishMasterList.csv") |>
  left_join(InvSpp, by = "FinalID")

# Possible names for the spatial variables -----
spatialValues <- c("REGION",
                   "REGION NAME",
                   "REGION_NAME",
                   "REGION-NAME",
                   "SITE",
                   "SITES",
                   "LOCATION",
                   "SUBREGION",
                   "SUB REGION",
                   "SUB-REGION",
                   "SUB_REGION",
                   "SUB REGION NAME",
                   "SUB_REGION_NAME",
                   "SUB-REGION-NAME",
                   "HABITAT",
                   "PRIMARY SAMPLE UNIT",
                   "PRIMARY_SAMPLE_UNIT",
                   "PRIMARY-SAMPLE-UNIT")

# Possible names for the temporal variables -----
temporalValues <- c("YEAR",
                    "MONTH",
                    "DAY",
                    "HOUR",
                    "TIME",
                    "TIMESTAMP",
                    "DATE")

# Helper functions ----
## F_threatSp ----
# finds out the quantity of records for threatened species
F_threatSp <- function(Samp, colSite, colYear) {
  Samp[[colSite]] <- as.character(Samp[[colSite]])
  ThreatSp2 <- Samp |>
    left_join(MstLst, by = c("SCIENTIFIC_NAME" = "FinalID")) |>
    mutate(CVE = paste(Samp[[colSite]], Samp[[colYear]], sep = "___"))  |>
    filter(RedListCategory == "VU" | RedListCategory == "EN" |
             RedListCategory == "CR" | RedListCategory == "EW" |
             RedListCategory == "EX") |>
    group_by(CVE, SCIENTIFIC_NAME) |>
    reframe(Status = RedListCategory) |>
    unique() |>
    ungroup() |>
    transmute(Site = str_extract(CVE, ".*(?=___)"),
              Year = str_extract(CVE, "(?<=___)[[:digit:]]*"),
              `Vulnerable species` = SCIENTIFIC_NAME,
              Status)

  if(dim(ThreatSp2)[1] > 0){
    ThreatSp2 <- ThreatSp2 |>
      arrange(Site, Year) |>
      setNames(c(str_to_sentence(colSite), str_to_sentence(colYear),
                 "Vulnerable species", "Status"))
      return(ThreatSp2)
  } else {
    ThreatSp2[1,1] <- "There are no records"
    return(ThreatSp2)
  }

}

## F_invasiveSp ----
# finds out the quantity of records for invasive species
F_invasiveSp <- function(Samp, colSite, colYear) {
  Samp[[colSite]] <- as.character(Samp[[colSite]])
  InvasiveSp2 <- Samp %>%
    left_join(InvSpp, by = c("SCIENTIFIC_NAME" = "FinalID")) |>
    mutate(CVE = paste(Samp[[colSite]], Samp[[colYear]], sep = "___")) %>%
    filter(Invasive == T) %>%
    group_by(CVE, SCIENTIFIC_NAME) %>%
    summarise(Count = n()) %>%
    unique() %>%
    ungroup() %>%
    transmute(Site = str_extract(CVE, ".*(?=___)"),
              Year = str_extract(CVE, "(?<=___)[[:digit:]]*"),
              `Invasive species` = SCIENTIFIC_NAME)

  if(dim(InvasiveSp2)[1] > 0){
    InvasiveSp2 %>%
      arrange(Site, Year) %>%
      setNames(c(str_to_sentence(colSite), str_to_sentence(colYear), "Invasive species")) %>%
      return()
  } else {
    InvasiveSp2[1,1] <- "There are no records"
    return(InvasiveSp2)
  }

}

# The map ----
# Test data to use for the drawing the map
# tLevel <- tribble(~Site, ~Year, ~Level,
#                   "2701", "5", "4+",
#                   "1008", "8", "4-")

## F_map ----
# prepare the data that will be mapped
F_map <- function(Samp, colSite, colYear, tLevel){
  Samp[[colSite]] <- as.character(Samp[[colSite]])
  map1 <- Samp |>
    group_by(Samp[[colSite]], Samp[[colYear]]) %>%
    summarise(LAT = mean(LAT_DEGREES, na.rm = T),
              LNG = mean(LON_DEGREES, na.rm = T)) %>%
    ungroup() %>%
    mutate(`Samp[[colYear]]` = as.character(`Samp[[colYear]]`)) %>%
    left_join(tLevel, by = c(`Samp[[colSite]]` = "Site", `Samp[[colYear]]` = "Year")) %>%
    transmute(Site = `Samp[[colSite]]`,
              Year = `Samp[[colYear]]`,
              LAT, LNG, Level,
              Label = paste0("Site: ", Site, ", ", Year, "\nLevel: ", Level)) %>%
    arrange(Site, desc(Year))

  return(map1)
}

## Color the map markers ----
getColor <- function(tLevel){
  tLevel <- tLevel %>%
    mutate(Level = str_sub(Level, 1,1),
           Level = ifelse(Level == "N", 6, Level),
           Level = ifelse(is.na(Level), 6, Level),
           Level = as.numeric(Level))
  sapply(tLevel$Level, function(Level) {
    if(Level == 1) {
      "blue"
    } else if(Level == 2) {
      "lightblue"
    } else if(Level == 3) {
      "lightgreen"
    } else if(Level == 4) {
      "beige"
    } else if(Level == 5) {
      "orange"
    } else {
      "red"
    }
  })
}

## Design the map ----
F_MapParam <- function(coord) {
  icons <- awesomeIcons(
    icon = "ios-information",
    iconColor = "black",
    library = "ion",
    markerColor = getColor(coord)
  )

  EsriToken <- Sys.getenv("EsriToken")

  map1 <- coord %>%
    leaflet() %>%
    addProviderTiles(providers$Esri.NatGeoWorldMap,
                     options = list(apikey = EsriToken)) %>%
    addAwesomeMarkers(
      lat = coord$LAT,
      lng = coord$LNG,
      icon = icons,
      popup = coord$Label,
      clusterOptions = markerClusterOptions(
        showCoverageOnHover = FALSE,
        zoomToBoundsOnClick = FALSE,
        spiderfyOnMaxZoom = TRUE,
        removeOutsideVisibleBounds = TRUE,
        spiderLegPolylineOptions = list(
          weight = 1,
          color = "#222",
          opacity = 0.3
        )
      )
    )

  return(map1)
}

# Function to calculate Maturity and prepare data for use in the app ----
F_tSamp <- function(MetricData){
  Samp_species <- MetricData |>
    filter(LENGTH > 0) |>
    select(SCIENTIFIC_NAME) |>
    unique() |>
    pull()

  Samp_species_list <- sapply(Samp_species,
                              rfishbase::maturity,
                              fields = c("Lm"),
                              simplify = FALSE) |>
    lapply(mutate, Lm = ifelse(is.na(Lm), 0, Lm))

  Samp_species_list <- Filter(function(x) nrow(x) > 0, Samp_species_list) |>
    lapply(function(x){
      x |>
        summarise(
          Lm = mean(Lm, na.rm = TRUE)
          )
      })

  Samp_species_list <- mapply(function(data, name){
      data |>
        mutate(SCIENTIFIC_NAME = name)
    }, Samp_species_list, names(Samp_species_list), SIMPLIFY = FALSE) |>
      bind_rows() |>
    filter(Lm > 0)

  Samp <- left_join(MetricData, Samp_species_list)

  return(Samp)
}

# group and summarise the data from the user ----
# this function is necessary for starting the Fish Model
F_tSamp2 <- function(Samp){
  Samp <- Samp |>
  group_by(PRIMARY_SAMPLE_UNIT,
           SCIENTIFIC_NAME) |>
  summarise(YEAR = nth(YEAR, 1),
            MONTH = nth(MONTH, 1),
            DAY = nth(DAY, 1),
            REGION = nth(REGION, 1),
            SUB_REGION = nth(SUB_REGION, 1),
            LOCATION = nth(LOCATION, 1),
            HABITAT = nth(HABITAT, 1),
            SITE = nth(SITE, 1),
            LATITUDE = mean(LAT_DEGREES),
            LONGITUDE = mean(LON_DEGREES),
            TOTAL_NUMBER = n(),
            TOTAL_BIOMASS = sum(BIOMASS)) |>
  left_join(MstLst, by = c("SCIENTIFIC_NAME" = "FinalID"))

  Samp[,"LOCATION"] <- case_when(Samp[,"LOCATION"] == "Florida" ~ "FL",
                                 Samp[,"LOCATION"] == "FLORIDA" ~ "FL",
                                 Samp[,"LOCATION"] == "florida" ~ "FL",
                                 TRUE ~ "PR/USVI")

  return(Samp)
}

# function to get the data available for plotting in the histogram ----
F_LenWei <- function(Samp, colSite, colYear){
  Samp[[colYear]] <- as.character(Samp[[colYear]])
  Samp[[colSite]] <- as.character(Samp[[colSite]])

  p1 <- Samp %>%
    filter(LENGTH > 0) %>%
    transmute(TIME = as.character(.[[colYear]]),
              PLACE = as.character(.[[colSite]]),
              SCIENTIFIC_NAME = SCIENTIFIC_NAME,
              Length = LENGTH,
              Biomass = BIOMASS,
              Lm = Lm)

  return(p1)
}

# function to plot an interactive histogram ----
pl_hist <- function(data, selected, timeSel, spatialSel){
  # filter the data according to the user specifications
  p1 <- data |>
    filter(SCIENTIFIC_NAME %in% selected) |>
    filter(TIME %in% timeSel) |>
    filter(PLACE %in% spatialSel)

  # calculate histogram info, used to mark the LengthMaturity line
  hist_data <- p1 |>
    pull(Length) |>
    hist(plot = FALSE)

  # plot depending of the availability of LengthMaturity data
  p1 <- if(!is.na(p1[1,"Lm"])){
    p1 %>%
      plot_ly(x = .$Length, type = "histogram",
              #nbinsx = length(hist_data$breaks),
              name = "Frequency") %>%
      add_trace(type = "scatter", mode = "lines",
                x = c(mean(p1$Lm), mean(p1$Lm)),
                y = c(0,max(hist_data$counts)),
                line = list(color = "black", width = 2, dash = "dash"),
                name = "Length at first maturity") %>%
      layout(bargap = 0.1,
             xaxis = list(title = "Length"),
             yaxis = list(title = "Count",
                          dtick = 1,
                          tickmode = "auto"))
  } else {
    p1 %>%
      plot_ly(x = .$Length, type = "histogram",
              nbinsx = length(hist_data$breaks),
              name = "Frequency") %>%
      layout(bargap = 0.1,
             xaxis = list(title = "Length"),
             yaxis = list(title = "Count",
                          dtick = 1,
                          tickmode = "auto"))
  }

  return(p1)
}

# function for getting unique values for the histogram ----
F_PlotParams <- function(LenWei, type = NULL){
  temp <- if(type == "Species"){
    LenWei[,"SCIENTIFIC_NAME"]
  } else if(type == "Spatial") {
    LenWei[,"PLACE"]
  } else {
    LenWei[,"TIME"]
  }

  temp <- temp |>
    unique()

  return(temp)
}
