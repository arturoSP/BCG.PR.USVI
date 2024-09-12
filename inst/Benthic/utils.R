# Functions ----

# Rule set file ----
rules <- import("./Rules.csv")

# Masterlist ----
# Prepare master list
MasterList <- import("./2019CoralMasterList.csv") %>%
  select(FinalID, ReefBuildingSpecies, Type, OrbicellaAcropora, 
         `BCG Attribute`, RedListCategory, MorphologyIndex, 
         OBSERVATIONS_COMMENTS) %>% 
  transmute(FinalID = str_to_sentence(FinalID), 
            ReefBuildingSpecies = ifelse(ReefBuildingSpecies == "X", T, F),
            Type, 
            OrbicellaAcropora = ifelse(OrbicellaAcropora == "OrbAcrp", T, F), 
            BCGAttr = `BCG Attribute`, 
            RedListCategory,
            MorphologyIndex,
            OBSERVATIONS_COMMENTS) %>%
  # list of invasive species comes from Global Invasive Species Database, looking for System = "Marine"
  # and Selected locations: "Caribbean Islands" http://www.iucngisd.org/gisd/search.php
  left_join(import("./invasive_spp.csv") %>%
              transmute(Species, Invasive = TRUE), by = c("FinalID" = "Species"))

# F_threatSp ----
# finds out the quantity of records for threatened species
F_threatSp <- function(Samp, colSite, colYear) {
  ThreatSp2 <- Samp %>%
    mutate(CVE = paste(Samp[[colSite]], Samp[[colYear]], sep = "___")) %>%
    filter(RedListCategory == "Vulnerable" | RedListCategory == "Endangered" |
             RedListCategory == "Critically endangered" | RedListCategory == "Extinct in the wild" |
             RedListCategory == "Extinct") %>%
    filter(TotalPoints > 0) %>%
    group_by(CVE, SPECIES_NAME) %>%
    # summarise(Status = RedListCategory) %>%
    reframe(Status = RedListCategory) %>%
    unique() %>%
    ungroup() %>%
    transmute(Site = str_extract(CVE, ".*(?=___)"),
              Year = str_extract(CVE, "(?<=___)[[:digit:]]*"),
              `Vulnerable species` = SPECIES_NAME,
              Status)
  
  if(dim(ThreatSp2)[1] > 0){
    ThreatSp2 %>%
      arrange(Site, Year) %>%
      setNames(c(str_to_sentence(colSite), str_to_sentence(colYear),
                 "Vulnerable species", "Status")) %>%
      return()
  } else {
    ThreatSp2[1,1] <- "There are no records"
    return(ThreatSp2)
  }
  
}

# F_invasiveSp ----
# finds out the quantity of records for invasive species
F_invasiveSp <- function(Samp, colSite, colYear) {
  InvasiveSp2 <- Samp %>%
    mutate(CVE = paste(Samp[[colSite]], Samp[[colYear]], sep = "___")) %>%
    filter(Invasive == T) %>%
    filter(TotalPoints > 0) %>%
    group_by(CVE, SPECIES_NAME) %>%
    summarise(Count = n()) %>%
    unique() %>%
    ungroup() %>%
    transmute(Site = str_extract(CVE, ".*(?=___)"),
              Year = str_extract(CVE, "(?<=___)[[:digit:]]*"),
              `Invasive species` = SPECIES_NAME)
  
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

# F_diadema ----
# finds out the quantity of records for diadema spp
F_diadema <- function(Samp) {
  Diadema <- Samp %>%
    select(SPECIES_NAME, TotalPoints) %>%
    mutate(taxa = str_extract(SPECIES_NAME, "Diadema")) %>%
    filter(!is.na(taxa)) %>%
    filter(TotalPoints > 0) %>%
    summarise(sum(TotalPoints, na.rm = T))
  
  return(Diadema)
}

# F_Panulirus ----
# finds out the quantity of records for Panulirus spp
F_panulirus <- function(Samp) {
  Panulirus <- Samp %>%
    select(SPECIES_NAME, TotalPoints) %>%
    mutate(taxa = str_extract(SPECIES_NAME, "Panulirus")) %>%
    filter(!is.na(taxa)) %>%
    filter(TotalPoints > 0) %>%
    summarise(sum(TotalPoints, na.rm = T))
  
  return(Panulirus)
}

# F_aliger ----
# finds out the quantity of records for Aliger gigas
F_aliger <- function(Samp) {
  Aliger <- Samp %>%
    select(SPECIES_NAME, TotalPoints) %>%
    mutate(taxa = str_extract(SPECIES_NAME, "Aliger gigas")) %>%
    filter(SPECIES_NAME == "Aliger gigas" | 
             SPECIES_NAME == "Lobatus gigas" | 
             SPECIES_NAME == "Strombus gigas") %>% 
    filter(TotalPoints > 0) %>%
    summarise(sum(TotalPoints, na.rm = T))
  
  return(Aliger)
}

# Test data to use for the drawing the map
# tLevel <- tribble(~Site, ~Year, ~Level,
#                   "2701", "5", "4+",
#                   "1008", "8", "4-")

# F_map ----
# prepare the data that will be mapped
F_map <- function(Samp, colSite, colYear, tLevel){
  map1 <- Samp %>%
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

# get colors for the map markers ----
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

# design the map ----
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
    addAwesomeMarkers(lat = coord$LAT, lng = coord$LNG,
                      icon = icons,
                      popup = coord$Label) 
  return(map1)
}

# set the results of other indicators into a table ----
F_otherInd_1 <- function(Samp, colTransect, colSite, colYear){
  tOther <- tibble("Site" = character(), "Year" = character(), "Diadema spp" = integer(),
                   "Panulirus argus" = integer(), "Aliger gigas" = integer(), 
                   "Vulnerable species" = integer(), "Invasive species" = integer(),
                   "Mean coral cover" = double(), "SD coral cover" = double())
  
  counter <- 1
  
  Samp <- Samp %>% mutate(CVE = paste(Samp[[colSite]], Samp[[colYear]], sep = "___"))
  
  numSite <- unique(Samp[[colSite]])
  numYear <- unique(Samp[[colYear]])
  numCVE <- unique(Samp$CVE)
  
  withProgress(
    message = "Please wait, this analysis may take a few minutes" ,
    {
      for (i in numCVE) {
        tSamp <- Samp %>%
          filter(Samp$CVE == i)
        
        # test for each indicator
        Diadema <- F_diadema(tSamp)
        Panulirus <- F_panulirus(tSamp)
        Aliger <- F_aliger(tSamp)
        Vulnerable <- F_threatSp(tSamp, colSite, colYear) 
        if(Vulnerable[1,1] != "There are no records"){
          Vulnerable <- Vulnerable %>% 
            group_by(`Vulnerable species`) %>%
            summarise(Count = n()) %>%
            summarise(Count = n())
        } else {
          Vulnerable <- 0
        }
        Invasive <- F_invasiveSp(tSamp, colSite, colYear)
        if(Invasive[1,1] != "There are no records"){
          Invasive <- Invasive %>%
            group_by(`Invasive species`) %>%
            summarise(Count = n()) %>%
            summarise(Count = n())
        } else {
          Invasive <- 0
        }
        MeanCoral <- F_meanCoral(tSamp, colTransect)
        SDCoral <- F_sdCoral(tSamp, colTransect)
        
        # assemble the results table
        tOther[counter, 1] <- i %>% str_extract(".*(?=___)")
        tOther[counter, 2] <- i %>% str_extract("(?<=___)[[:digit:]]*")
        tOther[counter, 3] <- Diadema
        tOther[counter, 4] <- Panulirus
        tOther[counter, 5] <- Aliger
        tOther[counter, 6] <- Vulnerable
        tOther[counter, 7] <- Invasive
        tOther[counter, 8] <- MeanCoral
        tOther[counter, 9] <- SDCoral
        counter = counter +1
        
        incProgress(1 / nrow(as.data.frame(numCVE)))
      }
    }
  )
  tOther %>%
    arrange(Site, Year) %>%
    setNames(c(str_to_sentence(colSite), str_to_sentence(colYear),
               "Diadema spp", "Panulirus argus",
               "Aliger gigas", "Vulnerable species",
               "Invasive species", "Mean coral cover",
               "SD coral cover")) %>%
    return()
}

F_otherInd_2 <- function(Samp, colTransect, colSite, colYear){
  tOther <- tibble("Site" = character(), "Year" = character(), "Diadema spp" = integer(),
                   "Panulirus argus" = integer(), "Aliger gigas" = integer(), 
                   "Vulnerable species" = integer(), "Invasive species" = integer(),
                   "Bleaching" = character(), "Disease" = character(),
                   "Mean coral cover" = double(), "SD coral cover" = double())
  
  counter <- 1
  
  Samp <- Samp %>% mutate(CVE = paste(Samp[[colSite]], Samp[[colYear]], sep = "___"))
  
  numSite <- unique(Samp[[colSite]])
  numYear <- unique(Samp[[colYear]])
  numCVE <- unique(Samp$CVE)
  
  withProgress(
    message = "Please wait, this analysis may take a few minutes" ,
    {
      for (i in numCVE) {
        tSamp <- Samp %>%
          filter(Samp$CVE == i)
        
        # test for each indicator
        Diadema <- F_diadema(tSamp)
        Panulirus <- F_panulirus(tSamp)
        Aliger <- F_aliger(tSamp)
        Vulnerable <- F_threatSp_2(tSamp, colSite, colYear) 
        if(Vulnerable[1,1] != "There are no records"){
          Vulnerable <- Vulnerable %>% 
            group_by(`Vulnerable species`) %>%
            summarise(Count = n()) %>%
            summarise(Count = n())
        } else {
          Vulnerable <- 0
        }
        Invasive <- F_invasiveSp_2(tSamp, colSite, colYear)
        if(Invasive[1,1] != "There are no records"){
          Invasive <- Invasive %>%
            group_by(`Invasive species`) %>%
            summarise(Count = n()) %>%
            summarise(Count = n())
        } else {
          Invasive <- 0
        }
        Bleaching <- F_bleaching(tSamp)
        Disease <- F_disease(tSamp)
        MeanCoral <- F_meanCoral(tSamp, colTransect)
        SDCoral <- F_sdCoral(tSamp, colTransect)
        
        # assemble the results table
        tOther[counter, 1] <- i %>% str_extract(".*(?=___)")
        tOther[counter, 2] <- i %>% str_extract("(?<=___)[[:digit:]]*")
        tOther[counter, 3] <- Diadema
        tOther[counter, 4] <- Panulirus
        tOther[counter, 5] <- Aliger
        tOther[counter, 6] <- Vulnerable
        tOther[counter, 7] <- Invasive
        tOther[counter, 8] <- Bleaching
        tOther[counter, 9] <- Disease
        tOther[counter, 10] <- MeanCoral
        tOther[counter, 11] <- SDCoral
        counter = counter +1
        
        incProgress(1 / nrow(as.data.frame(numCVE)))
      }
    }
  )
  tOther %>%
    arrange(Site, Year) %>%
    setNames(c(str_to_sentence(colSite), str_to_sentence(colYear),
               "Diadema spp", "Panulirus argus",
               "Aliger gigas", "Vulnerable species",
               "Invasive species", "Bleaching", "Disease",
               "Mean coral cover", "SD coral cover")) %>%
    return()
}

# F_threatSp 2 ----
# finds out the records for threatened species
F_threatSp_2 <- function(Samp, colSite, colYear) {
  ThreatSp2 <- Samp %>%
    mutate(CVE = paste(Samp[[colSite]], Samp[[colYear]], sep = "___")) %>%
    filter(RedListCategory == "Vulnerable" | RedListCategory == "Endangered" |
             RedListCategory == "Critically endangered" | RedListCategory == "Extinct in the wild" |
             RedListCategory == "Extinct") %>%
    filter(TotalPoints > 0 | N_COLONIES > 0) %>%
    group_by(CVE, SPECIES_NAME) %>%
    # summarise(Status = RedListCategory) %>%
    reframe(Status = RedListCategory) %>%
    unique() %>%
    ungroup() %>%
    transmute(Site = str_extract(CVE, ".*(?=___)"),
              Year = str_extract(CVE, "(?<=___)[[:digit:]]*"),
              `Vulnerable species` = SPECIES_NAME,
              Status)
  
  if(dim(ThreatSp2)[1] > 0){
    ThreatSp2 %>%
      arrange(Site, Year) %>%
      setNames(c(str_to_sentence(colSite), str_to_sentence(colYear),
                 "Vulnerable species", "Status")) %>%
      return()
  } else {
    ThreatSp2[1,1] <- "There are no records"
    return(ThreatSp2)
  }
}

# F_invasiveSp 2 ----
# finds out the records for invasive species
F_invasiveSp_2 <- function(Samp, colSite, colYear) {
  InvasiveSp2 <- Samp %>%
    mutate(CVE = paste(Samp[[colSite]], Samp[[colYear]], sep = "___")) %>%
    filter(Invasive == T) %>%
    filter(TotalPoints > 0 | N_COLONIES > 0) %>%
    group_by(CVE, SPECIES_NAME) %>%
    summarise(Count = n()) %>%
    unique() %>%
    ungroup() %>%
    transmute(Site = str_extract(CVE, ".*(?=___)"),
              Year = str_extract(CVE, "(?<=___)[[:digit:]]*"),
              `Invasive species` = SPECIES_NAME)
  
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

# F_bleaching ----
# finds out the quantity of records that include some bleaching
F_bleaching <- function(Samp) {
  Bleaching <- Samp %>%
    select(SPECIES_NAME, BLEACH_CONDITION) %>%
    filter(!is.na(BLEACH_CONDITION)) %>%
    filter(BLEACH_CONDITION > 0) %>%
    summarise(Bleach = sum(BLEACH_CONDITION, na.rm = T)) %>%
    mutate(Bleach = ifelse(Bleach > 0, "Yes", "No"))
  
  return(Bleaching)
}

# F_disease ----
# finds out the quantity of records that include diseases
F_disease <- function(Samp) {
  Disease <- Samp %>%
    select(SPECIES_NAME, DISEASE) %>%
    filter(!is.na(DISEASE)) %>%
    filter(DISEASE > 0) %>%
    summarise(Dis = sum(DISEASE, na.rm = T)) %>%
    mutate(Dis = ifelse(Dis > 0, "Yes", "No"))
  
  return(Disease)
}

# F_meanCoral ----
# Calculate mean of coral cover in a group of transects
F_meanCoral <- function(Samp, colTransect){
  MeanCoral <- Samp %>%
    filter(Type == "Scleractinian") %>%
    group_by(.[[colTransect]]) %>%
    summarise(SumPoints = sum(TotalPoints, na.rm = T)) %>% 
    summarise(meanCoral = round(mean(SumPoints),2))
  
  return(MeanCoral)
}

# F_sdCoral ----
# Calculate standard deviation of coral cover in a group of transects
F_sdCoral <- function(Samp, colTransect){
  SDCoral <- Samp %>%
    filter(Type == "Scleractinian") %>%
    group_by(.[[colTransect]]) %>%
    summarise(sumPoints = sum(TotalPoints, na.rm = T)) %>% 
    summarise(sdCoral = round(sd(sumPoints, na.rm = T),2))
  
  return(SDCoral)
}

# F_SampBSAT_Long ----
# function to read and select important variables in a tidy type file
# this function was developed for LPI files
F_SampBSAT_Long <- function(MetricData, colTransect, colSite, colYear) {
  Samp <- tryCatch({MetricData %>%
      select(matches(colYear), matches(colSite), matches(colTransect),
             SPECIES_NAME, TotalPoints,
             LAT_DEGREES, LON_DEGREES) %>%
      mutate(SPECIES_NAME = str_to_sentence(SPECIES_NAME),
             Model = "BSAT") %>%
      left_join(MasterList, by = c("SPECIES_NAME" = "FinalID"))} ,
      error = function(e){
        message("Select temporal and spatial factors.")
        print(e)
      },
      warning = function(w){
        message("Select temporal and spatial factors.")
        print(w)
      }
  )
  
  return(Samp)
}

# F_SampBCG_Long ----
# function to read and select important variables in a tidy type file
# this function was developed for DEMO files
F_SampBCG_Long <- function(MetricData, colTransect, colSite, colYear) {
  tryCatch({Samp <- MetricData %>%
    select(matches(colYear), matches(colSite), matches(colTransect),
           SPECIES_NAME, TotalPoints, N_COLONIES,
           MAX_DIAMETER, PERP_DIAMETER, HEIGHT, OLD_MORT,
           RECENT_MORT, BLEACH_CONDITION, DISEASE,
           LAT_DEGREES, LON_DEGREES, METERS_COMPLETED) %>%
    mutate(SPECIES_NAME = str_to_sentence(SPECIES_NAME),
           Model = "BCG") %>%
    left_join(MasterList, by = c("SPECIES_NAME" = "FinalID"))
  
  return(Samp)},
  error = function(e){message("Select temporal and/or spatial factors.")
  },
  warning = function(w){message("Select temporal and/or spatial factors.")
  }
  )
  
}


# F_Template_BSAT ---------------------------------------------------------
# function to read the template file and join the info from different sheets into one dataframe
F_Template_BSAT <- function(sheetLPI, sheetMOB) {
  dataLPI <- sheetLPI %>%
    mutate(YEAR = as.numeric(YEAR),
           MONTH = as.numeric(MONTH),
           DAY = as.numeric(DAY),
           REGION = as.character(REGION),
           SUB_REGION = as.character(SUB_REGION),
           SITE = as.character(SITE),
           LOCATION = as.character(LOCATION),
           LAT_DEGREES = as.numeric(LAT_DEGREES),
           LON_DEGREES = as.numeric(LON_DEGREES),
           METERS_COMPLETED = as.numeric(METERS_COMPLETED),
           PRIMARY_SAMPLE_UNIT = as.character(PRIMARY_SAMPLE_UNIT),
           SPECIES_NAME = as.character(SPECIES_NAME),
           COVER = as.numeric(COVER)) %>%
    group_by(YEAR, MONTH, DAY, REGION,SUB_REGION,
             SITE, LOCATION, LAT_DEGREES, LON_DEGREES,
             PRIMARY_SAMPLE_UNIT, SPECIES_NAME) %>%
    summarise(TotalPoints = sum(COVER))
  
  dataMOB <- sheetMOB %>%
    mutate(YEAR = as.numeric(YEAR),
           MONTH = as.numeric(MONTH),
           DAY = as.numeric(DAY),
           REGION = as.character(REGION),
           SUB_REGION = as.character(SUB_REGION),
           SITE = as.character(SITE),
           LOCATION = as.character(LOCATION),
           LAT_DEGREES = as.numeric(LAT_DEGREES),
           LON_DEGREES = as.numeric(LON_DEGREES),
           METERS_COMPLETED = as.numeric(METERS_COMPLETED),
           PRIMARY_SAMPLE_UNIT = as.character(PRIMARY_SAMPLE_UNIT),
           SPECIES_NAME = as.character(SPECIES_NAME),
           COUNTS = as.numeric(COUNTS)) %>%
    group_by(YEAR, MONTH, DAY, REGION,SUB_REGION,
             SITE, LOCATION, LAT_DEGREES, LON_DEGREES,
             PRIMARY_SAMPLE_UNIT, SPECIES_NAME) %>%
    summarise(TotalPoints = sum(COUNTS))
  
  MetricData <- dataLPI %>%
    full_join(dataMOB) %>%
    ungroup() %>%
    mutate(REGION = as.character(REGION),
           SUB_REGION = as.character(SUB_REGION),
           SITE = as.character(SITE),
           PRIMARY_SAMPLE_UNIT = as.character(PRIMARY_SAMPLE_UNIT))
  
  return(MetricData)
}

# F_Template_BCG ----------------------------------------------------------
# function to read the template file and join the info from different sheets into one dataframe
F_Template_BCG <- function(sheetLPI, sheetMOB, sheetDEMO) {
  dataLPI <- sheetLPI %>%
    mutate(YEAR = as.numeric(YEAR),
           MONTH = as.numeric(MONTH),
           DAY = as.numeric(DAY),
           REGION = as.character(REGION),
           SUB_REGION = as.character(SUB_REGION),
           SITE = as.character(SITE),
           LOCATION = as.character(LOCATION),
           LAT_DEGREES = as.numeric(LAT_DEGREES),
           LON_DEGREES = as.numeric(LON_DEGREES),
           METERS_COMPLETED = as.numeric(METERS_COMPLETED),
           PRIMARY_SAMPLE_UNIT = as.character(PRIMARY_SAMPLE_UNIT),
           SPECIES_NAME = as.character(SPECIES_NAME),
           COVER = as.numeric(COVER)) %>%
    group_by(YEAR, MONTH, DAY, REGION,SUB_REGION,
             SITE, LOCATION, LAT_DEGREES, LON_DEGREES,
             PRIMARY_SAMPLE_UNIT, SPECIES_NAME) %>%
    summarise(TotalPoints = sum(COVER))
  
  dataMOB <- sheetMOB %>%
    mutate(YEAR = as.numeric(YEAR),
           MONTH = as.numeric(MONTH),
           DAY = as.numeric(DAY),
           REGION = as.character(REGION),
           SUB_REGION = as.character(SUB_REGION),
           SITE = as.character(SITE),
           LOCATION = as.character(LOCATION),
           LAT_DEGREES = as.numeric(LAT_DEGREES),
           LON_DEGREES = as.numeric(LON_DEGREES),
           METERS_COMPLETED = as.numeric(METERS_COMPLETED),
           PRIMARY_SAMPLE_UNIT = as.character(PRIMARY_SAMPLE_UNIT),
           SPECIES_NAME = as.character(SPECIES_NAME),
           COUNTS = as.numeric(COUNTS)) %>%
    group_by(YEAR, MONTH, DAY, REGION,SUB_REGION,
             SITE, LOCATION, LAT_DEGREES, LON_DEGREES,
             PRIMARY_SAMPLE_UNIT, SPECIES_NAME) %>%
    summarise(TotalPoints = sum(COUNTS))
  
  dataDEMO <- sheetDEMO %>% 
    mutate(YEAR = as.numeric(YEAR),
           MONTH = as.numeric(MONTH),
           DAY = as.numeric(DAY),
           REGION = as.character(REGION),
           SUB_REGION = as.character(SUB_REGION),
           SITE = as.character(SITE),
           LOCATION = as.character(LOCATION),
           LAT_DEGREES = as.numeric(LAT_DEGREES),
           LON_DEGREES = as.numeric(LON_DEGREES),
           METERS_COMPLETED = as.numeric(METERS_COMPLETED),
           PRIMARY_SAMPLE_UNIT = as.character(PRIMARY_SAMPLE_UNIT),
           SPECIES_NAME = as.character(SPECIES_NAME),
           N = as.numeric(N_COLONIES),
           MAX_DIAMETER = as.numeric(MAX_DIAMETER),
           PERP_DIAMETER = as.numeric(PERP_DIAMETER),
           HEIGHT = as.numeric(HEIGHT),
           OLD_MORT = as.numeric(OLD_MORT),
           RECENT_MORT = as.numeric(RECENT_MORT),
           BLEACH_CONDITION = as.character(BLEACH_CONDITION),
           DISEASE = as.character(DISEASE)
    ) %>%
    mutate(BLEACH_CONDITION = ifelse(BLEACH_CONDITION == "T", 1, 
                                     ifelse(BLEACH_CONDITION == "P", 0.5, 0)),
           DISEASE = ifelse(DISEASE == "P", 1, 0)) %>%
    group_by(YEAR, MONTH, DAY, REGION,SUB_REGION,
             SITE, LOCATION, LAT_DEGREES, LON_DEGREES,
             PRIMARY_SAMPLE_UNIT, SPECIES_NAME) %>%
    summarise(N_COLONIES = sum(N, na.rm = T),
              MAX_DIAMETER = mean(MAX_DIAMETER, na.rm = T),
              PERP_DIAMETER = mean(PERP_DIAMETER, na.rm = T),
              HEIGHT = mean(HEIGHT, na.rm = T),
              OLD_MORT = mean(OLD_MORT, na.rm = T),
              RECENT_MORT = mean(RECENT_MORT, na.rm = T),
              BLEACH_CONDITION = mean(BLEACH_CONDITION, na.rm = T),
              DISEASE = mean(DISEASE, na.rm = T),
              METERS_COMPLETED = mean(METERS_COMPLETED, na.rm = T)) 
  
  MetricData <- dataLPI %>% 
    full_join(dataMOB) %>%
    full_join(dataDEMO) %>%
    ungroup() %>%
    mutate(REGION = as.character(REGION),
           SUB_REGION = as.character(SUB_REGION),
           SITE = as.character(SITE),
           PRIMARY_SAMPLE_UNIT = as.character(PRIMARY_SAMPLE_UNIT))
  
  return(MetricData)
}
