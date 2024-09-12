# Functions that work in quality control 

# F_Misspelling ----
# prints a table with the rownames and data that is not registered in the MasterList 
F_Misspelling_1 <- function(Samp) {
  # find the species names that are not present in the MasterList
  tryCatch({
    Msp1 <- Samp %>%
      select(SPECIES_NAME) %>%
      left_join(MasterList %>%
                  select(FinalID) %>%
                  mutate(Valid = "OK"),
                by = c("SPECIES_NAME" = "FinalID")) %>%
      filter(is.na(Valid)) %>%
      distinct(SPECIES_NAME) %>% 
      arrange((SPECIES_NAME))
    return(Msp1)
  },
  error = function(e){
    return(tribble(~vacio, "vacio "))
    message("Select temporal and/or spatial factors.")
  },
  warning = function(w){
    return(tribble(~vacio, "vacio "))
    message("Select temporal and/or spatial factors.")})
}

F_Misspelling_2 <- function(Msp1) {
  if(is.na(Msp1[1,])){
    return(tribble(~Status, "All taxa in file were correctly identified."))
  } else if(Msp1[1,] == "vacio ") {
    return(tribble(~WARNING, "Select temporal and/or spatial factors."))
  } else {
    Msp4 <- tibble("SPECIES_NAME" = character(), 
                   "status" = character(), "validName" = character())
    withProgress(
      message = "Please wait as this may take a few minutes..." ,
      detail = "Species not recognized on the master list are being verified at www.marinespecies.org.",
      {
        for(i in pull(Msp1)) {
          # find the adequate record using WORMS database
          temp <- tryCatch(wm_records_taxamatch(i), 
                           error = function(e){
                             list(tribble(~status, ~valid_name, 
                                          "unaccepted", "Unknown"))
                           }
          )
          # assign adequate records to the missing species
          Msp3 <- tibble(
            status = purrr::map(temp, "status"),
            validName = as.character(purrr::map(temp, "valid_name"))) %>%
            unnest(cols = c(status, validName)) %>%
            mutate(SPECIES_NAME = i) 
          
          
          Msp4 <- bind_rows(Msp4, Msp3)
          incProgress(1 / nrow(Msp1))
        }
      }
    )
    
    
    # order the information in the return value
    Misspelling <- Msp4 %>%
      filter(!duplicated(.[["SPECIES_NAME"]])) %>%
      mutate(validName = as.character(ifelse(validName == SPECIES_NAME, "", validName))) %>%
      left_join(MasterList %>%
                  select(FinalID) %>%
                  mutate(match = T),
                by = c("validName" = "FinalID")) %>%
      replace_na(list(match = F)) %>%
      transmute(`Name in file` = SPECIES_NAME,
                `Status` = status,
                `Suggested name (WoRMS)` = validName,
                `Suggested actions` = ifelse(validName == "Unknown" & status == "unaccepted", 
                                             "Check if this is a biological variable.",
                                             ifelse(validName != "Unknown" & status == "accepted" & match == F,
                                                    "Request to include in master list.",
                                                    "Check the spelling of the scientific name or if this is an unaccepted synonym.")
                )
      ) 
    if(nrow(Misspelling) == 0) {
      Misspelling[1,4] <- "All taxa in file were correctly identified."
      Misspelling <- Misspelling %>% select(`Suggested actions`)
    } else {
      Misspelling
    }
    
    return(Misspelling) 
  }
}

# F_OrbAcro ----
# finds out the quantity of acropora and orbicella records, then print a message if none is found
F_OrbAcro <- function(Samp) {
  tryCatch({
    tempA <- Samp %>%
    select(SPECIES_NAME, TotalPoints) %>%
    mutate(taxa = str_extract(SPECIES_NAME, "Acropora(?=[:space:])")) %>%
    filter(!is.na(taxa)) %>% 
    summarise(sum(TotalPoints, na.rm = T))
  
  tempO <- Samp %>%
    select(SPECIES_NAME, TotalPoints) %>%
    mutate(taxa = str_extract(SPECIES_NAME, "Orbicella(?=[:space:])")) %>%
    filter(!is.na(taxa)) %>%
    summarise(sum(TotalPoints, na.rm = T))
  
  OrbAcro <-  if(tempA + tempO == 0) {
    "WARNING: No records of Orbicella or Acropora were found."
  } else {""}
  
  return(OrbAcro)
  },
  error = function(e){
    return(paste("Select temporal and/or spatial factors."))
    message("Select temporal and/or spatial factors.")
  },
  warning = function(w){
    return(paste("Select temporal and/or spatial factors."))
    message("Select temporal and/or spatial factors.")})
}

F_OrbAcro2 <- function(Samp) {
  tryCatch({
    tempA <- Samp %>%
    select(SPECIES_NAME, TotalPoints, N_COLONIES) %>%
    mutate(taxa = str_extract(SPECIES_NAME, "Acropora(?=[:space:])")) %>%
    filter(!is.na(taxa)) %>% 
    summarise(TP = sum(TotalPoints, na.rm = T),
              NC = sum(N_COLONIES, na.rm = T))
  
  tempO <- Samp %>%
    select(SPECIES_NAME, TotalPoints, N_COLONIES) %>%
    mutate(taxa = str_extract(SPECIES_NAME, "Orbicella(?=[:space:])")) %>%
    filter(!is.na(taxa)) %>%
    summarise(TP = sum(TotalPoints, na.rm = T),
              NC = sum(N_COLONIES, na.rm = T))
  
  OrbAcro <-  if(tempA$TP + tempO$TP + tempA$NC + tempO$NC == 0) {
    "WARNING: No records of Orbicella or Acropora were found."
  } else {""}
  
  return(OrbAcro)
  },
  error = function(e){
    return(paste(" "))
    message("Select temporal and/or spatial factors.")
  },
  warning = function(w){
    return(paste(" "))
    message("Select temporal and/or spatial factors.")})
  
}

# F_NotRecognized ----
# finds out the number of non-recognized records in the file
F_NotRecognized <- function(Misspelling) {
  tryCatch({
    tempA <- nrow(Misspelling)
    tempB <- length(Misspelling)
    
    NotRecognized <- if(tempA == 1){
      if(tempB == 1){
        paste("")
      } else {
        paste("WARNING: There is", tempA, "species that was not recognized in the master list. Please check the suggested actions.")
      }
    } else if(tempA > 1) {
      paste("WARNING: There are", tempA, "species that were not recognized in the master list. Please check the suggested actions.")
    } else {""}
    
    return(NotRecognized)
  },
  error = function(e){
    return(paste("Warning: Select temporal and/or spatial factors."))
    message("Select temporal and/or spatial factors.")
  },
  warning = function(w){
    return(paste("Warning: Select temporal and/or spatial factors."))
    message("Select temporal and/or spatial factors.")
  })
}

# F_Not100 ----
# finds out the transects in which the sum of data is not 100
# this one is later used to identify empty transects too.
F_Not100 <- function(Samp, colTransect, colSite, colYear) {
  tryCatch({
    numTransect <- unique(Samp[[colTransect]] %>% as.character())
    Samp <- Samp %>%
      filter(SPECIES_NAME != "Diadema antillarum" & SPECIES_NAME != "Diadema antillarum ascensionis" & 
               SPECIES_NAME != "Panulirus argus" & SPECIES_NAME != "Aliger gigas") %>%
      mutate(Transect = .[[colTransect]] %>% as.character()) 
    
    tempA <- tibble("Transect" = character(), "TotalPoints" = double())
    
    for (i in numTransect) {
      tempTot <- Samp %>%
        filter(Transect == i) %>%
        group_by(.[[colYear]], .[[colSite]], .[[colTransect]], Transect) %>%
        summarise(TotalPoints = sum(TotalPoints, na.rm = T))
      
      tempA <- bind_rows(tempA, tempTot)
    }
    Not100 <- tempA %>%
      mutate(`100pt` = ifelse(TotalPoints == 100, T, F)) %>%
      filter(`100pt` == F) %>%
      transmute(Year = `.[[colYear]]`,
                Site = `.[[colSite]]`,
                Transect = Transect,
                RecordPoints = TotalPoints)
    
    return(Not100)
  },
  error = function(e){
    message("Select temporal and/or spatial factors.")
  },
  warning = function(w){
    message("Select temporal and/or spatial factors.")
  })
}

# F_DoubleTransect ----
# finds out if there are repeated transects 
F_DoubleTransect <- function(MetricData, colTransect){
  tempA <- MetricData %>%
    nrow()
  
  tempB <- length(unique(MetricData[[colTransect]]))
  
  DoubleTransect <- if((tempA - tempB) == 1) {
    paste("WARNING: There is", (tempA - tempB), "sampling unit that is repeated in the file.")
  } else if((tempA - tempB > 1)) {
    paste("WARNING: There are", (tempA - tempB), "sampling units that are repeated in the file")
  } else {""}
  
  return(DoubleTransect)
}

# F_NumericDate ----
# finds out if there are date data that are not numeric
F_NumericDate <- function(MetricData, colYear){
  tryCatch({
    numDate <- sum(is.na(MetricData[[colYear]]))
    NumericDate <- if(numDate > 1){
      paste0("WARNING: There are ", numDate, " records in which ", colYear, " is not a number.")
    } else if(numDate == 1){
      paste0("WARNING: There is one record in which ", colYear, " is not a number.")
    } else {
      cat("")
    }
    
    return(NumericDate)
  },
  error = function(e){
    message("Select temporal and/or spatial factors.")
  },
  warning = function(w){
    message("Select temporal and/or spatial factors.")
    })
}

# F_EmptySpecies ----
# finds out if there are species records that are empty
F_EmptySpecies <- function(MetricData){
  tryCatch({
    specName <- sum(is.na(MetricData$SPECIES_NAME))
    EmptySpecies <- if(specName > 1){
      paste0("WARNING: There are ", specName, " records in which SPECIES_NAME is empty.")
    } else if(specName == 1){
      paste0("WARNING: There is one record in which SPECIES_NAME is empty.")
    } else {
      cat("")
    }
    
    return(EmptySpecies)
  },
  error = function(e){
    message("Select temporal and/or spatial factors.")
  },
  warning = function(w){
    message("Select temporal and/or spatial factors.")
  })
}
