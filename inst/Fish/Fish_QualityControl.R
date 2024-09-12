# Functions to do quality control on the dataset

# Find names that are not registered ----
# prints a table with the rownames and data that is not registered in the MstLst 
F_Misspelling_1 <- function(Samp) {
  tryCatch({
    # find the species names that are not present in the MstLst
    Msp1 <- Samp |> 
      select(SCIENTIFIC_NAME) |> 
      left_join(MstLst |> 
                  select(FinalID) |> 
                  mutate(Valid = "OK"),
                by = c("SCIENTIFIC_NAME" = "FinalID")) |> 
      filter(is.na(Valid)) |> 
      distinct(SCIENTIFIC_NAME) |> 
      arrange((SCIENTIFIC_NAME))
    
    return(Msp1)
  },
  # define how the function deals with errors
  error = function(e){
    return(tribble(~vacio, "vacio "))
    message("Select temporal and/or spatial factors.")
  },
  warning = function(w){
    return(tribble(~vacio, "vacio "))
    message("Select temporal and/or spatial factors.")})
}

# Check names with WoRMS ----
# Second part of the function in which we check the non-present names with
# the records from WoRMS and then decide to give a suggestion for the user
F_Misspelling_2 <- function(Msp1) {
  
  if(is.na(Msp1[1,])){
    return(tribble(~Status, "All taxa in file were correctly identified."))
  } else if(Msp1[1,] == "vacio ") {
    return(tribble(~WARNING, "Select temporal and/or spatial factors."))
  } else {
    # create the template for filling out the resulting info
    Msp4 <- tibble("SCIENTIFIC_NAME" = character(), 
                   "status" = character(), "validName" = character())
    withProgress(
      message = "Please wait as this may take a few minutes...",
      detail = "Species not recognized on the master list are being verified at www.marinespecies.org.",
      {
        for(i in pull(Msp1)) {
          # find the adequate record using WORMS database
          temp <- tryCatch(worrms::wm_records_taxamatch(i), 
                           error = function(e){
                             list(tribble(~status, ~valid_name, 
                                          "unaccepted", "Unknown"))
                           }
          )
          # assign adequate records to the missing species
          Msp3 <- tibble(
            status = purrr::map(temp, "status"),
            validName = as.character(purrr::map(temp, "valid_name"))) |> 
            unnest(cols = c(status, validName)) |> 
            mutate(SCIENTIFIC_NAME = i) 
          
          
          Msp4 <- bind_rows(Msp4, Msp3)
          incProgress(1 / nrow(Msp1))
        }
      }
    )
    
    # order the information in the return value
    Misspelling <- Msp4 %>%
      filter(!duplicated(.[["SCIENTIFIC_NAME"]])) |> 
      mutate(validName = as.character(ifelse(validName == SCIENTIFIC_NAME, "", validName))) |> 
      left_join(MstLst |> 
                  select(FinalID) |> 
                  mutate(match = T),
                by = c("validName" = "FinalID")) |> 
      replace_na(list(match = F)) |> 
      transmute(`Name in file` = SCIENTIFIC_NAME,
                `Status` = status,
                `Suggested name (WoRMS)` = validName,
                `Suggested actions` = ifelse(validName == "Unknown" & status == "unaccepted", 
                                             "Use an appropriate scientific name or check if this is a biological variable.",
                                             ifelse(validName != "Unknown" & status == "accepted" & match == F,
                                                    "Request to include in master list.",
                                                    ifelse(is.null(validName) & is.null(SCIENTIFIC_NAME), "There are empty names in the file.",
                                                    "Check the spelling of the scientific name or if this is an unaccepted synonym."))
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

# Counts non-recognized records ----
# finds out the number of non-recognized records in the file
F_NotRecognized <- function(Misspelling) {
  tryCatch({
    tempA <- nrow(Misspelling)
    
    NotRecognized <- if(tempA == 1){
      if(length(Misspelling) == 1){
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

# F_NumericDate ----
# finds out if there are date data that are not numeric
F_NumericDate <- function(MetricData, colYear){
  tryCatch({
    MetricData[[colYear]] <- as.numeric(MetricData[[colYear]])
    numDate <- sum(is.na(MetricData[[colYear]]))
    NumericDate <- if(numDate > 1){
      paste0("WARNING: There are ", numDate, " records in which ", colYear, " is not a number.")
    } else if(numDate == 1){
      paste0("WARNING: There is one record in which ", colYear, " is not a number.")
    } else {
      paste0("")
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
    specName <- sum(is.na(MetricData$SCIENTIFIC_NAME))
    EmptySpecies <- if(specName > 1){
      paste0("WARNING: There are ", specName, " records in which SCIENTIFIC_NAME is empty.")
    } else if(specName == 1){
      paste0("WARNING: There is one record in which SCIENTIFIC_NAME is empty.")
    } else {
      paste0("")
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
