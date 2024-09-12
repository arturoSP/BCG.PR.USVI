#' LongColumns
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

# Function to calculate biomass ----
f_cBiomass <- function(inputDT, LEN, specV){
  # read the list of unique species
  Samp_species <- inputDT[inputDT[[LEN]] > 0,]
  Samp_species <- Samp_species[[specV]] |>
    unique()

  withProgress(
    message = "Please wait as this may take a few minutes" ,
    value = 0,
    {
      incProgress(1/4, detail = "Obtaining data from FishBase...")
      # find species with data in the FishBase
      Samp_species_list <- sapply(Samp_species,
                                  rfishbase::length_weight,
                                  simplify = FALSE)

      incProgress(1/4, detail = "Calculating biomass...")

      Samp_species_list <- Samp_species_list |>
        lapply(select, c(a, b, CoeffDetermination)) |>
        lapply(mutate, CoeffDetermination = ifelse(is.na(CoeffDetermination),
                                                   0,
                                                   CoeffDetermination)) |>
        lapply(filter, CoeffDetermination == max(CoeffDetermination))

      incProgress(1/4, detail = "Obtaining averages...")

      # Get the average values for A and B, then compute biomass with the formula
      Samp_species_list <- Filter(function(x) nrow(x) > 0, Samp_species_list) |>
        lapply(function(x){
          x |>
            summarise(
              A = mean(a, na.rm = TRUE),
              B = mean(b, na.rm = TRUE)
            )
        })
      incProgress(1/4, detail = "Ending the process...")
    }
  )

  Samp_species_list <- mapply(function(data, name){
    data |>
      mutate(SCIENTIFIC_NAME = name)
  }, Samp_species_list, names(Samp_species_list), SIMPLIFY = FALSE) |>
    bind_rows()

  if(specV != "SCIENTIFIC_NAME"){
    inputDT$SCIENTIFIC_NAME <- inputDT[[specV]]
    inputDT[[specV]] <- NULL
  }
  if(LEN != "LENGTH"){
    inputDT$LENGTH <- inputDT[[LEN]]
    inputDT[[LEN]] <- NULL
  }


  Samp <- left_join(inputDT, Samp_species_list) |>
    mutate(BIOMASS = A * LENGTH ^ B) |>
    select(!A) |>
    select(!B)

  Samp[is.na(Samp[,"BIOMASS"]), "BIOMASS"] <- 0

  return(Samp)
}

# Function to summarise the long format data after biomass calculation ----

f_L2LFish <- function(Samp, timeV, spatV, coorV, tranV,
                      specV = NULL, lenV = NULL, biomV = NULL,
                      type){
  # first we get a template in which data will be set
  Final <- templateFish

  # start filling the template with the input data
  Final <- bind_rows(templateFish, select(Samp, any_of(timeV)))
  Final[, spatV[spatV != ""]] <- select(Samp, any_of(spatV))

  temp <- select(Samp, any_of(coorV)) |>
    `colnames<-`(c("LAT_DEGREES", "LON_DEGREES")) |>
    as.data.frame()
  Final[, c("LAT_DEGREES", "LON_DEGREES")] <- temp

  temp <- select(Samp, any_of(tranV)) |>
    `colnames<-`("PRIMARY_SAMPLE_UNIT") |>
    as.data.frame()
  Final[, "PRIMARY_SAMPLE_UNIT"] <- temp


  if(type == "simple"){
    temp <- select(Samp, any_of(specV)) |>
      `colnames<-`("SCIENTIFIC_NAME") |>
      as.data.frame()
    Final[, "SCIENTIFIC_NAME"] <- temp

    temp <- select(Samp, any_of(lenV)) |>
      `colnames<-`("LENGTH") |>
      as.data.frame()
    Final[, "LENGTH"] <- temp

    temp <- select(Samp, any_of(biomV)) |>
      `colnames<-`("BIOMASS") |>
      as.data.frame()
    Final[, "BIOMASS"] <- temp
  } else {
    Final[, "SCIENTIFIC_NAME"] <- Samp[, "SCIENTIFIC_NAME"]
    Final[, "LENGTH"] <- Samp[, "LENGTH"]
    Final[, "BIOMASS"] <- Samp[, "BIOMASS"]
  }

  # delete rows that include the word "total" in the species name
  Final <- filter(Final, !grepl("total", SCIENTIFIC_NAME, ignore.case = TRUE))

  # quick adjustment so that location and habitat are always set
  Final[,"LOCATION"] <- stringr::str_to_upper(Final[,"LOCATION"])
  Final[,"LOCATION"] <- case_when(Final[,"LOCATION"] == "FLO" ~ "FL",
                                  Final[,"LOCATION"] == "FLORIDA" ~ "FL",
                                  Final[,"LOCATION"] == "FLRD" ~ "FL",
                                  TRUE ~ "PR")
  Final[,"HABITAT"] <- stringr::str_to_upper(Final[,"HABITAT"])
  Final[,"HABITAT"] <- case_when(stringr::str_detect(Final$HABITAT, "HARD") ~ "HARD",
                                 stringr::str_detect(Final$HABITAT, "HRD") ~ "HARD",
                                 stringr::str_detect(Final$HABITAT, "ROCK") ~ "HARD",
                                 stringr::str_detect(Final$HABITAT, "RCK") ~ "HARD",
                                 TRUE ~ "REEF")

  return(Final)
}

f_L2LBCGSimple <- function(Simple, timeV, spatV, coorV, compV,
                            tranV, specV, counV, type){
  # start by filling the template with the input data
  Final <- bind_rows(templateLPI, select(Simple, any_of(timeV)))

  temp <- dplyr::rename(dplyr::select(Simple, dplyr::any_of(spatV)),
                        dplyr::any_of(mapping_spatial))
  Final[, colnames(temp)] <- temp

  temp <- dplyr::rename(dplyr::select(Simple, dplyr::any_of(coorV)),
                        dplyr::any_of(mapping_coordinates))
  Final[, colnames(temp)] <- temp

  temp <- dplyr::rename(Simple[, tranV], "PRIMARY_SAMPLE_UNIT" = dplyr::any_of(tranV))
  Final[, colnames(temp)] <- temp

  temp <- dplyr::rename(Simple[, specV], "SPECIES_NAME" = dplyr::any_of(specV))
  Final[, colnames(temp)] <- temp

  temp <- dplyr::rename(Simple[, counV], "COVER" = dplyr::any_of(counV))
  Final[, colnames(temp)] <- temp

  if(compV != "None"){
    temp <- dplyr::rename(Simple[, compV], "METERS_COMPLETED" = dplyr::any_of(compV))
    Final[, colnames(temp)] <- temp
  } else {
    Final[, "METERS_COMPLETED"] <- NA
  }


  # delete the empty values
  Final <- filter(Final, !is.na(Final$COVER))

  # delete rows that include the word "total" in the species name
  Final <- filter(Final, !grepl("total", SPECIES_NAME, ignore.case = TRUE))

  # if it's for mobile species then change the name of the last column
  if(type == "Mobile"){
    colnames(Final)[14] <- "COUNTS"
  }

  # quick adjustment so that METERS_COMPLETED is always set
  Final[is.na(Final$METERS_COMPLETED),"METERS_COMPLETED"] <- 15

  return(Final)
}

f_L2LDEMOSimple <- function(Simple, timeV, spatV,
                            coorV, compV, tranV,
                            specV, NCol,
                            prefMaxDiam, prefPerpDiam,
                            prefHeight, prefOldMort, prefRecentMort,
                            prefBleach, prefDisease,
                            type = "long"){

  Final <- dplyr::bind_rows(templateDEMO,
                            dplyr::select(Simple, dplyr::any_of(timeV)))

  temp <- dplyr::rename(dplyr::select(Simple, dplyr::any_of(spatV)),
                        dplyr::any_of(mapping_spatial))
  Final[, colnames(temp)] <- temp

  temp <- dplyr::rename(dplyr::select(Simple, dplyr::any_of(coorV)),
                        dplyr::any_of(mapping_coordinates))
  Final[, colnames(temp)] <- temp

  temp <- dplyr::rename(Simple[, tranV], "PRIMARY_SAMPLE_UNIT" = dplyr::any_of(tranV))
  Final[, colnames(temp)] <- temp

  temp <- dplyr::rename(Simple[, specV], "SPECIES_NAME" = dplyr::any_of(specV))
  Final[, colnames(temp)] <- temp

  if(compV != "None"){
    temp <- dplyr::rename(Simple[, compV], "METERS_COMPLETED" = dplyr::any_of(compV))
    Final[, colnames(temp)] <- temp
  } else {
    Final[, "METERS_COMPLETED"] <- NA
  }

  measures <- c(prefMaxDiam, prefPerpDiam, prefHeight, prefOldMort,
                prefRecentMort, prefBleach, prefDisease)
  temp <- Simple[, measures] |>
    `colnames<-`(c("MAX_DIAMETER", "PERP_DIAMETER", "HEIGHT",
                   "OLD_MORT", "RECENT_MORT", "BLEACH_CONDITION", "DISEASE"))
  Final[, colnames(temp)] <- temp

  # Quick adjustment to mark number of colonies in wide type files
  if(type == "wide"){
    Final[, "N_COLONIES"] = 1
  } else {
    Final[, "N_COLONIES"] = select(Simple, all_of(NCol))
  }

  # quick adjustment so that METERS_COMPLETED is always set
  Final[is.na(Final$METERS_COMPLETED),"METERS_COMPLETED"] <- 10

  # delete rows that include the word "total" in the species name
  Final <- filter(Final, !grepl("total", SPECIES_NAME, ignore.case = TRUE))

  return(Final)
}

# Function to summarize long data for coded Fish lengths ----
# We use the codes to get the length range and median value. This is used as standard length
f_C2LFish <- function(InputDT, selected,
                      timeV, spatV, coorV, tranV,
                      specV = NULL, lenV = NULL, biomV = NULL,
                      FT1 = NULL, FT2 = NULL,
                      type = "Wide"){


  Final <- if(type == "Wide"){
    # Routine to convert wide coded data into long data
    FT1_i <- which(names(InputDT) == FT1)
    FT2_i <- which(names(InputDT) == FT2)

    lengthData <- InputDT |>
      dplyr::select(all_of(selected),
                    as.numeric(FT1_i:FT2_i))
    FT1_i <- which(names(lengthData) == FT1)
    FT2_i <- which(names(lengthData) == FT2)

    lengthData |>
      dplyr::mutate(index = rownames(InputDT)) |>
      tidyr::pivot_longer(cols = c(as.numeric(FT1_i:FT2_i)),
                          names_to = "SCIENTIFIC_NAME",
                          values_to = "COUNT") |>
      dplyr::filter(!is.na(COUNT)) |>
      dplyr::mutate(LengthCode = stringr::str_extract(SCIENTIFIC_NAME, "C[:digit:]+"),
                    SCIENTIFIC_NAME = stringr::str_replace(SCIENTIFIC_NAME, "C[:digit:]+", ""),
                    SCIENTIFIC_NAME = stringr::str_replace_all(SCIENTIFIC_NAME, "\\.", " "),
                    SCIENTIFIC_NAME = stringr::str_replace(SCIENTIFIC_NAME, " $", ""),
                    SCIENTIFIC_NAME = stringr::str_to_sentence(SCIENTIFIC_NAME)) |>
      dplyr::mutate(Class = purrr::map2(LengthCode, COUNT, ~rep(.x, .y))) |>
      dplyr::select(-COUNT, -LengthCode) |>
      tidyr::unnest(Class) |>
      dplyr::mutate(Class = stringr::str_to_lower(Class)) |>
      dplyr::left_join(FishLengthClass) |>
      dplyr::select(-index, -Class, -Size) |>
      f_cBiomass(LEN = "Median", specV = "SCIENTIFIC_NAME") |>
      f_L2LFish(timeV, spatV, coorV, tranV,
                specV = "SCIENTIFIC_NAME",
                lenV = "LENGTH",
                biomV = "BIOMASS",
                type = "simple")
  } else if(type == "LongCalc") {
    # This is the routine to calculate biomass from the coded length
    InputDT |>
      dplyr::select(all_of(selected), all_of(specV), all_of(lenV)) |>
      dplyr::rename("Class" = lenV) |>
      dplyr::left_join(FishLengthClass) |>
      dplyr::select(-Class, -Size) |>
      f_cBiomass(LEN = "Median", specV = specV) |>
      f_L2LFish(timeV, spatV, coorV, tranV,
                specV = "SCIENTIFIC_NAME",
                lenV = "LENGTH",
                biomV = "BIOMASS",
                type = "simple")
  } else if(type == "LongSimple"){
    # This is the routine for when the length is coded and biomass is already supplied
    InputDT |>
      dplyr::select(all_of(selected), all_of(specV),
                    all_of(lenV), all_of(biomV)) |>
      dplyr::rename("Class" = lenV) |>
      dplyr::left_join(FishLengthClass) |>
      dplyr::select(-Class, -Size) |>
      dplyr::rename("LENGTH" = Median) |>
      f_L2LFish(timeV, spatV, coorV, tranV,
                specV = "SCIENTIFIC_NAME",
                lenV = "LENGTH",
                biomV = "BIOMASS",
                type = "simple")
  }

  return(Final)
}
