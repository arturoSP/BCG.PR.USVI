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

  # Adding a couple of species, for testing purposes
  # Samp_species <- c(Samp_species, "Aluterus schoepfii", "Anchoa lyolepis",
  #                   "Synodus synodus")

  Samp_species_missing <- setdiff(Samp_species, FishMasterList[,1])

  withProgress(
    message = "Please wait as this may take a few minutes" ,
    value = 0,
    {
      total_steps <- length(Samp_species_missing)
      # Calculate progress
      progress <- 1 / total_steps
      # find species with data in the FishBase
      Samp_species_list_missing <- sapply(1:total_steps, function(i) {
        result <- rfishbase::length_weight(Samp_species_missing[i])

        # Update progress bar
        incProgress(amount = progress,
                    detail = paste("Processing species", i,
                                   "of", total_steps ))

        return(result)
      }, simplify = FALSE)

      Samp_species_list_missing <- Samp_species_list_missing |>
        lapply(select, c(a, b, CoeffDetermination)) |>
        lapply(mutate, CoeffDetermination = ifelse(is.na(CoeffDetermination),
                                                   0,
                                                   CoeffDetermination)) |>
        lapply(filter, CoeffDetermination == max(CoeffDetermination))

      # Get the average values for A and B, then compute biomass with the formula
      Samp_species_list_missing <- Filter(function(x) nrow(x) > 0, Samp_species_list_missing) |>
        lapply(function(x){
          x |>
            summarise(
              A = mean(a, na.rm = TRUE),
              B = mean(b, na.rm = TRUE)
            )
        })
    }
  )

  Samp_species_list_missing <- mapply(function(data, name){
    data |>
      mutate(SCIENTIFIC_NAME = name)
  }, Samp_species_list_missing, names(Samp_species_list_missing), SIMPLIFY = FALSE) |>
    bind_rows()

  if(specV != "SCIENTIFIC_NAME"){
    inputDT$SCIENTIFIC_NAME <- inputDT[[specV]]
    inputDT[[specV]] <- NULL
  }
  if(LEN != "LENGTH"){
    inputDT$LENGTH <- inputDT[[LEN]]
    inputDT[[LEN]] <- NULL
  }

  length_weight_list <- FishMasterList |>
    transmute(SCIENTIFIC_NAME = FinalID,
              A,
              B) |>
    bind_rows(Samp_species_list_missing) |>
    filter(!is.na(A))

  Samp <- left_join(inputDT, length_weight_list) |>
    mutate(BIOMASS = A * LENGTH ^ B) |>
    select(!A:B)

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
  Final[,"LOCATION"] <- case_when(Final[,"LOCATION"] == "FL" ~ "FL",
                                  Final[,"LOCATION"] == "FLO" ~ "FL",
                                  Final[,"LOCATION"] == "FLORIDA" ~ "FL",
                                  Final[,"LOCATION"] == "FLRD" ~ "FL",
                                  TRUE ~ "PR/USVI")
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
    Final[, "N_COLONIES"] = select(Simple, any_of(NCol))
  }

  # quick adjustment so that METERS_COMPLETED is always set
  Final[is.na(Final$METERS_COMPLETED),"METERS_COMPLETED"] <- 10

  # delete rows that include the word "total" in the species name
  Final <- filter(Final, !grepl("total", SPECIES_NAME, ignore.case = TRUE))

  # delete rows with 0 colonies
  Final <- filter(Final, N_COLONIES > 0)

  return(Final)
}

# Detect the type of length codes (classes vs ranges) ----
detect_code_type <- function(codes) {
  if (any(grepl("c\\d+", codes, ignore.case=TRUE))) {
    return("class")
  } else if (any(grepl("^[0-9]+(-[0-9]+)?$", codes))) {
    return("range")
  } else {
    stop("Unable to determine the length code type. Please check the data.")
  }
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

    length_cols <- names(InputDT)[FT1_i:FT2_i]
    code_type <- detect_code_type(length_cols)  # Identify code type
    renamed <- ifelse(code_type == "range", "Size", "Class")

    lengthData <- InputDT |>
      dplyr::select(any_of(selected),
                    any_of(specV),
                    as.numeric(FT1_i:FT2_i))
    FT1_i <- which(names(lengthData) == FT1)
    FT2_i <- which(names(lengthData) == FT2)

    lengthData <- lengthData |>
      tidyr::pivot_longer(cols = c(as.numeric(FT1_i:FT2_i)),
                          names_to = "LENGTH",
                          values_to = "COUNT") |>
      dplyr::filter(!is.na(COUNT)) |>
      tidyr::uncount(COUNT)

      lengthData <- lengthData |>
        dplyr::rename(!!renamed := "LENGTH") |>
        dplyr::left_join(FishLengthClass) |>
        dplyr::select(-Class, -Size) |>
        f_cBiomass(LEN = "Median", specV = specV) |>
        f_L2LFish(timeV, spatV, coorV, tranV,
                  specV = "SCIENTIFIC_NAME",
                  lenV = "LENGTH",
                  biomV = "BIOMASS",
                  type = "simple")

  } else if(type == "LongCalc") {
    # This is the routine to calculate biomass from the coded length
    code_type <- detect_code_type(InputDT[[lenV]])
    renamed <- ifelse(code_type == "range", "Size", "Class")
    InputDT <- InputDT |>
      dplyr::select(any_of(selected), any_of(specV), any_of(lenV)) |>
      dplyr::rename(!!renamed := any_of(lenV))

    InputDT |>
      dplyr::left_join(FishLengthClass, by = renamed) |>
      dplyr::select(-Class, -Size) |>
      f_cBiomass(LEN = "Median", specV = specV) |>
      f_L2LFish(timeV, spatV, coorV, tranV,
                specV = "SCIENTIFIC_NAME",
                lenV = "LENGTH",
                biomV = "BIOMASS",
                type = "simple")
  } else if(type == "LongSimple"){
    # This is the routine for when the length is coded and biomass is already supplied
    code_type <- detect_code_type(InputDT[[lenV]])
    renamed <- ifelse(code_type == "range", "Size", "Class")
    InputDT <- InputDT |>
      dplyr::select(any_of(selected), any_of(specV), any_of(lenV), any_of(biomV)) |>
      dplyr::rename(!!renamed := any_of(lenV))

    InputDT |>
      dplyr::left_join(FishLengthClass, by = renamed) |>
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
