#' WideColumns
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

# Functions to get the columns names ----
## Column names for "total" columns ----
f_WideTotal <- function(complete, avoid, type){
  filtered <- complete[!complete %in% avoid]
  selected <- filtered[grepl("TOTAL", filtered)]

  if (length(selected) == 0) {
    selected <- filtered
  }

  return(selected)
}

## Column names for "non-total" columns ----
f_WideOthers <- function(complete, avoid, type) {
  filtered <- complete[!complete %in% avoid]
  filtered <- filtered[!grepl("TOTAL", filtered)]

  if (type == "counts") {
    filtered <- filtered[!grepl("BIOMASS", filtered)]
  } else if (type == "weigths") {
    filtered <- filtered[grepl("BIOMASS", filtered)]
  }

  if (length(filtered) == 0) {
    filtered <- complete[!complete %in% avoid]
  }

  return(filtered)
}

## Main function ----
f_WideColumns <- function(complete, avoid, type){
  columnNames <- if(type == "total" | type == "biomass"){
    f_WideTotal(complete, avoid, type)
  } else {
    f_WideOthers(complete, avoid, type)
  }

  return(columnNames)
}

# Function to turn wide format to long format in Fish Model data ----
f_W2LFish <- function(dataLength, selected, FT1, FT2, BT1 = NULL, BT2 = NULL,
                      timeV, spatV, coorV, tranV, specV,
                      type = "WFOneSheet", dataBiomass = NULL){
  FT1_i <- which(names(dataLength) == FT1)
  FT2_i <- which(names(dataLength) == FT2)
  if(type == "WFOneSheet"){
    BT1_i <- which(names(dataLength) == BT1)
    BT2_i <- which(names(dataLength) == BT2)
  } else if(type == "WFTwoSheets"){
    BT1_i <- which(names(dataBiomass) == BT1)
    BT2_i <- which(names(dataBiomass) == BT2)
  }

  lengthData <- dataLength |>
    dplyr::select(any_of(selected),
                  as.numeric(FT1_i:FT2_i))
  FT1_i <- which(names(lengthData) == FT1)
  FT2_i <- which(names(lengthData) == FT2)

  lengthData <- lengthData |>
    dplyr::mutate(index = rownames(dataLength)) |>
    tidyr::pivot_longer(cols = c(as.numeric(FT1_i:FT2_i)),
                        names_to = "SCIENTIFIC_NAME",
                        values_to = "LENGTH") |>
    dplyr::mutate(SCIENTIFIC_NAME = stringr::str_replace(SCIENTIFIC_NAME, ".LENGTH", ""),
                  SCIENTIFIC_NAME = stringr::str_replace(SCIENTIFIC_NAME, "_LENGTH", ""),
                  SCIENTIFIC_NAME = stringr::str_replace(SCIENTIFIC_NAME, "LENGTH.", ""),
                  SCIENTIFIC_NAME = stringr::str_replace(SCIENTIFIC_NAME, "LENGTH_", ""),
                  SCIENTIFIC_NAME = stringr::str_replace(SCIENTIFIC_NAME, "LENGTH", ""),
                  SCIENTIFIC_NAME = stringr::str_to_sentence(SCIENTIFIC_NAME),
                  SCIENTIFIC_NAME = stringr::str_replace_all(SCIENTIFIC_NAME, "\\.", " ")
    )

  almost <- if(type == "WFOneSheet"){
    weigthData <- dataLength |>
      dplyr::select(any_of(selected),
                    as.numeric(BT1_i:BT2_i))
    BT1_i <- which(names(weigthData) == BT1)
    BT2_i <- which(names(weigthData) == BT2)

    weigthData <- weigthData |>
      dplyr::mutate(index = rownames(weigthData)) |>
      tidyr::pivot_longer(cols = c((length(selected)+1):(length(weigthData))),
                          names_to = "SCIENTIFIC_NAME",
                          values_to = "BIOMASS") |>
      dplyr::mutate(SCIENTIFIC_NAME = stringr::str_replace(SCIENTIFIC_NAME, ".BIOMASS", ""),
                    SCIENTIFIC_NAME = stringr::str_replace(SCIENTIFIC_NAME, "_BIOMASS", ""),
                    SCIENTIFIC_NAME = stringr::str_replace(SCIENTIFIC_NAME, "BIOMASS.", ""),
                    SCIENTIFIC_NAME = stringr::str_replace(SCIENTIFIC_NAME, "BIOMASS_", ""),
                    SCIENTIFIC_NAME = stringr::str_replace(SCIENTIFIC_NAME, "BIOMASS", ""),
                    SCIENTIFIC_NAME = stringr::str_to_sentence(SCIENTIFIC_NAME),
                    SCIENTIFIC_NAME = stringr::str_replace_all(SCIENTIFIC_NAME, "\\.", " ")
      )
    dplyr::full_join(lengthData, weigthData) |>
      filter(!is.na(LENGTH)) |>
      select(!index)
  } else if (type == "WFLength") {
    lengthData |>
      filter(!is.na(LENGTH)) |>
      select(!index) |>
      f_cBiomass(LEN = "LENGTH", specV = "SCIENTIFIC_NAME")
  } else if(type == "WFTwoSheets") {
    weigthData <- dataBiomass |>
      dplyr::select(any_of(selected),
                    as.numeric(BT1_i:BT2_i))
    BT1_i <- which(names(weigthData) == BT1)
    BT2_i <- which(names(weigthData) == BT2)

    weigthData <- weigthData |>
      dplyr::mutate(index = rownames(weigthData)) |>
      tidyr::pivot_longer(cols = c((length(selected)+1):(length(weigthData))),
                          names_to = "SCIENTIFIC_NAME",
                          values_to = "BIOMASS") |>
      dplyr::mutate(SCIENTIFIC_NAME = stringr::str_replace(SCIENTIFIC_NAME, ".BIOMASS", ""),
                    SCIENTIFIC_NAME = stringr::str_replace(SCIENTIFIC_NAME, "_BIOMASS", ""),
                    SCIENTIFIC_NAME = stringr::str_replace(SCIENTIFIC_NAME, "BIOMASS.", ""),
                    SCIENTIFIC_NAME = stringr::str_replace(SCIENTIFIC_NAME, "BIOMASS_", ""),
                    SCIENTIFIC_NAME = stringr::str_replace(SCIENTIFIC_NAME, "BIOMASS", ""),
                    SCIENTIFIC_NAME = stringr::str_to_sentence(SCIENTIFIC_NAME),
                    SCIENTIFIC_NAME = stringr::str_replace_all(SCIENTIFIC_NAME, "\\.", " ")
      )
    dplyr::full_join(lengthData, weigthData) |>
      filter(!is.na(LENGTH)) |>
      select(!index)
  }

  FINAL <- f_L2LFish(almost, timeV, spatV, coorV, tranV,
                     specV = "SCIENTIFIC_NAME",
                     lenV = "LENGTH",
                     biomV = "BIOMASS",
                     type = "simple")

  return(FINAL)
}

# Function to turn wide format into long format for data from Mobile species and BSAT corals ----
f_W2LBCGcount <- function(data, selected, FT1, FT2, timeV, spatV,
                          coorV, compV, tranV, type){

  FT1_i <- which(names(data) == FT1)
  FT2_i <- which(names(data) == FT2)

  if(compV != "None"){
    selected <- c(selected, compV)
  }

  data <- data |>
    dplyr::select(any_of(selected),
                  as.numeric(FT1_i:FT2_i))
  FT1_i <- which(names(data) == FT1)
  FT2_i <- which(names(data) == FT2)

  almost <- data |>
    tidyr::pivot_longer(cols = c(as.numeric(FT1_i:FT2_i)),
                        names_to = "SPECIES_NAME",
                        values_to = "COVER") |>
    dplyr::mutate(SPECIES_NAME = stringr::str_replace_all(SPECIES_NAME, "([.])", " "),
                  SPECIES_NAME = stringr::str_to_sentence(SPECIES_NAME)
    ) |>
    dplyr::filter(COVER > 0)

  FINAL <- f_L2LBCGSimple(almost, timeV, spatV,
                          coorV, compV, tranV,
                          specV = "SPECIES_NAME",
                          counV = "COVER",
                          type)

  return(FINAL)
}

# Function to turn wide format into long format from BCG corals ----
f_W2LDEMO <- function(data, selected, timeV, spatV, coorV, compV, tranV,
                      prefMaxDiam, prefPerpDiam, prefHeight, prefOldMort,
                      prefRecentMort, prefBleach, prefDisease){

  if(compV != "None"){
    selected <- c(selected, compV)
  }

  MaxDiam <- indexPrefixes(data, selected, prefMaxDiam)
  PerpDiam <- indexPrefixes(data, selected, prefPerpDiam)
  Height <- indexPrefixes(data, selected, prefHeight)
  OldMort <- indexPrefixes(data, selected, prefOldMort)
  RecentMort <- indexPrefixes(data, selected, prefRecentMort)
  Bleach <- indexPrefixes(data, selected, prefBleach)
  Disease <- indexPrefixes(data, selected, prefDisease)

  almost <- dplyr::full_join(MaxDiam, PerpDiam) |>
    dplyr::full_join(Height) |>
    dplyr::full_join(OldMort) |>
    dplyr::full_join(RecentMort) |>
    dplyr::full_join(Bleach) |>
    dplyr::full_join(Disease)

  FINAL <- f_L2LDEMOSimple(almost, timeV, spatV,
                           coorV, compV, tranV,
                           specV = "SPECIES_NAME",
                           NCol = NULL,
                           prefMaxDiam, prefPerpDiam,
                           prefHeight, prefOldMort, prefRecentMort,
                           prefBleach, prefDisease,
                           type = "wide")

  return(FINAL)
}


