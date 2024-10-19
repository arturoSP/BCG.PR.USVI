#' helpers
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

# possible names for the temporal variables -----
temporalValues <- c("YEAR",
                    "YEARS",
                    "MONTH",
                    "MONTHS",
                    "DAY",
                    "DAYS",
                    "HOUR",
                    "HOURS",
                    "TIME",
                    "TIMESTAMP",
                    "DATE")

# possible names for the spatial variables -----
spatialValues <- c("REGION",
                   "REGION NAME",
                   "REGION_NAME",
                   "REGION-NAME",
                   "SUBREGION",
                   "SUB REGION",
                   "SUB-REGION",
                   "SUB_REGION",
                   "SUB REGION NAME",
                   "SUB_REGION_NAME",
                   "SUB-REGION-NAME",
                   "SITE",
                   "SITE NAME",
                   "SITE_NAME",
                   "SITE.NAME",
                   "SITES",
                   "SITES NAMES",
                   "SITES",
                   "LOCATION",
                   "HABITAT")

# possible names for the transect variable -----
transectValues <- c("TRANSECT",
                    "SAMPLING UNIT",
                    "SAMPLING_UNIT",
                    "SAMPLINGUNIT",
                    "PRIMARY SAMPLE UNIT",
                    "PRIMARY_SAMPLE_UNIT",
                    "PRIMARY SAMPLING UNIT",
                    "PRIMARY_SAMPLING_UNIT",
                    "PRIMARYSAMPLINGUNIT",
                    "TRANSECTS",
                    "SAMPLING UNITS",
                    "SAMPLING_UNITS",
                    "SAMPLINGUNITS",
                    "PRIMARY SAMPLE UNITS",
                    "PRIMARY_SAMPLE_UNITS",
                    "PRIMARY SAMPLING UNITS",
                    "PRIMARY_SAMPLING_UNITS",
                    "PRIMARYSAMPLINGUNITS")

# possible names for the coordinates variables ----
coordinateValues <- c("LATITUDE",
                      "LAT",
                      "LAT_DEGREES",
                      "LAT_DEGREE",
                      "COORD_LAT",
                      "COORDLAT",
                      "COORD_LATITUDE",
                      "LONGITUDE",
                      "LON",
                      "LONG",
                      "LON_DEGREES",
                      "LONG_DEGREES",
                      "LON_DEGREE",
                      "LONG_DEGREE",
                      "COORD_LON",
                      "COORD_LONG",
                      "COORDLON",
                      "COORDLONG",
                      "COORD_LONGITUDE")

# possible names for the lenght variables ----
lengthValues <- c("LENGHT",
                  "LENGTH",
                  "LEN",
                  "LONG",
                  "L")

# possible names for the scientific name variable ----
namesValues <- c("SPECIES NAME",
                 "SPECIES_NAME",
                 "SPECIES-NAME",
                 "SPECIES NAMES",
                 "SPECIES_NAMES",
                 "SPECIES-NAMES",
                 "SCIENTIFIC NAME",
                 "SCIENTIFIC_NAME",
                 "SCIENTIFIC-NAME",
                 "SCIENTIFIC NAME",
                 "SCIENTIFIC_NAMES",
                 "SCIENTIFIC-NAMES",
                 "SPECIES",
                 "SPP",
                 "SPP.",
                 "NAME",
                 "NAMES",
                 "OBSERVED SPECIES",
                 "OBSERVED_SPECIES",
                 "OBSERVED-SPECIES")

# possible names for the total count variables ----
countValues <- c("COUNT",
                 "TOTAL COUNT",
                 "TOTAL.COUNT",
                 "TOTAL_COUNT",
                 "TOTAL-COUNT",
                 "OBSERVATIONS",
                 "OBSERVED",
                 "NUMBER",
                 "N")

# possible names for the total biomass variable ----
biomassValues <- c("BIOMASS",
                   "T BIOMASS",
                   "T_BIOMASS",
                   "T-BIOMASS",
                   "TOTAL BIOMASS",
                   "TOTAL_BIOMASS",
                   "TOTAL.BIOMASS",
                   "TOTAL-BIOMASS")

# Size class intervals and size class medians for fish length ----
FishLengthClass <- data.frame(Class = c("c0", "c1", "c2", "c3", "c4", "c5", "c6", "c7",
                                        "c8", "c9", "c10", "c11", "c12", "c13",
                                        "c14", "c15", "c16", "c17", "c18", "c19",
                                        "c20", "c21"),
                              Size = c("0", "1-5", "6-10", "11-15", "16-20", "21-25",
                                       "26-30", "31-35", "36-40", "41-45", "46-50",
                                       "51-55", "56-60", "61-65", "66-70", "71-75",
                                       "76-80", "81-85", "86-90", "91-95", "96-100",
                                       "101-105"),
                              Median = c(0, 3, 8, 13, 18, 23, 28, 33, 38, 43, 48, 53,
                                         58, 63, 68, 73, 78, 83, 88, 93, 98, 103))

# template for Fish data ----
templateFish <- data.frame("YEAR" = numeric(), "MONTH" = numeric(), "DAY" = numeric(),
                 "REGION" = character(), "SUB_REGION" = character(),
                 "SITE" = character(),
                 "LOCATION" = character(), "HABITAT" = character(),
                 "PRIMARY_SAMPLE_UNIT" = character(),
                 "LAT_DEGREES" = numeric(), "LON_DEGREES" = numeric(),
                 "SCIENTIFIC_NAME" = character(),
                 "LENGTH" = numeric(), "BIOMASS" = numeric())

# template for LPI data ----
templateLPI <- data.frame("YEAR" = numeric(), "MONTH" = numeric(), "DAY" = numeric(),
                          "REGION" = character(), "SUB_REGION" = character(),
                          "SITE" = character(),
                          "LOCATION" = character(), "HABITAT" = character(),
                          "PRIMARY_SAMPLE_UNIT" = character(),
                          "METERS_COMPLETED" = numeric(),
                          "LAT_DEGREES" = numeric(), "LON_DEGREES" = numeric(),
                          "SPECIES_NAME" = character(),
                          "COVER" = numeric())

# template for Mobile species data ----
templateMOB <- data.frame("YEAR" = numeric(), "MONTH" = numeric(), "DAY" = numeric(),
                          "REGION" = character(), "SUB_REGION" = character(),
                          "SITE" = character(),
                          "LOCATION" = character(), "HABITAT" = character(),
                          "PRIMARY_SAMPLE_UNIT" = character(),
                          "METERS_COMPLETED" = numeric(),
                          "LAT_DEGREES" = numeric(), "LON_DEGREES" = numeric(),
                          "SPECIES_NAME" = character(),
                          "COUNTS" = numeric())

# template for DEMO data ----
templateDEMO <- data.frame("YEAR" = numeric(), "MONTH" = numeric(), "DAY" = numeric(),
                           "REGION" = character(), "SUB_REGION" = character(),
                           "SITE" = character(),
                           "LOCATION" = character(), "HABITAT" = character(),
                           "PRIMARY_SAMPLE_UNIT" = character(),
                           "METERS_COMPLETED" = numeric(),
                           "LAT_DEGREES" = numeric(), "LON_DEGREES" = numeric(),
                           "SPECIES_NAME" = character(), "N_COLONIES" = numeric(),
                           "MAX_DIAMETER" = numeric(), "PERP_DIAMETER" = numeric(),
                           "HEIGHT" = numeric(), "OLD_MORT" = numeric(),
                           "RECENT_MORT" = numeric(), "BLEACH_CONDITION" = character(),
                           "DISEASE" = character()
                           )

# Function to find the values for each column category ----

f_CheckColumns <- function(C_names, type){
  selections <- if(type == "temporal"){
    temp1 <- temporalValues[temporalValues %in% C_names]
    ifelse(length(temp1) <= 3, C_names, temp1)
  } else if(type == "spatial"){
    temp1 <- spatialValues[spatialValues %in% C_names]
    ifelse(length(temp1) <= 5, C_names, temp1)
  } else if(type == "transect"){
    temp1 <- transectValues[transectValues %in% C_names]
    ifelse(length(temp1) < 1, C_names, temp1)
  } else if(type == "coordinates"){
    temp1 <- coordinateValues[coordinateValues %in% C_names]
    ifelse(length(temp1) < 2, C_names, temp1)
  } else if(type == "length"){
    temp1 <- lengthValues[lengthValues %in% C_names]
    ifelse(length(temp1) < 1, C_names, temp1)
  } else if(type == "names"){
    temp1 <- namesValues[namesValues %in% C_names]
    ifelse(length(temp1) < 1, C_names, temp1)
  } else if(type == "count"){
    temp1 <- countValues[countValues %in% C_names]
    ifelse(length(temp1) < 1, C_names, temp1)
  } else if(type == "biomass"){
    temp1 <- biomassValues[biomassValues %in% C_names]
    ifelse(length(temp1) < 1, C_names, temp1)
  }

  selections <- if(length(selections) <= 2){
    C_names
  } else {
    selections
  }

  return(selections)
}

# Function to conform data subsets that include pieces of BCG data, according to the prefix in its name ----
indexPrefixes <- function(data, selected, prefix){
  temp <- colnames(data)
  ind <- which(stringr::str_detect(temp, prefix))
  ST1 <- ind[1]
  ST2 <- ind[length(ind)]

  cropData <- data |>
    dplyr::select(any_of(selected),
                  as.numeric(ST1:ST2))
  temp <- colnames(cropData)
  ind <- which(stringr::str_detect(temp, prefix))
  ST1 <- ind[1]
  ST2 <- ind[length(ind)]

  result <- cropData |>
    tidyr::pivot_longer(cols = as.numeric(ST1:ST2),
                        names_to = "SPECIES_NAME",
                        values_to = prefix,
                        names_prefix = prefix) |>
    dplyr::mutate(SPECIES_NAME = stringr::str_replace_all(SPECIES_NAME, "([.])", " "),
                  SPECIES_NAME = stringr::str_replace_all(SPECIES_NAME, "([_])", ""),
                  SPECIES_NAME = stringr::str_to_sentence(SPECIES_NAME))

  return(result)
}

# Fish masterlist
FishMasterList <- read.csv("./www/FishMasterList.csv", na.strings="NA")
