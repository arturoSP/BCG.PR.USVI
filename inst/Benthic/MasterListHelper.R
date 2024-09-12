# carga los paquetes necesarios
if(!require(worrms)) install.packages("worrms")
if(!require(dplyr)) install.packages("dplyr")
if(!require(stringr)) install.packages("stringr")
if(!require(rredlist)) install.packages("rredlist")

MasterList <- rio::import("./R/2019CoralMasterList.csv") %>%
  select(FinalID, ReefBuildingSpecies, `BCG Attribute`) %>% 
  transmute(FinalID = str_to_sentence(FinalID), 
            ReefBuildingSpecies = ifelse(ReefBuildingSpecies == "X", T, F),
            BCGAttr = `BCG Attribute`)

findSpp <- function(name){
  # helper para interpretación de datos de Red List ----
  interpretRLResult <- function(RLRes){
    RL <- as.data.frame(matrix(nrow = 1, ncol = 3))
    RLSciName <- RLRes$name
    RLRes <- RLRes$result
    if(length(RLRes) > 0){
      RL[,1] <- RLSciName
      RL[,2] <- RLRes$main_common_name
      RL[,3] <- RLRes$category
      RL[,3] <- factor(RL[,3],
                       levels = c("NE", "DD",
                                  "LC", "NT",
                                  "VU", "EN",
                                  "CR", "EW",
                                  "EX", ""),
                       labels = c("Not Evaluated", "Data Deficient",
                                  "Least Concern", "Near Threatened",
                                  "Vulnerable", "Endangered", 
                                  "Critically Endangered", "Extinct in the Wild",
                                  "Extinct", ""))
      RL[,3] <- as.character(RL[,3])
    } else {
      RL[,1] <- RLSciName
      RL[,2] <- ""
      RL[,3] <- ""
    }
    return(RL)
  }
  
  # main function ----
  # busca el nombre en la lista de WoRMS y asigna una bandera eFlag de encontrado/no encontrado
  tempRes <- tryCatch(list(final = wm_records_name(name), 
                           eFlag = FALSE),
                      error = function(e){
                        list(final = tribble(~FinalID, ~order, ~family, ~CommonEnglish, ~Type, ~OrbicellaAcropora,
                                             name, "-", "-", "Not found", "-", "-"), 
                             eFlag = TRUE)
                      }
  )
  # en caso de encontrar el dato pasa a completar la información
  if(tempRes$eFlag == FALSE){
    # se guarda la información de WoRMS
    tempRes <- tempRes$final
    result <- data.frame(FinalID = tempRes[tempRes$status == "accepted", "scientificname"],
                         WORMSID = tempRes[tempRes$status == "accepted", "AphiaID"],
                         phylum = tempRes[tempRes$status == "accepted", "phylum"],
                         order = tempRes[tempRes$status == "accepted", "order"],
                         family = tempRes[tempRes$status == "accepted", "family"])
    colnames(result) <- c("FinalID", "AphiaID", "phylum", "order", "family")
    if(dim(result)[1] == 0){
      result <- data.frame(FinalID = tempRes[,"valid_name"],
                           AphiaID = tempRes[, "valid_AphiaID"],
                           phylum = tempRes[, "phylum"],
                           order = tempRes[, "order"],
                           family = tempRes[, "family"])
      colnames(result) <- c("FinalID", "AphiaID", "phylum", "order", "family")
    }
    # se usan condicionales para determinar tipos y pertenencia a Orbicella o Acropora, 
    # esto se puede crecer a conveniencia si se tienen más datos
    result <- result |> 
      dplyr::mutate(Type = ifelse(result$phylum == "Porifera", "Non-Coral",
                                  ifelse(result$phylum == "Rhodophyta", "Algae",
                                         ifelse(result$order == "Scleractinia", "Scleractinian", "")
                                  )
      )
      ) |>
      dplyr::mutate(OrbicellaAcropora = ifelse(stringr::str_detect(FinalID, "Orbicella") | stringr::str_detect(FinalID, "Acropora"), 
                                               "OrbAcrp", "")) 
    
    # se busca el nombre común en inglés a partir de los datos de WoRMS
    EngName <- as.data.frame(matrix(nrow = nrow(result), ncol = 2))
    colnames(EngName) <- c("AphiaID", "CommonEnglish")
    EngName$AphiaID <- result$AphiaID
    
    for(i in EngName$AphiaID){
      tempName = tryCatch(worrms::wm_common_id(i),
                          error = function(e){
                            tribble(~vernacular, ~language_code, ~language,
                                    "", "eng", "")
                          })
      
      # se especifica la búsqueda de códigos en inglés
      tempName <- tempName |> 
        dplyr::filter(language_code == "eng") |> 
        pull(vernacular)
      if(length(tempName) == 0){
        EngName[EngName$AphiaID == i, 2] <- ""
      } else {
        # en caso de haber más d eun nombre, estos se ponen en fila separados por comas
        EngName[EngName$AphiaID == i, 2] <- stringr::str_flatten_comma(tempName)
      }
      
    }
    
    EngName <- EngName |> 
      mutate(CommonEnglish = stringr::str_to_sentence(CommonEnglish))
    
    # se juntan los datos de tipos con el nombre común
    result <- dplyr::left_join(result, EngName, by = "AphiaID")
    
    result <- result |> 
      transmute(FinalID,
                order,
                family,
                CommonEnglish,
                Type,
                OrbicellaAcropora)
    
  } else {
    result <- tempRes$final
  }
  
  # se buscan datos de red list
  RLToken = '35c2054e6e9bc552f31d7b838f598336ea1c07b43460a6311f60aa89253c0c25'
  
  RLResult <- lapply(result$FinalID, function(z) {
    Sys.sleep(2) # hay que dar 2 segundos de descanso entre búsqueda y búsqueda
    rl_search(name = z, key = RLToken)
  })
  
  # Se guardan los resultados
  RL <- as.data.frame(matrix(unlist(lapply(RLResult, interpretRLResult)), 
                             ncol = 3, byrow = T))
  colnames(RL) <- c("scientificname", "RLMainCommonName", "RedListCategory")
  
  # se hace la unión entre resultados de WoRMS y RedList
  result <- dplyr::left_join(result, RL, by = c("FinalID" = "scientificname"))
  result <- dplyr::left_join(result, MasterList, by = "FinalID")
  result[,"ReefBuildingSpecies"] <- ifelse(result[,"ReefBuildingSpecies"] == TRUE, "X", "")
  result <- result |> 
    select(FinalID, ReefBuildingSpecies, CommonEnglish, Type,
           OrbicellaAcropora, BCGAttr, RedListCategory, RLMainCommonName, order, family)
  
  return(result)
}
