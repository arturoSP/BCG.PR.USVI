# Functions to calculate BCG Level

# Model for Fish ----
## Getting results of evaluating each rule on different sites/years ----
M_Fish_1 <- function(mData, colTransect, colSite, colYear){
  # Create a tibble to store the results
  M_Result <- tibble("Site" = character(), "Year" = double(), 
                     "Model" = character(), "Metric_name" = character(), 
                     "MCalc" = double(), "Numeric_rule" = character(),
                     "Symbol" = character(), "Lower" = integer(), 
                     "Upper" = integer(), "Level" = integer(), 
                     "Description" = character(), "Unit" = character(),
                     "MemberValue" = double(), "Membership" = double())
  
  withProgress(
    message = "Please wait...",
    detail = "this analysis may take a few minutes.",
    {
      # Order the information with the formatter function
      # Samp <- MetricData |> 
      #   F_tSamp() |> 
      #   ungroup() 
      Samp <- F_tSamp2(mData) |>
        ungroup()
      
      # Order the samples with an ID value
      Samp <- Samp |> 
        mutate(CVE = paste(Samp[[colSite]], Samp[[colYear]], sep = "___"))
      numCVE <- unique(Samp$CVE)
      
      # Test all the rules for the different combinations of site and year
      for(i in numCVE){
        tSamp <- Samp[Samp$CVE == i,]
        
        # test all the rules, then bind together the results
        T_Taxa <- Fish_T_Taxa(tSamp, colSite, colYear)
        T_RareEndemic <- Fish_T_RareEndemic(tSamp, colSite, colYear)
        T_HSensitive <- Fish_T_HSensitive(tSamp, colSite, colYear)
        P_Sensitive <- Fish_P_Sensitive(tSamp, colSite, colYear)
        T_Biomass <- Fish_T_Biomass(tSamp, colSite, colYear)
        T_LGroupers <- Fish_T_LGroupers(tSamp, colSite, colYear)
        T_LPredators <- Fish_T_LPredators(tSamp, colSite, colYear)
        T_Piscivore <- Fish_T_Piscivore(tSamp, colSite, colYear)
        T_Sensitive <- Fish_T_Sensitive(tSamp, colSite, colYear)
        T_Parrotfish <- Fish_T_Parrotfish(tSamp, colSite, colYear)
        T_Damselfish <- Fish_P_Damselfish(tSamp, colSite, colYear)
        T_Groupers <- Fish_T_Groupers(tSamp, colSite, colYear)
        
        testMatrix <- bind_rows(T_Taxa, T_RareEndemic, T_HSensitive,
                                P_Sensitive, T_Biomass, T_LGroupers,
                                T_LPredators, T_Piscivore, T_Sensitive,
                                T_Parrotfish, T_Damselfish, T_Groupers) |> 
          replace_na(list(MCalc = 0,
                          MemberValue = 0,
                          Membership = 0,
                          Site = unique(tSamp[[colSite]]),
                          Year = unique(tSamp[[colYear]]))) |> 
          mutate(Membership = as.numeric(Membership)) |> 
          arrange(Level, desc(Membership), desc(MemberValue))
        testMatrix$Site <- as.character(testMatrix$Site)
        
        M_Result <- bind_rows(M_Result, testMatrix)
        
        incProgress(1 / length(numCVE))
      }
  }
  )
  
  return(M_Result)
}

## Summarise results into levels ----
M_Fish_2 <- function(M_Result, Samp, colSite, colYear){
  tLevel <- tibble("Site" = character(), "Year" = character(), 
                   "Level" = integer())
  
  # get the habitat for each combination of site and timeframe
  habitat <- Samp |> 
    mutate(CVE = paste(Samp[[colSite]], Samp[[colYear]], sep = "___")) |> 
    group_by(CVE) |> 
    summarise(Habitat = nth(HABITAT, 1))
  
  # form the unique IDs and add the habitat for each site/time combination
  M_Result <- M_Result |> 
    transmute(CVE = paste(M_Result$Site, M_Result$Year, sep = "___"), Level, Membership) |> 
    left_join(habitat, by = "CVE")

  ## tests for each level, looking for the value at the nth position
  L2 <- M_Result |> 
    filter(Level == 2) |> 
    arrange(CVE, desc(Membership)) |> 
    group_by(CVE, Level) |> 
    summarise(Membership = nth(Membership, 8))
  
  L3 <- tribble(~CVE, ~Level, ~Membership)
  for(i in unique(M_Result$CVE)){
    t0 <- if(habitat[habitat$CVE == i,2] == "reef" |
             habitat[habitat$CVE == i,2] == "REEF" |
             habitat[habitat$CVE == i,2] == "AGRF" |
             habitat[habitat$CVE == i,2] == "agrf"){
        M_Result[M_Result$CVE == i & M_Result$Level == 3,] |> 
          arrange(CVE, desc(Membership)) |> 
          group_by(CVE, Level) |> 
          summarise(Membership = nth(Membership, 6))
      } else{
        M_Result[M_Result$CVE == i & M_Result$Level == 3,] |> 
          arrange(CVE, desc(Membership)) |> 
          group_by(CVE, Level) |> 
          summarise(Membership = nth(Membership, 5))
      }
    L3 <- bind_rows(L3, t0)
  }

  L4 <- M_Result |>
    filter(Level == 4) |>
    arrange(CVE, desc(Membership)) |>
    group_by(CVE, Level) |>
    summarise(Membership = nth(Membership, 3))
  
  L5 <- M_Result |>
    filter(Level == 5) |>
    arrange(CVE, desc(Membership)) |>
    group_by(CVE, Level) |>
    summarise(Membership = nth(Membership, 2))
  
  # binds together the results of testing and then look for a 1 to assign a level
  Def_1 <- bind_rows(L2, L3, L4, L5) |> 
    mutate(Level = paste0("L", Level, ".Sub")) |> 
    pivot_wider(names_from = "Level", values_from = "Membership", values_fn = max)
  
  Def_Result <- Def_1 %>% 
    ungroup() %>%
    transmute(CVE,
              L1.Sub = 0,
              L2.Sub,
              L3.Sub,
              L4.Sub,
              L5.Sub,
              L6.Sub = 1) %>%
    mutate(L1 = L1.Sub) %>%
    mutate(L2 = apply(.[,c("L1", "L2.Sub")], 1,
                      function(x) min(round(1 - x[1], 
                                            8),
                                      x[2],
                                      na.rm = T))) %>%
    mutate(L3 = apply(.[,c("L1", "L2", "L3.Sub")], 1,
                      function(x) min(round(1 - sum(x[1], 
                                                    x[2]), 
                                            8),
                                      x[3],
                                      na.rm = T))) %>%
    mutate(L4 = apply(.[,c("L1", "L2", "L3", "L4.Sub")], 1,
                      function(x) min(round(1 - sum(x[1],
                                                    x[2],
                                                    x[3]),
                                            8),
                                      x[4],
                                      na.rm = T))) %>%
    mutate(L5 = apply(.[,c("L1", "L2", "L3", "L4", "L5.Sub")], 1,
                      function(x) min(round(1 - sum(x[1],
                                                    x[2],
                                                    x[3],
                                                    x[4]),
                                            8),
                                      x[5],
                                      na.rm = T))) %>%
    mutate(L6 = apply(.[,c("L1", "L2", "L3", "L4", "L5", "L6.Sub")], 1,
                      function(x) min(round(1 - sum(x[1],
                                                    x[2],
                                                    x[3],
                                                    x[4],
                                                    x[5]),
                                            8),
                                      x[6],
                                      na.rm = T))) %>%
    mutate(Lev.1.Memb = apply(.[,c("L1", "L2", "L3", "L4", "L5", "L6")], 1,
                              max,
                              na.rm = T)) %>%
    mutate(Lev.1.Name = apply(.[,c("L1", "L2", "L3", "L4", "L5", "L6", "Lev.1.Memb")], 1,
                              function(x) match(x[7], x[1:6]))) %>%
    mutate(Lev.2.Memb = apply(.[,c("L1", "L2", "L3", "L4", "L5", "L6", "Lev.1.Name")], 1,
                              function(x) max(x[1:6][-x[7]], na.rm = T))) %>%
    mutate(Lev.2.Memb = ifelse(Lev.1.Memb == 1, 0, Lev.2.Memb)) %>%
    mutate(Lev.2.Name = apply(.[,c("L1", "L2", "L3", "L4", "L5", "L6", "Lev.2.Memb")], 1,
                              function(x) match(x[7], x[1:6]))) %>%
    mutate(Lev.1.Name = ifelse(Lev.1.Memb != 0, Lev.1.Name, NA),
           Lev.2.Name = ifelse(Lev.2.Memb != 0, Lev.2.Name, NA)) %>%
    mutate(Lev.2.Name = ifelse(Lev.2.Memb == 0.5, 
                               apply(.[,c("L1", "L2", "L3", "L4", "L5", "L6")], 1,
                                     function(x) which(x[1:6] == 0.5)[2]), 
                               Lev.2.Name)) %>%
    mutate(Diff = Lev.1.Memb - Lev.2.Memb,
           close = ifelse(Diff < 0.1, 
                          "tie", 
                          ifelse(Diff < 0.2,
                                 "yes",
                                 NA))) %>%
    mutate(Lev.Prop.Num = apply(t((1:6)*t(.[,c("L1", "L2", "L3", "L4", "L5", "L6")]))
                                , 1
                                , FUN = sum),
           Lev.Prop.Num.Int = round(Lev.Prop.Num, 0),
           Lev.Prop.Num.Rem = Lev.Prop.Num.Int - Lev.Prop.Num,
           Lev.Prop.Num.Sign = sign(Lev.Prop.Num.Rem),
           Lev.Prop.Num.Sign.Nar = ifelse(Lev.Prop.Num.Sign == -1, "-", ifelse(Lev.Prop.Num.Sign == 1, "+", "")),
           Lev.Prop.Nar = paste0(Lev.Prop.Num.Int, Lev.Prop.Num.Sign.Nar),
           Lev.Prop.Nar.Tie = ifelse(close == "tie",
                                     paste0(Lev.1.Name, "/", Lev.2.Name, " tie"),
                                     NA),
           Lev.Prop.Nar = ifelse(!is.na(Lev.Prop.Nar.Tie), Lev.Prop.Nar.Tie, Lev.Prop.Nar)
    ) 
  
  # assemble the results table
  tLevel <- Def_Result |> 
    transmute(Site = str_extract(CVE, ".*(?=___)"),
              Year = str_extract(CVE, "(?<=___)[[:digit:]]*"),
              Level = Lev.Prop.Nar) 
  
  return(tLevel)
}

# Additional functions ----

## Add ID data to each test ----
SiteYear <- function(data, base, colSite, colYear){
  data |> 
    mutate(Site = unique(base[[colSite]]),
           Year = unique(base[[colYear]]))
}

## Negate %in% ----
`%nin%` = Negate(`%in%`)

# Functions for evaluating the rules for the model ----

## Total taxa ----
# Count number of different taxa in the site, then compare with the threshold
Fish_T_Taxa <- function(tSamp, colSite, colYear){
  T_Taxa <- tSamp |> 
    filter(BCGAttribute %nin% c("x-MNS", "x-NRF") &
             !is.na(BCGAttribute)) |>
    summarise(MCalc = length(unique(SCIENTIFIC_NAME))) |> 
    transmute(Metric_name = "T_Taxa", MCalc) |> 
    left_join(Rls) |> 
    mutate(MemberValue = (MCalc - Lower) / (Upper - Lower),
           Membership = ifelse(MemberValue >= 1, 1,
                               ifelse(MemberValue <= 0, 0, MemberValue))) |> 
    SiteYear(tSamp, colSite, colYear)
  
  return(T_Taxa)
}

## Rare, endemic and special species ----
# Count the number of species with Attribute I, then compare with the threshold
Fish_T_RareEndemic <- function(tSamp, colSite, colYear){
  T_RareEndemic <- tSamp |> 
    filter(BCGAttribute == "I") |>
    summarise(MCalc = length(unique(SCIENTIFIC_NAME))) |> 
    transmute(Metric_name = "T_RareEndemic", MCalc) |> 
    left_join(Rls) |> 
    mutate(MemberValue = MCalc,
           Membership = ifelse(MemberValue >= Lower, 1, 0)) |> 
    SiteYear(tSamp, colSite, colYear)
  
  return(T_RareEndemic)
}

## Highly sensitive taxa ----
# Count the number of species with attribute II, then make it valid if its 1 or more
Fish_T_HSensitive <- function(tSamp, colSite, colYear){
  T_HSensitive <- tSamp |> 
    filter(BCGAttribute == "II") |> 
    summarise(MCalc = n()) |> 
    transmute(Metric_name = "T_HSensitive", MCalc) |> 
    left_join(Rls) |> 
    mutate(MemberValue = (MCalc - Lower) / (Upper - Lower),
           Membership = ifelse(MemberValue >= 1, 1,
                               ifelse(MemberValue <= 0, 0, MemberValue))) |> 
    SiteYear(tSamp, colSite, colYear)
  
  return(T_HSensitive)
}

## Proportion of sensitive taxa (Attr I to III) ----
# Count the number of species, then count the number of species with attribute
# I to III, calculate the proportion and then compare it with the threshold
Fish_P_Sensitive <- function(tSamp, colSite, colYear){
  step1 <- tSamp |> 
    select(SCIENTIFIC_NAME, BCGAttribute) |> 
    mutate(Sensitive = ifelse(BCGAttribute %in% c("I", "II", "III"), TRUE, 
                              FALSE))
  TotSp <- step1 |> 
    summarise(C1 = length(unique(SCIENTIFIC_NAME))) |> 
    pull()
  
  PropSp <- step1 |> 
    filter(Sensitive == TRUE) |> 
    summarise(C2 = length(unique(SCIENTIFIC_NAME))) |> 
    pull()
  
  P_Sensitive <- tribble(~Metric_name, ~MCalc,
          "P_Sensitive", PropSp / TotSp * 100) |> 
    left_join(Rls) |> 
    mutate(MemberValue = (MCalc - Lower) / (Upper - Lower),
           Membership = ifelse(MemberValue >= 1, 1,
                               ifelse(MemberValue <= 0, 0, MemberValue))) |> 
  SiteYear(tSamp, colSite, colYear)
  
  rm(TotSp, PropSp, step1)
  
  return(P_Sensitive)
}

## Total biomass ----
# Sum for total biomass, divide by the transect area, then compare
# with threshold
Fish_T_Biomass <- function(tSamp, colSite, colYear){
  trArea <- ifelse(tSamp[1,"LOCATION"] == "PR", 100, 176.7146)
  
  T_Biomass <- tSamp |> 
    filter(BCGAttribute %nin% c("x-MNS", "x-NRF") &
             !is.na(BCGAttribute)) |> 
    summarise(MCalc = sum(TOTAL_BIOMASS, na.rm = TRUE) / trArea) |> 
    transmute(Metric_name = paste0("T_Biomass_",tSamp[1,"LOCATION"]), MCalc) |> 
    left_join(Rls) |> 
    mutate(MemberValue = (MCalc - Lower) / (Upper - Lower),
           Membership = ifelse(MemberValue >= 1, 1,
                               ifelse(MemberValue <= 0, 0, MemberValue))) |> 
    SiteYear(tSamp, colSite, colYear)
  
  return(T_Biomass)
}

## Large groupers ----
# Filter for genera Epinephelus and Mycteroperca, sum to get the number of 
# individuals and then compare with threshold.
Fish_T_LGroupers <- function(tSamp, colSite, colYear){
  T_LGroupers <- tSamp |>
    filter(str_detect(SCIENTIFIC_NAME, "Epinephelus") |
             str_detect(SCIENTIFIC_NAME, "Mycteroperca")) |> 
    summarise(MCalc = sum(TOTAL_NUMBER)) |> 
    transmute(Metric_name = "T_LGroupers", MCalc) |> 
    left_join(Rls) |> 
    mutate(MemberValue = (MCalc - Lower) / (Upper - Lower),
           Membership = ifelse(MemberValue >= 1, 1,
                               ifelse(MemberValue <= 0, 0, MemberValue))) |> 
    SiteYear(tSamp, colSite, colYear)
  
  return(T_LGroupers)  
}

## Large predators ----
# Filter for large piscivores, then sum the total of individuals and compare
# with threshold
Fish_T_LPredators <- function(tSamp, colSite, colYear){
  T_LPredators <- tSamp |>
    filter(PiscivoreSize == "LP") |>
    summarise(MCalc = sum(TOTAL_NUMBER)) |> 
    transmute(Metric_name = "T_LPredators", MCalc) |> 
    left_join(Rls) |> 
    mutate(MemberValue = (MCalc - Lower) / (Upper - Lower),
           Membership = ifelse(MemberValue >= 1, 1,
                               ifelse(MemberValue <= 0, 0, MemberValue))) |> 
    SiteYear(tSamp, colSite, colYear)
  
  return(T_LPredators)
}

## Piscivores ----
# Filter for Guild of Piscivores, then sum the number of individuals and then
# compare with threshold
Fish_T_Piscivore <- function(tSamp, colSite, colYear){
  T_Piscivore <- tSamp |> 
    filter(Guild == "P") |> 
    summarise(MCalc = sum(TOTAL_NUMBER)) |> 
    transmute(Metric_name = "T_Piscivore", MCalc) |> 
    left_join(Rls) |> 
    mutate(MemberValue = MCalc,
           Membership = ifelse(MemberValue >= Lower, 1, 0)) |> 
    SiteYear(tSamp, colSite, colYear)
  
  return(T_Piscivore)
}

## Total sensitive taxa (Attr I to III) ----
# Filter for attributes I to III, count the number of species adn then compare
# with the threshold
Fish_T_Sensitive <- function(tSamp, colSite, colYear){
  T_Sensitive <- tSamp |> 
    filter(BCGAttribute %in% c("I", "II", "III")) |> 
    summarise(MCalc = length(unique(SCIENTIFIC_NAME))) |> 
    transmute(Metric_name = "T_Sensitive", MCalc) |> 
    left_join(Rls) |> 
    mutate(MemberValue = (MCalc - Lower) / (Upper - Lower),
           Membership = ifelse(MemberValue >= 1, 1,
                               ifelse(MemberValue <= 0, 0, MemberValue))) |> 
    SiteYear(tSamp, colSite, colYear)
  
  return(T_Sensitive)
}

## Parrotfish ----
# Find the genus "Scarus" and filter for them, then sum for the number of 
# individuals and compare with the threshold
Fish_T_Parrotfish <- function(tSamp, colSite, colYear){
  T_Parrotfish <- tSamp |> 
    filter(str_detect(SCIENTIFIC_NAME, "Scarus") |
             str_detect(SCIENTIFIC_NAME, "Chlorurus") |
             str_detect(SCIENTIFIC_NAME, "Bolbometopon") |
             str_detect(SCIENTIFIC_NAME, "Cetoscarus") |
             str_detect(SCIENTIFIC_NAME, "Hipposcarus") |
             str_detect(SCIENTIFIC_NAME, "Sparisoma") |
             str_detect(SCIENTIFIC_NAME, "Leptoscarus") |
             str_detect(SCIENTIFIC_NAME, "Cryptotomus") |
             str_detect(SCIENTIFIC_NAME, "Calotomus") |
             str_detect(SCIENTIFIC_NAME, "Nicholsina")) |> 
    summarise(MCalc = sum(TOTAL_NUMBER)) |> 
    transmute(Metric_name = "T_Parrotfish", MCalc) |> 
    left_join(Rls) |> 
    mutate(MemberValue = (MCalc - Lower) / (Upper - Lower),
           Membership = ifelse(MemberValue >= 1, 1,
                               ifelse(MemberValue <= 0, 0, MemberValue))) |> 
    SiteYear(tSamp, colSite, colYear)
  
  return(T_Parrotfish)
}

## Damselfish ----
# Compute total of individuals in site. Find the genera Stegastes and 
# Microspathodon, sum for the number of individuals, divide this number by 
# the total of individuals and then compare with threshold
Fish_P_Damselfish <- function(tSamp, colSite, colYear){
  TotInd <- tSamp |> 
    summarise(sum(TOTAL_NUMBER)) |> 
    pull()
  
  P_Damselfish <- tSamp |> 
    filter(str_detect(SCIENTIFIC_NAME, "Stegastes") |
             str_detect(SCIENTIFIC_NAME, "Microspathodon") |
             str_detect(SCIENTIFIC_NAME, "Abudefduf") |
             str_detect(SCIENTIFIC_NAME, "Chromis")) |> 
    summarise(MCalc = sum(TOTAL_NUMBER) / TotInd * 100) |> 
    transmute(Metric_name = "P_Damselfish", MCalc) |> 
    left_join(Rls) |> 
    mutate(MemberValue = (MCalc - Lower) / (Upper - Lower),
           Membership = ifelse(MemberValue < 0, 1,
                               ifelse(MemberValue > 1, 0, MemberValue))) |> 
    SiteYear(tSamp, colSite, colYear)
  
  rm(TotInd)
  
  return(P_Damselfish)
    
}

## Groupers ----
# filter for the 4 grouper genera, sum the number of individuals,
# then compare with the lower limit
Fish_T_Groupers <- function(tSamp, colSite, colYear){
  T_Groupers <- tSamp |>
    filter(str_detect(SCIENTIFIC_NAME, "Epinephelus") |
             str_detect(SCIENTIFIC_NAME, "Mycteroperca") |
             str_detect(SCIENTIFIC_NAME, "Dermatolepis") |
             str_detect(SCIENTIFIC_NAME, "Cephalopholis")) |> 
    summarise(MCalc = sum(TOTAL_NUMBER)) |> 
    transmute(Metric_name = "T_Groupers", MCalc) |> 
    left_join(Rls) |> 
    mutate(MemberValue = MCalc,
           Membership = ifelse(MemberValue >= 1, 1,
                               ifelse(MemberValue <= 0, 0, MemberValue))) |> 
    SiteYear(tSamp, colSite, colYear)
  
  return(T_Groupers)
}
