# Functions to calculate the BCG level membership

# BCG model (full) Part 1 ----
M_BCG_1 <- function(Samp, colTransect, colSite, colYear){
  M_Result <- tibble("Site" = character(), "Year" = double(), "Model" = character(),
                     "Metric_name" = character(), "MCalc" = double(), "Numeric_rule" = character(),
                     "Symbol" = character(), "Lower" = integer(), "Upper" = integer(),
                     "Level" = integer(), "Description" = character(), "Units" = character(),
                     "MemberValue" = double(), "Membership" = double())
  Samp <- Samp %>% mutate(CVE = paste(Samp[[colSite]], Samp[[colYear]]))
  numSite <- unique(Samp[[colSite]])
  numYear <- unique(Samp[[colYear]])
  numCVE <- unique(Samp$CVE)

  withProgress(
    message = "Please wait...",
    detail = "this analysis may take a few minutes.",
    {
      for (i in numCVE) {
        tSamp <- Samp %>%
          filter(Samp$CVE == i)

        #tests all the rules ant then binds together the results
        P_cc_lpi <- BCG_P_cc_lpi(tSamp, colTransect) %>%
          mutate(Site = unique(tSamp[[colSite]]),
                 Year = unique(tSamp[[colYear]]))
        P_cm_demo <- BCG_P_cm_demo(tSamp) %>%
          mutate(Site = unique(tSamp[[colSite]]),
                 Year = unique(tSamp[[colYear]]))
        P_lcc_demo <- BCG_P_lcc_demo(tSamp) %>%
          mutate(Site = unique(tSamp[[colSite]]),
                 Year = unique(tSamp[[colYear]]))
        P_lcrbc_demo <- BCG_P_lcrbc_demo(tSamp) %>%
          mutate(Site = unique(tSamp[[colSite]]),
                 Year = unique(tSamp[[colYear]]))
        P_bts_lpi <- BCG_P_bts_lpi(tSamp, colTransect) %>%
          mutate(Site = unique(tSamp[[colSite]]),
                 Year = unique(tSamp[[colYear]]))
        T_cr_lpi <- BCG_T_cr_lpi(tSamp) %>%
          mutate(Site = unique(tSamp[[colSite]]),
                 Year = unique(tSamp[[colYear]]))
        T_cratt1234 <- BCG_T_cratt1234(tSamp) %>%
          mutate(Site = unique(tSamp[[colSite]]),
                 Year = unique(tSamp[[colYear]]))
        T_cratt1234_lpi <- BCG_T_cratt1234_lpi(tSamp) %>%
          mutate(Site = unique(tSamp[[colSite]]),
                 Year = unique(tSamp[[colYear]]))
        P_oc_demo <- BCG_P_oc_demo(tSamp) %>%
          mutate(Site = unique(tSamp[[colSite]]),
                 Year = unique(tSamp[[colYear]]))
        P_cratt1234_lpi <- BCG_P_cratt1234_lpi(tSamp, colTransect) %>%
          mutate(Site = unique(tSamp[[colSite]]),
                 Year = unique(tSamp[[colYear]]))
        P_oc_lpi <- BCG_P_oc_lpi(tSamp, colTransect) %>%
          mutate(Site = unique(tSamp[[colSite]]),
                 Year = unique(tSamp[[colYear]]))
        Sa_lcc_demo <- BCG_Sa_lcc_demo(tSamp) %>%
          mutate(Site = unique(tSamp[[colSite]]),
                 Year = unique(tSamp[[colYear]]))
        T_mlcol_demo <- BCG_T_mlcol_demo(tSamp) %>%
          mutate(Site = unique(tSamp[[colSite]]),
                 Year = unique(tSamp[[colYear]]))
        T_col_demo <- BCG_T_col_demo(tSamp) %>%
          mutate(Site = unique(tSamp[[colSite]]),
                 Year = unique(tSamp[[colYear]]))

        testA <- bind_rows(P_cc_lpi, P_cm_demo, P_lcc_demo, P_lcrbc_demo,
                           P_bts_lpi, T_cr_lpi, T_cratt1234_lpi, T_cratt1234, P_oc_demo,
                           P_cratt1234_lpi, P_oc_lpi, Sa_lcc_demo, T_mlcol_demo,
                           T_col_demo)
        testA <- rules %>%
          filter(Model == "BCG") %>%
          left_join(testA) %>%
          replace_na(list(MCalc = 0,
                          MemberValue = 0,
                          Membership = 0,
                          Site = unique(tSamp[[colSite]]),
                          Year = unique(tSamp[[colYear]]))) %>%
          mutate(Membership = as.numeric(Membership)) %>%
          arrange(Level, desc(Membership), desc(MemberValue))

        M_Result <- bind_rows(M_Result, testA)

        incProgress(1 / length(numCVE))
      }
    })

  return(M_Result)
}

# BCG Model (full) Part 2 ----
M_BCG_2 <- function(M_Result){
  tLevel <- tibble("Site" = character(), "Year" = character(), "Level" = integer())
  M_Result <- M_Result %>% mutate(CVE = paste(M_Result$Site, M_Result$Year, sep = "___"))
  numSite <- unique(M_Result$Site)
  numYear <- unique(M_Result$Year)
  numCVE <- unique(M_Result$CVE)

  ## tests for each level, looking for the value at the nth position
  L2 <- M_Result %>%
    filter(Level == 2) %>%
    arrange(CVE, desc(Membership)) %>%
    group_by(CVE, Level) %>%
    summarise(Membership = nth(Membership, 4))

  L3a <- M_Result %>%
    filter(Level == 3 & Metric_name != "p_oc_demo") %>%
    arrange(CVE, desc(Membership)) %>%
    group_by(CVE, Level) %>%
    summarise(Membership = nth(Membership, 4))

  L3b <- M_Result %>%
    filter(Level == 3 & Metric_name == "p_oc_demo") %>%
    group_by(CVE, Level) %>%
    summarise(Membership)

  L4 <- M_Result %>%
    filter(Level == 4) %>%
    arrange(CVE, desc(Membership)) %>%
    group_by(CVE, Level) %>%
    summarise(Membership = nth(Membership, 3))

  L5 <- M_Result %>%
    filter(Level == 5) %>%
    arrange(CVE, desc(Membership)) %>%
    group_by(CVE, Level) %>%
    summarise(Membership = nth(Membership, 2))

  # binds together the results of testing and then look for a 1 to assign a level
  Def_1 <- bind_rows(L2, L3a, L3b, L4, L5) %>%
    mutate(Level = paste0("L", Level, ".Sub")) %>%
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
  tLevel <- Def_Result %>%
    transmute(Site = str_extract(CVE, ".*(?=___)"),
              Year = str_extract(CVE, "(?<=___)[[:digit:]]*"),
              Level = Lev.Prop.Nar) #%>%

  return(tLevel)
}


## percent of coral cover ----
BCG_P_cc_lpi <- function(Samp, colTransect){
  P_cc_lpi <- Samp %>%
    filter(Type == "Scleractinian") %>%
    mutate(Metric_name = "p_cc_lpi")

  if(nrow(P_cc_lpi) == 0){
    # Create a dummy data frame
    dummy <- data.frame(
      Model = unique(Samp$Model),
      Metric_name = "p_cc_lpi",
      TotalPoints = 0,
      PRIMARY_SAMPLE_UNIT = unique(Samp[[colTransect]])
    )
    # Bind the dummy data to P_oac_lpi
    P_cc_lpi <- bind_rows(P_cc_lpi, dummy)
  }

  P_cc_lpi <- P_cc_lpi %>%
    group_by(Model, Metric_name) %>%
    summarise(MCalc = sum(TotalPoints, na.rm = T) / length(unique(Samp[[colTransect]]))) %>%
    left_join(rules) %>%
    mutate(MemberValue = (MCalc - Lower) / (Upper - Lower),
           Membership = ifelse(MemberValue >= 1, 1,
                               ifelse(MemberValue <= 0, 0, MemberValue)))
  return(P_cc_lpi)
}

## percent of coral mortality ----
BCG_P_cm_demo <- function(Samp){
  P_cm_demo <- Samp %>%
    filter(Type == "Scleractinian") %>%
    mutate(CSA = pi * ((HEIGHT + (MAX_DIAMETER / 2)) / 2) ^ 2 * MorphologyIndex,
           Mort = CSA * ((OLD_MORT + RECENT_MORT) / 100),
           TotalPoints = (N_COLONIES * Mort) / (METERS_COMPLETED * 10000) * 100,
           Metric_name = "p_cm_demo") %>%
    group_by(Model, Metric_name) %>%
    summarise(MCalc = sum(TotalPoints, na.rm = T))

  if(nrow(P_cm_demo) == 0){
    P_cm_demo[1,1] <- "BCG"
    P_cm_demo[1,2] <- "p_cm_demo"
    P_cm_demo[1,3] <- 0
  }

  P_cm_demo <- P_cm_demo %>%
    left_join(rules) %>%
    mutate(MemberValue = (MCalc - Lower) / (Upper - Lower),
           Membership = ifelse(MemberValue >= 1, 0,
                               ifelse(MemberValue <= 0, 1, 1 - MemberValue)))
  return(P_cm_demo)
}

## percent live coral cover ----
BCG_P_lcc_demo <- function(Samp){
  P_lcc_demo <- Samp %>%
    filter(Type == "Scleractinian") %>%
    mutate(CSA_2D = pi * (2 * ((HEIGHT + (MAX_DIAMETER / 2)) / 2) / 2) ^ 2,
     # according to the formula CSA_2D = pi[2r(cm)/2]² (m²)
     # r = [height + (diameter / 2)] / 2
           LCSA_2D = CSA_2D * ((100 - (OLD_MORT + RECENT_MORT)) / 100),
     # and LCSA_2D = CSA_2D * (%LivingTissue) (m²)
            # %LivingTissue = (100 - (OldMortality + RecentMortality)) / 100
           TotalPoints = (N_COLONIES * LCSA_2D) / (METERS_COMPLETED * 10000) * 100,
     # divide LCSA by the cm² in the transect, then *100 to make it %
     # get cm² in the transect by mutliplying by 100 * 100
           Metric_name = "p_lcc_demo") %>%
    group_by(Model, Metric_name) %>%
    summarise(MCalc = sum(TotalPoints, na.rm = T))

  if(nrow(P_lcc_demo) == 0){
    P_lcc_demo[1,1] <- "BCG"
    P_lcc_demo[1,2] <- "p_lcc_demo"
    P_lcc_demo[1,3] <- 0
  }

  P_lcc_demo <- P_lcc_demo %>%
    left_join(rules) %>%
    mutate(MemberValue = (MCalc - Lower) / (Upper - Lower),
           Membership = ifelse(MemberValue >= 1, 1,
                               ifelse(MemberValue <= 0, 0, MemberValue)))
  return(P_lcc_demo)
}

## percent live cover of large, reef-building coral species ----
BCG_P_lcrbc_demo <- function(Samp){
  P_lcrbc_demo <- Samp %>%
    filter(ReefBuildingSpecies == T) %>%
    mutate(CSA = pi * ((HEIGHT + (MAX_DIAMETER / 2)) / 2) ^ 2 * MorphologyIndex,
           LCSA = CSA * ((100 - (OLD_MORT + RECENT_MORT)) / 100),
           TotalPoints = (N_COLONIES * LCSA) / (METERS_COMPLETED * 10000) * 100,
           Metric_name = "p_lcrbc_demo") %>%
    group_by(Model, Metric_name) %>%
    summarise(MCalc = sum(TotalPoints, na.rm = T))

  if(nrow(P_lcrbc_demo) == 0){
    P_lcrbc_demo[1,1] <- "BCG"
    P_lcrbc_demo[1,2] <- "p_lcrbc_demo"
    P_lcrbc_demo[1,3] <- 0
  }

  P_lcrbc_demo <- P_lcrbc_demo %>%
    left_join(rules) %>%
    mutate(MemberValue = (MCalc - Lower) / (Upper - Lower),
           Membership = ifelse(MemberValue >= 1, 1,
                               ifelse(MemberValue <= 0, 0, MemberValue)))
  return(P_lcrbc_demo)
}

## Percent of abiotic cover ----
BCG_P_bts_lpi <- function(Samp, colTransect){
  P_bts_lpi <- Samp %>%
    filter(Type == "Bare_TurfWSed") %>%
    mutate(Metric_name = "p_bts_lpi")

  # Check for cases when there is no bare substrate records.
  if(nrow(P_bts_lpi) == 0){
    # Create a dummy data frame with the necessary columns
    dummy <- data.frame(
      Model = unique(tSamp$Model),
      Metric_name = "p_bts_lpi",
      TotalPoints = 0,
      Type = "Bare_TurfWSed",
      PRIMARY_SAMPLE_UNIT = unique(Samp[[colTransect]])
    )
    # Bind the dummy data frame to P_bts_lpi
    P_bts_lpi <- bind_rows(P_bts_lpi, dummy)
  }

  P_bts_lpi <- P_bts_lpi %>%
    group_by(Model, Metric_name) %>%
    summarise(MCalc = sum(TotalPoints, na.rm = T) / length(unique(Samp[[colTransect]]))) %>%
    left_join(rules) %>%
    mutate(MemberValue = (MCalc - Lower) / (Upper - Lower),
           Membership = ifelse(MemberValue >= 1, 0,
                               ifelse(MemberValue <= 0, 1, 1 - MemberValue)))

  return(P_bts_lpi)
}

## Total coral richness ----
BCG_T_cr_lpi <- function(Samp){
  T_cr_lpi <- Samp %>%
    filter(Type == "Scleractinian") %>%
    filter(TotalPoints > 0) %>%
    mutate(Metric_name = "t_cr_lpi") %>%
    group_by(Model, Metric_name, SPECIES_NAME) %>%
    summarise(P1 = n()) %>%
    group_by(Model, Metric_name) %>%
    summarise(MCalc = n())

  if(nrow(T_cr_lpi) == 0){
    dummy <- data.frame(
      Model = "BCG",
      Metric_name = "t_cr_lpi",
      MCalc = 0
    )
    T_cr_lpi <- bind_rows(T_cr_lpi, dummy)
  }

  T_cr_lpi <- T_cr_lpi %>%
    left_join(rules) %>%
    mutate(MemberValue = (MCalc - Lower) / (Upper - Lower),
           Membership = ifelse(MemberValue >= 1, 1,
                               ifelse(MemberValue <= 0, 0, MemberValue)))
  return(T_cr_lpi)
}

## Coral richness for non-tolerant taxa LPI ----
BCG_T_cratt1234_lpi <- function(Samp){
  T_cratt1234_lpi <- Samp %>%
    filter(TotalPoints > 0) %>% # | N_COLONIES > 0) %>%
    filter(BCGAttr %in% c(1,2,3,4)) %>%
    mutate(Metric_name = "t_cratt1234_lpi") %>%
    group_by(Model, Metric_name, SPECIES_NAME) %>%
    summarise(P1 = n()) %>%
    group_by(Model, Metric_name) %>%
    summarise(MCalc = n())

  if(nrow(T_cratt1234_lpi) == 0){
    dummy <- data.frame(
      Model = "BCG",
      Metric_name = "t_cratt1234_lpi",
      MCalc = 0
    )
    T_cratt1234_lpi <- bind_rows(T_cratt1234_lpi, dummy)
  }

  T_cratt1234_lpi <- T_cratt1234_lpi  %>%
    left_join(rules) %>%
    mutate(MemberValue = (MCalc - Lower) / (Upper - Lower),
           Membership = ifelse(MemberValue >= 1, 1,
                               ifelse(MemberValue <= 0, 0, MemberValue)))
  return(T_cratt1234_lpi)
}

## Coral richness for non-tolerant taxa ----
BCG_T_cratt1234 <- function(Samp){
  T_cratt1234 <- Samp %>%
    filter(TotalPoints > 0 | N_COLONIES > 0) %>%
    filter(BCGAttr %in% c(1,2,3,4)) %>%
    mutate(Metric_name = "t_cratt1234")  %>%
    group_by(Model, Metric_name, SPECIES_NAME) %>%
    summarise(P1 = n()) %>%
    group_by(Model, Metric_name) %>%
    summarise(MCalc = n())

  if(nrow(T_cratt1234) == 0){
    dummy <- data.frame(
      Model = "BCG",
      Metric_name = "t_cratt1234",
      MCalc = 0
    )
    T_cratt1234 <- bind_rows(T_cratt1234, dummy)
  }

  T_cratt1234 <- T_cratt1234 %>%
    left_join(rules) %>%
    mutate(MemberValue = (MCalc - Lower) / (Upper - Lower),
           Membership = ifelse(MemberValue >= 1, 1,
                               ifelse(MemberValue <= 0, 0, MemberValue)))
  return(T_cratt1234)
}

## Percent of Orbicella cover ----
BCG_P_oc_lpi <- function(Samp, colTransect){
  P_oc_lpi <- Samp %>%
    mutate(taxa = str_extract(SPECIES_NAME, "Orbicella")) %>%
    filter(!is.na(taxa)) %>%
    mutate(Metric_name = "p_oc_lpi")

  if(nrow(P_oc_lpi) == 0){
    # Create dummy data frame
    dummy <- data.frame(
      Model = unique(Samp$Model),
      Metric_name = "p_oc_lpi",
      TotalPoints = 0,
      PRIMARY_SAMPLE_UNIT = unique(Samp[[colTransect]])
    )
    # Bind the dummy data to P_oc_lpi
    P_oc_lpi <- bind_rows(P_oc_lpi, dummy)
  }

  P_oc_lpi <- P_oc_lpi %>%
    group_by(Model, Metric_name) %>%
    summarise(MCalc = sum(TotalPoints, na.rm = T) / length(unique(Samp[[colTransect]]))) %>%
    left_join(rules) %>%
    mutate(MemberValue = (MCalc - Lower) / (Upper - Lower),
           Membership = ifelse(MemberValue >= 1, 1,
                               ifelse(MemberValue <= 0, 0, MemberValue)))
  return(P_oc_lpi)
}

## Percent of Orbicella cover ----
BCG_P_oc_demo <- function(Samp){
  P_oc_demo <- Samp %>%
    mutate(taxa = str_extract(SPECIES_NAME, "Orbicella")) %>%
    filter(!is.na(taxa))

  if(nrow(P_oc_demo) == 0){
    dummy <- data.frame(
      HEIGHT = 0,
      MAX_DIAMETER = 0,
      MorphologyIndex = 0,
      OLD_MORT = 0,
      RECENT_MORT = 0,
      N_COLONIES = 0,
      METERS_COMPLETED = 10,
      Model = "BCG"
    )
    P_oc_demo <- bind_rows(P_oc_demo, dummy)
  }

  P_oc_demo <- P_oc_demo %>%
    mutate(CSA = pi * ((HEIGHT + (MAX_DIAMETER / 2)) / 2) ^ 2 * MorphologyIndex,
           LCSA = CSA * ((100 - (OLD_MORT + RECENT_MORT)) / 100),
           TotalPoints = (N_COLONIES * LCSA) / (METERS_COMPLETED * 10000) * 100,
           Metric_name = "p_oc_demo") %>%
    group_by(Model, Metric_name) %>%
    summarise(MCalc = sum(TotalPoints, na.rm = T)) %>%
    left_join(rules) %>%
    mutate(MemberValue = (MCalc - Lower) / (Upper - Lower),
           Membership = ifelse(MemberValue >= 1, 1,
                               ifelse(MemberValue <= 0, 0, MemberValue)))
  return(P_oc_demo)
}

## Percent cover non-tolerant taxa ----
BCG_P_cratt1234_lpi <- function(Samp, colTransect) {
  P_cratt1234_lpi <- Samp %>%
    filter(BCGAttr < 5) %>%
    filter(TotalPoints > 0) %>%
    mutate(Metric_name = "p_cratt1234")

  if(nrow(P_cratt1234_lpi) == 0){
    dummy <- data.frame(
      Model = unique(Samp$Model),
      Metric_name = "p_cratt1234",
      TotalPoints = 0,
      PRIMARY_SAMPLE_UNIT = unique(Samp[[colTransect]])
    )
    P_cratt1234_lpi <- bind_rows(P_cratt1234_lpi, dummy)
  }

  P_cratt1234_lpi <- P_cratt1234_lpi %>%
    group_by(Model, Metric_name) %>%
    summarise(MCalc = sum(TotalPoints, na.rm = T) / length(unique(Samp[[colTransect]]))) %>%
    left_join(rules) %>%
    mutate(MemberValue = (MCalc - Lower) / (Upper - Lower),
           Membership = ifelse(MemberValue >= 1, 1,
                               ifelse(MemberValue <= 0, 0, MemberValue)))
  return(P_cratt1234_lpi)
}

## Surface area live coral cover ----
BCG_Sa_lcc_demo <- function(Samp) {
  Sa_lcc_demo <- Samp %>%
    mutate(CSA = (pi * ((HEIGHT + (MAX_DIAMETER / 2)) / 2)^2 * MorphologyIndex),
           LCSA = CSA * ((100 - (OLD_MORT + RECENT_MORT)) / 100),
           TotalPoints = (N_COLONIES * LCSA) / (METERS_COMPLETED),
           Metric_name = "sa_lcc_demo") %>%
    group_by(Model, Metric_name) %>%
    summarise(MCalc = sum(TotalPoints, na.rm = T))

  if(nrow(Sa_lcc_demo) == 0){
    Sa_lcc_demo[1,1] <- "BCG"
    Sa_lcc_demo[1,2] <- "sa_lcc_demo"
    Sa_lcc_demo[1,3] <- 0
  }

  Sa_lcc_demo <- Sa_lcc_demo %>%
    left_join(rules) %>%
    mutate(MemberValue = (MCalc - Lower) / (Upper - Lower),
           Membership = ifelse(MemberValue >= 1, 1,
                               ifelse(MemberValue <= 0, 0, MemberValue)))
  return(Sa_lcc_demo)
}

## Density of medium or large colonies ----
BCG_T_mlcol_demo <- function(Samp) {
  T_mlcol_demo <- Samp %>%
    filter(MAX_DIAMETER >= 20) %>%
    mutate(Metric_name = "t_mlcol_demo")

  if(nrow(T_mlcol_demo) == 0){
    dummy <- data.frame(
      Model = "BCG",
      Metric_name = "t_mlcol_demo",
      N_COLONIES = 0
    )
    T_mlcol_demo <- bind_rows(T_mlcol_demo, dummy)
  }

  T_mlcol_demo <- T_mlcol_demo %>%
    group_by(Model, Metric_name) %>%
    summarise(MCalc = sum(N_COLONIES)) %>%
    left_join(rules) %>%
    mutate(MemberValue = (MCalc - Lower) / (Upper - Lower),
           Membership = ifelse(MemberValue >= 1, 1,
                               ifelse(MemberValue <= 0, 0, MemberValue)))
  return(T_mlcol_demo)
}

## density of colonies per unit area ----
BCG_T_col_demo <- function(Samp) {
  T_col_demo <- Samp %>%
    filter(Type == "Scleractinian") %>%
    mutate(Metric_name = "t_col_demo")

  if(nrow(T_col_demo) == 0){
    dummy <- data.frame(
      Model = "BCG",
      Metric_name = "t_col_demo",
      N_COLONIES = 0,
      METERS_COMPLETED = 10,
      Type = "Scleractinian"
    )
    T_col_demo <- bind_rows(T_col_demo, dummy)
  }

  T_col_demo <- T_col_demo %>%
    group_by(Model, Metric_name) %>%
    summarise(METERS_COMPLETED = mean(METERS_COMPLETED, na.rm = T),
              MCalc = sum(N_COLONIES, na.rm = T) / (METERS_COMPLETED)) %>%
    left_join(rules) %>%
    mutate(MemberValue = (MCalc - Lower) / (Upper - Lower),
           Membership = ifelse(MemberValue >= 1, 1,
                               ifelse(MemberValue <= 0, 0, MemberValue))) %>%
    select(!METERS_COMPLETED)
  return(T_col_demo)
}
