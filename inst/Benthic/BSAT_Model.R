# Functions to calculate the BCG level membership

# BSAT model (reduced) Part 1 ----
M_BSAT_1 <- function(Samp, colTransect, colSite, colYear){
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

        # test all the rules and then binds together the results
        P_cc_lpi <- BSAT_P_cc_lpi(tSamp, colTransect) %>%
          mutate(Site = unique(tSamp[[colSite]]),
                 Year = unique(tSamp[[colYear]]))
        P_oac_lpi <- BSAT_P_oac_lpi(tSamp, colTransect) %>%
          mutate(Site = unique(tSamp[[colSite]]),
                 Year = unique(tSamp[[colYear]]))
        T_cratt45 <- BSAT_T_cratt45(tSamp) %>%
          mutate(Site = unique(tSamp[[colSite]]),
                 Year = unique(tSamp[[colYear]]))
        P_bts_lpi <- BSAT_P_bts_lpi(tSamp, colTransect) %>%
          mutate(Site = unique(tSamp[[colSite]]),
                 Year = unique(tSamp[[colYear]]))

        testB <- bind_rows(P_cc_lpi, P_oac_lpi, T_cratt45, P_bts_lpi)
        testB <- rules %>%
          filter(Model == "BSAT") %>%
          left_join(testB) %>%
          replace_na(list(MCalc = 0,
                          MemberValue = 0,
                          Membership = 0,
                          Site = unique(tSamp[[colSite]]),
                          Year = unique(tSamp[[colYear]]))) %>%
          mutate(Membership = as.numeric(Membership)) %>%
          arrange(Level, desc(Membership), desc(MemberValue))

        M_Result <- bind_rows(M_Result, testB)

        incProgress(1 / length(numCVE))
      }
    })

  return(M_Result)
}

# BSAT model (reduced) Part 2 ----
M_BSAT_2 <- function(M_Result){
  tLevel <- tibble("Site" = character(), "Year" = character(), "Level" = integer())
  M_Result <- M_Result %>% mutate(CVE = paste(M_Result$Site, M_Result$Year, sep = "___"))
  numSite <- unique(M_Result$Site)
  numYear <- unique(M_Result$Year)
  numCVE <- unique(M_Result$CVE)

  # tests for each level, looking for the value at the nth position
   L3 <- M_Result %>%
    filter(Level == 3) %>%
     arrange(CVE, desc(Membership)) %>%
     group_by(CVE, Level) %>%
     summarise(Membership = nth(Membership, 4))

   L4 <- M_Result %>%
     filter(Level == 4) %>%
     arrange(CVE, desc(Membership)) %>%
     group_by(CVE, Level) %>%
     summarise(Membership = nth(Membership, 4))

   L5 <- M_Result %>%
     filter(Level == 5) %>%
     arrange(CVE, desc(Membership)) %>%
     group_by(CVE, Level) %>%
     summarise(Membership = nth(Membership, 3))

   # prepare a matrix to check partial membership
   Def_1 <- bind_rows(L3, L4, L5) %>%
     mutate(Level = paste0("L", Level, ".Sub")) %>%
     pivot_wider(names_from = "Level", values_from = "Membership")

Def_Result <-  Def_1 %>%
   ungroup() %>%
     transmute(CVE,
               L1.Sub = 0,
               L2.Sub = 0,
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
              Level = Lev.Prop.Nar)

   return(tLevel)
}

## percent of coral cover ----
BSAT_P_cc_lpi <- function(Samp, colTransect){
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


## percent of orbicella and acropora cover ----
BSAT_P_oac_lpi <- function(Samp, colTransect){
  P_oac_lpi <- Samp %>%
    filter(OrbicellaAcropora == T) %>%
    mutate(Metric_name = "p_oac_lpi")

  if(nrow(P_oac_lpi) == 0){
    # Create dummy data frame
    dummy <- data.frame(
      Model = unique(Samp$Model),
      Metric_name = "p_oac_lpi",
      TotalPoints = 0,
      PRIMARY_SAMPLE_UNIT = unique(Samp[[colTransect]])
    )
    # Bind the dummy data to P_oac_lpi
    P_oac_lpi <- bind_rows(P_oac_lpi, dummy)
  }

  P_oac_lpi <- P_oac_lpi %>%
    group_by(Model, Metric_name) %>%
    summarise(MCalc = sum(TotalPoints, na.rm = T) / length(unique(Samp[[colTransect]]))) %>%
    left_join(rules) %>%
    mutate(MemberValue = (MCalc - Lower) / (Upper - Lower),
           Membership = ifelse(MemberValue >= 1, 1,
                               ifelse(MemberValue <= 0, 0, MemberValue)))
  return(P_oac_lpi)
}


## Coral richness for non-tolerant taxa ----
BSAT_T_cratt45 <- function(Samp){
  T_cratt45 <- Samp %>%
    filter(TotalPoints > 0) %>%
    filter(BCGAttr %in% c(4,5)) %>%
    mutate(Metric_name = "t_cratt45") %>%
    group_by(Model, Metric_name, SPECIES_NAME) %>%
    summarise(P1 = n()) %>%
    group_by(Model, Metric_name) %>%
    summarise(MCalc = n())

  if(nrow(T_cratt45) == 0){
    T_cratt45[1,1] = "BSAT"
    T_cratt45[1,2] = "t_cratt45"
    T_cratt45[1,3] = 0
  }

  T_cratt45 <- T_cratt45 %>%
    left_join(rules) %>%
    mutate(MemberValue = (MCalc - Lower) / (Upper - Lower),
           Membership = ifelse(MemberValue >= 1, 1,
                               ifelse(MemberValue <= 0, 0, MemberValue)))
  return(T_cratt45)
}


## Percent of abiotic cover ----
BSAT_P_bts_lpi <- function(Samp, colTransect){
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
