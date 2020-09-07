## import and process data

##TODO - 
# clean against BIEN
# transform traits as appropriate

import_plan <- drake_plan(
  
  #import community
  community = {
    #make community database connection
    con = DBI::dbConnect(RSQLite::SQLite(), community_download, create = FALSE)
    #import
    load_comm(con = con)
    }, 

  #import trait data
  traits_leaf0 = read_csv(traits_leaf_download, col_types = cols(StoichLabel = col_character())),
  traits_chemical0 = read_csv(traits_chemical_download),
  
  traits_leaf = traits_leaf0 %>% 
    select(-Envelope_Name_Corrected, -Date, -matches("Flag$"), -allComments) %>% 
    #remove leaf-thickness measurement (keep mean)
    select(-matches("Leaf_Thickness_\\d_mm")) %>% 
    rename("Thickness_mm" = "Leaf_Thickness_Ave_mm") %>% 
    pivot_longer(
      cols = c("Wet_Mass_g", "Dry_Mass_g", "Thickness_mm", "Leaf_Area_cm2", "SLA_cm2_g", "LDMC"), 
      names_to = "trait", 
      values_to = "value"),
  
  traits_chemical = traits_chemical0 %>% 
    select(-Date, -n) %>%
    filter() %>% 
    mutate(NP_ratio = N_percent / P_percent) %>% 
    pivot_longer(
      cols = c("P_percent", "C_percent", "N_percent", "CN_ratio", "NP_ratio", "dN15_permil", "dC13_permil"),
      names_to = "trait",
      values_to = "value"
    ),
  
  #combine leaf and chemical traits
  traits0 = bind_rows(traits_leaf,
                     traits_chemical) %>%
    filter(!is.na(value)) %>%
    mutate(Site = factor(Site, levels = levels(community$originSiteID))), 
  
  
  #clean impossible trait values using BIEN
  #trait_outliers = check_BIEN_trait_values(traits0), # finds 5000 - not useful
  
  #calculate derived traits
  #transform
  traits = traits0 %>% 
    filter(Treatment %in% c("LOCAL", "C", "0")) %>% # only gradient plots
    mutate(Treatment = case_when(Treatment %in% c("LOCAL", "C", "0") ~ "control",
                                 Treatment == "1" ~ "warm1",
                                 Treatment == "2" ~ "cool1",
                                 Treatment == "3" ~ "warm3",
                                 Treatment == "4" ~ "cool3",
                                 Treatment == "OTC" ~ "OTC")) %>% 
    rename(blockID = destBlockID) %>% 
    #log transform size and area traits
    mutate(
      value_trans = if_else(
        trait %in% c(
          "Wet_Mass_g",
          "Dry_Mass_g",
          "Leaf_Area_cm2",
          "Thickness_mm"
        ), 
        true = suppressWarnings(log(value)),# suppress warnings from log(-value) in isotopes (these are calculated but not kept)
        false = value
      ), 
      trait_trans = recode(
        trait,
        "Wet_Mass_g" = "Wet_Mass_g_log",
        "Dry_Mass_g" = "Dry_Mass_g_log",
        "Leaf_Area_cm2" = "Leaf_Area_cm2_log",
        "Thickness_mm" = "Thickness_mm_log"
      )),
  
  #import environmental data
  env = read_csv(env_download) %>% 
    filter(
      lubridate::month(month) %in% 5:9 #May to September
      ) %>% 
    mutate(site = factor(site, levels = c("H", "A", "M", "L"))) %>% 
    group_by(logger, variable, site) %>% 
    summarise(value = mean(value, na.rm = TRUE))
)
