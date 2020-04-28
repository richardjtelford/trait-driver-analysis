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
    filter() %>% 
    pivot_longer(cols = c("Wet_Mass_g", "Dry_Mass_g", "Leaf_Thickness_Ave_mm", "Leaf_Area_cm2", "SLA_cm2_g", "LDMC"), names_to = "trait", values_to = "value"),
  
  traits_chemical = traits_chemical0 %>% 
    select(-Date, -n) %>%
    filter() %>% 
    pivot_longer(cols = c("P_percent", "C_percent", "N_percent", "CN_ratio", "dN15_permil", "dC13_permil"), names_to = "trait", values_to = "value"),
  
  #combine leaf and chemical traits
  traits0 = bind_rows(traits_leaf,
                     traits_chemical) %>%
    filter(!is.na(value)) %>%
    mutate(Site = factor(Site, levels = levels(community$originSiteID))), 
  
  
  #TODO clean impossible trait values using BIEN
  #calculate derived traits
  #transform
  traits = traits0 %>% 
    filter(Treatment == "LOCAL") %>% # only gradient plots
    rename(Location = destBlockID),
  
  #import environmental data
  env = get(load(env_download)) %>% 
    filter(
      variable %in% c("Tair", "Tsoil0"),
      lubridate::month(month) %in% 5:9 #May to September
      ) %>% 
    group_by(logger, variable, site) %>% 
    summarise(value = mean(value, na.rm = TRUE))
  
)
