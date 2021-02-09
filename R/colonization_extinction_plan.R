#data colonization extinction plan

colonization_extinction_plan <- drake_plan(
  
  # replace destBlockID for OTC for this analysis
  comm_newOTC = community %>% 
    filter(TTtreat == "OTC",
           destSiteID != "L") %>% 
    mutate(destBlockID_new = recode(destBlockID, "H1" = "A1", "H2" = "A2", "H3" = "A3", "H4" = "A4", "H5" = "A5", "H6" = "A6", "H7" = "A7",
                                "A1" = "M1", "A2" = "M2", "A3" = "M3", "A4" = "M4", "A5" = "M5", "A6" = "M6", "A7" = "M7",
                                "M1" = "L1", "M2" = "L2", "M3" = "L3", "M4" = "L4", "M5" = "L5", "M6" = "L6", "M7" = "L7")),
  
  # combine with new OTC comm
  community2 = community %>% 
    # remove OTC
    filter(TTtreat != "OTC") %>% 
    bind_rows(comm_newOTC),
    
    
  #colonization and extinction
  #first and last year transplant comm by treatment
  first_transplant = community2 %>% 
    filter(year %in% c(2012)) %>%
    group_by(turfID, destBlockID, TTtreat) %>% 
    distinct(species),
  
  last_transplant = community2 %>% 
    filter(year %in% c(2016)) %>%
    group_by(turfID, destBlockID, TTtreat) %>% 
    distinct(species),
  
  #extinciton = first - last year
  extinction = anti_join(first_transplant, last_transplant, by = c("turfID", "destBlockID", "TTtreat", "species")) %>% 
    group_by(destBlockID, TTtreat) %>% 
    count(),
  
  #colonization = last - first year
  colonization = anti_join(last_transplant, first_transplant, by = c("turfID", "destBlockID", "TTtreat", "species")) %>% 
    group_by(destBlockID, TTtreat) %>% 
    count(),
  
  
  #predicted colonization and extinction
  #first year destination site
  first_dest_control = community2 %>% 
    filter(year %in% c(2012),
           TTtreat == "control") %>%
    group_by(destBlockID) %>% 
    distinct(species),
  
  #expected exctinction
  #reshuffle blocks for controls to get expected extinction/colonization
  first_dest_control_reshuffeled = first_dest_control %>% 
    mutate(destBlockID = recode(destBlockID, "A1" = "A2", "A2" = "A3", "A3" = "A4", "A4" = "A5", "A5" = "A6", "A6" = "A7", "A7" = "A1",
                                "H1" = "H2", "H2" = "H3", "H3" = "H4", "H4" = "H5", "H5" = "H6", "H6" = "H7", "H7" = "H1",
                                "M1" = "M2", "M2" = "M3", "M3" = "M4", "M4" = "M5", "M5" = "M6", "M6" = "M7", "M7" = "M1",
                                "L1" = "L2", "L2" = "L3", "L3" = "L4", "L4" = "L5", "L5" = "L6", "L6" = "L7", "L7" = "L1")),
  
  expected_extinctions_control = anti_join(first_transplant %>% filter(TTtreat == "control"), first_dest_control_reshuffeled, by = c("destBlockID", "species")) %>% 
    group_by(destBlockID, TTtreat) %>% 
    count(),
  
  expected_extinction = anti_join(first_transplant, first_dest_control, by = c("destBlockID", "species")) %>% 
    group_by(destBlockID, TTtreat) %>% 
    count() %>% 
    bind_rows(expected_extinctions_control),
  
  
  #expected colonization
  # only controls with reshuffling
  
  expected_colonoization_control = community2 %>% 
    filter(year %in% c(2012),
           TTtreat == "control") %>% 
    select(destBlockID, species) %>% 
    mutate(destBlockID = recode(destBlockID, "A1" = "A2", "A2" = "A3", "A3" = "A4", "A4" = "A5", "A5" = "A6", "A6" = "A7", "A7" = "A1",
                                "H1" = "H2", "H2" = "H3", "H3" = "H4", "H4" = "H5", "H5" = "H6", "H6" = "H7", "H7" = "H1",
                                "M1" = "M2", "M2" = "M3", "M3" = "M4", "M4" = "M5", "M5" = "M6", "M6" = "M7", "M7" = "M1",
                                "L1" = "L2", "L2" = "L3", "L3" = "L4", "L4" = "L5", "L5" = "L6", "L6" = "L7", "L7" = "L1"),
           TTtreat = "control") %>% 
    anti_join(first_transplant %>% filter(TTtreat == "control")) %>% 
    group_by(destBlockID, TTtreat) %>% 
    count(),
  
  # other treatments
  treat_block = community2 %>% 
    distinct(TTtreat, destBlockID) %>% 
    filter(TTtreat != "control"),
  
  expected_colonoization = community %>% 
    filter(year %in% c(2012),
           TTtreat == "control") %>% 
    select(destBlockID, species) %>% 
    full_join(treat_block, by = "destBlockID") %>% 
    anti_join(first_transplant) %>% 
    group_by(destBlockID, TTtreat) %>% 
    count() %>% 
    bind_rows(expected_colonoization_control),
  
  predicted = bind_rows(
    extinction = expected_extinction,
    colonization = expected_colonoization,
    .id = "process") %>%
    group_by(process, TTtreat) %>% 
    summarise(predicted = mean(n)) %>% 
    pivot_wider(names_from = process, values_from = predicted) %>% 
    mutate(TTtreat = factor(TTtreat, levels = c("control", "cool3", "cool1", "OTC", "warm1", "warm3"))),
  
  
  #colonization and extinction over time
  #first and last year transplant comm by treatment
  first_transplant_all_years = community2 %>% 
    filter(year != 2016) %>%
    group_by(turfID, destBlockID, TTtreat, year) %>% 
    distinct(species),
  
  #extinciton = first - last year
  extinction_all = anti_join(first_transplant_all_years, last_transplant, by = c("turfID", "destBlockID", "TTtreat", "species")) %>% 
    group_by(destBlockID, TTtreat, year) %>% 
    count() %>% 
    group_by(TTtreat, year) %>% 
    summarise(count = mean(n)),
  
  #colonization = last - first year
  year = c(2012, 2013, 2014, 2015),
  colonization_all = anti_join(crossing(last_transplant, year), first_transplant_all_years, by = c("turfID", "destBlockID", "TTtreat", "species", "year")) %>% 
    group_by(destBlockID, TTtreat, year) %>% 
    count() %>% 
    group_by(TTtreat, year) %>% 
    summarise(count = mean(n)),

)

