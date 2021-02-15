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
    bind_rows(comm_newOTC) %>% 
    mutate(TTtreat = if_else(str_detect(turfID, "O$"), "local", as.character(TTtreat))),
    
    
  #colonization and extinction
  #first and last year transplant comm by treatment
  #get first_transplant for each year, except last for temporal plot.
  first_transplant = community2 %>% 
    filter(year == 2012,
           TTtreat != "control") %>%
    select(turfID, destBlockID, TTtreat, year, species),
  
  last_transplant = community2 %>% 
    filter(year == 2016,
           TTtreat != "control") %>%
    select(turfID, destBlockID, TTtreat, species),
  
  #extinction = first - last year
  extinction = anti_join(first_transplant, last_transplant, by = c("turfID", "destBlockID", "TTtreat", "species")) %>% 
    count(destBlockID, TTtreat),
  
  #colonization = last - first year
  colonization = anti_join(last_transplant, first_transplant, by = c("turfID", "destBlockID", "TTtreat", "species")) %>% 
    count(destBlockID, TTtreat),
  
  #predicted colonization and extinction
  #first year destination site only controls
  first_dest_control = community2 %>% 
    filter(year %in% c(2012),
           TTtreat == "control") %>%
    select(TTtreat, destBlockID, species),
  
  expected_extinction = anti_join(first_transplant, first_dest_control, by = c("destBlockID", "species")) %>% 
    count(destBlockID, TTtreat),
  
  
  #expected colonization
  expected_colonization = first_dest_control %>% 
    select(-TTtreat) %>% 
    crossing(first_transplant %>% distinct(TTtreat)) %>% 
  anti_join(first_transplant, by = c("destBlockID", "species", "TTtreat")) %>% 
    count(destBlockID, TTtreat),
  
  predicted = bind_rows(
    extinction = expected_extinction,
    colonization = expected_colonization,
    .id = "process") %>% 
    group_by(process, TTtreat) %>% 
    summarise(predicted = mean(n)) %>% 
    pivot_wider(names_from = process, values_from = predicted) %>% 
    mutate(TTtreat = factor(TTtreat, levels = c("local", "cool3", "cool1", "OTC", "warm1", "warm3"))),
  
  
  #colonization and extinction over time
  first_transplant_all_years = community2 %>% 
    filter(year != 2016,
           TTtreat != "control") %>%
    select(turfID, destBlockID, TTtreat, year, species),
  
  #extinciton = first - last year
  extinction_all = anti_join(first_transplant_all_years, last_transplant, by = c("turfID", "destBlockID", "TTtreat", "species")) %>% 
    count(destBlockID, TTtreat, year) %>% 
    group_by(TTtreat, year) %>% 
    summarise(nr_species = mean(n),
              se = sd(n)/sqrt(n())),
  
  #colonization = last - first year
  year = c(2012, 2013, 2014, 2015),
  colonization_all = anti_join(crossing(last_transplant, year), first_transplant_all_years, by = c("turfID", "destBlockID", "TTtreat", "species", "year")) %>% 
    group_by(destBlockID, TTtreat, year) %>% 
    count() %>% 
    group_by(TTtreat, year) %>% 
    summarise(nr_species = mean(n),
              se = sd(n)/sqrt(n())),

)

