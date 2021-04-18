#get correlations

# function to get correlation coefficient and p value
cors <- function(df) { 
  # turn all three matrices (r, n, and P into a data frame)
  M <- rcorr(as.matrix(df))
  # return the three data frames in a list return(Mdf)
  Mdf <- map(M, ~ data.frame(.x))
}



# correlations
get_trait_correlations <- function(dat, control = TRUE) {
  
  if (control){
    # filter for control
    dat <- dat %>%
      filter(TTtreat != "control")
  } else{
    # all treatments
   dat <- dat
  }
  
  trait_table <- dat %>% 
    ungroup() %>% 
    select(originSiteID:mean, -n, trait_trans, -trait_fancy) %>% 
    pivot_wider(names_from = trait_trans, values_from = mean) %>% 
    select(C_percent:Thickness_mm_log)
  
  # get correlations
  correlations = cors(trait_table) %>% 
    # row to col for first trait
    map(~rownames_to_column(.x, var="trait1")) %>%
    # make long table with second trait
    map(~pivot_longer(.x, -trait1, "trait2")) %>% 
    # merge our three list elements by binding the rows
    bind_rows(.id = "id") %>% 
    pivot_wider(names_from = id, values_from = value) %>% 
    mutate(sig_p = if_else(P < 0.05, "significant", "non-significant"),
           r_sig = if_else(sig_p == "significant", r, NA_real_)) %>% 
    mutate(trait1 = recode(trait1,
                           C_percent = "C",
                           CN_ratio = "CN",
                           dC13_permil = "dC13",
                           dN15_permil = "dN15",
                           Dry_Mass_g_log = "Dry",
                           LDMC = "LDMC",
                           Leaf_Area_cm2_log = "Area",
                           N_percent = "N",
                           NP_ratio = "NP",
                           P_percent = "P",
                           SLA_cm2_g = "SLA",
                           Thickness_mm_log = "Thick"),
           trait2 = recode(trait2,
                           C_percent = "C",
                           CN_ratio = "CN",
                           dC13_permil = "dC13",
                           dN15_permil = "dN15",
                           Dry_Mass_g_log = "Dry",
                           LDMC = "LDMC",
                           Leaf_Area_cm2_log = "Area",
                           N_percent = "N",
                           NP_ratio = "NP",
                           P_percent = "P",
                           SLA_cm2_g = "SLA",
                           Thickness_mm_log = "Thick"),
           trait1 = factor(trait1, levels = c("Dry", "Area", "Thick", "LDMC", "SLA", "C", "N", "P", "CN", "NP", "dC13", "dN15")),
           trait2 = factor(trait2, levels = c("Dry", "Area", "Thick", "LDMC", "SLA", "C", "N", "P", "CN", "NP", "dC13", "dN15")))
  
  return(correlations)
}

