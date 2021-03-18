## Impute traits and bootstrap plan

bootstrap_moment_plan <- drake_plan(
  
  #fixed
  #impute traits for control and pre-transplant
  community_fixed = community %>%
    select(Site = originSiteID, blockID = originBlockID, turfID, year, Treatment = TTtreat, Taxon = speciesName, Genus, cover, destBlockID, destSiteID),
  imputed_traits_fixed = trait_impute(comm = community_fixed,
                                    traits = traits, 
                                    scale_hierarchy = c("Site", "blockID"),
                                    trait_col = "trait_trans",
                                    treatment_col = "Treatment",
                                    treatment_level = c("Site"),
                                    taxon_col = c("Taxon", "Genus"), 
                                    value_col = "value_trans", 
                                    abundance_col = "cover", 
                                    min_n_leaves = 5,
                                    other_col = c("year", "turfID", "destBlockID", "destSiteID")),
  
  #plastic
  #impute traits for control and pre-transplant
  community_plastic = community %>%
    select(Site = destSiteID, blockID = destBlockID, turfID, year, Treatment = TTtreat, Taxon = speciesName, Genus, cover, originBlockID, originSiteID) %>% 
    # change destination control plots from 2012 to origin (pre-transplant)
    mutate(blockID = if_else(year == 2012, originBlockID, blockID),
           Site = if_else(year == 2012, originSiteID, Site)),
  imputed_traits_plastic =  trait_impute(comm = community_plastic,
                                      traits = traits,
                                      scale_hierarchy = c("Site", "blockID"),
                                      trait_col = "trait_trans",
                                      treatment_col = "Treatment",
                                      treatment_level = c("Site"),
                                      taxon_col = c("Taxon", "Genus"),
                                      value_col = "value_trans",
                                      abundance_col = "cover",
                                      min_n_leaves = 5,
                                      other_col = c("year", "turfID", "originBlockID", "originSiteID")),
  
  #traits moments 
  bootstrapped_trait_moments_fixed  = trait_np_bootstrap(imputed_traits_fixed, nrep = 100),
  
  bootstrapped_trait_moments_plastic  = trait_np_bootstrap(imputed_traits_plastic, nrep = 100),

  #summarise bootstrap moments
  #fixed
  sum_boot_moment_fixed = trait_summarise_boot_moments(bootstrapped_trait_moments_fixed, sd_mult = 1.96) %>% 
    # calculate range
    left_join(bootstrapped_trait_moments_fixed %>% 
                group_by(Site, blockID, destBlockID, destSiteID, turfID, trait_trans, year, Treatment_comm) %>% 
                summarise(range = max(mean) - min(mean)),
              by = c("Site", "blockID", "destBlockID", "destSiteID", "turfID", "trait_trans", "year", "Treatment_comm")) %>% 
    rename("originBlockID" = "blockID", "originSiteID" = "Site", "TTtreat" = "Treatment_comm") %>% 
    fancy_trait_name_dictionary(),
  
  #plastic
  sum_boot_moment_plastic = trait_summarise_boot_moments(bootstrapped_trait_moments_plastic, sd_mult = 1.96) %>%
    # calculate range
    left_join(bootstrapped_trait_moments_plastic %>% 
                group_by(Site, blockID, originBlockID, originSiteID, turfID, trait_trans, year, Treatment_comm) %>% 
                summarise(range = max(mean)  - min(mean)),
              by = c("Site", "blockID", "originBlockID", "originSiteID", "turfID", "trait_trans", "year", "Treatment_comm")) %>% 
    ungroup() %>% 
    select(-Site, -blockID) %>% 
    # rescue true destination site and block
    left_join(community %>% select(destSiteID, destBlockID, turfID) %>% distinct(), by = "turfID") %>% 
    rename("TTtreat" = "Treatment_comm") %>% 
    fancy_trait_name_dictionary(),
  
  #summarise bootstrap moments with climate
  #add fancy trait names
 summarised_boot_moments_climate = bind_rows(
    fixed = sum_boot_moment_fixed %>% 
      left_join(env, by = c("originSiteID" = "site")),
    plastic = sum_boot_moment_plastic %>% 
      left_join(env, by = c("destSiteID" = "site")),
    .id = "plasticity") %>% 
   # select climate data for otc and other plots
    filter(
      (logger == "otc" & TTtreat == "OTC") | (logger != "otc" & TTtreat != "OTC")) %>% 
    select(-logger),#no longer needed,

  #traits with climate
 bootstrapped_trait_moments_climate = bind_rows(
   fixed = bootstrapped_trait_moments_fixed %>% 
     rename("originBlockID" = "blockID", "originSiteID" = "Site", "TTtreat" = "Treatment_comm") %>% 
     left_join(env, by = c("originSiteID" = "site")),
   plastic = bootstrapped_trait_moments_plastic %>% 
     rename("destBlockID" = "blockID", "destSiteID" = "Site", "TTtreat" = "Treatment_comm") %>% 
     left_join(env, by = c("destSiteID" = "site")),
   .id = "plasticity") %>% 
   fancy_trait_name_dictionary() %>% 
   # select climate data for otc and other plots
   filter(
     (logger == "otc" & TTtreat == "OTC") | (logger != "otc" & TTtreat != "OTC")) %>% 
   select(-logger)#no longer needed
  
)
