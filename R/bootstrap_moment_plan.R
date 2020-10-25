## Impute traits and bootstrap plan

bootstrap_moment_plan <- drake_plan(
  
  #fixed
  #impute traits for control and pre-transplant
  community_fixed = community %>%
    select(Site = originSiteID, blockID = originBlockID, turfID, year, TTtreat, Taxon = speciesName, Genus, cover, destBlockID, destSiteID),
  imputed_traits_fixed = trait_impute(comm = community_fixed,
                                    traits = traits, 
                                    scale_hierarchy = c("Site", "blockID"),
                                    trait_col = "trait_trans",
                                    taxon_col = c("Taxon", "Genus"), 
                                    value_col = "value_trans", 
                                    abundance_col = "cover", 
                                    other_col = c("TTtreat", "year", "turfID", "destBlockID", "destSiteID")),
  
  #plastic
  #impute traits for control and pre-transplant
  community_plastic = community %>%
    select(Site = destSiteID, blockID = destBlockID, turfID, year, TTtreat, Taxon = speciesName, Genus, cover, originBlockID, originSiteID) %>% 
    # change destination control plots from 2012 to origin (pre-transplant)
    mutate(blockID = if_else(year == 2012, originBlockID, blockID),
           Site = if_else(year == 2012, originSiteID, Site)),
  imputed_traits_plastic =  trait_impute(comm = community_plastic,
                                      traits = traits,
                                      scale_hierarchy = c("Site", "blockID"),
                                      trait_col = "trait_trans",
                                      taxon_col = c("Taxon", "Genus"),
                                      value_col = "value_trans",
                                      abundance_col = "cover",
                                      other_col = c("TTtreat", "year", "turfID", "originBlockID", "originSiteID")),
  
  #traits moments 
  bootstrapped_trait_moments_fixed  = trait_np_bootstrap(imputed_traits_fixed, nrep = 100),
  bootstrapped_trait_moments_plastic  = trait_np_bootstrap(imputed_traits_plastic, nrep = 100),
  
  #summarise bootstrap moments
  sum_boot_moment_fixed = trait_summarise_boot_moments(bootstrapped_trait_moments_fixed) %>% 
    rename("originBlockID" = "blockID", "originSiteID" = "Site"),
  sum_boot_moment_plastic = trait_summarise_boot_moments(bootstrapped_trait_moments_plastic) %>% 
    rename("destBlockID" = "blockID", "destSiteID" = "Site"),
  
  #summarise bootstrap moments with climate
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
     rename("originBlockID" = "blockID", "originSiteID" = "Site") %>% 
     left_join(env, by = c("originSiteID" = "site")),
   plastic = bootstrapped_trait_moments_plastic %>% 
     rename("destBlockID" = "blockID", "destSiteID" = "Site") %>% 
     left_join(env, by = c("destSiteID" = "site")),
   .id = "plasticity") %>% 
   # select climate data for otc and other plots
   filter(
     (logger == "otc" & TTtreat == "OTC") | (logger != "otc" & TTtreat != "OTC")) %>% 
   select(-logger)#no longer needed
  
)

