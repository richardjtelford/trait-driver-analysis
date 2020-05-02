## Impute traits and bootstrap plan

bootstrap_moment_plan <- drake_plan(
  
  #divergence
  #impute traits for control and pre-transplant
  imputed_traits_div = community %>%
    select(Site = originSiteID, Location = originBlockID, turfID, year, TTtreat, Taxon = speciesName, cover) %>% 
    trait_impute(traits = traits, 
                 scale_hierarchy = c("Site", "Location"),
                 taxon_col = "Taxon", 
                 value_col = "value", 
                 abundance_col = "cover", 
                 other_col = c("TTtreat", "year", "turfID")),
  
  #convergence
  #impute traits for control and pre-transplant
  imputed_traits_conv = community %>%
    select(Site = destSiteID, Location = destBlockID, turfID, year, TTtreat, Taxon = speciesName, cover) %>% 
    trait_impute(traits = traits, 
                 scale_hierarchy = c("Site", "Location"),
                 taxon_col = "Taxon",
                 value_col = "value",
                 abundance_col = "cover",
                 other_col = c("year", "TTtreat", "turfID")),
  
  #traits moments 
  bootstrapped_trait_moments_div  = trait_np_bootstrap(imputed_traits_div, nrep = 100),
  bootstrapped_trait_moments_conv  = trait_np_bootstrap(imputed_traits_conv, nrep = 100),
  
  #summarise bootstrap moments
  sum_boot_moment_div <- trait_summarise_boot_moments(bootstrapped_trait_moments_div),
  sum_boot_moment_conv <- trait_summarise_boot_moments(bootstrapped_trait_moments_conv),
  
  #traits with climate
  bootstrapped_trait_moments_climate = bootstrapped_trait_moments_div %>% 
    left_join(env, by = c("Site" = "site")) %>% 
    filter(
      (logger == "otc" & TTtreat == "OTC") | (logger != "otc" & TTtreat != "OTC")) %>% 
    select(-logger)#no longer needed
  
)
