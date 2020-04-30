## Impute traits and bootstrap plan

bootstrap_moment_plan <- drake_plan(
  
  #impute traits for control, OTC, and pre-transplant
  imputed_traits = {
    imputed_traits_home <- community %>%
      filter(year == min(year) | TTtreat %in% c("control", "local", "OTC")) %>% 
      select(Site = originSiteID, blockID = originBlockID, turfID, year, TTtreat, Taxon = speciesName, cover) %>% 
      trait_impute(traits = traits, 
                   scale_hierarchy = c("Site", "blockID"),
                   taxon_col = "Taxon", 
                   value_col = "value_trans", 
                   abundance_col = "cover", 
                   other_col = c("TTtreat", "year", "turfID"))
    
    #impute traits for transplanted turfs
    imputed_traits_transplant <- community %>%
      filter(year > min(year), !TTtreat %in% c("control", "local", "OTC")) %>%
      select(Site = destSiteID, blockID = destBlockID, turfID, year, TTtreat, Taxon = speciesName, cover) %>% 
      trait_impute(traits = traits, 
                   scale_hierarchy = c("Site", "blockID"),
                   taxon_col = "Taxon",
                   value_col = "value_trans",
                   abundance_col = "cover",
                   other_col = c("year", "TTtreat", "turfID"))
    
    
    x <- bind_rows(
      imputed_traits_home %>% select(everything()),#hack to deal with new vctrs 
      imputed_traits_transplant %>% select(everything()))
    class(x) <- class(imputed_traits_home)# recover traits
      
    attr(x, "attrib") <- attr(imputed_traits_home, "attrib")
    x
  },
  
  #traits moments 
  bootstrapped_trait_moments  = trait_np_bootstrap(imputed_traits, nrep = 100),
  
  #summarise bootstrap moments
  summarised_boot_moments = trait_summarise_boot_moments(bootstrapped_trait_moments),
  
  #traits with climate
  bootstrapped_trait_moments_climate = bootstrapped_trait_moments %>% 
    left_join(env, by = c("Site" = "site")) %>% 
    filter(
      (logger == "otc" & TTtreat == "OTC") | (logger != "otc" & TTtreat != "OTC")) %>% 
    select(-logger)#no longer needed
  
)
