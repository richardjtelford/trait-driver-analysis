## Impute traits and bootstrap plan

bootstrap_moment_plan <- drake_plan(
  
  #impute traits for control and pre-transplant
  imputed_traits = {
    imputed_traits_home = community %>%
      filter(year == min(year) | TTtreat %in% c("control", "local", "OTC")) %>% 
      select(Site = originSiteID, Location = originBlockID, turfID, year, TTtreat, Taxon = speciesName, cover) %>% 
      trait_impute(traits = traits, scale_hierarchy = c("Site", "Location"), taxon_col = "Taxon", value_col = "value", abundance_col = "cover", other_col = c("TTtreat", "year", "turfID"))
    
    imputed_traits_transplant = community %>%
      filter(year > min(year), !TTtreat %in% c("control", "local", "OTC")) %>%
      select(Site = destSiteID, Location = destBlockID, turfID, year, TTtreat, Taxon = speciesName, cover) %>% 
      trait_impute(traits = traits, scale_hierarchy = c("Site", "Location"), taxon_col = "Taxon", value_col = "value", abundance_col = "cover", other_col = c("year", "TTtreat", "turfID"))
    
    
    x <- bind_rows(imputed_traits_home, imputed_traits_transplant)
    attr(x, "attrib") <- attr(imputed_traits_home, "attrib")
    x
  },
  
  #traits moments 
  bootstrapped_trait_moments  = trait_np_bootstrap(imputed_traits, nrep = 100)
)
