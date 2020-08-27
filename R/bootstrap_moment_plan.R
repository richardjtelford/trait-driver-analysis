## Impute traits and bootstrap plan

bootstrap_moment_plan <- drake_plan(
  
  #divergence
  #impute traits for control and pre-transplant
  imputed_traits_div = community %>%
    select(Site = originSiteID, blockID = originBlockID, turfID, year, TTtreat, Taxon = speciesName, cover) %>% 
    trait_impute(traits = traits, 
                 scale_hierarchy = c("Site", "blockID"),
                 trait_col = "trait_trans",
                 taxon_col = "Taxon", 
                 value_col = "value_trans", 
                 abundance_col = "cover", 
                 other_col = c("TTtreat", "year", "turfID")),
  
  #convergence
  #impute traits for control and pre-transplant
  imputed_traits_conv = community %>%
    select(Site = destSiteID, blockID = destBlockID, turfID, year, TTtreat, Taxon = speciesName, cover) %>% 
    trait_impute(traits = traits, 
                 scale_hierarchy = c("Site", "blockID"),
                 trait_col = "trait_trans",
                 taxon_col = "Taxon",
                 value_col = "value_trans",
                 abundance_col = "cover",
                 other_col = c("year", "TTtreat", "turfID")),
  
  #traits moments 
  bootstrapped_trait_moments_div  = trait_np_bootstrap(imputed_traits_div, nrep = 100),
  bootstrapped_trait_moments_conv  = trait_np_bootstrap(imputed_traits_conv, nrep = 100),
  
  #summarise bootstrap moments
  sum_boot_moment_div = trait_summarise_boot_moments(bootstrapped_trait_moments_div),
  sum_boot_moment_conv = trait_summarise_boot_moments(bootstrapped_trait_moments_conv),
  
  #summarise bootstrap moments with climate
  summarised_boot_moments_climate = bind_rows(
    divergence = sum_boot_moment_div,
    convergence = sum_boot_moment_conv, 
    .id = "direction") %>% 
    left_join(env, by = c("Site" = "site")) %>% 
    filter(
      (logger == "otc" & TTtreat == "OTC") | (logger != "otc" & TTtreat != "OTC")) %>% 
    select(-logger),#no longer needed,
  
  #traits with climate
  bootstrapped_trait_moments_climate =
    bind_rows(
      divergence = bootstrapped_trait_moments_div,
      convergence = bootstrapped_trait_moments_conv, 
      .id = "direction") %>%
    left_join(env, by = c("Site" = "site")) %>% 
    filter(
      (logger == "otc" & TTtreat == "OTC") | (logger != "otc" & TTtreat != "OTC")) %>% 
    select(-logger),#no longer needed
  
  #H1Q1: moments by climate regressions (Table 1)
  trait_climate_regression = summarised_boot_moments_climate %>% 
    filter(year == 2012,
           TTtreat %in% c("control"),
           direction == "convergence") %>% 
    select(Site:Mean, variable, value, -n) %>% 
    nest(data = -c(trait_trans)) %>% 
    # mutate(mod = map(data, ~lme(Mean ~ value, random = ~1|Site, data = .x)),
    #        result = map(mod, tidy, "fixed")) %>% 
    mutate(mod = map(data, ~lm(Mean ~ value + Site, data = .x)),
           result = map(mod, tidy)) %>% 
    unnest(result) %>% 
    filter(term == "value",
           p.value < 0.05)
  
)



#fit <- lme(Mean ~ value, random = ~1|Site, data = summarised_boot_moments_climate)
#summary(fit)
