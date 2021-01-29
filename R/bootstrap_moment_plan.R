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
                                      other_col = c("year", "turfID", "originBlockID", "originSiteID")),
  
  #traits moments 
  bootstrapped_trait_moments_fixed  = trait_np_bootstrap(imputed_traits_fixed, nrep = 100) %>% 
    rename(Treatment = Treatment_comm),
  
  bootstrapped_trait_moments_plastic  = trait_np_bootstrap(imputed_traits_plastic, nrep = 100)%>% 
    rename(Treatment = Treatment_comm),

  #summarise bootstrap moments
  #fixed
  sum_boot_moment_fixed = bootstrapped_trait_moments_fixed %>% 
    #trait_summarise_boot_moments(bootstrapped_trait_moments_fixed) %>%
    ungroup() %>% 
    group_by(global, Site, blockID, trait_trans, year, turfID, destBlockID, destSiteID, Treatment) %>% 
    summarise(
      n = n(),
      mean = mean(mean),
      ci_low_mean = mean - sd(mean),
      ci_high_mean = mean + sd(mean),
      
      var = mean(variance),
      ci_low_var = var - sd(variance),
      ci_high_var = var + sd(variance),
      
      skew = mean(skewness),
      ci_low_skew = skew - sd(skewness),
      ci_high_skew = skew + sd(skewness),
      
      kurt = mean(kurtosis),
      ci_low_kurt = kurt - sd(kurtosis),
      ci_high_Kurt = kurt + sd(kurtosis)
    ) %>% 
    rename("originBlockID" = "blockID", "originSiteID" = "Site"),
  #plastic
  sum_boot_moment_plastic = bootstrapped_trait_moments_plastic %>% 
    #trait_summarise_boot_moments(bootstrapped_trait_moments_plastic) %>%
    ungroup() %>% 
    group_by(global, Site, blockID, trait_trans, year, turfID, originBlockID, originSiteID, Treatment) %>% 
    summarise(
      n = n(),
      mean = mean(mean),
      ci_low_mean = mean - sd(mean),
      ci_high_mean = mean + sd(mean),
      
      var = mean(variance),
      ci_low_var = var - sd(variance),
      ci_high_var = var + sd(variance),
      
      skew = mean(skewness),
      ci_low_skew = skew - sd(skewness),
      ci_high_skew = skew + sd(skewness),
      
      kurt = mean(kurtosis),
      ci_low_kurt = kurt - sd(kurtosis),
      ci_high_Kurt = kurt + sd(kurtosis)
    ) %>% 
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
      (logger == "otc" & Treatment == "OTC") | (logger != "otc" & Treatment != "OTC")) %>% 
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
     (logger == "otc" & Treatment == "OTC") | (logger != "otc" & Treatment != "OTC")) %>% 
   select(-logger)#no longer needed
  
)
