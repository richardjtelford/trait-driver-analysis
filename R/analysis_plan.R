#data analysis plan

analysis_plan <- drake_plan(
  
  #calculate effect size
  effect_size = bind_rows(
    divergence = bind_rows(
      fixed = sum_boot_moment_fixed,
      plastic = sum_boot_moment_plastic,
      .id = "plasticity") %>%
      ungroup() %>% 
      rename(Site = originSiteID, Block = originBlockID) %>% 
      select(-destSiteID, -destBlockID),
    
    convergence = bind_rows(
      fixed = sum_boot_moment_fixed,
      plastic = sum_boot_moment_plastic,
      .id = "plasticity") %>%
      ungroup() %>% 
      rename(Site = destSiteID, Block = destBlockID) %>% 
      select(-originSiteID, -originBlockID),
    
    .id = "direction"
  ) %>% 
    select(-c(ci_low_mean:ci_high_Kurt)) %>% 
    group_by(direction, plasticity, year, trait_trans, TTtreat, Site, Block) %>% 
    summarise(mean = mean(mean, na.rm = TRUE)) %>% 
    pivot_wider(names_from = "TTtreat", values_from = "mean") %>% 
    mutate(warm1 = warm1 - control,
           cool1 = cool1 - control,
           OTC = OTC - control,
           warm3 = warm3 - control,
           cool3 = cool3 - control) %>% 
    select(-control) %>% 
    pivot_longer(cols = c(warm1:OTC), names_to = "TTtreat", values_to = "mean") %>% 
    filter(!is.na(mean)) %>% 
    #scale
    mutate(mean = mean / sd(mean)),

  
  #effect of gradient (control plots, 2016)
  # control = origin, only done for fixed traits
  trait_climate_regression = summarised_boot_moments_climate %>% 
    ungroup() %>% 
    filter(year == 2016,
           TTtreat %in% c("control"),
           plasticity == "fixed") %>% 
    select(originSiteID:mean, variable, value, -n) %>% 
    nest(data = -c(trait_trans)) %>% 
    mutate(mod = map(data, ~lm(mean ~ value, data = .x)),
           result = map(mod, tidy)) %>%
    unnest(result) %>% 
    mutate(term = plyr::mapvalues(term, from = c("(Intercept)", "value"),
                                  to = c("intercept", "slope"))) %>% 
    rename(traits = trait_trans, 'standard error' = std.error, 'P value' = p.value) %>% 
    select(-data, -mod),
  
  
  #effect of experiments across all elevations
  treatment_effect = effect_size %>% 
    nest(data = -c(direction, plasticity, trait_trans)) %>% 
    mutate(mod = map(data, ~lm(mean ~ TTtreat*year, data = .x)),
           result = map(mod, tidy)) %>% 
    unnest(result) %>% 
    mutate(term = plyr::mapvalues(term, from = c("(Intercept)", "TTtreatcool3", "TTtreatOTC", "TTtreatwarm1", "TTtreatwarm3", "year", "TTtreatcool3:year", "TTtreatOTC:year", "TTtreatwarm1:year", "TTtreatwarm3:year"),
                                  to = c("Tcool1", "Tcool3", "TOTC", "Twarm1", "Twarm3", "cool1", "cool3", "OTC", "warm1", "warm3")),
           signi = if_else(p.value < 0.05, "significant", "non-signigicant")) %>% 
    select(-data, -mod),
  
  
  #effect of experiments by elevations
  # treatment_by_site = effect_size %>% 
  #   ungroup() %>% 
  #   filter(year == 2016) %>% 
  #   nest(data = -c(direction, plasticity, trait_trans)) %>% 
  #   mutate(mod = map(data, ~lm(mean ~ TTtreat*Site, data = .x)),
  #          result = map(mod, tidy)) %>% 
  #   unnest(result),
  
  
  #effect of experiment over time
  # treatment_time_effect = effect_size %>% 
  #   nest(data = -c(direction, trait_trans)) %>% 
  #   mutate(mod = map(data, ~lme(mean ~ year*TTtreat, random = ~1|Site, data = .x)),
  #          result = map(mod, tidy, "fixed")) %>% 
  #   unnest(result),
  
  #happy higher moment - make long table with var, skew and kurt
  happymoments = bind_rows(
    fixed = sum_boot_moment_fixed,
    plastic = sum_boot_moment_plastic, 
    .id = "plasticity") %>%
    ungroup() %>% 
    select(plasticity:TTtreat, mean, var, skew, kurt) %>% 
    pivot_longer(cols = c(var, skew, kurt), names_to = "happymoment", values_to = "value"),
  

  #using non-parametric test (kruskal-wallies) to test if treatments differ each other? (only last year)
  happymoment_TTtreat = happymoments %>%
    filter(year == 2016,
           plasticity == "fixed") %>% 
      nest(data = -c(trait_trans, happymoment)) %>% 
    mutate(mod = map(data, ~ kruskal.test(value ~ TTtreat, data = .x)),
           result = map(mod, broom::tidy)) %>% 
    unnest(result) %>% 
    select(-data, -mod),
  
  #test difference among treatments if kruskal-wallies is significant
  group_test = happymoment_TTtreat %>% 
    filter(p.value <= 0.05) %>% 
    distinct(trait_trans, happymoment),
  
  happymoments_Diff = happymoments %>%
    filter(year == 2016,
           plasticity == "fixed") %>% 
    inner_join(group_test) %>% 
    nest(data = -c(trait_trans, happymoment)) %>% 
    mutate(mod = map(data, ~ pairwise.wilcox.test(.x$value, .x$TTtreat, p.adjust.method = "BH")),
           result = map(mod, broom::tidy)) %>% 
    unnest(result)

)
