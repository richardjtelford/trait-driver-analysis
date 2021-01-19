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
  
  #happy higher moment - treatment and time
  happymoments = bind_rows(
    fixed = sum_boot_moment_fixed,
    plastic = sum_boot_moment_plastic, 
    .id = "plasticity") %>%
    ungroup() %>% 
    select(plasticity:destSiteID, mean, var, skew, kurt) %>% 
    pivot_longer(cols = c(var, skew, kurt), names_to = "happymoment", values_to = "value"),
    
    happymoment_effect = happymoments %>% 
      nest(data = -c(plasticity, trait_trans, happymoment)) %>% 
    mutate(mod = map(data, ~ lm(value ~ TTtreat*year, data = .x)),
           result = map(mod, tidy)) %>% 
    unnest(result) %>% 
    mutate(term = plyr::mapvalues(term, from = c("(Intercept)", "TTtreatwarm1", "TTtreatcool1", "TTtreatcool3", "TTtreatwarm3", "TTtreatOTC", "year", "TTtreatwarm1:year", "TTtreatcool1:year", "TTtreatwarm3:year", "TTtreatcool3:year", "TTtreatOTC:year"),
                                  to = c("Tcontrol", "Twarm1", "Tcool1", "Tcool3", "Twarm3", "TOTC", "control", "warm1", "cool1", "warm3", "cool3", "OTC")),
           signi = if_else(p.value < 0.05, "significant", "non-signigicant")) %>% 
    select(-data, -mod)
    
  
)
