#data analysis plan

bootstrap_moment_plan <- drake_plan(
  #effect size
  effect_size = summarised_boot_moments_climate %>% 
    filter(year %in% c(2016),
           direction == "convergence") %>% 
    select(Site:Mean, -n, -turfID) %>%
    group_by(trait_trans, TTtreat, Site, blockID) %>% 
    summarise(mean = mean(Mean, na.rm = TRUE)) %>% 
    pivot_wider(names_from = "TTtreat", values_from = "mean") %>% 
    mutate(warm1 = warm1 - control,
           cool1 = cool1 - control,
           OTC = OTC - control,
           warm3 = warm3 - control,
           cool3 = cool3 - control) %>% 
    select(-control) %>% 
    pivot_longer(cols = c(warm1:OTC), names_to = "TTtreat", values_to = "mean"),
  
  #effect of gradient
  trait_climate_regression = summarised_boot_moments_climate %>% 
    filter(year == 2012,
           TTtreat %in% c("control"),
           direction == "convergence") %>% 
    select(Site:Mean, variable, value, -n) %>% 
    ungroup() %>% 
    nest(data = -c(trait_trans)) %>% 
    mutate(mod = map(data, ~lme(Mean ~ value, random = ~1|Site, data = .x)),
           result = map(mod, tidy, "fixed")) %>%
    unnest(result) %>% 
    filter(term != "(Intercept)"),
  
  #effect of experiments across all elevations
  treatment_effect = summarised_boot_moments_climate %>% 
    filter(year == 2016,
           direction == "convergence") %>% 
    select(Site:Mean, variable, value, -n) %>% 
    nest(data = -c(trait_trans)) %>% 
    mutate(mod = map(data, ~lme(Mean ~ TTtreat, random = ~1|Site, data = .x)),
           result = map(mod, tidy, "fixed")) %>% 
    unnest(result) %>% 
    filter(term != "(Intercept)"),
  
  
  #effect of experiments by elevations
  treatment_by_site = summarised_boot_moments_climate %>% 
    filter(year == 2016,
           direction == "convergence") %>% 
    select(Site:Mean, variable, value, -n) %>% 
    nest(data = -c(trait_trans)) %>% 
    mutate(mod = map(data, ~lm(Mean ~ TTtreat*Site, data = .x)),
           result = map(mod, tidy)) %>% 
    unnest(result) %>% 
    filter(term != "(Intercept)"),
  
  
  #effect of time
  time_effect <- summarised_boot_moments_climate %>% 
    filter(direction == "convergence") %>% 
    select(Site:Mean, variable, value, -n) %>% 
    nest(data = -c(trait_trans)) %>% 
    mutate(mod = map(data, ~lme(Mean ~ year, random = ~1|Site, data = .x)),
           result = map(mod, tidy, "fixed")) %>% 
    unnest(result) %>% 
    filter(term != "(Intercept)")
)