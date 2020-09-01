#data analysis plan

analysis_plan <- drake_plan(
  #calculate effect size (control - treatment per block)
  effect_size = summarised_boot_moments_climate %>% 
    ungroup() %>% 
    select(-global, -n, -turfID, -c(CIlow.mean:CIhigh.Kurt)) %>%
    group_by(direction, year, trait_trans, TTtreat, Site, blockID) %>% 
    summarise(mean = mean(Mean, na.rm = TRUE)) %>% 
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
  # need only conv or div, because only the control plots will be the same.
  trait_climate_regression = summarised_boot_moments_climate %>% 
    ungroup() %>% 
    filter(year == 2016,
           TTtreat %in% c("control"),
           direction == "convergence") %>% 
    select(Site:Mean, variable, value, -n) %>% 
    nest(data = -c(trait_trans)) %>% 
    mutate(mod = map(data, ~lme(Mean ~ value, random = ~1|Site, data = .x)),
           result = map(mod, tidy, "fixed")) %>%
    unnest(result) %>% 
    mutate(term = plyr::mapvalues(term, from = c("(Intercept)", "value"),
                                  to = c("intercept", "slope"))),
  
  
  #effect of experiments across all elevations (only 2016)
  treatment_effect = effect_size %>%
    filter(year == 2016) %>% 
    nest(data = -c(direction, trait_trans)) %>% 
    mutate(mod = map(data, ~lme(mean ~ TTtreat, random = ~1|Site, data = .x)),
           result = map(mod, tidy, "fixed")) %>% 
    unnest(result) %>% 
    mutate(term = plyr::mapvalues(term, from = c("(Intercept)", "TTtreatcool3", "TTtreatOTC", "TTtreatwarm1", "TTtreatwarm3"),
                                  to = c("cool1", "cool3", "OTC", "warm1", "warm3"))),
  
  
  #effect of experiments by elevations
  treatment_by_site = effect_size %>% 
    filter(year == 2016) %>% 
    nest(data = -c(direction, trait_trans)) %>% 
    mutate(mod = map(data, ~lm(mean ~ TTtreat*Site, data = .x)),
           result = map(mod, tidy)) %>% 
    unnest(result),
  
  
  #effect of experiment over time
  treatment_time_effect = effect_size %>% 
    nest(data = -c(direction, trait_trans)) %>% 
    mutate(mod = map(data, ~lme(mean ~ year*TTtreat, random = ~1|Site, data = .x)),
           result = map(mod, tidy, "fixed")) %>% 
    unnest(result)
)
