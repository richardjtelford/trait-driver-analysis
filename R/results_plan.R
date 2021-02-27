results_plan <- drake_plan(
  
  #trait climate regressions table
  moments_by_climate_table = trait_climate_regression %>% 
    mutate(trait_fancy = factor(trait_fancy, levels = trait_order$trait_fancy)) %>% 
    ungroup() %>% 
    select(trait_fancy, estimate:'P value') %>% 
    rename(p = 'P value') %>% 
    mutate(estimate = round(estimate, 2),
           `standard error` = round(`standard error`, 2),
           statistic = round(statistic, 2),
           p = round(p, 3),
           'P value' = case_when(p < 0.001 ~ paste("<0.001", "***"),
                                 p < 0.01 ~ paste(p, "**"),
                                 p < 0.05 ~ paste(p, "*"),
                                 p >= 0.05 ~ paste(p, ""))) %>% 
    select(-p),

  #divergence convergence table
  treatment_effect_table = treatment_effect %>% 
    select(direction, trait_trans, term:p.value) %>% 
    mutate(term = plyr::mapvalues(term, from = c("Tcool1", "Tcool3", "TOTC", "Twarm1", "Twarm3", "cool1", "cool3", "OTC", "warm1", "warm3"),
                                  to = c("Tcool1", "Tcool3", "TOTC", "Twarm1", "Twarm3", "cool1*year", "cool3*year", "OTC*year", "warm1*year", "warm3*year"))) %>%
    mutate(estimate = round(estimate, 2),
           std.error = round(std.error, 2),
           statistic = round(statistic, 2),
           p.value = round(p.value, 3),
           "p value" = case_when(p.value < 0.001 ~ paste("<0.001", "***"),
                               p.value < 0.01 ~ paste(p.value, "**"),
                               p.value < 0.05 ~ paste(p.value, "*"),
                               p.value >= 0.05 ~ paste(p.value, ""))) %>% 
    select(-p.value),
  
  
  #euclidean distance table
  euclidean_dist_table = euclidean_results %>% 
    select(plasticity, originSiteID.row, term:p.value) %>% 
    rename(originSiteID = originSiteID.row) %>% 
    mutate(term = plyr::mapvalues(term, from = c("(Intercept)", "TTtreat.rowcool1", "TTtreat.rowcool3", "TTtreat.rowwarm1", "TTtreat.rowwarm3", "TTtreat.rowOTC"),
                                  to = c("Intercept", "cool1", "cool3", "warm1", "warm3", "OTC"))) %>% 
    mutate(estimate = round(estimate, 2),
           std.error = round(std.error, 2),
           statistic = round(statistic, 2),
           p.value = round(p.value, 3),
           "p value" = case_when(p.value < 0.001 ~ paste("<0.001", "***"),
                                 p.value < 0.01 ~ paste(p.value, "**"),
                                 p.value < 0.05 ~ paste(p.value, "*"),
                                 p.value >= 0.05 ~ paste(p.value, ""))) %>% 
    select(-p.value),
  
  #happymoments results table
  # happymoment_effect_table = happymoment_effect %>% 
  #   select(plasticity:happymoment, term:p.value) %>% 
  #   mutate(term = plyr::mapvalues(term, from = c("Tcontrol", "Tcool1", "Tcool3", "TOTC", "Twarm1", "Twarm3", "control", "cool1", "cool3", "OTC", "warm1", "warm3"),
  #                                 to = c("control", "cool1", "cool3", "OTC", "warm1", "warm3", "control*year", "cool1*year", "cool3*year", "OTC*year", "warm1*year", "warm3*year"))) %>%
  #   mutate(estimate = round(estimate, 2),
  #          std.error = round(std.error, 2),
  #          statistic = round(statistic, 2),
  #          p.value = round(p.value, 3),
  #          p.value = case_when(p.value < 0.001 ~ paste(p.value, "***"),
  #                              p.value < 0.01 ~ paste(p.value, "**"),
  #                              p.value < 0.05 ~ paste(p.value, "*"),
  #                              p.value >= 0.05 ~ paste(p.value, ""))) %>% 
  #   knitr::kable()
  
)
