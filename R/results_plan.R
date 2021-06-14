results_plan <- drake_plan(
  
  #trait climate regressions table
  moments_by_climate_table = trait_climate_regression %>% 
    mutate(trait_fancy = factor(trait_fancy, levels = trait_order$trait_fancy)) %>% 
    ungroup() %>% 
    select(trait = trait_fancy, estimate, `standard error`, `t value` = statistic, p = `P value`) %>% 
    mutate(estimate = round(estimate, 2),
           `standard error` = round(`standard error`, 2),
           `t value` = round(`t value`, 2),
           p = round(p, 3),
           'p value' = case_when(p < 0.001 ~ paste("<0.001", "***"),
                                 p < 0.01 ~ paste(p, "**"),
                                 p < 0.05 ~ paste(p, "*"),
                                 p >= 0.05 ~ paste(p, ""))) %>% 
    select(-p),

  #divergence convergence table
  treatment_effect_table = treatment_model %>% 
    fancy_trait_name_dictionary() %>% 
    unnest(result) %>% 
    ungroup() %>% 
    select(direction, plasticity, trait_fancy, term:p.value) %>% 
    mutate(term = str_remove(term, "TTtreat")) %>% 
    mutate(estimate = round(estimate, 2),
           std.error = round(std.error, 2),
           statistic = round(statistic, 2),
           p.value = round(p.value, 3),
           p.value = case_when(p.value < 0.001 ~ paste("<0.001", "***"),
                               p.value < 0.01 ~ paste(p.value, "**"),
                               p.value < 0.05 ~ paste(p.value, "*"),
                               p.value >= 0.05 ~ paste(p.value, ""))) %>% 
    select(plasticity:direction, trait = trait_fancy, term, estimate, "std error" = std.error, "t value" = statistic, "p value" = p.value),
  
  
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
    select(plasticity, site = originSiteID, treatment = term, estimate, "std error" = std.error, "t value" = statistic, `p value`),
  
  #happymoments summary table
  moment_summary = sum_boot_moment_fixed %>% 
    ungroup() %>% 
    select(trait_trans, trait_fancy, year, turfID, destBlockID, destSiteID, TTtreat, mean, var, skew, kurt, range) %>% 
    pivot_longer(cols = c(mean, var, skew, kurt, range), names_to = "moment", values_to = "value") %>% 
    group_by(trait_trans, trait_fancy, destSiteID, TTtreat, moment) %>% 
    summarise(se = sd(value)/sqrt(n()),
              value = mean(value)) %>% 
    mutate(value = paste0(round(value, 2), " ± ", round(se, 2))) %>% 
    ungroup() %>% 
    select(-trait_trans, -se) %>%
    pivot_wider(names_from = moment, values_from = value) %>% 
    select(trait = trait_fancy, "dest. site" = destSiteID, treatment = TTtreat, mean, variance = var, skewness = skew, kurtosis = kurt, range)

  
)
