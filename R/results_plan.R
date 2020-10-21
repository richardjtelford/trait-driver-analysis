results_plan <- drake_plan(
  
  #trait climate regressions table
  moments_by_climate_table = trait_climate_regression %>% 
    mutate(trait_trans = factor(trait_trans, levels = c("dN15_permil", "Wet_Mass_g_log", "Leaf_Area_cm2_log", "Dry_Mass_g_log", "C_percent", "SLA_cm2_g", "NP_ratio", "LDMC", "P_percent", "N_percent", "dC13_permil", "Thickness_mm_log", "CN_ratio"))) %>% 
    select(trait_trans, term:p.value) %>% 
    mutate(estimate = round(estimate, 2),
           std.error = round(std.error, 2),
           statistic = round(statistic, 2),
           p.value = round(p.value, 3),
           p.value = case_when(p.value < 0.001 ~ paste(p.value, "***"),
                               p.value < 0.01 ~ paste(p.value, "**"),
                               p.value < 0.05 ~ paste(p.value, "*"),
                               p.value >= 0.05 ~ paste(p.value, ""))) %>% 
    knitr::kable(),
  
  
  #divergence convergence table
  treatment_effect_table = treatment_effect %>% 
    select(direction, trait_trans, term:p.value) %>% 
    mutate(term = plyr::mapvalues(term, from = c("Tcool1", "Tcool3", "TOTC", "Twarm1", "Twarm3", "cool1", "cool3", "OTC", "warm1", "warm3"),
                                  to = c("Tcool1", "Tcool3", "TOTC", "Twarm1", "Twarm3", "cool1*year", "cool3*year", "OTC*year", "warm1*year", "warm3*year"))) %>%
    mutate(estimate = round(estimate, 2),
           std.error = round(std.error, 2),
           statistic = round(statistic, 2),
           p.value = round(p.value, 3),
           p.value = case_when(p.value < 0.001 ~ paste(p.value, "***"),
                               p.value < 0.01 ~ paste(p.value, "**"),
                               p.value < 0.05 ~ paste(p.value, "*"),
                               p.value >= 0.05 ~ paste(p.value, ""))) %>% 
    knitr::kable(),
  
  
  #euclidean distance table
  euclidean_dist_table = results_distance %>% 
    select(direction.row, Site.row, term:p.value) %>% 
    rename(direction = direction.row, Site = Site.row) %>% 
    mutate(term = plyr::mapvalues(term, from = c("(Intercept)", "TTtreat.rowcool1", "TTtreat.rowcool3", "TTtreat.rowwarm1", "TTtreat.rowwarm3", "TTtreat.rowOTC"),
                                  to = c("Intercept", "cool1", "cool3", "warm1", "warm3", "OTC"))) %>% 
    mutate(estimate = round(estimate, 2),
           std.error = round(std.error, 2),
           statistic = round(statistic, 2),
           p.value = round(p.value, 3),
           p.value = case_when(p.value < 0.001 ~ paste(p.value, "***"),
                               p.value < 0.01 ~ paste(p.value, "**"),
                               p.value < 0.05 ~ paste(p.value, "*"),
                               p.value >= 0.05 ~ paste(p.value, ""))) %>% 
    knitr::kable()
  
)
