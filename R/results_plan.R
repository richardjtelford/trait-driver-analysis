results_plan <- drake_plan(
  
  #trait climate regressions table
  moments_by_climate_table = trait_climate_regression %>% 
    mutate(trait_trans = factor(trait_trans, levels = c("dN15_permil", "Wet_Mass_g_log", "Leaf_Area_cm2_log", "Dry_Mass_g_log", "C_percent", "SLA_cm2_g", "NP_ratio", "LDMC", "P_percent", "N_percent", "dC13_permil", "Thickness_mm_log", "CN_ratio"))) %>% 
    select(trait_trans, term:p.value),
  
  
  #divergence convergence table
  treatment_effect_table = treatment_effect %>% 
    select(direction, trait_trans, term:p.value) %>% 
    mutate(term = plyr::mapvalues(term, from = c("Tcool1", "Tcool3", "TOTC", "Twarm1", "Twarm3", "cool1", "cool3", "OTC", "warm1", "warm3"),
                                  to = c("Tcool1", "Tcool3", "TOTC", "Twarm1", "Twarm3", "cool1*year", "cool3*year", "OTC*year", "warm1*year", "warm3*year"))),
  
  
  sheets = list("Table_2X" = moments_by_climate_table, "Table_4X" = treatment_effect_table),
  writexl::write_xlsx(x = sheets, path = "TDT_results_2020.xlsx", col_names = TRUE)
  
)