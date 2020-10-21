#make rda
rda_plan <- drake_plan(
  
  #warming plot
  #make fat table
  trait_warm_fat = imputed_traits_div %>% 
    ungroup() %>% 
    filter(TTtreat %in% c("control", "warm1", "warm3", "OTC")) %>%
    filter(Site == "H" | Site == "L" & TTtreat == "control") %>% 
    distinct() %>% 
    spread(key = trait_trans, value = value_trans, fill = 0) %>% 
    mutate(TTtreat = case_when(TTtreat == "control" & Site == "L" ~ "control_L",
                               TRUE ~ as.character(TTtreat)),
           year = factor(year),
           TTtreat = factor(TTtreat, levels = c("control", "OTC", "warm1", "warm3", "control_L"))),
  
  #only traits
  trait_warm_data = trait_warm_fat %>% 
    select(C_percent:Wet_Mass_g_log),
  
  #prc
  fit_Warming = prc(response = trait_warm_data, treatment = trait_warm_fat$TTtreat, time = trait_warm_fat$year, scale = TRUE),
  
  #make plots
  w1 = autoplot.prcCustom(fit_Warming, xlab = "", ylab = "Effect of treatment", legend.position="top") +
    scale_colour_manual(values = c("orange", "pink2", "red", "red")) +
    scale_linetype_manual(values = c("solid", "dashed", "dashed", "solid")) +
    scale_y_continuous(breaks = pretty(fortify(fit_Warming)$Response, n = 5)),
  
  w2 = autoplot.prcWithoutSP(fit_Warming, xlab = "", ylab = "Treatment effect on \n  trait composition") +
    scale_colour_manual(values = c("orange", "pink2", "red", "red")) +
    scale_linetype_manual(values = c("solid", "dashed", "dashed", "solid")) +
    scale_y_continuous(breaks = pretty(fortify(fit_Warming)$Response, n = 5)) +
    theme(legend.position = c(0.2, 0.2), legend.title = element_blank()),
  
  w3 = fortify(fit_Warming) %>% 
    filter(Score == "Species") %>% 
    mutate(X = 1) %>% 
    ggplot(aes(x = X, y = (Response), label = Label)) +
    geom_text(aes(x = X), size = 4) +
    geom_hline(yintercept = 0) +
    scale_y_continuous(breaks = pretty(fortify(fit_Warming)$Response, n = 5)) +
    labs(x = "", y = "") +
    theme(panel.background = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank()),
  
  prcLegend = cowplot::get_legend(w1),
  
  TraitRDA_Warming = gridExtra::grid.arrange(w2, w3,
                                              layout_matrix = rbind(c(2,2,2,2,2,3,3))),
  
  
  #cooling plot
  #fat table
  trait_cool_fat = imputed_traits_div %>% 
    ungroup() %>% 
    filter(TTtreat %in% c("control", "cool1", "cool3")) %>%
    filter(Site == "L" | Site == "H" & TTtreat == "control") %>% 
    distinct() %>% 
    spread(key = trait_trans, value = value_trans, fill = 0) %>% 
    mutate(TTtreat = case_when(TTtreat == "control" & Site == "H" ~ "control_H",
                               TRUE ~ as.character(TTtreat)),
           year = factor(year),
           TTtreat = factor(TTtreat, levels = c("control", "cool1", "cool3", "control_H"))),
  
  #only traits
  trait_cool_data <- trait_cool_fat %>% select(C_percent:Wet_Mass_g_log),
  
  #prc
  fit_Cool = prc(response = trait_cool_data, treatment = trait_cool_fat$TTtreat, time = trait_cool_fat$year, scale = TRUE),
  
  #make plots
  c2 = autoplot.prcWithoutSP(fit_Cool, xlab = "", ylab = "") +
    scale_colour_manual(values = c("steelblue2", "blue", "blue")) +
    scale_linetype_manual(values = c("dashed", "dashed", "solid")) +
    scale_y_continuous(breaks = pretty(fortify(fit_Cool)$Response, n = 5)) +
    theme(legend.position = c(0.7, 0.85), legend.title = element_blank()),
  
  c3 = fortify(fit_Cool) %>% 
    filter(Score == "Species") %>% 
    mutate(X = 1) %>% 
    ggplot(aes(x = X, y = Response, label = Label)) +
    geom_text(aes(x = X), size = 4) +
    geom_hline(yintercept = 0) +
    scale_y_continuous(breaks = pretty(fortify(fit_Cool)$Response, n = 5)) +#, trans = "reverse") +
    labs(x = "", y = "") +
    theme(panel.background = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank()),
  
  TraitRDA_Cooling = gridExtra::grid.arrange(p2, p3,
                                              layout_matrix = rbind(c(1,1,1,1,1,2,2))),
  
  #patchwork together
  TraitRDA = TraitRDA_Warming + TraitRDA_Cooling
)