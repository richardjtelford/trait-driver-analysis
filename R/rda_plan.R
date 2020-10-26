#make rda
rda_plan <- drake_plan(
  
  #warming plot - fixed
  #make fat table
  trait_warm_fat_fixed = sum_boot_moment_fixed %>% 
    ungroup() %>% 
    select(originSiteID:destSiteID, mean) %>%
    filter(TTtreat %in% c("control", "warm1", "warm3", "OTC")) %>%
    filter(originSiteID == "H" | originSiteID == "L" & TTtreat == "control") %>% 
    spread(key = trait_trans, value = mean, fill = 0) %>% 
    mutate(TTtreat = case_when(TTtreat == "control" & originSiteID == "L" ~ "control_L",
                               TRUE ~ as.character(TTtreat)),
           year = factor(year),
           TTtreat = factor(TTtreat, levels = c("control", "OTC", "warm1", "warm3", "control_L"))),
  
  #prc
  fit_Warming_fixed = prc(response = trait_warm_fat_fixed %>% select(C_percent:Wet_Mass_g_log), 
                          treatment = trait_warm_fat_fixed$TTtreat, 
                          time = trait_warm_fat_fixed$year, 
                          scale = TRUE),
  
  #make plots
  wf2 = autoplot.prcWithoutSP(fit_Warming_fixed, xlab = "", ylab = "Treatment effect on \n  trait composition") +
    scale_colour_manual(values = c("orange", "pink2", "red", "red")) +
    scale_linetype_manual(values = c("solid", "dashed", "dashed", "solid")) +
    scale_y_continuous(breaks = pretty(fortify(fit_Warming_fixed)$Response, n = 5)) +
    ggtitle("fixed") +
    theme_minimal() +
    theme(legend.position = c(0.2, 0.8),
          legend.title = element_blank(),
          text = element_text(size = 8)),
  
  wf3 = fortify(fit_Warming_fixed) %>% 
    filter(Score == "Species") %>% 
    mutate(X = 1) %>% 
    ggplot(aes(x = X, y = (Response), label = Label)) +
    geom_text(aes(x = X), size = 1.5) +
    geom_hline(yintercept = 0) +
    scale_y_continuous(breaks = pretty(fortify(fit_Warming_fixed)$Response, n = 5)) +
    labs(x = "", y = "") +
    theme(panel.background = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank()),
  
  
  #warming plot - plastic
  #make fat table
  trait_warm_fat_plastic = sum_boot_moment_plastic %>% 
    ungroup() %>% 
    select(originSiteID:destSiteID, mean) %>%
    filter(TTtreat %in% c("control", "warm1", "warm3", "OTC")) %>%
    filter(originSiteID == "H" | originSiteID == "L" & TTtreat == "control") %>% 
    spread(key = trait_trans, value = mean, fill = 0) %>% 
    mutate(TTtreat = case_when(TTtreat == "control" & originSiteID == "L" ~ "control_L",
                               TRUE ~ as.character(TTtreat)),
           year = factor(year),
           TTtreat = factor(TTtreat, levels = c("control", "OTC", "warm1", "warm3", "control_L"))),
  
  #prc
  fit_Warming_plastic = prc(response = trait_warm_fat_plastic %>% select(C_percent:Wet_Mass_g_log),
                            treatment = trait_warm_fat_plastic$TTtreat, 
                            time = trait_warm_fat_plastic$year, 
                            scale = TRUE),
  
  #make plots
  wp2 = autoplot.prcWithoutSP(fit_Warming_plastic, xlab = "", ylab = "Treatment effect on \n  trait composition") +
    scale_colour_manual(values = c("orange", "pink2", "red", "red")) +
    scale_linetype_manual(values = c("solid", "dashed", "dashed", "solid")) +
    scale_y_continuous(breaks = pretty(fortify(fit_Warming_plastic)$Response, n = 5)) +
    ggtitle("plastic") +
    theme_minimal() +
    theme(legend.position = "none",
          text = element_text(size = 8)),
  
  wp3 = fortify(fit_Warming_plastic) %>% 
    filter(Score == "Species") %>% 
    mutate(X = 1) %>% 
    ggplot(aes(x = X, y = (Response), label = Label)) +
    geom_text(aes(x = X), size = 1.5) +
    geom_hline(yintercept = 0) +
    scale_y_continuous(breaks = pretty(fortify(fit_Warming_plastic)$Response, n = 5)) +
    labs(x = "", y = "") +
    theme(panel.background = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank()),
  
  
  #cooling plot - fixed
  #fat table
  trait_cool_fat_fixed = sum_boot_moment_fixed %>% 
    ungroup() %>% 
    select(originSiteID:destSiteID, mean) %>%
    filter(TTtreat %in% c("control", "cool1", "cool3")) %>%
    filter(originSiteID == "L" | originSiteID == "H" & TTtreat == "control") %>% 
    distinct() %>% 
    spread(key = trait_trans, value = mean, fill = 0) %>% 
    mutate(TTtreat = case_when(TTtreat == "control" & originSiteID == "H" ~ "control_H",
                               TRUE ~ as.character(TTtreat)),
           year = factor(year),
           TTtreat = factor(TTtreat, levels = c("control", "cool1", "cool3", "control_H"))),
  
  #prc
  fit_Cool_fixed = prc(response = trait_cool_fat_fixed %>% select(C_percent:Wet_Mass_g_log),
                       treatment = trait_cool_fat_fixed$TTtreat,
                       time = trait_cool_fat_fixed$year,
                       scale = TRUE),
  
  #make plots
  cf2 = autoplot.prcWithoutSP(fit_Cool_fixed, xlab = "", ylab = "") +
    scale_colour_manual(values = c("steelblue2", "blue", "blue")) +
    scale_linetype_manual(values = c("dashed", "dashed", "solid")) +
    scale_y_continuous(breaks = pretty(fortify(fit_Cool_fixed)$Response, n = 5)) +
    ggtitle("fixed") +
    theme_minimal() +
    theme(legend.position = c(0.2, 0.4), 
          legend.title = element_blank(),
          text = element_text(size = 8)),
  
  cf3 = fortify(fit_Cool_fixed) %>% 
    filter(Score == "Species") %>% 
    mutate(X = 1) %>% 
    ggplot(aes(x = X, y = Response, label = Label)) +
    geom_text(aes(x = X), size = 1.5) +
    geom_hline(yintercept = 0) +
    scale_y_continuous(breaks = pretty(fortify(fit_Cool_fixed)$Response, n = 5)) +#, trans = "reverse") +
    labs(x = "", y = "") +
    theme(panel.background = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank()),

  #cooling plot - plastic
  #fat table
  trait_cool_fat_plastic = sum_boot_moment_plastic %>% 
    ungroup() %>% 
    select(originSiteID:destSiteID, mean) %>%
    filter(TTtreat %in% c("control", "cool1", "cool3")) %>%
    filter(originSiteID == "L" | originSiteID == "H" & TTtreat == "control") %>% 
    distinct() %>% 
    spread(key = trait_trans, value = mean, fill = 0) %>% 
    mutate(TTtreat = case_when(TTtreat == "control" & originSiteID == "H" ~ "control_H",
                               TRUE ~ as.character(TTtreat)),
           year = factor(year),
           TTtreat = factor(TTtreat, levels = c("control", "cool1", "cool3", "control_H"))),
  
  #prc
  fit_Cool_plastic = prc(response = trait_cool_fat_plastic %>% select(C_percent:Wet_Mass_g_log),
                         treatment = trait_cool_fat_plastic$TTtreat,
                         time = trait_cool_fat_plastic$year,
                         scale = TRUE),
  
  #make plots
  cp2 = autoplot.prcWithoutSP(fit_Cool_plastic, xlab = "", ylab = "") +
    scale_colour_manual(values = c("steelblue2", "blue", "blue")) +
    scale_linetype_manual(values = c("dashed", "dashed", "solid")) +
    scale_y_continuous(breaks = pretty(fortify(fit_Cool_plastic)$Response, n = 5)) +
    ggtitle("plastic") +
    theme_minimal() +
    theme(legend.position = "none",
          text = element_text(size = 8)),
  
  cp3 = fortify(fit_Cool_plastic) %>% 
    filter(Score == "Species") %>% 
    mutate(X = 1) %>% 
    ggplot(aes(x = X, y = Response, label = Label)) +
    geom_text(aes(x = X), size = 1.5) +
    geom_hline(yintercept = 0) +
    scale_y_continuous(breaks = pretty(fortify(fit_Cool_plastic)$Response, n = 5)) +#, trans = "reverse") +
    labs(x = "", y = "") +
    theme(panel.background = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank()),

  #patchwork together
  TraitRDA = ((wf2 + wf3 + wp2 + wp3) + plot_layout(widths = c(4, 1, 4, 1))) / ((cf2 + cf3 + cp2 + cp3) + plot_layout(widths = c(4, 1, 4, 1))),

  
  #permutation test
  #warm
  rda_warm = bind_rows(
    fixed = trait_warm_fat_fixed,
    plastic = trait_warm_fat_plastic,
    .id = "Plasticity"
  ) %>% 
    filter(TTtreat != "control_L") %>% 
    nest(data = -Plasticity) %>% 
    mutate(mod = map(data, ~ prc(response = .x %>% select(C_percent:Wet_Mass_g_log),
                                       treatment = .x$TTtreat,
                                       time = .x$year,
                                       scale = TRUE))) %>% 
    mutate(res = map(mod, anova),
           Variance = map(res, "Variance") %>% map_dbl(1),
           Fvalue = map(res, "F") %>% map_dbl(1),
           Pvalue = map(res, "Pr(>F)") %>% map_dbl(1)) %>% 
    select(-data, -mod, -res),
  
  #cool
  rda_cool = bind_rows(
    fixed = trait_warm_fat_fixed,
    plastic = trait_warm_fat_plastic,
    .id = "Plasticity"
  ) %>% 
    filter(TTtreat != "control_H") %>% 
    nest(data = -Plasticity) %>% 
    mutate(mod = map(data, ~ prc(response = .x %>% select(C_percent:Wet_Mass_g_log),
                                 treatment = .x$TTtreat,
                                 time = .x$year,
                                 scale = TRUE))) %>% 
    mutate(res = map(mod, anova),
           Variance = map(res, "Variance") %>% map_dbl(1),
           Fvalue = map(res, "F") %>% map_dbl(1),
           Pvalue = map(res, "Pr(>F)") %>% map_dbl(1)) %>% 
    select(-data, -mod, -res)
  
  
  
  
  
  
  # #warm - fixed
  # trait_warm_fat_fixed2 = trait_warm_fat_fixed %>% 
  #   filter(TTtreat != "control_L"),
  # 
  # perm_warm_fixed = anova(prc(response = trait_warm_fat_fixed2 %>% select(-(originSiteID:destSiteID)),
  #                             treatment = trait_warm_fat_fixed2$TTtreat,
  #                             time = trait_warm_fat_fixed2$year,
  #                             scale = TRUE)),
  # 
  # #warm - plastic
  # trait_warm_fat_plastic2 = trait_warm_fat_plastic %>% 
  #   filter(TTtreat != "control_L"),
  # 
  # perm_warm_plastic = anova(prc(response = trait_warm_fat_plastic2 %>% select(-(originSiteID:destSiteID)),
  #                               treatment = trait_warm_fat_plastic2$TTtreat,
  #                               time = trait_warm_fat_plastic2$year,
  #                               scale = TRUE)),
  # 
  # #cool - fixed
  # trait_cool_fat_fixed2 = trait_cool_fat_fixed %>% 
  #   filter(TTtreat != "control_H"),
  # 
  # perm_cool_fixed = anova(prc(response = trait_cool_fat_fixed2 %>% select(-(originSiteID:destSiteID)),
  #                             treatment = trait_cool_fat_fixed2$TTtreat,
  #                             time = trait_cool_fat_fixed2$year,
  #                             scale = TRUE)),
  # 
  # #cool - plastic
  # trait_cool_fat_plastic2 = trait_cool_fat_plastic %>% 
  #   filter(TTtreat != "control_H"),
  # 
  # perm_cool_plastic = anova(prc(response = trait_cool_fat_plastic2 %>% select(-(originSiteID:destSiteID)),
  #                               treatment = trait_cool_fat_plastic2$TTtreat,
  #                               time = trait_cool_fat_plastic2$year,
  #                               scale = TRUE))
  
)
