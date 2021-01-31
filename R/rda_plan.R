#make rda
rda_plan <- drake_plan(
  
  ## species community
  
  #warm
  comm_warm_fat = community %>% 
    select(-speciesName, -Genus) %>% 
    filter(TTtreat %in% c("control", "warm1", "warm3", "OTC")) %>%
    filter(originSiteID == "H" | originSiteID == "L" & TTtreat == "control") %>% 
    # # remove rare species
    # group_by(species) %>%
    # filter(n() > 3) %>% 
    pivot_wider(names_from = species, values_from = cover, values_fill = list(cover = 0)) %>% 
    mutate(TTtreat = case_when(TTtreat == "control" & originSiteID == "L" ~ "control_L",
                               TRUE ~ as.character(TTtreat)),
           year = factor(year),
           TTtreat = factor(TTtreat, levels = c("control", "OTC", "warm1", "warm3", "control_L"))),
  
  #prc
  fit_warm_comm = prc(response = comm_warm_fat %>% select(And.min:Ane.riv), 
                          treatment = comm_warm_fat$TTtreat, 
                          time = comm_warm_fat$year, 
                          scale = TRUE),
  
  #make plots
  wc2 = autoplot.prcWithoutSP(fit_warm_comm, xlab = "", ylab = "Treatment effect on \n  species composition") +
    scale_colour_manual(values = c("orange", "pink2", "red", "red")) +
    scale_linetype_manual(values = c("solid", "dashed", "dashed", "solid")) +
    scale_y_continuous(breaks = pretty(fortify(fit_warm_comm)$Response, n = 5)) +
    ggtitle("species community") +
    theme_minimal() +
    theme(legend.position = c(0.2, 0.7),
          legend.title = element_blank(),
          text = element_text(size = 8)),
  
  wc3 = fortify(fit_warm_comm) %>% 
    filter(Score == "Species") %>% 
    mutate(X = 1) %>% 
    ggplot(aes(x = X, y = (Response), label = Label)) +
    geom_text(aes(x = X), size = 1.8) +
    geom_hline(yintercept = 0) +
    scale_y_continuous(breaks = pretty(fortify(fit_warm_comm)$Response, n = 5)) +
    labs(x = "", y = "") +
    theme(panel.background = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank()),

  
  #cool
  comm_cool_fat = community %>% 
    select(-speciesName, -Genus) %>% 
    filter(TTtreat %in% c("control", "cool1", "cool3")) %>%
    filter(originSiteID == "L" | originSiteID == "H" & TTtreat == "control") %>% 
    # # remove rare species
    # group_by(species) %>%
    # filter(n() > 3) %>% 
    pivot_wider(names_from = species, values_from = cover, values_fill = list(cover = 0)) %>% 
    mutate(TTtreat = case_when(TTtreat == "control" & originSiteID == "H" ~ "control_H",
                               TRUE ~ as.character(TTtreat)),
           year = factor(year),
           TTtreat = factor(TTtreat, levels = c("control", "cool1", "cool3", "control_H"))),
  
  #prc
  fit_cool_comm = prc(response = comm_cool_fat %>% select(Ale.pau:Myr.nep), 
                      treatment = comm_cool_fat$TTtreat, 
                      time = comm_cool_fat$year, 
                      scale = TRUE),
  
  #make plots
  cc2 = autoplot.prcWithoutSP(fit_cool_comm, xlab = "", ylab = "Treatment effect on \n  species composition") +
    scale_colour_manual(values = c("steelblue2", "blue", "blue")) +
    scale_linetype_manual(values = c("dashed", "dashed", "solid")) +
    scale_y_continuous(breaks = pretty(fortify(fit_cool_comm)$Response, n = 5)) +
    #ggtitle("species community") +
    theme_minimal() +
    theme(legend.position = c(0.2, 0.6),
          legend.title = element_blank(),
          text = element_text(size = 8)),
  
  cc3 = fortify(fit_cool_comm) %>% 
    filter(Score == "Species") %>% 
    mutate(X = 1) %>% 
    ggplot(aes(x = X, y = (Response), label = Label)) +
    geom_text(aes(x = X), size = 1.8) +
    geom_hline(yintercept = 0) +
    scale_y_continuous(breaks = pretty(fortify(fit_cool_comm)$Response, n = 5)) +
    labs(x = "", y = "") +
    theme(panel.background = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank()),
  
  
  
  
  ## traits
  
  #warming plot - fixed
  #make fat table
  trait_warm_fat_fixed = sum_boot_moment_fixed %>% 
    ungroup() %>% 
    select(originSiteID:trait_fancy, mean, -trait_trans) %>%
    filter(TTtreat %in% c("control", "warm1", "warm3", "OTC")) %>%
    filter(originSiteID == "H" | originSiteID == "L" & TTtreat == "control") %>% 
    spread(key = trait_fancy, value = mean, fill = 0) %>% 
    mutate(TTtreat = case_when(TTtreat == "control" & originSiteID == "L" ~ "control_L",
                               TRUE ~ as.character(TTtreat)),
           year = factor(year),
           TTtreat = factor(TTtreat, levels = c("control", "OTC", "warm1", "warm3", "control_L"))),
  
  #prc
  fit_Warming_fixed = prc(response = trait_warm_fat_fixed %>% select(C:Thickness), 
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
    theme(legend.position = "none",
          legend.title = element_blank(),
          text = element_text(size = 8)),
  
  wf3 = fortify(fit_Warming_fixed) %>% 
    filter(Score == "Species") %>% 
    mutate(X = 1) %>% 
    ggplot(aes(x = X, y = (Response), label = Label)) +
    geom_text(aes(x = X), size = 1.8) +
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
    select(destSiteID:trait_fancy, mean, -trait_trans) %>%
    filter(TTtreat %in% c("control", "warm1", "warm3", "OTC")) %>%
    filter(originSiteID == "H" | originSiteID == "L" & TTtreat == "control") %>% 
    spread(key = trait_fancy, value = mean, fill = 0) %>% 
    mutate(TTtreat = case_when(TTtreat == "control" & originSiteID == "L" ~ "control_L",
                               TRUE ~ as.character(TTtreat)),
           year = factor(year),
           TTtreat = factor(TTtreat, levels = c("control", "OTC", "warm1", "warm3", "control_L"))),
  
  #prc
  fit_Warming_plastic = prc(response = trait_warm_fat_plastic %>% select(C:Thickness),
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
    geom_text(aes(x = X), size = 1.8) +
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
    select(originSiteID:trait_fancy, mean, -trait_trans) %>%
    filter(TTtreat %in% c("control", "cool1", "cool3")) %>%
    filter(originSiteID == "L" | originSiteID == "H" & TTtreat == "control") %>% 
    distinct() %>% 
    spread(key = trait_fancy, value = mean, fill = 0) %>% 
    mutate(TTtreat = case_when(TTtreat == "control" & originSiteID == "H" ~ "control_H",
                               TRUE ~ as.character(TTtreat)),
           year = factor(year),
           TTtreat = factor(TTtreat, levels = c("control", "cool1", "cool3", "control_H"))),
  
  #prc
  fit_Cool_fixed = prc(response = trait_cool_fat_fixed %>% select(C:Thickness),
                       treatment = trait_cool_fat_fixed$TTtreat,
                       time = trait_cool_fat_fixed$year,
                       scale = TRUE),
  
  #make plots
  cf2 = autoplot.prcWithoutSP(fit_Cool_fixed, xlab = "", ylab = "") +
    scale_colour_manual(values = c("steelblue2", "blue", "blue")) +
    scale_linetype_manual(values = c("dashed", "dashed", "solid")) +
    scale_y_continuous(breaks = pretty(fortify(fit_Cool_fixed)$Response, n = 5)) +
    #ggtitle("fixed") +
    theme_minimal() +
    theme(legend.position = "none",
          legend.title = element_blank(),
          text = element_text(size = 8)),
  
  cf3 = fortify(fit_Cool_fixed) %>% 
    filter(Score == "Species") %>% 
    mutate(X = 1) %>% 
    ggplot(aes(x = X, y = Response, label = Label)) +
    geom_text(aes(x = X), size = 1.8) +
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
    select(destSiteID:trait_fancy, mean, -trait_trans) %>%
    filter(TTtreat %in% c("control", "cool1", "cool3")) %>%
    filter(originSiteID == "L" | originSiteID == "H" & TTtreat == "control") %>% 
    distinct() %>% 
    spread(key = trait_fancy, value = mean, fill = 0) %>% 
    mutate(TTtreat = case_when(TTtreat == "control" & originSiteID == "H" ~ "control_H",
                               TRUE ~ as.character(TTtreat)),
           year = factor(year),
           TTtreat = factor(TTtreat, levels = c("control", "cool1", "cool3", "control_H"))),
  
  #prc
  fit_Cool_plastic = prc(response = trait_cool_fat_plastic %>% select(C:Thickness),
                         treatment = trait_cool_fat_plastic$TTtreat,
                         time = trait_cool_fat_plastic$year,
                         scale = TRUE),
  
  #make plots
  cp2 = autoplot.prcWithoutSP(fit_Cool_plastic, xlab = "", ylab = "") +
    scale_colour_manual(values = c("steelblue2", "blue", "blue")) +
    scale_linetype_manual(values = c("dashed", "dashed", "solid")) +
    scale_y_continuous(breaks = pretty(fortify(fit_Cool_plastic)$Response, n = 5)) +
    #ggtitle("plastic") +
    theme_minimal() +
    theme(legend.position = "none",
          text = element_text(size = 8)),
  
  cp3 = fortify(fit_Cool_plastic) %>% 
    filter(Score == "Species") %>% 
    mutate(X = 1) %>% 
    ggplot(aes(x = X, y = Response, label = Label)) +
    geom_text(aes(x = X), size = 1.8) +
    geom_hline(yintercept = 0) +
    scale_y_continuous(breaks = pretty(fortify(fit_Cool_plastic)$Response, n = 5)) +#, trans = "reverse") +
    labs(x = "", y = "") +
    theme(panel.background = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank()),

  #patchwork together
  TraitRDA = ((wc2 + wf2 + wf3 + wp2 + wp3) + plot_layout(widths = c(4, 4, 1, 4, 1))) / ((cc2 + cf2 + cf3 + cp2 + cp3) + plot_layout(widths = c(4, 4, 1, 4, 1))),

  
  #permutation test
  #warm
  #comm
  rda_warm_c = comm_warm_fat %>% 
    filter(TTtreat != "control_L") %>% 
    mutate(datatype = "community") %>% 
    nest(data = -datatype) %>% 
    mutate(mod = map(data, ~ prc(response = .x %>% select(And.min:Ane.riv),
                                 treatment = .x$TTtreat,
                                 time = .x$year,
                                 scale = TRUE))) %>% 
    mutate(res = map(mod, anova),
           Variance = map(res, "Variance") %>% map_dbl(1),
           Fvalue = map(res, "F") %>% map_dbl(1),
           Pvalue = map(res, "Pr(>F)") %>% map_dbl(1)) %>% 
    select(-data, -mod, -res),
  
  #trait  
  rda_warm_t = bind_rows(
    fixed = trait_warm_fat_fixed,
    plastic = trait_warm_fat_plastic,
    .id = "datatype"
  ) %>% 
    filter(TTtreat != "control_L") %>% 
    nest(data = -datatype) %>% 
    mutate(mod = map(data, ~ prc(response = .x %>% select(C:Thickness),
                                       treatment = .x$TTtreat,
                                       time = .x$year,
                                       scale = TRUE))) %>% 
    mutate(res = map(mod, anova),
           Variance = map(res, "Variance") %>% map_dbl(1),
           Fvalue = map(res, "F") %>% map_dbl(1),
           Pvalue = map(res, "Pr(>F)") %>% map_dbl(1)) %>% 
    select(-data, -mod, -res),
  
  #cool
  #comm
  rda_cool_c = comm_cool_fat %>% 
    filter(TTtreat != "control_H") %>% 
    mutate(datatype = "community") %>% 
    nest(data = -datatype) %>% 
    mutate(mod = map(data, ~ prc(response = .x %>% select(Ale.pau:Myr.nep),
                                 treatment = .x$TTtreat,
                                 time = .x$year,
                                 scale = TRUE))) %>% 
    mutate(res = map(mod, anova),
           Variance = map(res, "Variance") %>% map_dbl(1),
           Fvalue = map(res, "F") %>% map_dbl(1),
           Pvalue = map(res, "Pr(>F)") %>% map_dbl(1)) %>% 
    select(-data, -mod, -res),
  
  #trait  
  rda_cool_t = bind_rows(
    fixed = trait_warm_fat_fixed,
    plastic = trait_warm_fat_plastic,
    .id = "datatype"
  ) %>% 
    filter(TTtreat != "control_H") %>% 
    nest(data = -datatype) %>% 
    mutate(mod = map(data, ~ prc(response = .x %>% select(C:Thickness),
                                 treatment = .x$TTtreat,
                                 time = .x$year,
                                 scale = TRUE))) %>% 
    mutate(res = map(mod, anova),
           Variance = map(res, "Variance") %>% map_dbl(1),
           Fvalue = map(res, "F") %>% map_dbl(1),
           Pvalue = map(res, "Pr(>F)") %>% map_dbl(1)) %>% 
    select(-data, -mod, -res),
  
  # combine all results
  result_rda = tibble(climate = c(rep("warm", 3), rep("cool", 3))) %>% 
    bind_cols(
      bind_rows(
        comm_warm = rda_warm_c, 
        trait_warm = rda_warm_t, 
        comm_cool = rda_cool_c, 
        trait_cool = rda_cool_t
      )
    )
  
)
