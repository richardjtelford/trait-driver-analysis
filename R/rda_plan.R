#make rda
rda_plan <- drake_plan(
  
  ## species community
  
  #warm
  comm_warm_fat = community %>% 
    select(-speciesName, -Genus) %>% 
    filter(TTtreat %in% c("control", "warm1", "warm3", "OTC")) %>%
    filter(originSiteID == "H" | originSiteID == "L" & TTtreat == "control") %>% 
    # remove rare species
    group_by(species) %>%
    filter(n() > 3) %>%
    pivot_wider(names_from = species, values_from = cover, values_fill = list(cover = 0)) %>% 
    mutate(TTtreat = case_when(TTtreat == "control" & originSiteID == "L" ~ "control (Lowland)",
                               TRUE ~ as.character(TTtreat)),
           year = factor(year),
           TTtreat = factor(TTtreat, levels = c("control", "OTC", "warm1", "warm3", "control (Lowland)"))),
  
  #prc
  fit_warm_comm = prc(response = comm_warm_fat %>% select(-c(originSiteID:flag)), 
                          treatment = comm_warm_fat$TTtreat, 
                          time = comm_warm_fat$year, 
                          scale = TRUE),
  
  #make plots
  wc2 = autoplot.prcWithoutSP(fit_warm_comm, xlab = "", ylab = "Treatment effect on \n  species composition") +
    scale_colour_manual(values = c("orange", "pink", "red", "grey")) +
    scale_y_continuous(breaks = pretty(fortify(fit_warm_comm)$Response, n = 5)) +
    ggtitle("species community") +
    theme_minimal() +
    theme(legend.position = "bottom",
          legend.key.size = unit(0.3, "cm"),
          legend.title = element_blank()),
  
  #cool
  comm_cool_fat = community %>% 
    select(-speciesName, -Genus) %>% 
    filter(TTtreat %in% c("control", "cool1", "cool3")) %>%
    filter(originSiteID == "L" | originSiteID == "H" & TTtreat == "control") %>% 
    # # remove rare species
    # group_by(species) %>%
    # filter(n() > 3) %>% 
    pivot_wider(names_from = species, values_from = cover, values_fill = list(cover = 0)) %>% 
    mutate(TTtreat = case_when(TTtreat == "control" & originSiteID == "H" ~ "control (High Alpine)",
                               TRUE ~ as.character(TTtreat)),
           year = factor(year),
           TTtreat = factor(TTtreat, levels = c("control", "cool1", "cool3", "control (High Alpine)"))),
  
  #prc
  fit_cool_comm = prc(response = comm_cool_fat %>% select(-c(originSiteID:flag)), 
                      treatment = comm_cool_fat$TTtreat, 
                      time = comm_cool_fat$year, 
                      scale = TRUE),
  
  #make plots
  cc2 = autoplot.prcWithoutSP(fit_cool_comm, xlab = "", ylab = "Treatment effect on \n  species composition") +
    scale_colour_manual(values = c("steelblue2", "blue", "grey")) +
    scale_y_continuous(breaks = pretty(fortify(fit_cool_comm)$Response, n = 5)) +
    theme_minimal() +
    theme(legend.position = "bottom",
          legend.key.size = unit(0.3, "cm"),
          legend.title = element_blank()),

  
  ## traits
  
  #warming plot - fixed
  #make fat table
  trait_warm_fat_fixed = sum_boot_moment_fixed %>% 
    ungroup() %>% 
    select(originSiteID:TTtreat, trait_fancy, mean, -trait_trans) %>%
    filter(TTtreat %in% c("control", "warm1", "warm3", "OTC")) %>%
    filter(originSiteID == "H" | originSiteID == "L" & TTtreat == "control") %>% 
    spread(key = trait_fancy, value = mean, fill = 0) %>% 
    mutate(TTtreat = case_when(TTtreat == "control" & originSiteID == "L" ~ "control (Lowland)",
                               TRUE ~ as.character(TTtreat)),
           year = factor(year),
           TTtreat = factor(TTtreat, levels = c("control", "OTC", "warm1", "warm3", "control (Lowland)"))),
  
  #prc
  fit_Warming_fixed = prc(response = trait_warm_fat_fixed %>% select(`C %`:`SLA cm2/g`), 
                          treatment = trait_warm_fat_fixed$TTtreat, 
                          time = trait_warm_fat_fixed$year, 
                          scale = TRUE),
  
  #make plots
  wf2 = autoplot.prcWithoutSP(fit_Warming_fixed, xlab = "", ylab = "Treatment effect on \n  trait composition") +
    scale_colour_manual(values = c("orange", "pink", "red", "grey")) +
    scale_y_continuous(breaks = pretty(fortify(fit_Warming_fixed)$Response, n = 5)) +
    ggtitle("fixed traits") +
    theme_minimal() +
    theme(legend.position = "none"),
  
  wf3 = fortify(fit_Warming_fixed) %>% 
    filter(Score == "Species") %>% 
    mutate(X = 1,
           Label = recode(Label, "Dry mass g" = "mass", "Leaf area cm2" = "area", "SLA cm2/g" = "SLA", "Thickness mm" = "thickness")) %>% 
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
    select(year:TTtreat, mean, trait_fancy, destSiteID, destBlockID) %>%
    filter(TTtreat %in% c("control", "warm1", "warm3", "OTC")) %>%
    filter(originSiteID == "H" | originSiteID == "L" & TTtreat == "control") %>% 
    spread(key = trait_fancy, value = mean, fill = 0) %>% 
    mutate(TTtreat = case_when(TTtreat == "control" & originSiteID == "L" ~ "control (Lowland)",
                               TRUE ~ as.character(TTtreat)),
           year = factor(year),
           TTtreat = factor(TTtreat, levels = c("control", "OTC", "warm1", "warm3", "control (Lowland)"))),
  
  #prc
  fit_Warming_plastic = prc(response = trait_warm_fat_plastic %>% select(`C %`:`SLA cm2/g`),
                            treatment = trait_warm_fat_plastic$TTtreat, 
                            time = trait_warm_fat_plastic$year, 
                            scale = TRUE),
  
  #make plots
  wp2 = autoplot.prcWithoutSP(fit_Warming_plastic, xlab = "", ylab = "") +
    scale_colour_manual(values = c("orange", "pink2", "red", "grey")) +
    scale_y_continuous(breaks = pretty(fortify(fit_Warming_plastic)$Response, n = 5)) +
    ggtitle("plastic traits") +
    theme_minimal() +
    theme(legend.position = "none"),
  
  wp3 = fortify(fit_Warming_plastic) %>% 
    filter(Score == "Species") %>% 
    mutate(X = 1,
           Label = recode(Label, "Dry mass g" = "mass", "Leaf area cm2" = "area", "SLA cm2/g" = "SLA", "Thickness mm" = "thickness")) %>% 
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
    select(originSiteID:TTtreat, trait_fancy, mean, -trait_trans) %>%
    filter(TTtreat %in% c("control", "cool1", "cool3")) %>%
    filter(originSiteID == "L" | originSiteID == "H" & TTtreat == "control") %>% 
    distinct() %>% 
    spread(key = trait_fancy, value = mean, fill = 0) %>% 
    mutate(TTtreat = case_when(TTtreat == "control" & originSiteID == "H" ~ "control (High Alpine)",
                               TRUE ~ as.character(TTtreat)),
           year = factor(year),
           TTtreat = factor(TTtreat, levels = c("control", "cool1", "cool3", "control (High Alpine)"))),
  
  #prc
  fit_Cool_fixed = prc(response = trait_cool_fat_fixed %>% select(`C %`:`SLA cm2/g`),
                       treatment = trait_cool_fat_fixed$TTtreat,
                       time = trait_cool_fat_fixed$year,
                       scale = TRUE),
  
  #make plots
  cf2 = autoplot.prcWithoutSP(fit_Cool_fixed, xlab = "", ylab = "Treatment effect on \n  trait composition") +
    scale_colour_manual(values = c("steelblue2", "blue", "grey")) +
    scale_y_continuous(breaks = pretty(fortify(fit_Cool_fixed)$Response, n = 5)) +
    #ggtitle("fixed") +
    theme_minimal() +
    theme(legend.position = "none"),
  
  cf3 = fortify(fit_Cool_fixed) %>% 
    filter(Score == "Species") %>% 
    mutate(X = 1,
           Label = recode(Label, "Dry mass g" = "mass", "Leaf area cm2" = "area", "SLA cm2/g" = "SLA", "Thickness mm" = "thickness")) %>% 
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
    select(year:TTtreat, trait_fancy, mean, destSiteID, destBlockID) %>%
    filter(TTtreat %in% c("control", "cool1", "cool3")) %>%
    filter(originSiteID == "L" | originSiteID == "H" & TTtreat == "control") %>% 
    distinct() %>% 
    spread(key = trait_fancy, value = mean, fill = 0) %>% 
    mutate(TTtreat = case_when(TTtreat == "control" & originSiteID == "H" ~ "control (High Alpine)",
                               TRUE ~ as.character(TTtreat)),
           year = factor(year),
           TTtreat = factor(TTtreat, levels = c("control", "cool1", "cool3", "control (High Alpine)"))),
  
  #prc
  fit_Cool_plastic = prc(response = trait_cool_fat_plastic %>% select(`C %`:`SLA cm2/g`),
                         treatment = trait_cool_fat_plastic$TTtreat,
                         time = trait_cool_fat_plastic$year,
                         scale = TRUE),
  
  #make plots
  cp2 = autoplot.prcWithoutSP(fit_Cool_plastic, xlab = "", ylab = "") +
    scale_colour_manual(values = c("steelblue2", "blue", "grey")) +
    scale_y_continuous(breaks = pretty(fortify(fit_Cool_plastic)$Response, n = 5)) +
    #ggtitle("plastic") +
    theme_minimal() +
    theme(legend.position = "none"),
  
  cp3 = fortify(fit_Cool_plastic) %>% 
    filter(Score == "Species") %>% 
    mutate(X = 1,
           Label = recode(Label, "Dry mass g" = "mass", "Leaf area cm2" = "area", "SLA cm2/g" = "SLA", "Thickness mm" = "thickness")) %>% 
    ggplot(aes(x = X, y = Response, label = Label)) +
    geom_text(aes(x = X), size = 1.8) +
    geom_hline(yintercept = 0) +
    scale_y_continuous(breaks = pretty(fortify(fit_Cool_plastic)$Response, n = 5)) +#, trans = "reverse") +
    labs(x = "", y = "") +
    theme(panel.background = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank()),

  #patchwork together
  # TraitRDA = ((wc2 + wf2 + wf3 + wp2 + wp3) + 
  #               guide_area() +
  #               plot_layout(widths = c(4, 4, 1, 4, 1),
  #                           guides = 'keep', heights = c(10, 0.3))) / ((cc2 + cf2 + cf3 + cp2 + cp3) +
  #                                                  guide_area() +
  #                                                     plot_layout(widths = c(4, 4, 1, 4, 1), 
  #                                                                 guides = 'keep', heights = c(10, 0.3))),
  
  #permutation test
  #warm
  #comm
  rda_warm_c = comm_warm_fat %>% 
    filter(TTtreat != "control_L") %>% 
    mutate(datatype = "community") %>% 
    nest(data = -datatype) %>% 
    mutate(mod = map(data, ~ prc(response = .x %>% select(-c(originSiteID:flag)),
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
    mutate(mod = map(data, ~ prc(response = .x %>% select(`C %`:`SLA cm2/g`),
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
    mutate(mod = map(data, ~ prc(response = .x %>% select(-c(originSiteID:flag)),
                                 treatment = .x$TTtreat,
                                 time = .x$year,
                                 scale = TRUE))) %>% 
    mutate(res = map(mod, anova),
           variance = map(res, "Variance") %>% map_dbl(1),
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
    mutate(mod = map(data, ~ prc(response = .x %>% select(`C %`:`SLA cm2/g`),
                                 treatment = .x$TTtreat,
                                 time = .x$year,
                                 scale = TRUE))) %>% 
    mutate(res = map(mod, anova),
           variance = map(res, "Variance") %>% map_dbl(1),
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
    ) %>% 
    rename("F value" = Fvalue, "p value" = Pvalue),
  
  
  # RDA propotion converged
  rda_proportions = bind_rows(warm_comm_none = fortify(fit_warm_comm),
            cool_comm_none = fortify(fit_cool_comm),
            warm_trait_fixed = fortify(fit_Warming_fixed), 
            warm_trait_plastic = fortify(fit_Warming_plastic),
            cool_trait_fixed = fortify(fit_Cool_fixed),
            cool_trait_plastic = fortify(fit_Cool_plastic),
            .id = "treatment_response_process") %>% 
    filter(Time == 2016) %>% 
    separate(treatment_response_process, into = c("climate", "variable", "process"), sep = "_") %>% 
    group_by(climate, variable, process) %>% 
    mutate(top = Response[n()],
           proportion = Response/top) %>% 
    filter(!Label %in% c("control_L|2016", "control_H|2016")) %>% 
    mutate(process = if_else(process == "none", "", process),
           variable = if_else(variable == "comm", "community", variable),
           variable = if_else(variable == "trait", " trait", variable),
           variable = paste0(process, variable)) %>% 
    ungroup() %>% 
    select(variable, treatment = Treatment, proportion) %>% 
    filter(!grepl("control", treatment)) %>% 
    pivot_wider(names_from = treatment, values_from = proportion)
  
)
