
plot_plan <- drake_plan(
  
  # trait correlation plot only controls
  trait_corr_plot_c = get_trait_correlations(sum_boot_moment_fixed, control = TRUE) %>% 
    ggplot(aes(x = trait1, y = trait2, fill = r, label = round(r_sig, 2))) +
    geom_tile() +
    labs(x = NULL, y = NULL, 
         title = "Control plots",
         fill = "Pearson's\nCorrelation") +
    scale_fill_gradient2(mid = "#f7f7f7", low = "#f1a340", 
                         high = "#998ec3", 
                         limits = c(-1, 1)) +
    geom_text() +
    theme_minimal() +
    theme(legend.position = "top") +
    scale_x_discrete(expand=c(0,0)) +
    scale_y_discrete(expand=c(0,0)),
  
  # trait correlation plot all data
  trait_corr_plot_all = get_trait_correlations(sum_boot_moment_fixed, control = FALSE) %>% 
    ggplot(aes(x = trait1, y = trait2, fill = r, label = round(r_sig, 2))) +
    geom_tile() +
    labs(x = NULL, y = NULL, 
         title = "Control and treatment plots",
         fill = "Pearson's\nCorrelation") +
    scale_fill_gradient2(mid = "#f7f7f7", low = "#f1a340", 
                         high = "#998ec3", 
                         limits = c(-1, 1)) +
    geom_text() +
    theme_minimal() +
    theme(legend.position = "none") +
    scale_x_discrete(expand=c(0,0)) +
    scale_y_discrete(expand=c(0,0)),
  
  trait_corr = (trait_corr_plot_c / trait_corr_plot_all),
  
  
  ## ----trait-climate
  # without Wet_Mass_g_log to make nice plot with 3x4 panels (same as dry mass anyway)
  # trait order
  trait_order = trait_climate_regression %>% ungroup() %>% select(trait_fancy),
  
  moments_by_climate_plot = summarised_boot_moments_climate %>% 
    filter(year == 2016,
           TTtreat %in% c("control"),
           plasticity == "fixed") %>% 
    ungroup() %>% 
    left_join(trait_climate_regression, by = c("trait_trans", "trait_fancy")) %>% 
    mutate(originSiteID = recode(originSiteID, "H" = "High Alpine", "A" = "Alpine", "M" = "Middle", "L" = "Lowland"),
           trait_fancy = factor(trait_fancy, levels = trait_order$trait_fancy)) %>%
    ggplot(aes(x = value, y = mean, linetype = signi, colour = signi)) +
    geom_point(aes(shape = originSiteID), colour = "grey") +
    geom_smooth(method = "lm", formula = "y ~ x") +
    stat_poly_eq(formula = "y ~ x", aes(label = paste(..rr.label..)), parse = TRUE, colour = "black", label.x = "right", label.y = "bottom", size = 3.3) +
    #geom_text(aes(x = 9, y = 0.5, label = paste0("R^2 = ", adj.r.squared)), colour = "black") +
    scale_linetype_manual(name = "", values = c("dashed", "solid")) +
    scale_colour_manual(name = "", values = c("grey50", "red")) +
    scale_shape_manual(name = "", values = c(17, 16, 15, 18)) +
    labs(y = "Mean trait value", x = "Summer air temperature in °C") +
    facet_wrap(~ trait_fancy, scales = "free_y") +
    theme_minimal() +
    theme(legend.position="top"),
  
  ## ----
  #colonization extinction plot
  ex = textGrob("\u2190 Extinctions", gp = gpar(fontsize = 9), rot = 90),
  col = textGrob(paste0("Colonizations ", "\u2192"), gp = gpar(fontsize = 9), rot = 90),
  legend = tibble(x1 = c(5, 5), x2 = c(6, 6), y1 = c(10.5, 5), y2 = c(10.5, 5),
         TTtreat = c("warm1", "warm1"), t = c("expected","realized"), 
         c = c("blue", "blue"), f = c("blue", "white")),
  colo_extinction_plot = bind_rows(
    extinction = extinction,
    colonization = colonization,
    .id = "process"
  ) %>% 
    group_by(process, TTtreat) %>% 
    summarise(count = mean(n)) %>% 
    mutate(TTtreat = factor(TTtreat, levels = c("local", "cool3", "cool1", "OTC", "warm1", "warm3"))) %>% 
    pivot_wider(names_from = process, values_from = count) %>% 
    ggplot(aes(x = TTtreat)) +
    geom_col(aes(x = TTtreat, y = predicted_nr_colonization, colour = TTtreat), fill = "white", data = predicted) +
    geom_col(aes(x = TTtreat, y = -predicted_nr_extinction, colour = TTtreat), fill = "white", data = predicted) +
    geom_col(aes(y = colonization, fill = TTtreat), alpha = 0.6) +
    geom_col(aes(y = - extinction, fill = TTtreat), alpha = 0.6) +
    scale_fill_manual(name = "", values = c("grey", "blue","lightblue", "orange", "pink", "red")) +
    scale_colour_manual(name = "", values = c("grey", "blue","lightblue", "orange", "pink", "red")) +
    geom_hline(yintercept = 0, colour = "grey") +
    #geom_rect(data = legend, aes(xmin = TTtreat, xmax = TTtreat, ymin = y1, ymax = y2, colour = c, fill = f)) +
    geom_text(data = legend, aes(x = x2, y = y2, label = t), size = 3) +
    labs(x = "", y = "", title = "Number of species") +
    annotation_custom(ex, xmin = 0.1, xmax= 0.1, ymin = -6, ymax = -6) +
    annotation_custom(col, xmin = 0.1, xmax= 0.1, ymin = 8, ymax = 8) +
    coord_cartesian(xlim = c(1, 6), clip="off") +
    theme_minimal() +
    theme(legend.position = "none",
          axis.line = element_line(colour = "black"),
          panel.grid.major.x = element_blank()),
  
  #colonization extinction with abundnace plot 
  legend2 = tibble(x1 = c(5, 5), x2 = c(6, 6), y1 = c(58, 20), y2 = c(58, 20),
                  TTtreat = c("warm1", "warm1"), t = c("expected","realized"), 
                  c = c("blue", "blue"), f = c("blue", "white")),
  colo_ext_abundance_plot = bind_rows(
    extinction = extinction,
    colonization = colonization,
    .id = "process"
  ) %>% 
    group_by(process, TTtreat) %>% 
    summarise(sum = mean(abundance)) %>% 
    mutate(TTtreat = factor(TTtreat, levels = c("local", "cool3", "cool1", "OTC", "warm1", "warm3"))) %>% 
    pivot_wider(names_from = process, values_from = sum) %>% 
    ggplot(aes(x = TTtreat)) +
    geom_col(aes(x = TTtreat, y = predicted_abundance_colonization, colour = TTtreat), fill = "white", data = predicted) +
    geom_col(aes(x = TTtreat, y = -predicted_abundance_extinction, colour = TTtreat), fill = "white", data = predicted) +
    geom_col(aes(y = colonization, fill = TTtreat), alpha = 0.6) +
    geom_col(aes(y = - extinction, fill = TTtreat), alpha = 0.6) +
    scale_fill_manual(name = "", values = c("grey", "blue","lightblue", "orange", "pink", "red")) +
    scale_colour_manual(name = "", values = c("grey", "blue","lightblue", "orange", "pink", "red")) +
    geom_hline(yintercept = 0, colour = "grey") +
    #geom_rect(data = legend, aes(xmin = TTtreat, xmax = TTtreat, ymin = y1, ymax = y2, colour = c, fill = f)) +
    geom_text(data = legend2, aes(x = x2, y = y2, label = t), size = 3) +
    labs(x = "", y = "", title = "Abundance") +
    annotation_custom(ex, xmin = 0.1, xmax= 0.1, ymin = -40, ymax = -40) +
    annotation_custom(col, xmin = 0.1, xmax= 0.1, ymin = 30, ymax = 30) +
    coord_cartesian(xlim = c(1, 6), clip="off") +
    theme_minimal() +
    theme(legend.position = "none",
          axis.line = element_line(colour = "black"),
          panel.grid.major.x = element_blank()),
  
  #colonization extinction over time plot
  col_ext_over_time_plot = bind_rows(
          extinction = extinction_all,
          colonization = colonization_all,
          .id = "process") %>% 
    mutate(TTtreat = factor(TTtreat, levels = c("local", "cool3", "cool1", "OTC", "warm1", "warm3")),
           TTtreat = recode(TTtreat, "control" = "local")) %>% 
    left_join(predicted %>% 
                select(TTtreat:predicted_nr_extinction) %>% 
                pivot_longer(cols = predicted_nr_colonization:predicted_nr_extinction, names_to = c("x", "process"), values_to = "predicted", names_sep = "_nr_"),
              by = c("TTtreat", "process")) %>% 
    mutate(proportion = nr_species / predicted) %>% 
    ggplot(aes(x = year, y = proportion, colour = TTtreat)) +
    #ggplot(aes(x = year, y = nr_species, ymin = nr_species - se, ymax = nr_species + se, colour = TTtreat)) +
    geom_point() + #position = position_dodge(width = 0.15)
    geom_line() +
    #geom_errorbar(position = position_dodge(width = 0.15), width = 0) +
    scale_colour_manual(name = "", values = c("grey", "blue","lightblue", "orange", "pink", "red")) +
    labs(x = "", y = "Proportion") +
    facet_wrap(~ process) +
    theme_minimal(),
  
  
  
  #H1Q2+3: Conv/div (univariate)
  
  # make boxes with predictions
  pred_pos = tibble(direction = c(rep("divergence", 16), rep("convergence", 16)),
                    plasticity = c(rep("fixed", 8), rep("plastic", 8), rep("fixed", 8), rep("plastic", 8)),
                    TTtreat = c(rep(c("warm3", "warm3", "warm1", "warm1", "cool3", "cool3", "cool1", "cool1"), 4)),
                    year = rep(c(2012, 2016), 16),
                    trait_trans = rep("Positive slope", 32),
                    trait_fancy = rep("Positive slope", 32),
                    delta = c(0,1,0,0.5,0,-1,0,-0.5,0,1,0,0.5,0,-1,0,-0.5,-1,0,-0.5,0,1,0,0.5,0,-1,0,-0.5,0,1,0,0.5,0),
                    signi = rep("significant", 32)),
  pred_neg = tibble(direction = c(rep("divergence", 16), rep("convergence", 16)),
                    plasticity = c(rep("fixed", 8), rep("plastic", 8), rep("fixed", 8), rep("plastic", 8)),
                    TTtreat = c(rep(c("warm3", "warm3", "warm1", "warm1", "cool3", "cool3", "cool1", "cool1"), 4)),
                    year = rep(c(2012, 2016), 16),
                    trait_trans = rep("Negative slope", 32),
                    trait_fancy = rep("Negative slope", 32),
                    delta = c(0,-1,0,-0.5,0,1,0,0.5,0,-1,0,-0.5,0,1,0,0.5,1,0,0.5,0,-1,0,-0.5,0,1,0,0.5,0,-1,0,-0.5,0),
                    signi = rep("significant", 32)),
  pred_no = tibble(direction = c(rep("divergence", 4), rep("convergence", 4)),
                   plasticity = c(rep(c("fixed", "fixed", "plastic", "plastic"), 2)),
                   TTtreat = c(rep("none", 8)),
                   year = rep(c(2012, 2016), 4),
                   trait_trans = rep("No sign. slope", 8),
                   trait_fancy = rep("No sign. slope", 8),
                   delta = rep(0, 8),
                   signi = rep("non-signigicant", 8)),
  
  #divergence-convergence plot
    conv_div_plot = fancy_trait_name_dictionary(treatment_effect) %>% 
    bind_rows(pred_pos, pred_neg) %>% 
    # simplify figure by only showing traits with significant slope
    filter(!trait_trans %in% c("SLA_cm2_g", "NP_ratio", "LDMC")) %>%
    mutate(direction = factor(direction, levels = c("divergence", "convergence")),
           trait_fancy = factor(trait_fancy, levels = c("Positive slope", "dN15 ‰", "Area cm2", "Dry mass g", "C %", "Negative slope", "P %", "N %", "Thickness mm", "dC13 ‰", "CN")),
           TTtreat = factor(TTtreat, levels = c("cool3", "cool1", "OTC", "warm1", "warm3"))) %>% 
    ggplot(aes(x = year, y = delta, colour = TTtreat, linetype = signi)) +
    geom_rect(data = . %>% filter(trait_trans %in% c("Positive slope", "Negative slope")), 
              aes(fill = trait_trans), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.6, fill = "grey75", show.legend = FALSE) +
    geom_line() +
    scale_colour_manual(values = c("lightblue", "blue", "orange", "pink", "red"), name = "") +
    scale_linetype_manual(values = c("dashed", "solid"), name = "") +
    scale_x_continuous(breaks = c(2013, 2015)) +
    geom_hline(yintercept = 0, colour = "grey50", linetype = "dotted") +
    labs(x = "Year", y = "Mean trait value") +
    facet_grid(trait_fancy ~ direction * plasticity, scales = "free_y") +
    theme_bw() +
    theme(legend.position = "top",
          strip.text.y = element_text(angle=360),
          panel.grid = element_blank()),
  ## ----
  
  conv_div_no_slope_plot = fancy_trait_name_dictionary(treatment_effect) %>% 
    bind_rows(pred_no) %>% 
    # only traits without significant slope
    filter(trait_trans %in% c("No sign. slope", "SLA_cm2_g", "NP_ratio", "LDMC")) %>%
    mutate(direction = factor(direction, levels = c("divergence", "convergence")),
           trait_fancy = factor(trait_fancy, levels = c("No sign. slope", "SLA cm2/g", "NP", "LDMC", "CN")),
           TTtreat = factor(TTtreat, levels = c("cool3", "cool1", "OTC", "warm1", "warm3"))) %>%
    ggplot(aes(x = year, y = delta, colour = TTtreat, linetype = signi)) +
    geom_rect(data = . %>% filter(trait_trans %in% c("No sign. slope")), 
              aes(fill = trait_trans), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.6, fill = "grey75", show.legend = FALSE) +
    geom_line() +
    scale_colour_manual(values = c("lightblue", "blue", "orange", "pink", "red"), name = "") +
    scale_linetype_manual(values = c("dashed", "solid"), name = "") +
    scale_x_continuous(breaks=c(2013, 2015)) +
    geom_hline(yintercept = 0, colour = "grey50", linetype = "dotted") +
    labs(x = "Year", y = "Mean trait value") +
    facet_grid(trait_fancy ~ direction * plasticity, scales = "free_y") +
    theme_bw() +
    theme(legend.position = "top",
          strip.text.y = element_text(angle=360),
          panel.grid = element_blank()),
  
  
  
  
  #HISTOGRAMM
  hist_warm = sum_boot_moment_fixed %>%
    ungroup() %>%  
    filter(TTtreat %in% c("warm3"),
           trait_trans %in% c("Thickness_mm_log")) %>% 
    ggplot(aes(x = mean, fill = factor(year))) + 
    geom_density(alpha = 0.5) + 
    scale_fill_brewer(palette = "Reds", name = "") +
    labs(title = "Leaf thickness") +
    facet_wrap(~ TTtreat) +
    theme_bw() +
    theme(legend.position = "top",
          legend.key.size = unit(0.3, "cm")),
  
  hist_cool = hist_warm %+% (sum_boot_moment_fixed %>%
                               ungroup() %>%  
                               filter(TTtreat %in% c("cool3"),
                                      trait_trans %in% c("Thickness_mm_log"))) +
    scale_fill_brewer(palette = "Blues", name = "") +
    labs(y = "", title = ""),
  
  temporal_trait_histograms = hist_warm + hist_cool,
  
  
  # Histograms for appendix
  hist_N15_w = hist_warm %+% (sum_boot_moment_fixed %>%
                   ungroup() %>%  
                   filter(TTtreat %in% c("warm3"),
                          trait_trans %in% c("dN15_permil"))) +
    labs(title = "dN15 ‰", x = ""),
  
  hist_N15_c = hist_warm %+% (sum_boot_moment_fixed %>%
                               ungroup() %>%  
                               filter(TTtreat %in% c("cool3"),
                                      trait_trans %in% c("dN15_permil"))) +
    scale_fill_brewer(palette = "Blues", name = "") +
    labs(y = "", title = "", x = ""),
  
  hist_area_w = hist_warm %+% (sum_boot_moment_fixed %>%
                                ungroup() %>%  
                                filter(TTtreat %in% c("warm3"),
                                       trait_trans %in% c("Leaf_Area_cm2_log"))) +
    labs(title = "Area cm2", x = "") +
    theme(legend.position = "none"),
  
  hist_area_c = hist_warm %+% (sum_boot_moment_fixed %>%
                                ungroup() %>%  
                                filter(TTtreat %in% c("cool3"),
                                       trait_trans %in% c("Leaf_Area_cm2_log"))) +
    scale_fill_brewer(palette = "Blues", name = "") +
    labs(y = "", title = "", x = "") +
    theme(legend.position = "none"),
  
  hist_mass_w = hist_warm %+% (sum_boot_moment_fixed %>%
                                 ungroup() %>%  
                                 filter(TTtreat %in% c("warm3"),
                                        trait_trans %in% c("Dry_Mass_g_log"))) +
    labs(title = "Dry mass g") +
    theme(legend.position = "none"),
  
  hist_mass_c = hist_warm %+% (sum_boot_moment_fixed %>%
                                 ungroup() %>%  
                                 filter(TTtreat %in% c("cool3"),
                                        trait_trans %in% c("Dry_Mass_g_log"))) +
    scale_fill_brewer(palette = "Blues", name = "") +
    labs(y = "", title = "") +
    theme(legend.position = "none"),
  
  hist_C_w = hist_warm %+% (sum_boot_moment_fixed %>%
                                 ungroup() %>%  
                                 filter(TTtreat %in% c("warm3"),
                                        trait_trans %in% c("C_percent"))) +
    labs(title = "C %", x = ""),
  
  hist_C_c = hist_warm %+% (sum_boot_moment_fixed %>%
                                 ungroup() %>%  
                                 filter(TTtreat %in% c("cool3"),
                                        trait_trans %in% c("C_percent"))) +
    scale_fill_brewer(palette = "Blues", name = "") +
    labs(y = "", title = "", x = ""),
  
  hist_N_w = hist_warm %+% (sum_boot_moment_fixed %>%
                              ungroup() %>%  
                              filter(TTtreat %in% c("warm3"),
                                     trait_trans %in% c("N_percent"))) +
    labs(title = "N %", x = "") +
    theme(legend.position = "none"),
  
  hist_N_c = hist_warm %+% (sum_boot_moment_fixed %>%
                              ungroup() %>%  
                              filter(TTtreat %in% c("cool3"),
                                     trait_trans %in% c("N_percent"))) +
    scale_fill_brewer(palette = "Blues", name = "") +
    labs(y = "", title = "", x = "") +
    theme(legend.position = "none"),
  
  hist_P_w = hist_warm %+% (sum_boot_moment_fixed %>%
                              ungroup() %>%  
                              filter(TTtreat %in% c("warm3"),
                                     trait_trans %in% c("P_percent"))) +
    labs(title = "P %") +
    theme(legend.position = "none"),
  
  hist_P_c = hist_warm %+% (sum_boot_moment_fixed %>%
                              ungroup() %>%  
                              filter(TTtreat %in% c("cool3"),
                                     trait_trans %in% c("P_percent"))) +
    scale_fill_brewer(palette = "Blues", name = "") +
    labs(y = "", title = "") +
    theme(legend.position = "none"),
  
  hist_C13_w = hist_warm %+% (sum_boot_moment_fixed %>%
                              ungroup() %>%  
                              filter(TTtreat %in% c("warm3"),
                                     trait_trans %in% c("dC13_permil"))) +
    labs(title = "dC13 ‰", x = ""),
  
  hist_C13_c = hist_warm %+% (sum_boot_moment_fixed %>%
                              ungroup() %>%  
                              filter(TTtreat %in% c("cool3"),
                                     trait_trans %in% c("dC13_permil"))) +
    scale_fill_brewer(palette = "Blues", name = "") +
    labs(y = "", title = ""),
  
  hist_CN_w = hist_warm %+% (sum_boot_moment_fixed %>%
                              ungroup() %>%  
                              filter(TTtreat %in% c("warm3"),
                                     trait_trans %in% c("CN_ratio"))) +
    labs(title = "CN", x = "") +
    theme(legend.position = "none"),
  
  hist_CN_c = hist_warm %+% (sum_boot_moment_fixed %>%
                              ungroup() %>%  
                              filter(TTtreat %in% c("cool3"),
                                     trait_trans %in% c("CN_ratio"))) +
    scale_fill_brewer(palette = "Blues", name = "") +
    labs(y = "", title = "") +
    theme(legend.position = "none"),
  
  
  temporal_trait_histograms_all_1 = (hist_mass_w + hist_mass_c) / 
    (hist_area_w + hist_area_c) /
    (hist_C_w + hist_C_c),
  
  temporal_trait_histograms_all_2 = (hist_N_w + hist_N_c) /
    (hist_P_w + hist_P_c) /
    (hist_CN_w + hist_CN_c),
  
  temporal_trait_histograms_all_3 = (hist_C13_w + hist_C13_c) /
    (hist_N15_w + hist_N15_c),
  
  
  #trait coverage
  #trait_coverage = autoplot(imputed_traits_div),
  
  
  
  #trait treatment ordinations - convergence with fixed traits
  ordination_plot = bind_rows(
     moderate = treatment_pca(sum_boot_moment_fixed, "warm1", "cool1")[[1]],
     extreme = treatment_pca(sum_boot_moment_fixed, "warm3", "cool3")[[1]],
     otc = treatment_pca(sum_boot_moment_fixed, "OTC", NULL)[[1]],
     .id = "treatment"
     ) %>% 
    mutate(treatment = fct_relevel(treatment, c("moderate", "extreme", "otc")),
           destSiteID = recode(destSiteID, "H" = "High Alpine", "A" = "Alpine", "M" = "Middle", "L" = "Lowland")) %>% 
    ggplot(aes(x = PC1, y = PC2, colour = TTtreat, shape = destSiteID, group = turfID, linetype = destSiteID)) + 
    geom_point(aes(size = ifelse(year == min(year), "First", "Other"))) +
    geom_path() +
    coord_equal() +
    scale_size_discrete(range = c(1, 2.5), limits = c("Other", "First"), breaks = c("First", "Other")) +
    scale_colour_manual(values = c("grey50", "pink", "lightblue", "red", "blue", "orange")) +
    scale_shape_manual(name = "Site", values = c(17, 16, 15, 18)) +
    scale_x_continuous(expand = c(.15, 0)) +
    labs(x = "PC 1", y = "PC 2", shape = "Site", colour = "Treatment", size = "Year", linetype = "Site", title = "Fixed traits") +
    facet_wrap(~ treatment, ncol = 2) +
    theme_minimal(),
  
  sum_boot_moment_fixed_fancy = fancy_trait_name_dictionary(sum_boot_moment_fixed) %>% 
    mutate(trait_trans = trait_fancy) %>% 
    select(-trait_fancy),
  arrows = ggplot(treatment_pca(sum_boot_moment_fixed, "warm1", "cool1")[[1]], aes(x = PC1, y = PC2, group = turfID)) +
    geom_segment(data = treatment_pca(sum_boot_moment_fixed_fancy, "warm1", "cool1")[[2]], 
                 aes(x = 0, y = 0, xend = PC1, yend = PC2), 
                 arrow = arrow(length = unit(0.2, "cm")), 
                 colour = "grey50",
                 inherit.aes = FALSE) +
    geom_text(data = treatment_pca(sum_boot_moment_fixed_fancy, "warm1", "cool1")[[2]], 
              aes(x = PC1 * 1.1,y = PC2 * 1.1, label = Label), 
              size = 3,
              inherit.aes = FALSE, colour = "grey50") +
    labs(x = "", y = "") +
    scale_x_continuous(expand = c(.2, 0)) +
    theme_minimal(),
  
  ordination = cowplot::ggdraw(ordination_plot) +
    cowplot::draw_plot(arrows, .43, 0.04, .33, .43),
    
  
  
## HIGHER MOMENTS

deviation = sum_boot_moment_fixed %>%
  filter(!trait_trans %in% c("SLA_cm2_g", "NP_ratio", "LDMC", "dN15_permil"),
         year == 2016,
         TTtreat != "control") %>%
  ungroup() %>%
  select(originSiteID:TTtreat, mean, -year) %>%
  group_by(trait_trans, TTtreat) %>%
  mutate(global_mean = mean(mean),
         deviation = (mean - global_mean) / global_mean * 100),

  # Trait-climate-skewness
  change_skew = sum_boot_moment_fixed %>% 
  # remove non-slope traits
  filter(!trait_trans %in% c("SLA_cm2_g", "NP_ratio", "LDMC", "dN15_permil")) %>%
  ungroup() %>% 
  select(-c(global, n, mean:ci_high_var, ci_low_skew:range)) %>%
  mutate(block = substr(originBlockID, 2, 2)) %>% 
  filter(year %in% c(2012, 2016),
         TTtreat != "control",
         !block %in% c("6", "7")) %>%
  pivot_wider(names_from = year, values_from = skew, names_prefix = "Y") %>% 
  mutate(delta = Y2016 - Y2012) %>% 
  inner_join(treatment_effect %>% 
               filter(signi == "significant") %>% 
               ungroup() %>% 
               distinct(trait_trans, TTtreat), by = c("trait_trans", "TTtreat")) %>% 
  left_join(deviation, by = c("originSiteID", "originBlockID", "trait_trans", 
                              "turfID", "destBlockID", "destSiteID", "TTtreat")),

# scaled_mean = sum_boot_moment_fixed %>% 
#   ungroup() %>% 
#   select(originSiteID:TTtreat, trait_fancy, mean, -year) %>% 
#   group_by(originSiteID, originBlockID, trait_trans, turfID, destBlockID, destSiteID, TTtreat, trait_fancy) %>% 
#   mutate(mean_sc = scale(mean)[,1]) %>% 
#   group_by(trait_trans, TTtreat, trait_fancy) %>% 
#   summarise(mean_sc = mean(mean_sc)),

# change_skew <- sum_boot_moment_fixed %>% 
#   ungroup() %>% 
#   select(originSiteID:TTtreat, skew) %>% 
#   # remove non-slope traits
#   filter(!trait_trans %in% c("SLA_cm2_g", "NP_ratio", "LDMC"),
#          year %in% c(2012, 2016),
#          TTtreat != "control") %>%
#   pivot_wider(names_from = year, values_from = skew, names_prefix = "Y") %>% 
#   mutate(delta = Y2016 - Y2012) %>% 
#   inner_join(treatment_effect %>% 
#                filter(signi == "significant") %>% 
#                ungroup() %>% 
#                distinct(trait_trans, TTtreat), by = c("trait_trans", "TTtreat")) %>% 
#   inner_join(scaled_mean, by = c("originSiteID", "originBlockID", "trait_trans", "turfID", "destBlockID", "destSiteID", "TTtreat")),


skew_warm = change_skew %>% 
  filter(TTtreat %in% c("warm1", "warm3", "OTC")) %>% 
  group_by(trait_trans, trait_fancy, TTtreat) %>% 
  summarise(delta = mean(delta, na.rm = TRUE),
            deviation = mean(deviation)) %>% 
  ggplot(aes(x = deviation, y = delta, colour = TTtreat)) +
  annotate("rect", xmin = 0, xmax = Inf, ymin = -Inf, 
           ymax = 0, alpha = 0.05, fill = "red") +
  annotate("rect", xmin = -Inf, xmax = 0, ymin = 0, 
           ymax = Inf, alpha = 0.05, fill = "red") +
  geom_text(aes(x = deviation - 0.4, y = delta + 0.05, label = trait_fancy, 
                colour = TTtreat), show.legend = FALSE) +
  geom_point() +
  geom_hline(yintercept = 0, colour = "grey", linetype = "dashed") +
  geom_vline(xintercept = 0, colour = "grey", linetype = "dashed") +
  #scale_shape_manual(values = c(1, 16)) +
  scale_colour_manual(values = c("pink1", "red", "orange")) +
  labs(x = "% deviation in mean \n from the gradient mean", y = "Change in skewness") +
  theme_minimal() +
  theme(legend.position = "top"),

skew_cool = change_skew %>% 
  filter(TTtreat %in% c("cool1", "cool3")) %>% 
  group_by(trait_trans, trait_fancy, TTtreat) %>% 
  summarise(delta = mean(delta, na.rm = TRUE),
            deviation = mean(deviation, na.rm = TRUE)) %>% 
  bind_cols(text_space = c(0, 0, -0.5, 0, 0.5, 0, 0, 0)) %>% 
  mutate(text_location = deviation + text_space) %>% 
  ggplot(aes(x = deviation, y = delta, colour = TTtreat)) +
  annotate("rect", xmin = -Inf, xmax = 0, ymin = 0, 
           ymax = -Inf, alpha = 0.1, fill = "blue") +
  annotate("rect", xmin = 0, xmax = Inf, ymin = 0, 
           ymax = Inf, alpha = 0.1, fill = "blue") +
  #geom_text(aes(x = text_location, y = delta + 0.1, label = trait_fancy, colour = TTtreat), show.legend = FALSE) +
  geom_point() +
  geom_hline(yintercept = 0, colour = "grey", linetype = "dashed") +
  geom_vline(xintercept = 0, colour = "grey", linetype = "dashed") +
  #scale_shape_manual(values = c(1, 16)) +
  scale_colour_manual(values = c("lightblue", "blue")) +
  labs(x = "% deviation in mean \n from the gradient mean", y = "") +
  theme_minimal()  +
  theme(legend.position = "top"),

skewness_plot = skew_warm + skew_cool,




happymoment_data = sum_boot_moment_fixed %>% 
  pivot_longer(cols = c(mean, var, skew, kurt, range), names_to = "happymoment", values_to = "value") %>% 
    filter(trait_trans %in% c("dN15_permil", "Leaf_Area_cm2_log", "Thickness_mm_log", "P_percent", "dC13_permil")) %>%
  mutate(happymoment = recode(happymoment, "var" = "variance", "skew" = "skewness", "kurt" = "kurtosis")) %>% 
  group_by(TTtreat, trait_trans, trait_fancy, happymoment, year) %>%
  summarise(mean = mean(value),
            se = sd(value, na.rm = TRUE)/sqrt(n())) %>% 
  mutate(trait_fancy = factor(trait_fancy, levels = c("Area cm2", "Thickness mm", "P %", "dC13 ‰", "dN15 ‰"))) %>%
  left_join(treatment_effect %>% 
              filter(signi == "significant") %>% 
              ungroup() %>% 
              select(trait_trans, TTtreat, signi), by = c("TTtreat", "trait_trans")) %>% 
  mutate(signi = if_else(is.na(signi), "non-significant", signi)),

  mean_plot = happymoment_data %>% 
  filter(happymoment == "mean") %>%
  mutate(TTtreat = factor(TTtreat, levels = c("control", "cool1", "cool3", "OTC", "warm1", "warm3"))) %>% 
  ggplot(aes(x = year, y = mean, ymin = mean - se, ymax = mean + se, colour = TTtreat, linetype = signi, alpha = signi)) +
  geom_line() +
  geom_errorbar(position = position_dodge(width = 0.15), width = 0) +
  scale_x_continuous(labels = NULL) +
  labs(x = "", y = "Mean") +
  scale_colour_manual(name = "", values = c("grey", "lightblue", "blue", "orange", "pink", "red")) +
  scale_linetype_manual(name = "", values = c("dashed", "solid")) +
  scale_alpha_manual(name = "", values = c(0.4, 1)) +
  scale_y_continuous(breaks = scales::breaks_extended(n = 4)) +
  guides(colour = guide_legend(nrow = 1)) +
  facet_wrap(~ trait_fancy, nrow = 1, scales = "free_y") +
  theme_minimal() +
  theme(legend.position = "top",
        legend.margin = margin(t = 0, unit = "cm"),
        plot.margin = unit(c(0.1,0.1,-0.3,0.5), "cm"),
        text = element_text(size = 10),
        strip.text.x = element_text(size = 7)),

  var_plot = mean_plot %+% (happymoment_data %>% 
                              filter(happymoment == "variance")) + 
    labs(x = "", y = "Variance") +
    theme(legend.position = "none",
          strip.text.x = element_blank()),
  
  skew_plot = mean_plot %+% (happymoment_data %>% 
                               filter(happymoment == "skewness"))  +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey") +
    labs(x = "", y = "Skewness") +
    theme(legend.position = "none",
          strip.text.x = element_blank()),

  skew_plot2 = skew_plot + 
  scale_x_continuous(breaks = c(2013, 2015), minor_breaks = c(2012, 2014, 2016)) +
  labs(x = "Year"),

  kurt_plot = mean_plot %+% (happymoment_data %>% 
                               filter(happymoment == "kurtosis")) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey") +
    labs(x = "", y = "Kurtosis") +
    theme(legend.position = "none",
          strip.text.x = element_blank()),

range_plot = mean_plot %+% (happymoment_data %>% 
                             filter(happymoment == "range")) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey") +
  scale_x_continuous(breaks = c(2013, 2015), minor_breaks = c(2012, 2014, 2016)) +
  labs(x = "Year", y = "Range") +
  theme(legend.position = "none",
        strip.text.x = element_blank()),

half_happymoment_plot = mean_plot / skew_plot2 + plot_layout(guides = "collect") & theme(legend.position = "top"),

full_happymoment_plot = mean_plot / var_plot / skew_plot / kurt_plot /range_plot + plot_layout(guides = "collect") & theme(legend.position = "top")
  
)
