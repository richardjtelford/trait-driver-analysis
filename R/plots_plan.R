
plot_plan <- drake_plan(
  
  ## ----trait-climate
  
  #moments by climate in original plots
  trait_order = trait_climate_regression %>% 
    filter(term == "slope") %>% 
    select(traits, estimate, `P value`) %>% 
    mutate(signi = case_when(`P value` < 0.05 ~ "significant",
                             `P value` > 0.05 ~ "non-significant"),
           slope = case_when(`P value` < 0.05 & estimate > 0 ~ "positive slope",
                             `P value` < 0.05 & estimate < 0 ~ "negative slope",
                             `P value` > 0.05 ~ "no slope"),
           slope = factor(slope, levels = c("positive slope", "no slope", "negative slope"))) %>% 
    arrange(slope, desc(estimate)),
  
  # without Wet_Mass_g_log to make nice plot with 3x4 panels (same as dry mass anyway)
  moments_by_climate_plot = summarised_boot_moments_climate %>% 
    rename(traits = trait_trans) %>% 
    filter(year == 2016,
           TTtreat %in% c("control"),
           plasticity == "fixed") %>% 
    ungroup() %>% 
    left_join(trait_climate_regression %>% 
                filter(term == "slope"), 
              by = "traits") %>% 
    select(originSiteID:mean, term, estimate, `P value`, value) %>% 
    mutate(signi = if_else(`P value` < 0.05, "significant", "non-signigicant"),
           traits = factor(traits, levels = trait_order$traits),
           originSiteID = recode(originSiteID, "H" = "High Alpine", "A" = "Alpine", "M" = "Middle", "L" = "Lowland")) %>% 
    ggplot(aes(x = value, y = mean, linetype = signi, colour = signi)) +
    geom_point(aes(shape = originSiteID), colour = "grey") +
    geom_smooth(method = "lm") +
    scale_linetype_manual(name = "", values = c("dashed", "solid")) +
    scale_colour_manual(name = "", values = c("grey50", "red")) +
    scale_shape_manual(name = "Site", values = c(17, 16, 15, 18)) +
    labs(y = "Mean trait value", x = "Summer air temperature in °C") +
    facet_wrap(~traits, scales = "free_y") +
    theme_minimal() +
    theme(legend.position="top"),
  
  ## ----
  #colonization extinction plot
  ex = textGrob("\u2190 Extinctions", gp = gpar(fontsize = 9), rot = 90),
  col = textGrob(paste0("Colonizations ", "\u2192"), gp = gpar(fontsize = 9), rot = 90),
  legend = tibble(x1 = c(5, 5), x2 = c(6, 6), y1 = c(8, 13), y2 = c(8, 13),
         TTtreat = c("warm1", "warm1"), t = c("expected","realized"), c = c("blue", "blue"), f = c("blue", "white")) %>% 
    mutate(t = factor(t, levels = c("expected", "realized"))),
  colo_extinction_plot = bind_rows(
    extinction = extinction,
    colonization = colonization,
    .id = "process"
  ) %>% 
    group_by(process, TTtreat) %>% 
    summarise(count = mean(n)) %>% 
    mutate(TTtreat = factor(TTtreat, levels = c("control", "cool3", "cool1", "OTC", "warm1", "warm3"))) %>% 
    pivot_wider(names_from = process, values_from = count) %>% 
    ggplot(aes(x = TTtreat)) +
    geom_col(aes(x = TTtreat, y = colonization, colour = TTtreat), fill = "white", data = predicted) +
    geom_col(aes(x = TTtreat, y = -extinction, colour = TTtreat), fill = "white", data = predicted) +
    geom_col(aes(y = colonization, fill = TTtreat), alpha = 0.6) +
    geom_col(aes(y = - extinction, fill = TTtreat), alpha = 0.6) +
    scale_fill_manual(name = "", values = c("grey", "blue","lightblue", "orange", "pink", "red")) +
    scale_colour_manual(name = "", values = c("grey", "blue","lightblue", "orange", "pink", "red")) +
    geom_hline(yintercept = 0, colour = "grey") +
    #geom_rect(data = legend, aes(xmin = TTtreat, xmax = TTtreat, ymin = y1, ymax = y2, colour = c, fill = f)) +
    geom_text(data = legend, aes(x = x2, y = y2, label = t), size = 3) +
    labs(x = "", y = "") +
    annotation_custom(ex, xmin = 0.1, xmax= 0.1, ymin = -6, ymax = -6) +
    annotation_custom(col, xmin = 0.1, xmax= 0.1, ymin = 8, ymax = 8) +
    coord_cartesian(xlim = c(1, 6), clip="off") +
    theme_minimal() +
    theme(legend.position = "none",
          axis.line = element_line(colour = "black"),
          panel.grid.major.x = element_blank()),
  
  #colonization extinction over time plot
  col_ext_over_time_plot = bind_rows(extinction = extinction_all,
            colonization = colonization_all,
            .id = "process") %>% 
    ggplot(aes(x = year, y = count, colour = TTtreat)) +
    geom_point() +
    geom_line() +
    scale_colour_manual(name = "", values = c("grey", "pink","lightblue", "red", "blue", "orange")) +
    labs(x = "", y = "Number of species") +
    facet_wrap(~ process) +
    theme_minimal(),
  
  
  
  #H1Q2+3: Conv/div (univariate)
  
  # make boxes with predictions
  pred_pos = tibble(direction = c(rep("divergence", 16), rep("convergence", 16)),
                    plasticity = c(rep("fixed", 8), rep("plastic", 8), rep("fixed", 8), rep("plastic", 8)),
                    TTtreat = c(rep(c("warm3", "warm3", "warm1", "warm1", "cool3", "cool3", "cool1", "cool1"), 4)),
                    year = rep(c(2012, 2016), 16),
                    trait_trans = rep("Positive slope", 32),
                    mean = c(0,1,0,0.5,0,-1,0,-0.5,0,1,0,0.5,0,-1,0,-0.5,-1,0,-0.5,0,1,0,0.5,0,-1,0,-0.5,0,1,0,0.5,0),
                    signi = rep("significant", 32)),
  pred_neg = tibble(direction = c(rep("divergence", 16), rep("convergence", 16)),
                    plasticity = c(rep("fixed", 8), rep("plastic", 8), rep("fixed", 8), rep("plastic", 8)),
                    TTtreat = c(rep(c("warm3", "warm3", "warm1", "warm1", "cool3", "cool3", "cool1", "cool1"), 4)),
                    year = rep(c(2012, 2016), 16),
                    trait_trans = rep("Negative slope", 32),
                    mean = c(0,-1,0,-0.5,0,1,0,0.5,0,-1,0,-0.5,0,1,0,0.5,1,0,0.5,0,-1,0,-0.5,0,1,0,0.5,0,-1,0,-0.5,0),
                    signi = rep("significant", 32)),
  pred_no = tibble(direction = c(rep("divergence", 4), rep("convergence", 4)),
                   plasticity = c(rep(c("fixed", "fixed", "plastic", "plastic"), 2)),
                   TTtreat = c(rep("none", 8)),
                   year = rep(c(2012, 2016), 4),
                   trait_trans = rep("No sign. slope", 8),
                   mean = rep(0, 8),
                   signi = rep("non-signigicant", 8)),
  
  #divergence-convergence plot
  conv_div_plot = effect_size %>% 
    filter(year %in% c(2012, 2016)) %>% 
    ungroup() %>% 
    group_by(direction, plasticity, TTtreat, year, trait_trans) %>% 
    summarise(mean = mean(mean)) %>% 
    left_join(treatment_effect, by = c("direction", "plasticity", "trait_trans", "TTtreat" = "term")) %>% 
    bind_rows(pred_pos, pred_neg) %>% 
    # simplify figure by only showing traits with significant slope
    filter(!trait_trans %in% c("SLA_cm2_g", "NP_ratio", "LDMC", "CN_ratio")) %>%
    mutate(direction = factor(direction, levels = c("divergence", "convergence")),
           trait_trans = factor(trait_trans, levels = c("Positive slope", "dN15_permil", "C_percent", "Leaf_Area_cm2_log", "Dry_Mass_g_log", "Negative slope", "P_percent", "N_percent", "Thickness_mm_log", "dC13_permil")),
           TTtreat = factor(TTtreat, levels = c("cool3", "cool1", "OTC", "warm1", "warm3"))) %>% 
    ggplot(aes(x = year, y = mean, colour = TTtreat, linetype = signi)) +
    geom_rect(data = . %>% filter(trait_trans %in% c("Positive slope", "Negative slope")), aes(fill = trait_trans), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.6, fill = "grey75") +
    geom_line() +
    scale_colour_manual(values = c("lightblue", "blue", "orange", "pink", "red"), name = "") +
    scale_linetype_manual(values = c("dashed", "solid"), name = "") +
    scale_x_continuous(breaks=c(2012, 2014, 2016)) +
    geom_hline(yintercept = 0, colour = "grey50", linetype = "dotted") +
    labs(x = "", y = "Mean trait value") +
    facet_grid(trait_trans ~ direction * plasticity, scales = "free_y") +
    theme_bw() +
    theme(legend.position = "top",
          strip.text.y = element_text(angle=360),
          panel.grid = element_blank()),
  ## ----
  
  conv_div_no_slope_plot = effect_size %>% 
    filter(year %in% c(2012, 2016)) %>% 
    ungroup() %>% 
    group_by(direction, plasticity, TTtreat, year, trait_trans) %>% 
    summarise(mean = mean(mean)) %>% 
    left_join(treatment_effect, by = c("direction", "plasticity", "trait_trans", "TTtreat" = "term")) %>% 
    bind_rows(pred_no) %>% 
    # only traits without significant slope
    filter(trait_trans %in% c("No sign. slope", "SLA_cm2_g", "NP_ratio", "LDMC", "CN_ratio")) %>%
    mutate(direction = factor(direction, levels = c("divergence", "convergence")),
           trait_trans = factor(trait_trans, levels = c("No sign. slope", "SLA_cm2_g", "NP_ratio", "LDMC", "CN_ratio")),
           TTtreat = factor(TTtreat, levels = c("cool3", "cool1", "OTC", "warm1", "warm3"))) %>% 
    ggplot(aes(x = year, y = mean, colour = TTtreat, linetype = signi)) +
    geom_rect(data = . %>% filter(trait_trans %in% c("No sign. slope")), aes(fill = trait_trans), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.6, fill = "grey75") +
    geom_line() +
    scale_colour_manual(values = c("lightblue", "blue", "orange", "pink", "red"), name = "") +
    scale_linetype_manual(values = c("dashed", "solid"), name = "") +
    scale_x_continuous(breaks=c(2012, 2014, 2016)) +
    geom_hline(yintercept = 0, colour = "grey50", linetype = "dotted") +
    labs(x = "", y = "Mean trait value") +
    facet_grid(trait_trans ~ direction * plasticity, scales = "free_y") +
    theme_bw() +
    theme(legend.position = "top",
          strip.text.y = element_text(angle=360),
          panel.grid = element_blank()),
  
  
  
  
  #Histograms - shows need for cleaning
  trait_histograms = bind_rows(
    fixed = sum_boot_moment_fixed,
    plastic = sum_boot_moment_plastic, 
    .id = "plasticity") %>%
    ungroup() %>%  
    filter(TTtreat == "control") %>% 
    ggplot(aes(x = mean, fill = originSiteID)) + 
    geom_density(alpha = 0.5) + 
    facet_grid(plasticity ~ trait_trans, scales = "free"),
  
  #trait coverage
  #trait_coverage = autoplot(imputed_traits_div),
  
  #trait site ordinations - convergence with fixed traits
  # A_H = two_site_pca(data = sum_boot_moment_fixed, low = "A", high = "H"),
  # M_A = two_site_pca(data = sum_boot_moment_fixed, low = "M", high = "A"),
  # L_M = two_site_pca(data = sum_boot_moment_fixed, low = "L", high = "M"),
  # L_H = two_site_pca(data = sum_boot_moment_fixed, low = "L", high = "H"),
  # 
  # ordination_plot = bind_rows(A_H = A_H[[1]],
  #                                M_A = M_A[[1]],
  #                                L_M = L_M[[1]],
  #                                L_H = L_H[[1]],
  #                                .id = "comparison") %>% 
  #   ggplot(aes(x = PC1, y = PC2, colour = TTtreat, shape = destSiteID, group = turfID, linetype = destSiteID)) + 
  #   geom_point(aes(size = ifelse(year == min(year), "First", "Other"))) +
  #   geom_path() +
  #   #coord_equal() +
  #   scale_size_discrete(range = c(1, 2.5), limits = c("Other", "First"), breaks = c("First", "Other")) +
  #   scale_colour_manual(values = c("grey50", "pink", "lightblue", "red", "blue", "orange")) +
  #   scale_x_continuous(expand = c(.15, 0)) +
  #   labs(x = "PC 1", y = "PC 2", shape = "Site", colour = "Treatment", size = "Year", linetype = "Site", title = "Convergence") +
  #   facet_wrap(~ comparison) +
  #   theme_minimal(),
  
  # arrows = ggplot(A_H[[1]], aes(x = PC1, y = PC2, group = turfID)) +
  #   geom_segment(data = A_H[[2]], 
  #                aes(x = 0, y = 0, xend = PC1, yend = PC2), 
  #                arrow = arrow(length = unit(0.2, "cm")), 
  #                colour = "grey80",
  #                inherit.aes = FALSE) +
  #   geom_text(data = A_H[[2]], 
  #             aes(x = PC1 * 1.1,y = PC2 * 1.1, label = Label), 
  #             size = 3,
  #             inherit.aes = FALSE, colour = "grey80") +
  #   scale_x_continuous(expand = c(.2, 0)) +
  #   theme_minimal(),
  
  
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
  
  
  arrows = ggplot(treatment_pca(sum_boot_moment_fixed, "warm1", "cool1")[[1]], aes(x = PC1, y = PC2, group = turfID)) +
    geom_segment(data = treatment_pca(sum_boot_moment_fixed, "warm1", "cool1")[[2]], 
                 aes(x = 0, y = 0, xend = PC1, yend = PC2), 
                 arrow = arrow(length = unit(0.2, "cm")), 
                 colour = "grey80",
                 inherit.aes = FALSE) +
    geom_text(data = treatment_pca(sum_boot_moment_fixed, "warm1", "cool1")[[2]], 
              aes(x = PC1 * 1.1,y = PC2 * 1.1, label = Label), 
              size = 3,
              inherit.aes = FALSE, colour = "grey80") +
    labs(x = "", y = "") +
    scale_x_continuous(expand = c(.2, 0)) +
    theme_minimal(),
  
  ordination = cowplot::ggdraw(ordination_plot) +
    cowplot::draw_plot(arrows, .43, 0.04, .33, .43),
    
  
  
  #other plots
  #control means by site
  # control_mean_boxplot = bootstrapped_trait_moments_div %>% 
  #   filter(TTtreat == "control") %>% 
  #   ggplot(aes(x = Site, y = mean, group = blockID)) + 
  #   geom_boxplot() +
  #   labs(title = "Divergence") + 
  #   facet_wrap(~trait_trans, scales = "free_y"),
  
  #H1Q1: gradient predicts treatment effect
  # gradient_treatment_plot = treatment_effect %>% 
  #   left_join(trait_climate_regression %>% filter(term == "slope"), by = "trait_trans", suffix = c(".treatment", ".gradient")) %>% 
  #   mutate(term.treatment = factor(term.treatment, levels = c("warm3", "warm1", "OTC", "cool1", "cool3"))) %>% 
  #   ggplot(aes(x = estimate.gradient, y = estimate.treatment, colour = trait_trans)) +
  #   geom_point() +
  #   geom_abline(slope = 1, intercept = 0, colour = "grey", linetype = "dashed") +
  #   #scale_colour_manual(values = c("lightblue", "blue", "orange", "pink", "red"), name = "") +
  #   facet_grid(direction ~ term.treatment, scales = "free_y") +
  #   theme_bw(),
  
  #treatment_time_effect %>% 
    
  
  #space/time R2 relationship
  
  #temporal trait change
  #mean
  # trait_mean_by_time_plot = bootstrapped_trait_moments_div %>% 
  #   filter(!TTtreat %in% c("control", "local", "OTC")) %>%
  #   group_by(year, turfID, TTtreat, trait_trans, Site) %>% 
  #   summarise(mean = mean(mean)) %>% 
  #   ggplot(aes(x = year, y = mean, colour = TTtreat, group = turfID)) +
  #   geom_line() +
  #   facet_grid(trait_trans~Site, scales = "free_y"),
  
  
## HIGHER MOMENTS
  
happymoments %>%
    filter(plasticity == "fixed",
           trait_trans %in% c("dN15_permil", "Leaf_Area_cm2_log", "Thickness_mm_log")) %>%
    mutate(happymoment = fct_relevel(happymoment, c("var", "skew", "kurt"))) %>%
  group_by(TTtreat, trait_trans, happymoment, year) %>% 
  summarise(value = mean(value)) %>% 
    ggplot(aes(x = year, y = value, colour = TTtreat)) +
    #geom_vline(xintercept = 0, colour = "grey", linetype = "dashed") +
    geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed") +
    scale_colour_manual(values = c("grey", "pink", "lightblue", "red", "blue", "orange")) +
    facet_grid(happymoment ~ trait_trans, scale = "free") +
    theme_minimal(),
  # 
  # # skewness
  # skew_contrast_plot = contrasts %>% 
  #   filter(plasticity == "fixed",
  #          trait_trans %in% c("dN15_permil", "Leaf_Area_cm2_log", "Thickness_mm_log"),
  #          happymoment == "skew",
  #          lhs %in% c("cool1 - warm1", "cool3 - warm3", "OTC - warm1")) %>% 
  #   ggplot(aes(x = estimate, y = lhs, xmin = conf.low, xmax = conf.high, colour = lhs)) +
  #   geom_vline(xintercept = 0, colour = "grey", linetype = "dashed") +
  #   geom_point(size = 3) +
  #   geom_errorbarh(height = 0) +
  #   scale_colour_manual(values = c("pink", "red", "lightblue")) +
  #   labs(y = NULL, title = "Skewness contrasts") +
  #   facet_wrap(~ trait_trans) +
  #   theme_minimal(),
  # 
  # # kurtosis
  # kurt_contrast_plot = contrasts %>% 
  #   filter(plasticity == "fixed",
  #          trait_trans %in% c("dN15_permil", "Leaf_Area_cm2_log", "Thickness_mm_log"),
  #          happymoment == "kurt") %>% 
  #   ggplot(aes(x = estimate, y = lhs, xmin = conf.low, xmax = conf.high)) +
  #   geom_vline(xintercept = 0, colour = "grey", linetype = "dashed") +
  #   geom_point(size = 3) +
  #   geom_errorbarh(height = 0) +
  #   #scale_colour_manual(values = c("pink", "red", "lightblue")) +
  #   labs(y = NULL, title = "Skewness contrasts") +
  #   facet_wrap(~ trait_trans) +
  #   theme_minimal(),
  
# Pearson plot
# p1 = sum_boot_moment_fixed %>% 
#     filter(trait_trans %in% c("dN15_permil"),
#            TTtreat %in% c("warm3", "cool3", "control")) %>% 
#     ggplot(aes(x = skew^2, y = kurt, colour = TTtreat)) +
#     geom_point(alpha = 0.6) +
#     scale_y_reverse() +
#     scale_colour_manual(values = c("red", "blue", "grey")) +
#     labs(x = expression(S^2), y = "K", title = "dN15") +
#     geom_abline(slope = -1, intercept = 1, linetype = "dashed") +
#     theme_minimal()

  
)
