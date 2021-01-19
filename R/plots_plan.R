
plot_plan <- drake_plan(
  
  ## ----trait-climate
  
  #moments by climate in original plots
  trait_order = trait_climate_regression %>% 
    filter(term == "slope") %>% 
    select(trait_trans, estimate, p.value) %>% 
    mutate(signi = case_when(p.value < 0.05 ~ "significant",
                             p.value > 0.05 ~ "non-significant"),
           slope = case_when(p.value < 0.05 & estimate > 0 ~ "positive slope",
                             p.value < 0.05 & estimate < 0 ~ "negative slope",
                             p.value > 0.05 ~ "no slope"),
           slope = factor(slope, levels = c("positive slope", "no slope", "negative slope"))) %>% 
    arrange(slope, desc(estimate)),
  
  # without Wet_Mass_g_log to make nice plot with 3x4 panels (same as dry mass anyway)
  moments_by_climate_plot = summarised_boot_moments_climate %>% 
    filter(year == 2016,
           TTtreat %in% c("control"),
           plasticity == "fixed") %>% 
    ungroup() %>% 
    left_join(trait_climate_regression %>% 
                filter(term == "slope"), 
              by = "trait_trans") %>% 
    select(originSiteID:mean, term, estimate, p.value, value) %>% 
    mutate(signi = if_else(p.value < 0.05, "significant", "non-signigicant"),
           trait_trans = factor(trait_trans, levels = trait_order$trait_trans)) %>% 
    ggplot(aes(x = value, y = mean, linetype = signi, colour = signi)) +
    geom_point(aes(shape = originSiteID), colour = "grey") +
    geom_smooth(method = "lm") +
    scale_linetype_manual(name = "", values = c("dashed", "solid")) +
    scale_colour_manual(name = "", values = c("grey50", "red")) +
    labs(y = "Mean trait value", x = "Summer air temperature in °C") +
    facet_wrap(~trait_trans, scales = "free_y") +
    theme_minimal() +
    theme(legend.position="top"),
  
  ## ----
  
  
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
    bind_rows(pred_pos, pred_neg, pred_no) %>% 
    mutate(direction = factor(direction, levels = c("divergence", "convergence")),
           trait_trans = factor(trait_trans, levels = c("Positive slope", "dN15_permil", "Leaf_Area_cm2_log", "Dry_Mass_g_log", "C_percent", "No sign. slope", "SLA_cm2_g", "NP_ratio", "LDMC", "Negative slope", "P_percent", "N_percent", "dC13_permil", "Thickness_mm_log", "CN_ratio")),
           TTtreat = factor(TTtreat, levels = c("cool3", "cool1", "OTC", "warm1", "warm3", "none"))) %>% 
    ggplot(aes(x = year, y = mean, colour = TTtreat, linetype = signi)) +
    geom_rect(data = . %>% filter(trait_trans %in% c("Positive slope", "Negative slope", "No sign. slope")), aes(fill = trait_trans), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.6, fill = "grey75") +
    geom_line() +
    scale_colour_manual(values = c("lightblue", "blue", "grey", "orange", "pink", "red"), name = "") +
    scale_linetype_manual(values = c("dotted", "solid"), name = "") +
    scale_x_continuous(breaks=c(2012, 2014, 2016)) +
    geom_hline(yintercept = 0, colour = "grey50", linetype = "dashed") +
    labs(x = "", y = "Mean trait value") +
    facet_grid(trait_trans ~ direction * plasticity, scales = "free_y") +
    theme_bw() +
    theme(legend.position = "none",
          strip.text.y = element_text(angle=360)),
  ## ----
  
  
  
  
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
  bind_rows(
     moderate = treatment_pca(sum_boot_moment_fixed, "warm1", "cool1")[[1]],
     extreme = treatment_pca(sum_boot_moment_fixed, "warm3", "cool3")[[1]],
     otc = treatment_pca(sum_boot_moment_fixed, "OTC", NULL)[[1]],
     .id = "treatment"
     ) %>% 
    ggplot(aes(x = PC1, y = PC2, colour = TTtreat, shape = destSiteID, group = turfID, linetype = destSiteID)) + 
    geom_point(aes(size = ifelse(year == min(year), "First", "Other"))) +
    geom_path() +
    #coord_equal() +
    scale_size_discrete(range = c(1, 2.5), limits = c("Other", "First"), breaks = c("First", "Other")) +
    scale_colour_manual(values = c("grey50", "pink", "lightblue", "red", "blue", "orange")) +
    scale_x_continuous(expand = c(.15, 0)) +
    labs(x = "PC 1", y = "PC 2", shape = "Site", colour = "Treatment", size = "Year", linetype = "Site", title = "Convergence") +
    facet_wrap(~ treatment) +
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
    scale_x_continuous(expand = c(.2, 0)) +
    theme_minimal(),
    
  
  
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
  
  ## ----var-ske-kurt
  #variance
  higher_moment_data = happymoments %>%
    filter(trait_trans %in% c("dN15_permil", "Leaf_Area_cm2_log", "C_percent", "Thickness_mm_log")) %>% 
    inner_join(happymoment_effect, by = c("plasticity", "trait_trans", "happymoment", "TTtreat" = "term")) %>% 
    mutate(TTtreat = factor(TTtreat, levels = c("control", "warm3", "warm1", "OTC", "cool1", "cool3"))),
    
    var_plot = higher_moment_data %>% 
    filter(plasticity == "fixed", 
           happymoment == "var") %>% 
    ggplot(aes(x = factor(year), y = value, fill = TTtreat, alpha = signi)) +
    geom_boxplot() +
    scale_fill_manual(values = c("grey50", "pink", "red", "orange", "lightblue",  "blue")) +
    scale_alpha_manual(values = c(0.1, 1)) +
    scale_x_discrete(breaks=c("2012", "2014", "2016")) +
    labs(title = "Temporal change in variance", x = "", y = "Variance") +
    facet_grid(trait_trans ~ TTtreat, scales = "free_y") +
    theme_bw() +
    theme(legend.position = "none",
          strip.text.y = element_text(angle=360)),
  
    #skewness
    skew_plot = var_plot %+% filter(higher_moment_data, 
                        plasticity == "fixed",
                        happymoment == "skew") +
      labs(title = "Temporal change in skewness", x = "", y = "Skewness"),
  
    #kurtois
    kurt_plot = var_plot %+% filter(higher_moment_data, 
                        plasticity == "fixed",
                        happymoment == "kurt") +
      labs(title = "Temporal change in kurtosis", x = "", y = "Kurtosis")
  ## ----
  
  
)
