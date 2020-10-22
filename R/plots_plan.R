
plot_plan <- drake_plan(
  #Histograms - shows need for cleaning
  trait_histograms = traits %>% 
    ggplot(aes(x = value_trans, fill = Site)) + 
    geom_histogram() + 
    facet_wrap(~ trait_trans, scales = "free") +
    labs(title = "Clean me"),
  
  #trait coverage
  trait_coverage = autoplot(imputed_traits_div),
  
  #trait ordinations
  treat_colours = c("grey50", "pink", "lightblue", "orange"),
  AH = twoSites(data = sum_boot_moment_conv, low = "A", high = "H", treat_colours = treat_colours),
  MA = twoSites(data = sum_boot_moment_conv, low = "M", high = "A", treat_colours = treat_colours),
  LM = twoSites(data = sum_boot_moment_conv, low = "L", high = "M", treat_colours = treat_colours),
  LH = twoSites(data = sum_boot_moment_conv, low = "L", high = "H", treat_colours = c("grey50", "red", "blue")),
  trait_ordination_plot = (AH + MA) / (LM + LH),
  
  #control means by site
  control_mean_boxplot = bootstrapped_trait_moments_div %>% 
    filter(TTtreat == "control") %>% 
    ggplot(aes(x = Site, y = mean, group = blockID)) + 
    geom_boxplot() +
    labs(title = "Divergence") + 
    facet_wrap(~trait_trans, scales = "free_y"),
  
  #H1Q1: gradient predicts treatment effect
  gradient_treatment_plot = treatment_effect %>% 
    left_join(trait_climate_regression %>% filter(term == "slope"), by = "trait_trans", suffix = c(".treatment", ".gradient")) %>% 
    mutate(term.treatment = factor(term.treatment, levels = c("warm3", "warm1", "OTC", "cool1", "cool3"))) %>% 
    ggplot(aes(x = estimate.gradient, y = estimate.treatment, colour = trait_trans)) +
    geom_point() +
    geom_abline(slope = 1, intercept = 0, colour = "grey", linetype = "dashed") +
    #scale_colour_manual(values = c("lightblue", "blue", "orange", "pink", "red"), name = "") +
    facet_grid(direction ~ term.treatment, scales = "free_y") +
    theme_bw(),
  
  #H1Q2+3: Conv/div (univariate)
  
  # make boxes with predictions
  pred_pos = tibble(direction = c(rep("divergence", 4), rep("convergence", 4)),
                     TTtreat = c(rep("warm3", 2), rep("cool3", 2), rep("warm3", 2), rep("cool3", 2)),
                     year = rep(c(2012, 2016), 4),
                     trait_trans = rep("Positive slope", 8),
                     mean = c(0,0.5,0,-0.5,-0.5,0,0.5,0),
                     signi = rep("significant", 8)),
  pred_neg = tibble(direction = c(rep("divergence", 4), rep("convergence", 4)),
                     TTtreat = c(rep("cool3", 2), rep("warm3", 2), rep("cool3", 2), rep("warm3", 2)),
                     year = rep(c(2012, 2016), 4),
                     trait_trans = rep("Negative slope", 8),
                     mean = c(0,0.5,0,-0.5,-0.5,0,0.5,0),
                     signi = rep("significant", 8)),
  pred_no = tibble(direction = c(rep("divergence", 4), rep("convergence", 4)),
                    TTtreat = c(rep("none", 8)),
                    year = rep(c(2012, 2016), 4),
                    trait_trans = rep("No slope", 8),
                    mean = rep(0, 8),
                    signi = rep("non-signigicant", 8)),
  
  #divergence-convergence plot
  conv_div_plot = effect_size %>% 
    filter(year %in% c(2012, 2016)) %>% 
    ungroup() %>% 
    group_by(direction, TTtreat, year, trait_trans) %>% 
    summarise(mean = mean(mean)) %>% 
    left_join(treatment_effect, by = c("direction", "trait_trans", "TTtreat" = "term")) %>% 
    bind_rows(pred_pos, pred_neg, pred_no) %>% 
    mutate(direction = factor(direction, levels = c("divergence", "convergence")),
           trait_trans = factor(trait_trans, levels = c("Positive slope", "dN15_permil", "Wet_Mass_g_log", "Leaf_Area_cm2_log", "Dry_Mass_g_log", "C_percent", "No slope", "SLA_cm2_g", "NP_ratio", "LDMC", "Negative slope", "P_percent", "N_percent", "dC13_permil", "Thickness_mm_log", "CN_ratio")),
           TTtreat = factor(TTtreat, levels = c("cool3", "cool1", "OTC", "warm1", "warm3", "none"))) %>% 
    ggplot(aes(x = year, y = mean, colour = TTtreat, linetype = signi)) +
    geom_rect(data = . %>% filter(trait_trans %in% c("Positive slope", "Negative slope", "No slope")), aes(fill = trait_trans), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.6, fill = "grey90") +
    geom_line() +
    scale_colour_manual(values = c("lightblue", "blue", "grey", "orange", "pink", "red"), name = "") +
    scale_linetype_manual(values = c("dotted", "solid"), name = "") +
    geom_hline(yintercept = 0, colour = "grey", linetype = "dashed") +
    labs(x = "", y = "Mean trait value") +
    facet_grid(trait_trans ~ direction, scales = "free_y") +
    theme_bw() +
    theme(legend.position = "none",
          strip.text.y = element_text(angle=360)),
    ## ----
  
  
  #treatment_time_effect %>% 
    
  
  #space/time R2 relationship
  
  #temporal trait change
  #mean
  trait_mean_by_time_plot = bootstrapped_trait_moments_div %>% 
    filter(!TTtreat %in% c("control", "local", "OTC")) %>%
    group_by(year, turfID, TTtreat, trait_trans, Site) %>% 
    summarise(mean = mean(mean)) %>% 
    ggplot(aes(x = year, y = mean, colour = TTtreat, group = turfID)) +
    geom_line() +
    facet_grid(trait_trans~Site, scales = "free_y"),
  
  ## ----var-ske-kurt
  #variance
  variance_plot = happymoments %>%
    filter(direction == "divergence", 
           happymoment == "var") %>% #,
           #trait_trans %in% c("SLA_cm2_g", "Thickness_mm_log", "Leaf_Area_cm2_log")) %>% 
    inner_join(happymoment_effect, by = c("direction", "trait_trans", "happymoment", "TTtreat" = "term")) %>% 
    mutate(TTtreat = factor(TTtreat, levels = c("control", "warm3", "warm1", "OTC", "cool1", "cool3"))) %>% 
    ggplot(aes(x = factor(year), y = value, fill = signi)) +
    geom_boxplot() +
    scale_fill_manual(name = "Treatment * year interaction", values = c("grey", "red")) +
    labs(title = "Temporal change in variance", x = "", y = "Variance") +
    facet_grid(trait_trans ~ TTtreat, scales = "free_y") +
    theme_bw(),
  
  #skewness
  skewness_plot = happymoments %>%
    filter(direction == "divergence", 
           happymoment == "skew") %>% 
    inner_join(happymoment_effect, by = c("direction", "trait_trans", "happymoment", "TTtreat" = "term")) %>% 
    mutate(TTtreat = factor(TTtreat, levels = c("control", "warm3", "warm1", "OTC", "cool1", "cool3"))) %>% 
    ggplot(aes(x = factor(year), y = value, fill = signi)) +
    geom_boxplot() +
    scale_fill_manual(name = "Treatment * year interaction", values = c("grey", "red")) +
    labs(title = "Temporal change in skewness", x = "", y = "Skewness") +
    facet_grid(trait_trans ~ TTtreat, scales = "free_y") +
    theme_bw(),
  
  #kurtois
  kurtosis_plot = happymoments %>%
    filter(direction == "divergence", 
           happymoment == "kurt") %>% 
    inner_join(happymoment_effect, by = c("direction", "trait_trans", "happymoment", "TTtreat" = "term")) %>% 
    mutate(TTtreat = factor(TTtreat, levels = c("control", "warm3", "warm1", "OTC", "cool1", "cool3"))) %>% 
    ggplot(aes(x = factor(year), y = value, fill = signi)) +
    geom_boxplot() +
    scale_fill_manual(name = "Treatment * year interaction", values = c("grey", "red")) +
    labs(title = "Temporal change in kurtosis", x = "", y = "Kurtosis") +
    facet_grid(trait_trans ~ TTtreat, scales = "free_y") +
    theme_bw(),
  ## ----
  
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
  
  ## ----trait-climate
  moments_by_climate_plot = summarised_boot_moments_climate %>% 
    filter(year == 2016,
           TTtreat %in% c("control"),
           direction == "divergence") %>% 
    ungroup() %>% 
    left_join(trait_climate_regression %>% 
                filter(term == "slope"), 
              by = "trait_trans") %>% 
    select(Site:mean, -n, term, estimate, p.value, value) %>% 
    mutate(signi = if_else(p.value < 0.05, "significant", "non-signigicant"),
           trait_trans = factor(trait_trans, levels = trait_order$trait_trans)) %>% 
    ggplot(aes(x = value, y = mean, linetype = signi, colour = signi)) +
    geom_point(aes(shape = Site), colour = "grey") +
    geom_smooth(method = "lm") +
    scale_linetype_manual(name = "", values = c("dashed", "solid")) +
    scale_colour_manual(name = "", values = c("grey50", "red")) +
    labs(y = "Mean trait value", x = "Summer air temperature in °C") +
    facet_wrap(~trait_trans, scales = "free_y") +
    theme_minimal() +
    theme(legend.position="top"),
    
  ## ----
)
