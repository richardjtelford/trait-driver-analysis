
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
  treat_colours = c("grey50", "pink", "lightblue", "purple"),
  AH = twoSites(data = sum_boot_moment_conv, low = "A", high = "H"),
  MA = twoSites(data = sum_boot_moment_conv, low = "M", high = "A"),
  LM = twoSites(data = sum_boot_moment_conv, low = "L", high = "M"),
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

  #H1Q2 + 3: Conv/div along gradient and across treatments (multivariate)
  Site_order = c(H = "High Alpine",
                 A = "Alpine",        
                 M = "Middle",
                 L = "Lowland"),
  
  euclidean_distance = distances %>% 
    mutate(direction.row = factor(direction.row, levels = c("divergence", "convergence"))) %>% 
    ggplot(aes(x = TTtreat.row, y = dist, fill = TTtreat.row)) +
    geom_boxplot() + 
    scale_fill_manual(values = c("grey", "orange", "pink", "red", "lightblue", "blue"), name = "") +
    labs(title = "Distance over time", y = "Eucleading distance over time", x = "") +
    facet_grid(direction.row ~ Site.row, labeller = labeller(Site.row = Site_order)) +
    theme_minimal() +
    theme(axis.text.x=element_blank(),
          legend.position = "bottom"),
  
  #H1Q2+3: Conv/div (univariate)
  conv_div_plot = effect_size %>% 
    filter(year %in% c(2012, 2016)) %>% 
    group_by(direction, TTtreat, year, trait_trans) %>% 
    summarise(mean = mean(mean)) %>% 
    ggplot(aes(x = year, y = mean, colour = TTtreat)) +
    geom_line() +
    scale_colour_manual(values = c("lightblue", "blue", "orange", "pink", "red"), name = "") +
    geom_hline(yintercept = 0, colour = "grey", linetype = "dashed") +
    facet_grid(trait_trans ~ direction, scales = "free_y") +
    theme_bw(),
  
  #treatment_time_effect %>% 
    
  
  #space/time R2 relationship
  
  #variance
  
  #skewness
  
  #kurtois
  
  
  trait_mean_by_time_plot = bootstrapped_trait_moments_div %>% 
    filter(!TTtreat %in% c("control", "local", "OTC")) %>%
    group_by(year, turfID, TTtreat, trait_trans, Site) %>% 
    summarise(mean = mean(mean)) %>% 
    ggplot(aes(x = year, y = mean, colour = TTtreat, group = turfID)) +
    geom_line() +
    facet_grid(trait_trans~Site, scales = "free_y"),
  
  #moments by climate in original plots
  trait_order = trait_climate_regression %>% 
    filter(term == "slope") %>% 
    select(trait_trans, estimate, p.value) %>% 
    mutate(signi = if_else(p.value < 0.05, "significant", "non-signigicant")) %>% 
    arrange(desc(signi), desc(estimate)),
  
  moments_by_climate = summarised_boot_moments_climate %>% 
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
    labs(x = "", y = "Mean trait value") +
    facet_wrap(~trait_trans, scales = "free_y") +
    theme_minimal()
)
