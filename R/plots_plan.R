
plot_plan <- drake_plan(
  #Histograms - shows need for cleaning
  trait_histograms = traits %>% 
    ggplot(aes(x = value_trans, fill = Site)) + 
    geom_histogram() + 
    facet_wrap(~ trait_trans, scales = "free") +
    labs(title = "Clean me"),
  
  #trait coverage
  trait_coverage = autoplot(imputed_traits_div),
  
  #control means by site
  control_mean_boxplot = bootstrapped_trait_moments %>% 
    filter(TTtreat == "control") %>% 
    ggplot(aes(x = Site, y = mean, group = blockID)) + 
    geom_boxplot() +
    facet_wrap(~trait, scales = "free_y"),
  

  
  #space/time R2 relationship
  
  #variance
  
  #skewness
  
  #kurtois
  
  
  trait_mean_by_time_plot = bootstrapped_trait_moments_div %>% 
    filter(!TTtreat %in% c("control", "local", "OTC")) %>%
    group_by(year, turfID, TTtreat, trait, Site) %>% 
    summarise(mean = mean(mean)) %>% 
    ggplot(aes(x = year, y = mean, colour = TTtreat, group = turfID)) +
    geom_line() +
    facet_grid(trait~Site, scales = "free_y")
  
  
  
)