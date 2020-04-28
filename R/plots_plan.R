
plot_plan <- drake_plan(
  #trait coverage
  trait_coverage = autoplot(imputed_traits),
  
  #control means by site
  control_mean_boxplot = bootstrapped_trait_moments_climate %>% 
    filter(TTtreat == "control") %>% 
    ggplot(aes(x = Site, y = mean, group = Location)) + 
    geom_boxplot() +
    facet_wrap(~trait, scales = "free_y")
)