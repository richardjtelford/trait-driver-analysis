
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
  control_mean_boxplot = bootstrapped_trait_moments_div %>% 
    filter(TTtreat == "control") %>% 
    ggplot(aes(x = Site, y = mean, group = blockID)) + 
    geom_boxplot() +
    labs(title = "Divergence") + 
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
    facet_grid(trait~Site, scales = "free_y"),
  
  #moments by climate in original plots
  moments_by_climate = summarised_boot_moments_climate %>% 
    filter(year == 2012,
           TTtreat %in% c("control")) %>% 
    ggplot(aes(x = value, y = Mean)) +
    geom_boxplot(aes(group = value)) +
    geom_point(aes(colour = TTtreat, shape = Site)) +
    geom_smooth(method = "lm") +
    facet_wrap(~trait_trans, scales = "free_y")
)


# summarised_boot_moments_climate %>% 
#   filter(year == 2012,
#          TTtreat %in% c("control", "local")) %>% 
#   select(Site, trait, Mean, value) %>% 
#   #    pivot_longer(cols = c("Mean", "Var", "Skew", "Kurt"), names_to = "moment", values_to = "moment_value") %>% 
#   ggplot(aes(x = value, y = Mean)) +
#   geom_boxplot(aes(group = value)) +
#   geom_point(aes(colour = TTtreat, shape = Site)) +
#   geom_smooth(method = "lm") +
#   facet_wrap(~trait, scales = "free_y")
# 
# summarised_boot_moments_climate %>% 
#   filter(year == 2012,
#          TTtreat %in% c("control", "local")) %>% 
#   ggplot(aes(x = value, y = Var)) +
#   geom_boxplot(aes(group = value)) +
#   geom_point(aes(colour = TTtreat, shape = Site)) +
#   geom_smooth(method = "lm") +
#   facet_wrap(~trait, scales = "free_y")
# 
# 
# summarised_boot_moments_climate %>% 
#   filter(year == 2012,
#          TTtreat %in% c("control", "local")) %>% 
#   ggplot(aes(x = value, y = Kurt)) +
#   geom_boxplot(aes(group = value)) +
#   geom_point(aes(colour = TTtreat, shape = Site)) +
#   geom_smooth(method = "lm") +
#   facet_wrap(~trait, scales = "free_y")
# 
# summarised_boot_moments_climate %>% 
#   filter(year == 2012,
#          TTtreat %in% c("control", "local")) %>% 
#   ggplot(aes(x = value, y = Skew)) +
#   geom_boxplot(aes(group = value)) +
#   geom_point(aes(colour = TTtreat, shape = Site)) +
#   geom_smooth(method = "lm") +
#   facet_wrap(~trait, scales = "free_y")
# 
# 
# summarised_boot_moments_climate %>% 
#   ggplot(aes(x = year, y = Mean)) +
#   geom_line(aes(colour = Site, group = turfID)) +
# #  geom_smooth(method = "lm") +
#   facet_grid(trait ~ TTtreat, scales = "free_y")
