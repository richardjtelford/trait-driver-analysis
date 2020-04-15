#trait driver theory

#load packages
library("drake")
library("tidyverse")
library("rjt.misc")
library("BIEN", quietly = TRUE)
library("traitstrap")
library("PFTCFunctions")

# more required packages
requireNamespace("visNetwork")

#drake configuration
pkgconfig::set_config("drake::strings_in_dots" = "literals")

#set up parallel processing for drake
#options(future.fork.enable = TRUE)
future::plan(future::multiprocess) 

#source subplans
source("R/data_import.R")

#drake plan
analysis_plan <- drake_plan(
  
  #Histograms - shows need for cleaning
  trait_histograms = traits %>% 
    ggplot(aes(x = value, fill = Site)) + 
    geom_histogram() + 
    facet_wrap(~trait, scales = "free") +
    labs(title = "Clean me"),
  
  #impute traits for control and pre-transplant
  imputed_traits = {
    imputed_traits_home = community %>%
      filter(year == min(year) | TTtreat %in% c("control", "local", "OTC")) %>% 
      select(Site = originSiteID, Location = originBlockID, turfID, year, TTtreat, Taxon = speciesName, cover) %>% 
      trait_impute(traits = traits, scale_hierarchy = c("Site", "Location"), taxon_col = "Taxon", value_col = "value", abundance_col = "cover", other_col = c("TTtreat", "year", "turfID"))
      
    imputed_traits_transplant = community %>%
      filter(year > min(year), !TTtreat %in% c("control", "local", "OTC")) %>%
      select(Site = destSiteID, Location = destBlockID, turfID, year, TTtreat, Taxon = speciesName, cover) %>% 
      trait_impute(traits = traits, scale_hierarchy = c("Site", "Location"), taxon_col = "Taxon", value_col = "value", abundance_col = "cover", other_col = c("year", "TTtreat", "turfID"))
    
    
    x <- bind_rows(imputed_traits_home, imputed_traits_transplant)
    attr(x, "attrib") <- attr(imputed_traits_home, "attrib")
    x
  },

  #traits moments 
  bootstrapped_trait_moments  = trait_np_bootstrap(imputed_traits, nrep = 100),  
  
  #space/time R2 relationship
  
  #means
  mean_plot = bootstrapped_trait_moments %>% ggplot(aes(x = Site, y = mean)) +
    geom_boxplot() +
    facet_wrap(~trait, scales = "free_y"),
  
  #bootstrapped_trait_moments %>% filter(trait == "Leaf_Thickness_Ave_mm") %>% 
  #  ggplot(aes(x = year, y = value, fill = TTtreat)) + facet_wrap(~Site)
  
  #variance
  
  #skewness
  
  #kurtois
  
  
  moments_plot = bootstrapped_trait_moments %>% ggplot(aes(x = Site, y = mean)) +
    geom_boxplot() +
    facet_wrap(~trait, scales = "free_y"), 
  
  plot = bootstrapped_trait_moments %>% 
    filter(!TTtreat %in% c("control", "local", "OTC")) %>%
    group_by(year, turfID, TTtreat, trait, Site) %>% 
    summarise(mean = mean(mean)) %>% 
    ggplot(aes(x = year, y = mean, colour = TTtreat, group = turfID)) +
      geom_line() +
      facet_grid(trait~Site, scales = "free_y")
  
)

#### combine plans ####
trait_plan <- bind_rows(import_plan, 
                        bootstrap_moment_plan,
                        analysis_plan)

#### configure drake plan ####
trait_config <- drake_config(plan = trait_plan, jobs = 2, parallelism = "future", keep_going = TRUE)
trait_config
