#trait driver theory

#load packages
library("drake")
library("tidyverse")
library("rjt.misc")
library("BIEN", quietly = TRUE)
library("traitstrap")
library("dataDownloader")
library("DBI")

# more required packages
requireNamespace("visNetwork")

#drake configuration
pkgconfig::set_config("drake::strings_in_dots" = "literals")

#source subplans
source("R/download_plan.R")
source("R/data_import_plan.R")
source("R/bootstrap_moment_plan.R")
source("R/plots_plan.R")

#source extra function
source("R/functions/load_comm.R")
source("R/functions/check_BIEN_trait_values.R")

#drake plan
analysis_plan <- drake_plan(
  
)

#### combine plans ####
trait_plan <- bind_plans(download_plan, 
                        import_plan, 
                        bootstrap_moment_plan,
                        analysis_plan, 
                        plot_plan)
#quick plot
plot(trait_plan)

#### configure drake plan ####
trait_config <- drake_config(plan = trait_plan, keep_going = TRUE)
trait_config
