#load packages
library("drake")

#plan
trait_driver_plan <- "R/1_trait_driver_drake.R"

#Build the right things
r_make(source = trait_driver_plan)
drake_failed()

#view dependency graph
r_vis_drake_graph(source = trait_driver_plan, targets_only = TRUE)
