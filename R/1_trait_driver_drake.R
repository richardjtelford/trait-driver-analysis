#trait driver theory

#load packages
library("drake")
library("tidyverse")
library("broom")
library("rjt.misc")
library("traitstrap")
library("dataDownloader")
library("DBI")
library("nlme")
library("vegan")
library("ggvegan")
library("patchwork")
library("gridExtra")
library("grid")
library("Hmisc")

#drake configuration
pkgconfig::set_config("drake::strings_in_dots" = "literals")

#source subplans
source("R/download_plan.R")
source("R/data_import_plan.R")
source("R/bootstrap_moment_plan.R")
source("R/euclidean_distance_plan.R")
#source("R/rda_plan.R")
source("R/colonization_extinction_plan.R")
source("R/analysis_plan.R")
source("R/plots_plan.R")
source("R/results_plan.R")

#source extra function
source("R/functions/load_comm.R")
source("R/functions/trait_ordination_fun.R")
source("R/functions/rda_plots.R")
source("R/functions/fancy_traits.R")
source("R/functions/correlations.R")


#drake plan
# manuscript plan
manuscript_plan <- drake_plan(
  #add extra packages to bibliography
  biblio2 = package_citations(
    packages = c("e1071", "traitstrap", "drake", "tidyverse", "rmarkdown", "renv"), 
    old_bib = file_in("Rmd/TDT.bib"), 
    new_bib = file_out("Rmd/TDT2.bib")),
  
  #knit manuscript
  manuscript = {
    file_in("Rmd/elsevier-harvard_rjt.csl")
    file_in("Rmd/TDT2.bib")
    rmarkdown::render(
      input = knitr_in("methods_results.Rmd"), 
      clean = FALSE)
  }
)

#### combine plans ####
trait_plan <- bind_plans(download_plan, 
                        import_plan,
                        bootstrap_moment_plan,
                        euclidean_distance_plan,
                        #rda_plan,
                        colonization_extinction_plan,
                        analysis_plan,
                        plot_plan,
                        results_plan,
                        manuscript_plan)
#quick plot
plot(trait_plan)

#### configure drake plan ####
trait_config <- drake_config(plan = trait_plan, keep_going = TRUE)
trait_config
