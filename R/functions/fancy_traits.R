#rename traits to fancy names for figures

fancy_trait_name_dictionary <- function(dat){
  
  dat <- dat %>% 
    mutate(trait_fancy = recode(trait_trans,
                              C_percent = "C %",
                              CN_ratio = "CN",
                              dC13_permil = "dC13 ‰",
                              dN15_permil = "dN15 ‰",
                              Dry_Mass_g_log = "Dry mass g",
                              LDMC = "LDMC",
                              Leaf_Area_cm2_log = "Area cm2",
                              N_percent = "N %",
                              NP_ratio = "NP",
                              P_percent = "P %",
                              SLA_cm2_g = "SLA cm2/g",
                              Thickness_mm_log = "Thickness mm"))
 
  return(dat) 
}
