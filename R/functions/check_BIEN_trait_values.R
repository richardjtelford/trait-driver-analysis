## Check BIEN trait values
# A function that makes a list of all genus, downloads all BIEN data from the same genus and calculates min and max values. These values are likely outliers and can be removed.

check_BIEN_trait_values <- function(traits){
  #create list with all genus
  genus_list <- traits0 %>% 
    mutate(Genus = word(Taxon, 1)) %>% 
    distinct(Genus) %>% 
    as.list() %>% 
    unlist()
  
  #downloads relevant traits from BIEN using the genus_list
  bien_traits <- BIEN_trait_traitbygenus(genus = genus_list, 
                                         trait = c("leaf fresh mass", 
                                                   "leaf dry mass", 
                                                   "leaf area", 
                                                   "leaf thickness", 
                                                   "leaf area per leaf dry mass", 
                                                   "leaf dry mass per leaf fresh mass", 
                                                   "leaf nitrogen content per leaf dry mass", 
                                                   "leaf phosphorus content per leaf dry mass", 
                                                   "leaf carbon content per leaf dry mass")) %>% 
    #convert units 
    select(scrubbed_genus, trait_name, trait_value, unit) %>% 
    #fix units (mass: not needed; area: mm2 to cm2; SLA: m2/kg to cm2/g; LDMC: mg g-1 to g/g; nitrogen: mg.g-1 to percent)
    mutate(trait_value = as.numeric(trait_value),
           trait_value = case_when(trait_name == "leaf fresh mass" ~ trait_value * 1,
                                   trait_name == "leaf dry mass" ~ trait_value * 1,
                                   trait_name == "leaf area" ~ trait_value / 100,
                                   trait_name == "leaf thickness" ~ trait_value * 1,
                                   trait_name == "leaf area per leaf dry mass" ~ trait_value * 10000 / 1000,
                                   trait_name == "leaf dry mass per leaf fresh mass" ~ trait_value /1000,
                                   trait_name == "leaf nitrogen content per leaf dry mass" ~ trait_value * 0.1,
                                   trait_name == "leaf phosphorus content per leaf dry mass" ~ trait_value * 0.1,
                                   trait_name == "leaf carbon content per leaf dry mass" ~ trait_value * 0.1)) %>% 
    #add column with new unit (not needed but it was useful to see the conversion)
    mutate(new_unit = case_when(trait_name == "leaf fresh mass" ~ "g",
                                trait_name == "leaf dry mass" ~ "g",
                                trait_name == "leaf area" ~ "cm2",
                                trait_name == "leaf thickness" ~ "mm",
                                trait_name == "leaf area per leaf dry mass" ~ "cm2/g",
                                trait_name == "leaf dry mass per leaf fresh mass" ~ "g/g",
                                trait_name == "leaf nitrogen content per leaf dry mass" ~ "percent",
                                trait_name == "leaf phosphorus content per leaf dry mass" ~ "percent",
                                trait_name == "leaf carbon content per leaf dry mass" ~ "percent")) %>% 
    #calc min and max (not sure if min is relevant)
    group_by(trait_name, unit, new_unit) %>% 
    summarise(min_value = min(trait_value, na.rm = TRUE),
              max_value = max(trait_value, na.rm = TRUE)) %>% 
    mutate(trait_name = recode(trait_name, "leaf fresh mass" = "Wet_Mass_g",
                               "leaf dry mass" = "Dry_Mass_g",
                               "leaf area" = "Leaf_Area_cm2",
                               "leaf thickness" = "Leaf_Thickness_Ave_mm",
                               "leaf area per leaf dry mass" = "SLA_cm2_g",
                               "leaf dry mass per leaf fresh mass" = "LDMC",
                               "leaf carbon content per leaf dry mass" = "C_percent",
                               "leaf nitrogen content per leaf dry mass" = "N_percent",
                               "leaf phosphorus content per leaf dry mass" = "P_percent"))
  
  
  trait_outliers <- traits0 %>% 
    left_join(bien_traits, by = c("trait" = "trait_name")) %>% 
    filter(value > max_value)
  
}
