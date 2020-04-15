## import and process data

##TODO - 
# download from osf TRAIT, COMMUNITY
# import and process osf files TRAIT COMMUNITY
# trigger on osf change TRAIT COMMUNITY CLIMATE
# clean against BIEN

import_plan <- drake_plan(

  
  #import community
  community = get(load("trait_driver_analyses/data/Community.Rdata")), 
  
  #import trait data
  traits0 = get(load("trait_driver_analyses/data/traits.Rdata")),
  
  traits = traits0 %>% 
    select(-Full_Envelope_Name, -Envelope_Name_Corrected, -Date, -Elevation, -P_FILE_NAME, -matches("Flag$"), -allComments, -FileName, -Taxon_written_on_envelopes, -CN_FILE_NAME , -StoichLabel, -P_Std_Dev, -P_Co_Var, -LeafID) %>% 
    select(-matches("Leaf_Thickness_\\d_mm")) %>% 
    pivot_longer(cols = -(Site:Leaf_number), names_to = "trait", values_to = "value") %>% 
    filter(!is.na(value)) %>% 
    mutate(Site = factor(Site, levels = levels(community$originSiteID))),
  #TODO clean impossible trait values using BIEN
  #calculate derived traits
  #transform
  
  #import environmental data
  env = {
    #download from osf
    get_file(node = "4hjzu", remote_path = ".", file = "climate_month.Rdata", path = "data") 
    #import
    get(load("data/climate_month.Rdata"))
    }
)
