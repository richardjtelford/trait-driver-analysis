## import and process data

##TODO - 
# import and process osf files TRAIT COMMUNITY
# clean against BIEN

import_plan <- drake_plan(
  
  #import community
  community = community_download, 
  
  #import trait data
  traits0 = {
      
    #import
    leaf_traits = read_csv(traits_leaf_download)
    chemical_traits = read_csv(traits_chemical_download)
    },
  
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
    #import
    get(load(env_download))
    }
)
