## pair site ordinations
two_site_pca <- function(data, low, high){
  #make fat table
  cwm_fat <- data %>% 
    ungroup() %>% 
    select(originSiteID:mean, -n) %>% 
    pivot_wider(names_from = "trait_trans", values_from = "mean")
  
  #chose extreme or short distance
  if(low == "L" & high == "H"){
    cwm_sites <- cwm_fat %>% 
      filter(destSiteID %in% c(low, high), !TTtreat %in% c("warm1", "OTC", "cool1"))
  } else {
    cwm_sites <- cwm_fat %>% 
      filter(destSiteID %in% c(low, high), !TTtreat %in% c("warm3", "cool3"))
  }
  
  high_pca <- cwm_sites %>% 
    select(-(originSiteID:destSiteID)) %>% rda(scale = TRUE)

  high_sites <- bind_cols(
    cwm_sites %>% select(originSiteID:destSiteID), 
    fortify(high_pca, display = "sites")
  )
  
  high_traits <- fortify(high_pca, display = "species")
  
  outputList <- list(high_sites, high_traits)
  
  return(outputList)
}
