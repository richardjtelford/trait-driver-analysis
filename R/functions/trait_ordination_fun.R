## pair site ordinations
twoSites <- function(data, low, high, treat_colours){
  #make fat table
  cwm_fat <- data %>% 
    ungroup() %>% 
    select(Site:mean, -n) %>% 
    pivot_wider(names_from = "trait_trans", values_from = "mean") %>% 
    filter(!is.na(C_percent),
           !is.na(LDMC))
  
  #chose extreme or short distance
  if(low == "L" & high == "H"){
    cwm_sites <- cwm_fat %>% 
      filter(Site %in% c(low, high), !TTtreat %in% c("warm1", "OTC", "cool1"))
  } else {
    cwm_sites <- cwm_fat %>% 
      filter(Site %in% c(low, high), !TTtreat %in% c("warm3", "cool3"))
  }
  
  high_pca <- cwm_sites %>% 
    select(-(Site:turfID)) %>% rda(scale = TRUE)
  
  high_sites <- bind_cols(
    cwm_sites %>% select(Site:turfID), 
    fortify(high_pca, display = "sites")
  )
  
  high_traits <- fortify(high_pca, display = "species")
  
  g <- ggplot(high_sites, aes(x = PC1, y = PC2, colour = TTtreat, shape = Site, group = turfID, linetype = Site)) + 
    geom_point(aes(size = ifelse(year == min(year), "First", "Other"))) +
    geom_path() +
    #arrows
    geom_segment(data = high_traits, 
                 aes(x = 0, y = 0, xend = PC1, yend = PC2), 
                 arrow = arrow(length = unit(0.2, "cm")), 
                 colour = "grey80",
                 inherit.aes = FALSE) +
    geom_text(data = high_traits, 
              aes(x = PC1 * 1.1,y = PC2 * 1.1, label = Label), 
              size = 3,
              inherit.aes = FALSE, colour = "grey80") +
    coord_equal() +
    scale_size_discrete(range = c(1, 2.5), limits = c("Other", "First"), breaks = c("First", "Other")) +
    scale_colour_manual(values = treat_colours) +
    #  scale_colour_brewer(palette = "RdBu", direction = -1) +
    #  scale_shape_manual(values = c(16, 21)) +
    scale_x_continuous(expand = c(.15, 0)) +
    labs(x = "PC 1", y = "PC 2", shape = "Site", colour = "Treatment", size = "Year", linetype = "Site", title = paste(high, "-", low)) +
    theme_bw()
  return(g)
}