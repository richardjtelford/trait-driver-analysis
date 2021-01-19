## euclidean distance plan
euclidean_distance_plan <- drake_plan(
  
  #join bootstraped moments
  fat_table = summarised_boot_moments_climate %>%
    ungroup() %>% 
    select(plasticity, originSiteID, originBlockID, destSiteID, destBlockID, trait_trans, TTtreat, year, turfID, mean) %>% 
    pivot_wider(names_from = "trait_trans", values_from = "mean") %>% 
    filter(year %in% c(2012, 2016)),
  
  #fixed traits
  #separate meta data
  fat_meta_fixed = fat_table %>% 
    filter(plasticity == "fixed") %>% 
    select(originSiteID:turfID) %>% 
    rowid_to_column(),
  
  #select only traits
  only_traits_fixed = fat_table %>% 
    filter(plasticity == "fixed") %>% 
    ungroup() %>% 
    select(C_percent:Thickness_mm_log),
  
  #scale, calculate euclidean distance
  dist_fixed = as.matrix(dist(scale(only_traits_fixed))),
  distances_fixed = tibble(row = as.vector(row(dist_fixed)), 
                      col = as.vector(col(dist_fixed)), 
                      dist = as.vector(dist_fixed)) %>% 
    filter(row > col) %>% # one half of the matrix
    # join meta data by row and col
    left_join(fat_meta_fixed, by = c("row" = "rowid")) %>% 
    left_join(fat_meta_fixed, by = c("col" = "rowid"), suffix = c(".row", ".col")) %>% 
    mutate(TTtreat.row = factor(TTtreat.row, levels = c("control", "OTC", "warm1", "warm3", "cool1", "cool3"))) %>%
    #filter for change trough time (same turf and direction)
    filter(turfID.row == turfID.col),
  
  
  #plastic traits
  #separate meta data
  fat_meta_plastic = fat_table %>% 
    filter(plasticity == "plastic") %>% 
    select(originSiteID:turfID) %>% 
    rowid_to_column(),
  
  #select only traits
  only_traits_plastic = fat_table %>% 
    filter(plasticity == "plastic") %>% 
    ungroup() %>% 
    select(C_percent:Thickness_mm_log),
  
  #scale, calculate euclidean distance
  dist_plastic = as.matrix(dist(scale(only_traits_plastic))),
  distances_plastic = tibble(row = as.vector(row(dist_plastic)), 
                           col = as.vector(col(dist_plastic)), 
                           dist = as.vector(dist_plastic)) %>% 
    filter(row > col) %>% # one half of the matrix
    # join meta data by row and col
    left_join(fat_meta_plastic, by = c("row" = "rowid")) %>% 
    left_join(fat_meta_plastic, by = c("col" = "rowid"), suffix = c(".row", ".col")) %>% 
    mutate(TTtreat.row = factor(TTtreat.row, levels = c("control", "OTC", "warm1", "warm3", "cool1", "cool3"))) %>%
    #filter for change trough time (same turf and direction)
    filter(turfID.row == turfID.col),
  
  # analyze euclidean distance
  distances = bind_rows(
    fixed = distances_fixed,
    plastic = distances_plastic,
    .id = "plasticity"
  ) %>% 
    select(plasticity, originSiteID.row:turfID.row, dist),
  
  euclidean_results = distances %>% 
    nest(data = -c(plasticity, originSiteID.row)) %>% 
    mutate(mod = map(data, ~lm(dist ~ TTtreat.row, data = .x)),
           result = map(mod, tidy)) %>%
    unnest(result),
  
  #H1Q2 + 3: Conv/div along gradient and across treatments (multivariate)
  Site_order = c(H = "High Alpine",
                 A = "Alpine",        
                 M = "Middle",
                 L = "Lowland"),
  ## ----eucledian
  euclidean_distance_plot = distances %>% 
    mutate(plasticity = factor(plasticity, levels = c("fixed", "plastic"))) %>% 
    ggplot(aes(x = TTtreat.row, y = dist, fill = TTtreat.row)) +
    geom_boxplot() + 
    scale_fill_manual(values = c("grey", "orange", "pink", "red", "lightblue", "blue"), name = "") +
    labs(title = "Distance over time", y = "Euclidean distance over time", x = "") +
    facet_grid(plasticity ~ originSiteID.row, labeller = labeller(originSiteID.row = Site_order)) +
    theme_minimal() +
    theme(axis.text.x=element_blank(),
          legend.position = "bottom")
    
)
