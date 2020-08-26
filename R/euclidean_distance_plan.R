## euclidean distance plan
euclidean_distance_plan <- drake_plan(
  
  #join bootstraped moments
  fat_table = bind_rows(
    convergence = sum_boot_moment_conv,
    divergence = sum_boot_moment_div, 
    .id = "direction") %>%
    ungroup() %>% 
    select(direction, Site, blockID, trait, TTtreat, year, turfID, Mean) %>% 
    pivot_wider(names_from = trait, values_from = Mean) %>% 
    filter(!is.na(C_percent),
           year %in% c(2012, 2016),
           !(direction == "convergence" & Site == "L"),
           !(direction == "convergence" & TTtreat == "OTC")),
  
  #separate meta data
  fat_meta = fat_table %>% 
    select(direction:turfID) %>% 
    rowid_to_column(),
  
  #select only traits
  only_traits = fat_table %>% 
    ungroup() %>% 
    select(C_percent:Wet_Mass_g),
  
  #scale, calculate euclidean distance
  dist = as.matrix(dist(scale(only_traits))),
  distances = tibble(row = as.vector(row(dist)), 
                      col = as.vector(col(dist)), 
                      dist = as.vector(dist)) %>% 
    filter(row > col) %>% # one half of the matrix
    # join meta data by row and col
    left_join(fat_meta, by = c("row" = "rowid")) %>% 
    left_join(fat_meta, by = c("col" = "rowid"), suffix = c(".row", ".col")) %>% 
    mutate(TTtreat.row = factor(TTtreat.row, levels = c("control", "OTC", "warm1", "warm3", "cool1", "cool3"))) %>%
    #filter for change trough time (same turf and direction)
    filter(turfID.row == turfID.col,
           direction.row == direction.col)
)
