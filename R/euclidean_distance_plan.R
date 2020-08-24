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
           !c(direction == "convergence" & Site == "L"),
           !c(direction == "convergence" & TTtreat == "OTC")),
  
  #separate meta data
  fat_meta = fat_table %>% 
    select(direction:turfID) %>% 
    rowid_to_column(),
  
  #select only traits
  only_traits = fat_table %>% 
    ungroup() %>% 
    select(C_percent:Wet_Mass_g),
  
  #scale, calculate euclidean distance
  d = dist(scale(only_traits)),
  D = as.matrix(d),
  distances = tibble(row = as.vector(row(D)), 
                      col = as.vector(col(D)), 
                      dist = as.vector(D)) %>% 
    filter(row > col) %>% # one half of the matrix
    # join meta data by row and col
    left_join(fat_meta, by = c("row" = "rowid")) %>% 
    left_join(fat_meta, by = c("col" = "rowid"), suffix = c(".row", ".col")) %>% 
    #sfilter distances within the same block
    filter(blockID.row == blockID.col) %>% # not sure if this step is needed...?
    mutate(TTtreat.row = factor(TTtreat.row, levels = c("control", "OTC", "warm1", "warm3", "cool1", "cool3"))) %>%
    #filter for change trough time (same turf and direction)
    filter(turfID.row == turfID.col,
           direction.row == direction.col)
)
