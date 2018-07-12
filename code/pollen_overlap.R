
## calculate global and community pollen overlap
get_pollen_overlap <- function(tra_frame){
  
  grain_matrix <- tra_frame %>%
    dplyr::group_by(site_name, plant_name, individual) %>%
    dplyr::summarise(grain = sum(grain)) %>%
    tidyr::spread(individual, grain, fill= 0) 
  
  # global_and_site_overlap(grain_matrix, function(x) log(x + 1), horn_fun) %>%
  #   dplyr::mutate(var_trans = 'log') %>%
  #   dplyr::group_by(scale) %>%
  #   dplyr::mutate(pov = scale(tov)) %>%
  #   dplyr::rename(pollen_overlap = tov)
  # 
  global_and_site_overlap(grain_matrix, function(x) log(x + 1), overlap_contrib) %>%
    dplyr::mutate(var_trans = 'log') %>%
    dplyr::group_by(scale) %>%
    dplyr::mutate(pov = scale(tov)) %>%
    dplyr::rename(pollen_overlap = tov)
  
  # red <- . %>% dplyr::filter(scale == "community")
  # 
  # plot(red(a)$pov, red(b)$pov)
}
