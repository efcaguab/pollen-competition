
## calculate global and community pollen overlap
get_pollen_contribution <- function(tra_frame){
  
  pollen <- tra_frame %>%
    dplyr::group_by(site_name, plant_name) %>%
    dplyr::summarise(grain = sum(grain)) %>%
    dplyr::mutate(grain_log = log(grain + 1)) %>%
    dplyr::group_by(site_name) %>%
    dplyr::mutate(total_pollen = sum(grain_log), 
                  poc = grain_log/total_pollen, 
                  poc = scale(poc), 
                  var_trans = "log", 
                  scale = "community")
  
  pollen_global <- pollen %>%
    dplyr::group_by() %>%
    dplyr::mutate(total_pollen = sum(grain_log), 
                  poc = grain_log/total_pollen, 
                  poc = scale(poc), 
                  var_trans = "log", 
                  scale = "global")
    
  dplyr::bind_rows(pollen, pollen_global) %>%
    dplyr::select(-grain_log, -total_pollen) 
}
