
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


# Pollen dominance is the proportion of pollen that each plant species has in
# the pollen pool of each interacting species, multiplied by the proportion of
# visits that each of the interacting species makes to the focal plant species
get_pollen_dominance <- function(tra_frame, vis_frame){
  
  vis_for_tra <- vis_frame %>%
    dplyr::mutate(n_visits = dplyr::if_else(survey_type == "quantitative", n_visits, 1L)) %>%
    dplyr::group_by(site_name, plant_name, animal_name) %>%
    dplyr::summarise(n_visits = sum(n_visits))
  
  pollen <- tra_frame %>%
    dplyr::group_by(site_name, plant_name, animal_name) %>%
    dplyr::summarise(grain = mean(grain)) %>%
    dplyr::group_by(site_name, animal_name) %>%
    dplyr::mutate(grain_pol = sum(grain), 
                  prop_grain = grain/grain_pol) %>% 
    dplyr::left_join(vis_for_tra) %>% 
    dplyr::mutate(n_visits = dplyr::if_else(is.na(n_visits), 1L, n_visits)) %>%
    dplyr::group_by(site_name, animal_name) %>%
    dplyr::mutate(n_visits_sp = sum(n_visits, na.rm = T), 
                  prop_visits = n_visits/n_visits_sp) %>%
    dplyr::group_by(site_name, plant_name) %>%
    dplyr::summarise(pollen_cont = sum(grain * prop_visits * prop_grain), 
                     grain = sum(grain),
                     # the mean number of grains of plant in pollinator sp. i *
                     # the proportion of grains of plant in pollinator sp. i *
                     # the proportion of visits that pollinator sp. i makes on plant
                     poc = log(sum(grain * prop_visits * prop_grain))) %>%
    # dplyr::group_by() %>%
    dplyr::mutate(poc = scale(poc), 
                  var_trans = "log", 
                  scale = "community") 
  
  vis_for_tra_global <- vis_frame %>%
    dplyr::mutate(n_visits = dplyr::if_else(survey_type == "quantitative", n_visits, 1L)) %>%
    dplyr::group_by(plant_name, animal_name) %>%
    dplyr::summarise(n_visits = sum(n_visits))
  
  pollen_global <- tra_frame %>%
    dplyr::group_by(site_name, plant_name, animal_name) %>%
    dplyr::summarise(grain = mean(grain)) %>% 
    dplyr::group_by(site_name, animal_name) %>%
    dplyr::mutate(grain_pol = sum(grain), 
                  prop_grain = grain/grain_pol) %>% 
    dplyr::left_join(vis_for_tra_global) %>% 
    dplyr::mutate(n_visits = dplyr::if_else(is.na(n_visits), 1L, n_visits)) %>%
    dplyr::group_by(animal_name) %>%
    dplyr::mutate(n_visits_sp = sum(n_visits, na.rm = T), 
                  prop_visits = n_visits/n_visits_sp) %>%
    dplyr::group_by(site_name, plant_name) %>%
    dplyr::summarise(pollen_cont = sum(grain * prop_visits * prop_grain),
                     grain = sum(grain),
                     poc = log(sum(grain * prop_visits * prop_grain))) %>%
    # dplyr::group_by() %>%
    dplyr::mutate(poc = scale(poc), 
                  var_trans = "log", 
                  scale = "global") 
  
  dplyr::bind_rows(pollen, pollen_global)
}

