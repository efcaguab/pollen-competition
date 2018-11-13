
## calculate global and community pollen overlap
get_pollen_contribution <- function(tra_frame){
  
  pollen <- tra_frame %>%
    dplyr::group_by(site_name, plant_name) %>%
    dplyr::summarise(grain = sum(grain)) %>%
    dplyr::mutate(grain_log = log(grain + 1)) %>%
    dplyr::group_by(site_name) %>%
    dplyr::mutate(total_pollen = sum(grain), 
                  poc = grain_log/total_pollen, 
                  pollen_cont = poc,
                  poc = scale(poc), 
                  var_trans = "log", 
                  scale = "community")
  
  pollen_global <- pollen %>%
    dplyr::group_by() %>%
    dplyr::mutate(total_pollen = sum(grain), 
                  poc = grain_log/total_pollen, 
                  pollen_cont = poc,
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
    dplyr::group_by() %>%
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
    dplyr::group_by() %>%
    dplyr::mutate(poc = scale(poc), 
                  var_trans = "log", 
                  scale = "global") 
  
  dplyr::bind_rows(pollen, pollen_global)
}


logspace <- function(d1, d2, n) {
  exp(log(10)*seq(log10(d1),log10(d2),length.out = n))
}

make_plot_visit_efficiency <- function(pollen_contribution, vis_frame, bins_x, bins_y){
  
  # drake::loadd(pollen_contribution, vis_frame)
  # bins <- 6
  require(ggplot2)
  
  vis <- vis_frame %>%
    dplyr::mutate(n_visits = dplyr::if_else(survey_type == "quantitative", n_visits, 1L)) %>%
    dplyr::group_by(site_name, plant_name, animal_name) %>%
    dplyr::summarise(n_visits = sum(n_visits)) %>%
    dplyr::group_by(site_name, plant_name) %>%
    dplyr::summarise(n_visits = sum(n_visits)) %>%
    dplyr::group_by()
  
  eff <- pollen_contribution %>%
    dplyr::filter(scale == "community", var_trans == "log") %>%
    dplyr::group_by()
  
  cuts_n_visits <- logspace(1,1000, bins_x + 1)
  cuts_grain <- logspace(1,100000, bins_y + 1)
  
  visit_efficiency <- dplyr::full_join(vis, eff, by = c("site_name", "plant_name")) %>% 
    dplyr::filter_all(dplyr::all_vars(!is.na(.))) 
  
  visit_efficiency %>%
    bin_visit_efficiency(cuts_n_visits, cuts_grain) %>% 
    ggplot(aes(x = n_visits, y = grain)) +
    # geom_point() +
    # geom_smooth(method = "lm")
    geom_tile(aes(fill = pollen_cont), 
              alpha = 0.5) +
    # geom_contour(aes(z = pollen_cont)) +
    geom_point(data = visit_efficiency, aes(fill = poc),
               colour = "black",
               shape = 21,
               size = 2) +
    scale_x_log10(breaks = round(cuts_n_visits[c(T,F)], digits = 1)) +
    scale_y_log10(breaks = round(cuts_grain[c(T,F, F, F)], digits = 1)) +
    scale_fill_viridis_c() +
    labs(fill ="", 
         x = "visits received", 
         y = "grains carried by pollinators") +
    pub_theme() +
    theme(legend.position = "right", 
          legend.justification = c(1,0), 
          legend.direction = "vertical", 
          legend.box.background = element_rect(colour = NA, fill = NA), 
          legend.background = element_rect(colour = NA, fill = NA),
          legend.box.margin = margin(2,2,2,2), 
          legend.key.height = unit(7, "mm"), 
          legend.title.align = 0)
}

bin_visit_efficiency <- function(x,cuts_n_visits = logspace(1,250, 11), cuts_grain = logspace(1,100000, 11)){
  x %>%
    # bin
    dplyr::mutate(n_visits = cut_visit_metric(n_visits, cuts_n_visits),
                  grain = cut_visit_metric(grain, cuts_grain)) %>% 
    dplyr::group_by(n_visits, grain) %>% 
    dplyr::summarise(pollen_cont = median(poc, na.rm = T)) %>% 
    dplyr::group_by() %>%
    dplyr::mutate_at(c("n_visits", "grain"), as.character) %>%
    dplyr::mutate_at(c("n_visits", "grain"), as.numeric) 
}

cut_visit_metric <- function(x, breaks){
  cut(x, 
      breaks = breaks, 
      include.lowest = TRUE, 
      right = FALSE, 
      labels = 10^na.omit((log10(breaks) + log10(dplyr::lead(breaks)))/2))
}
