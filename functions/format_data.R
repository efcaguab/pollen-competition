# Extract a data frame with deposition information 
extract_dep_frame <- function(x){
  x$deposition %>% 
    # change list to a data frame
    to_data_frame('deposition') %>%
    # add column to specify between heterospecific and conspecific pollen 
    mutate(pollen_category = recipient == donor, 
           pollen_category = if_else(pollen_category, 
                                     "conspecific", 
                                     "heterospecific"), 
           pollen_density = n_grains/n_stigma, 
           open = as.numeric(treatment == 'open'), 
           closed = as.numeric(treatment == 'closed')) %>%
    mutate(plant_name = recipient)
}

# Build a data frame from the lists joining the specified name
to_data_frame <- function(z, frame_name){
  plyr::ldply(z, function(y, frame_name){
    y %>% extract2(frame_name) %>%
      mutate(site_name = y$name)
  }, frame_name = frame_name)
}

#' Extract a data frame with abundance information
#'
#' @param x armonised data list
#'
#' @return a data frame
#'
extract_abu_frame <- function(x){
  x <- x$abundance %>%
    to_data_frame('abundance') %>%
    mutate(flowers = as.integer(flowers))
  
  species_site_abu <- x %>%
    filter(plant_name != 'No plant',
           !is.na(flowers)) %>%
    group_by(plant_name, site_name) %>%
    summarise(abu_com = sum(flowers)) %>%
    group_by(site_name) %>%
    mutate(abu_com_rel = scale(abu_com), 
           abu_com_rel_log = scale(log(abu_com)))
  
  species_abu <- species_site_abu %>%
    group_by(plant_name) %>%
    summarise(abu_tot = sum(abu_com)) %>%
    mutate(abu_tot_rel = scale(abu_tot), 
           abu_tot_rel_log = scale(log(abu_tot)))
  
  inner_join(species_site_abu, species_abu, by = 'plant_name')
}

extract_closed_treatment_means <- function(x, grouping_vars = NULL){
  
  grouping_vars <- c(grouping_vars, "recipient", "pollen_category")

  control <- x %>%
    filter(treatment == 'closed') %>%
    group_by_at(grouping_vars) %>%
    summarise(pollen_density_closed = median(pollen_density, na.rm = T), 
              pollen_density_closed_stdv = sd(pollen_density, na.rm = T))
}

calculate_pollination_gain <- function(x, grouping_vars = NULL){
  
  pollination_gain <- pollen_density %>%
    filter(treatment == 'open') %>%
    select_at(c(grouping_vars, 'plant', 'pollen_category', 'pollen_density')) %>%
    left_join(control, by = grouping_vars) %>% 
    mutate(pollination_gain = pollen_density - pollen_density_closed)
  
  # Explore control 
  plants_without_control <- pollination_gain %>%
    filter(is.na(pollen_density_closed)) %$%
    recipient %>% unique() 
  
  pollen_density %>% filter(recipient %in% plants_without_control) %>% View
}
