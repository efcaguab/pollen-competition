
# GENERAL -----------------------------------------------------------------

# Build a data frame from the lists joining the specified name
to_data_frame <- function(z, frame_name){
  plyr::ldply(z, function(y, frame_name){
    y %>% extract2(frame_name) %>%
      mutate(site_name = y$name)
  }, frame_name = frame_name)
}

# DEPOSITION --------------------------------------------------------------

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


# PLANT ABUNDANCE ------------------------------------------------------------

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
}

#' Calculate relative plant species abundance
#'
#' @param x abu_frame
#'
#' @return a data frame with relative abiundance per site and global in linear and logarithmic scale
#'
calculate_relative_abundance <- function(x){
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


#' Calculate phenology overlap in linear and logarithmic scale
#'
#' @param x abu frame
#'
#' @return a data frame
#'
calculate_phenology_overlap <- function(x) {
  
  n_flowers <- flower_matrix(x)
  
  overlap_linear <- global_and_site_overlap(n_flowers)
  overlap_log <- global_and_site_overlap(n_flowers, function(x) log(x + 1)) %>%
    rename_if(is.numeric, function(x) paste0(x, '_log'))
  
  inner_join(overlap_linear, overlap_log, by = c("plant_name", "site_name"))
}

#' Calculate phenology overlap at the site and global scale
#'
#' @param x a data frame with columns as the date resource
#' @param transformation function to transfor data with
#'
#' @return a data frame
#'
global_and_site_overlap <- function(x, transformation = I){
  overlap_site <- x %>%
    niche_overlap(x$site_name, transformation = transformation) %>% 
    rename(site_name = split, 
           temp_overlap_site = niche_overlap)
  
  overlap_global <- x %>%
    group_by(plant_name) %>% 
    summarise_if(is.numeric, sum) %>%
    niche_overlap(transformation = transformation) %>% 
    rename(temp_overlap_global = niche_overlap)
  
  inner_join(overlap_global, overlap_site, by = 'plant_name')
}

#' Spread flower abundance data so that each date is in a column
#'
#' @param x abu_frame
#'
#' @return a spreaded data frame
#'
flower_matrix <- function(x){
  x %>%
    filter(plant_name != 'No plant',
           !is.na(flowers)) %>%
    group_by(site_name, date, plant_name) %>%
    summarise(flowers = sum(flowers)) %>%
    # complete(site_name, date, plant_name, fill = list(flowers = 0)) %>%
    spread(date, flowers, fill= 0)
}

#' Calculate niche overlap over a splitting factor
#'
#' @param x data frame with columns as dates
#' @param split_by factor to splot things by
#' @param transformation function to transfor the counts by
#'
#' @return a data frame
#'
niche_overlap <- function(x, split_by = 1, transformation = I){
  
  if(length(split_by) == 1) {
    .id = NULL
  } else {
    .id = 'split'
  }
  
  x %>%
    group_by() %>%
    `class<-`('data.frame') %>%
    split(split_by) %>%
    map(~ `row.names<-`(., .$plant_name)) %>%
    map(~ as.matrix(select_if(., is.numeric))) %>%
    map(~ transformation(.)) %>%
    map(~ t(.)) %>%
    map(~ niche.overlap(., method = 'pianka')) %>%
    map(~ as.matrix(.)) %>%
    map(~ rowMeans(.)) %>%
    map_dfr(~ tibble(plant_name = names(.), niche_overlap = .), .id = .id) 
}

