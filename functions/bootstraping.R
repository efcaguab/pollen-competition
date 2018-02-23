#' Generate replicate datasets for analysis of open-closed flowers
#'
#' @param de dep_frame
#' @param category wether to generate dataset for 'conspecific' or 'heterospecific' pollen
#' @param transformation function to transform density data
#'
#' @return a data frame with pollen_gain column
#'
data_replicate <- function(de, category, ab, ph, transformation = I, dummy_id){
  
  # de <- readd(dep_frame)
  # ab <- readd(plant_rel_abu)
  # ph <- readd(plant_pheno_overlap)
  
  category <- case_when(
    category == 'con' ~ 'conspecific',
    TRUE ~ 'heterospecific'
  )
  
  get_deposition_sampled_data(de, category) %>%
    left_join(per_plant(ab), by = 'plant_name') %>%
    left_join(per_community(ab), by = c('plant_name', 'site_name')) %>% 
    left_join(per_plant(ph), by = "plant_name") %>%
    left_join(per_community(ab), by = c('plant_name', 'site_name'))
}

#' Get global values per plant
#'
#' @param x data frame with some colums that contain the string 'tot'
#'
#' @return a data frame organised per plant
#' 
per_plant <- function(x){
  x %>%
    group_by(plant_name) %>%
    summarise_at(vars(contains('tot')), unique)
}

#' Select only community relevant columns in a data frame
#'
#' @param x a data frame with plant and site name
#'
per_community <- function(x){
  x %>% 
    select_at(vars(-contains('tot')))
}

#' Sample the deposition data to get a boostrap data replicate 
#'
#' @param x dep_frame
#' @param category either 'con' or 'het'
#'
#' @return a dataframe with bootstrap replicates
#' 
get_deposition_sampled_data <- function(x, category){
  
  x %>%
    # only work with one category at a time
    filter(pollen_category == category) %>%
    select(plant, site_name, plant_name, treatment, pollen_density) %>%
    group_by(site_name, plant_name) %>%
    # find number of pairs that should be done per species-site combination
    mutate(n_closed = length(pollen_density[treatment == 'closed']), 
           n_open = length(pollen_density[treatment == 'open']), 
           n_samples = min(n_closed, n_open)) %>% 
    # sample the number of pairs with replacement
    split(list(.$site_name, .$plant_name, .$treatment), drop = T) %>%
    map_df(~ sample_n(., size = .$n_samples[1], replace = T)) %>%
    group_by(site_name, plant_name, treatment) %>%
    # make a dummy id to pair samples
    mutate(dummy_id = 1:n()) %>% 
    select(-plant, -n_open, -n_closed, -n_samples) %>%
    # transform count data if desired
    mutate(pollen_density = transformation(pollen_density)) %>%
    # pair samples
    spread(treatment, pollen_density) %>% 
    mutate(pollen_gain = open - closed) %>%
    mutate(pollen_category = category)
}