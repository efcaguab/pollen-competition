
# GENERAL -----------------------------------------------------------------

# Build a data frame from the lists joining the specified name
to_data_frame <- function(z, frame_name){
  plyr::ldply(z, function(y, frame_name){
    y %>% magrittr::extract2(frame_name) %>%
      dplyr::mutate(site_name = y$name)
  }, frame_name = frame_name)
}

# DEPOSITION --------------------------------------------------------------

# Extract a data frame with deposition information 
extract_dep_frame <- function(x){
  x$deposition %>% 
    # change list to a data frame
    to_data_frame('deposition') %>%
    # add column to specify between heterospecific and conspecific pollen 
    dplyr::mutate(
      pollen_category = recipient == donor, 
      pollen_category = dplyr::if_else(pollen_category, "conspecific", "heterospecific"),
      pollen_density = n_grains/n_stigma, 
      open = as.numeric(treatment == 'open'), 
      closed = as.numeric(treatment == 'closed')) %>%
    dplyr::mutate(plant_name = recipient) %>%
    dplyr::mutate(pollen_density = dplyr::if_else(
      pollen_category == 'heterospecific' & treatment == 'closed', 
      0, 
      pollen_density))
}

extract_closed_treatment_means <- function(x, grouping_vars = NULL){
  
  grouping_vars <- c(grouping_vars, "recipient", "pollen_category")

  control <- x %>%
    dplyr::filter(treatment == 'closed') %>%
    dplyr::group_by_at(grouping_vars) %>%
    dplyr::summarise(pollen_density_closed = median(pollen_density, na.rm = T), 
              pollen_density_closed_stdv = sd(pollen_density, na.rm = T))
}

calculate_pollination_gain <- function(x, grouping_vars = NULL){
  
  pollination_gain <- pollen_density %>%
    dplyr::filter(treatment == 'open') %>%
    select_at(c(grouping_vars, 'plant', 'pollen_category', 'pollen_density')) %>%
   dplyr::left_join(control, by = grouping_vars) %>% 
    dplyr::mutate(pollination_gain = pollen_density - pollen_density_closed)
  
  # Explore control 
  plants_without_control <- pollination_gain %>%
    dplyr::filter(is.na(pollen_density_closed)) %$%
    recipient %>% unique() 
  
  pollen_density %>% dplyr::filter(recipient %in% plants_without_control) %>% View
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
    dplyr::mutate(flowers = as.integer(flowers))
}

#' Calculate relative plant species abundance
#'
#' @param x abu_frame
#'
#' @return a data frame with relative abiundance per site and global in linear and logarithmic scale
#'
calculate_relative_abundance <- function(x, dep_frame){
  
  focal_plants <- unique(dep_frame$plant_name)
  
  # abundance per site
  species_site_abu <- x %>%
    dplyr::filter(plant_name != 'No plant',
           !is.na(flowers)) %>%
    dplyr::group_by(plant_name, site_name) %>%
    dplyr::summarise(abu = sum(flowers)) %>%
    dplyr::group_by() %>%
    dplyr::filter(plant_name %in% focal_plants) %>%
    dplyr::mutate(lin = scale(abu), 
           log = scale(log(abu))) %>% 
    tidyr::gather("var_trans", "rab", lin, log) %>% 
    dplyr::mutate(scale = 'community')
  
  # abudance per plant
  species_abu <- species_site_abu %>%
    dplyr::group_by(plant_name) %>%
    dplyr::summarise(abu = sum(abu)) %>%
    dplyr::group_by() %>%
    dplyr::filter(plant_name %in% focal_plants) %>%
    dplyr::mutate(lin = scale(abu), 
           log = scale(log(abu))) %>%
    tidyr::gather("var_trans", "rab", lin, log) %>%
    dplyr::right_join(plant_site_combinations(species_site_abu), by = 'plant_name') %>%
    dplyr::mutate(scale= 'global')
    
  # put both together in a tidy frame
    dplyr::bind_rows(species_site_abu, species_abu) %>% dplyr::group_by()
}


#' Calculate phenology overlap in linear and logarithmic scale
#'
#' @param x abu frame
#'
#' @return a data frame
#'
calculate_phenology_overlap <- function(x, dep_frame) {
  
  focal_plants <- unique(dep_frame$plant_name)
  n_flowers <- flower_matrix(x)
  
  overlap_linear <- global_and_site_overlap(n_flowers) %>%
    dplyr::mutate(var_trans = 'lin') %>%
    dplyr::filter(plant_name %in% focal_plants) %>%
    dplyr::group_by(scale) %>%
    dplyr::mutate(tov = scale(tov))
           
  overlap_log <- global_and_site_overlap(n_flowers, function(x) log(x + 1)) %>%
    dplyr::mutate(var_trans = 'log') %>%
    dplyr::filter(plant_name %in% focal_plants) %>%
    dplyr::group_by(scale) %>%
    dplyr::mutate(tov = scale(tov))
  
  dplyr::bind_rows(overlap_linear, overlap_log)
}

#' Calculate phenology overlap at the site and global scale
#'
#' @param x a data frame with columns as the date resource
#' @param transformation function to transfor data with
#'
#' @return a data frame
#'
global_and_site_overlap <- function(x, transformation = I, dist_fun = pianka_fun){
  overlap_site <- x %>%
    niche_overlap(x$site_name, transformation = transformation, dist_fun = dist_fun) %>% 
    dplyr::rename(site_name = split, 
           tov = niche_overlap) %>%
    dplyr::mutate(scale = "community")
  
  overlap_global <- x %>%
    dplyr::group_by(plant_name) %>% 
    dplyr::summarise_if(is.numeric, sum) %>%
    niche_overlap(transformation = transformation, dist_fun = dist_fun) %>% 
    dplyr::rename(tov = niche_overlap) %>%
    dplyr::mutate(scale = 'global') %>%
    dplyr::right_join(plant_site_combinations(overlap_site), by = c('plant_name'))
  
  dplyr::bind_rows(overlap_site, overlap_global)
}

plant_site_combinations <- function(x){
  x %>%
    dplyr::group_by() %>%
    dplyr::select(plant_name, site_name) %>%
    dplyr::distinct() %>%
    tidyr::complete(plant_name, site_name)
}

#' Spread flower abundance data so that each date is in a column
#'
#' @param x abu_frame
#'
#' @return a spreaded data frame
#'
flower_matrix <- function(x){
  x %>%
    dplyr::filter(plant_name != 'No plant',
           !is.na(flowers)) %>%
    dplyr::group_by(site_name, date, plant_name) %>%
    dplyr::summarise(flowers = sum(flowers)) %>%
    # tidyr::complete(site_name, date, plant_name, fill = list(flowers = 0)) %>%
    tidyr::spread(date, flowers, fill= 0)
}

#' Calculate niche overlap over a splitting factor
#'
#' @param x data frame with columns as dates
#' @param split_by factor to splot things by
#' @param transformation function to transfor the counts by
#'
#' @return a data frame
#'
niche_overlap <- function(x, split_by = 1, transformation = I, dist_fun = pianka_fun){
  
  if(length(split_by) == 1) {
    .id = NULL
  } else {
    .id = 'split'
  }
  
  x %>%
    dplyr::group_by() %>%
    `class<-`('data.frame') %>%
    split(split_by) %>%
    purrr::map(~ `row.names<-`(., .$plant_name)) %>%
    purrr::map(~ as.matrix(dplyr::select_if(., is.numeric))) %>%
    purrr::map(~ transformation(.)) %>%
    purrr::map(dist_fun) %>%
    purrr::map(~ as.matrix(.)) %>%
    purrr::map(~ rowMeans(.)) %>%
    purrr::map_dfr(~ dplyr::data_frame(plant_name = names(.), niche_overlap = .), .id = .id) 
}

pianka_fun <- function(x){
  x %>%
    t() %>%
    spaa::niche.overlap(method = "pianka")
}

horn_fun <- function(x){
  x %>%
    vegan::vegdist(method = "horn") %>%
    subtract(1, .)
}

overlap_contrib <- function(y){
  total <- mean(horn_fun(y))
  contrib <- matrix(1, ncol = 1, nrow = nrow(y))
  for (i in 1:nrow(y)) {
    contrib[i, 1] <- total - mean(horn_fun(y[-i, ]))
  }
  rownames(contrib) <- rownames(y)
  contrib
}
# DEGREE ------------------------------------------------------------------

#' Extract visitation
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
extract_vis_frame <- function(x){

  quant <- x$visitation_quant %>% 
  to_data_frame("visitation") %>%
    dplyr::mutate(survey_type = "quantitative")
  
  qual <- x$visitation_qual %>%
    to_data_frame("visitation") %>%
    dplyr::mutate(survey_type = "qualitative") %>%
    dplyr::select(-locality)
  
  dplyr::bind_rows(quant, qual) 
}

#' Get degree info
#'
#' @param vis_frame vis_frame
#' @param dep_frame for filtering out species that are not focal prior to scaling
#'
#' @return degree data frame
#' 
get_degree <- function(vis_frame, dep_frame){

  focal_plants <- unique(dep_frame$plant_name)
  
  species_site_vis <- vis_frame %>%
    dplyr::group_by(plant_name, site_name) %>%
    dplyr::summarise(kn = dplyr::n_distinct(animal_name)) %>%
    dplyr::group_by() %>%
    dplyr::filter(plant_name %in% focal_plants) %>%
    dplyr::mutate(lin = scale(kn), 
           log = scale(log(kn))) %>%
    tidyr::gather("var_trans", "k", lin, log) %>%
    dplyr::mutate(scale = "community")
      
  species_vis <- vis_frame %>%
    dplyr::group_by(plant_name) %>%
    dplyr::summarise(kn = dplyr::n_distinct(animal_name)) %>%
    dplyr::group_by() %>%
    dplyr::filter(plant_name %in% focal_plants) %>%
    dplyr::mutate(lin = scale(kn), 
           log = scale(log(kn))) %>%
    tidyr::gather("var_trans", "k", lin, log) %>%
    dplyr::right_join(plant_site_combinations(species_site_vis), by = 'plant_name') %>%
    dplyr::mutate(scale = "global")
  
  dplyr::bind_rows(species_site_vis, species_vis)
}

# format pollen transfer data
extract_tra_frame <- function(armonised_data){
  armonised_data$transfer %>%
    to_data_frame('transfer') 
}
