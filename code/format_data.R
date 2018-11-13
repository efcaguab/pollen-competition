
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


# number of shared pollinators --------------------------------------------

# find the species that interact with another one
# x is the data frame with at least two columns
# sp_name is the name of the species of interest
# type is the name of the column containing sp_name
# int_col_name the name of the column containing the name of int. species
find_interacting_species <- function(x, sp_name, type, int_col_name){
  type <- rlang::sym(type)
  # int_col_name <- rlang::enquo(int_col_name)
  x %>%
    dplyr::filter(!!type == sp_name) %>%
    extract2(int_col_name) %>%
    unique()
}

# A wrapper to find_interacting_species when the species of interest is a plant
find_interacting_animals <- function(x, plant_name){
  find_interacting_species(x, plant_name, "plant_name", "animal_name")
}

# Gives the number of plants an animal interacts with
find_n_interacting_plants <- function(x, animal_name){
  find_interacting_species(x, animal_name, "animal_name", "plant_name") %>%
    dplyr::n_distinct()
}

# see if the animal of interest interacts with more than 1 species
is_animal_shared <- function(x, animal_name) {
  find_n_interacting_plants(x, animal_name) > 1
}

# count the number of shared animals a plant has
count_n_shared_animals <- function(x, plant_name){
  interacting_animals <- find_interacting_animals(x, plant_name)
  # X represents that the plant species was sampled but no interacting species were found. In that case we assume no shared polinator
  n_shared_animals <- interacting_animals %>%
    purrr::map_lgl(is_animal_shared, x = x) %>%
    sum()
  if ("X" %in% interacting_animals) {
    n_shared_animals - 1
  } else {
    n_shared_animals
  }
}

# count the number of shared animals for all plants in data frame X
count_n_shared_pol_community <- function(x){
  x %$%
    unique(plant_name) %>% 
    {`names<-`(., .)} %>%
    purrr::map_df(~ dplyr::data_frame(n_shared_pol = count_n_shared_animals(x, plant_name = .)), .id = "plant_name")
}

get_shared_pol <- function(vis_frame){
  
  species_site_shar <- vis_frame %>%
    split(.$site_name) %>%
    purrr::map_df(count_n_shared_pol_community, .id = "site_name") %>%
    dplyr::group_by() %>%
    dplyr::mutate(kn = n_shared_pol, 
                  lin = scale(kn), 
                  log = scale(log(kn + 1))) %>%
    tidyr::gather("var_trans", "k", lin, log) %>%
    dplyr::mutate(scale = "community")
  
  species_shar <- vis_frame %>%
    count_n_shared_pol_community() %>%
    dplyr::group_by() %>%
    dplyr::mutate(kn = n_shared_pol, 
                  lin = scale(kn), 
                  log = scale(log(kn + 1))) %>%
    tidyr::gather("var_trans", "k", lin, log) %>%
    dplyr::right_join(plant_site_combinations(species_site_shar), by = 'plant_name') %>%
    dplyr::mutate(scale = "global")
  
  dplyr::bind_rows(species_site_shar, species_shar)
}
