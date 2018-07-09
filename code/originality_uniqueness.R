#' Get the originality at the community and global level for species in the community
#'
#' @param species_coords 
#' @param abu_frame 
#'
#' @return a data frame with global/community level originality
#'
get_species_originality <- function(species_coords, abu_frame) {
  
  originality_community <- species_coords[[1]] %>%
    purrr::imap(function(x, this_site){
      these_abu <- abu_frame %>%
        dplyr::filter(site_name == this_site) %>%
        dplyr::group_by(plant_name) %>%
        dplyr::summarise(flowers = sum(flowers, na.rm = T), 
                         flowers = log(flowers))
      functional_originality(x, these_abu)
    }) %>%
    purrr::imap_dfr(~dplyr::mutate(.x, site_name = .y)) %>%
    dplyr::mutate(scale = "community")
  
  originality_global <- species_coords[[2]] %>%
    purrr::imap_dfr(function(x, this_site){
      these_abu <- abu_frame %>%
        # dplyr::filter(site_name == this_site) %>%
        dplyr::group_by(plant_name) %>%
        dplyr::summarise(flowers = sum(flowers, na.rm = T), 
                         flowers = log(flowers))
      functional_originality(x, these_abu)
    }) %>%
    dplyr::mutate(scale = "global") %>%
    dplyr::right_join(plant_site_combinations(originality_community), by = c('plant_name'))
  
  dplyr::bind_rows(originality_community, originality_global) %>%
    dplyr::group_by(scale) %>%
    dplyr::mutate(org = scale(originality))
  
  # dplyr::inner_join(originality_global, originality_community, by = c("plant_name", "site_name")) %$%
  # plot(originality.x, originality.y)
}

#' Functional originality of species 
#'
#'The centroid of the functional space is weigted by species abundance
#'
#' @param species_coord_matrix species functional coordinates in the community
#' @param species_abundances abundances of species. 
#'
#' @return a data frame with species names and their originality
#'
functional_originality <- function(species_coord_matrix, species_abundances) {
  weights <- species_abundances %>% 
    dplyr::filter(plant_name %in% rownames(species_coord_matrix)) %>%
    dplyr::arrange(plant_name) %$%
    flowers
  
  # order alphabetically so it matches the weights
  species_coord_matrix <- species_coord_matrix[order(rownames(species_coord_matrix)), ] 
  centroid <- species_coord_matrix %>%
    as.list() %>%
    purrr::map(weighted.mean, weights) %>%
    unlist()
  species_coord_matrix %>%
    rbind(centroid) %>%
    dist(diag = TRUE, upper = TRUE) %>%
    as.matrix() %>%
    extract(nrow(species_coord_matrix) + 1, 1:nrow(species_coord_matrix)) %>%
    dplyr::data_frame(plant_name = names(.), originality = .)
}

#' Get the uniqueness at the community and global level for species in the community
#'
#' @param species_coords 
#'
#' @return a data frame with global/community level originality
#'
get_species_uniqueness <- function(species_coords){
  
  uniqueness_community <- species_coords[[1]] %>%
    purrr::map_dfr(functional_uniqueness, .id = "site_name") %>%
    dplyr::mutate(scale = "community")
  
  uniqueness_global <- species_coords[[2]] %>%
    purrr::map_dfr(functional_uniqueness) %>%
    dplyr::mutate(scale = "global") %>%
    dplyr::right_join(plant_site_combinations(uniqueness_community), by = c('plant_name'))
  
  # dplyr::inner_join(uniqueness_global, uniquenes_community, by = c("plant_name", "site_name")) %$%
  # plot(uniqueness.x, uniqueness.y)
  
  dplyr::bind_rows(uniqueness_community, uniqueness_global)
}

#' Functional uniqueness of species
#'
#' @param species_coord_matrix species functional coordinates in the community
#'
#' @return a data frame with species names and their uniqueness
#'
functional_uniqueness <- function(species_coord_matrix){
  species_coord_matrix %>%
    dist(diag = FALSE, upper = TRUE) %>%
    as.matrix() %>%
    `diag<-`(NA) %>%
    purrr::array_branch(1) %>%
    purrr::map_dbl(min, na.rm = TRUE) %>%
    set_names(rownames(species_coord_matrix)) %>%
    dplyr::data_frame(plant_name = names(.), uniqueness = .)
}
