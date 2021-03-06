
#'Read & clean plant traits data
#'
#'Reads the plant trait data and manually changes the names of species so that
#'they all match with the species names in the armonised data names. They might
#'be incorrect but at least they match...
#'
#'@param file file with the csv raw plant trait data
#'
#'@return a data frame
#'
read_plant_traits <- function(traits_file) {
  suppressMessages(readr::read_csv(traits_file, skip = 1)) %>% 
    dplyr::rename(plant_name = `plant species`,
                  growth_form = `habit (tree/shrub/herb/climber/parasite, etc)`, 
                  native = `native in study region (yes/no)`,
                  longevity = `life cycle (annual/perennial)`, 
                  flower_color = `flower colour (as seen by humans)`) %>%
    dplyr::select(plant_name, growth_form, native, longevity, flower_color) %>%
    change_names("plant_name", "Adesmia sp", "Adesmia sp.") %>%
    change_names("plant_name", "Anagallis arvensis", "Anagalis sp.") %>%
    change_names("plant_name", "Cuscuta sp", "Cuscuta sp.") %>%
    change_names("plant_name", "Descuraina argentina", "Descurania argentina")  %>%
    change_names("plant_name", "Dipsacus sativus", "Dipsacus sp.") %>%
    change_names("plant_name", "Gamochaeta sp", "Gamochaeta sp.") %>%
    change_names("plant_name", "Geranium sp", "Geranium sp.") %>%
    change_names("plant_name", "Hydrocotyle sp", "Hydrocotyle sp.") %>%
    change_names("plant_name", "Hypochaeris radicata", "Hypochoeris radicata") %>%
    change_names("plant_name", "Hypochaeris pampasica", "Hypochoeris pampasica") %>%
    change_names("plant_name", "Lythrum hyssopifolia", "Lythrum sp.") %>%
    change_names("plant_name", "Nothoscordum nudicaule", "Nothoscordum euosimum") %>%
    change_names("plant_name", "Oenothera sp", "Oenothera sp.") %>%
    change_names("plant_name", "Oxalis sp", "Oxalis amarillo") %>%
    change_names("plant_name", "Oxalis amarillo cf.  O. articulata", "Oxalis violeta") %>%
    change_names("plant_name", "Panphalea bupleurifolia", "Pamphalea bupleurifolia") %>%
    change_names("plant_name", "Polygala linoides", "Poligala linoides") %>%
    change_names("plant_name", "Portulaca oleracea", "Portulaca oleracea oleracea") %>%
    change_names("plant_name", "Salsola Kali", "Salsola kali") %>%
    change_names("plant_name", "Scutellaria racemosa", "Scutelaria racemosa") %>%
    change_names("plant_name", "Sonchus sp", "Sonchus sp.") %>%
    change_names("plant_name", "Vicia linearifolia", "Vicia linealifolia") %>%
    change_names("plant_name", "Hydrocotyle sp.", "Hydrocotile sp.")
  
  # The following code was used to manually test that the species names coincide
  # ap <- select_plant_name(abu_frame, col_name = "in_abu")
  # dp <- select_plant_name(dep_frame, col_name = "in_dep")
  # dplyr::full_join(ap, dp, by = "plant_name") %>%
  #   dplyr::full_join(trait_df) %>% 
  #   dplyr::arrange(plant_name) %>% View
}

#' Select the plant_name column and create a column that is TRUE
#'
#' To be used to indicate that a species is present in a data frame. Used to
#' manually check the species names in the traits file
#'
#' @param x a data frame
#' @param col_name name of the column with TRUES
#'
#' @return a data frame with two columns, plant_name and the indicated by
#'   col_name
#'   
select_plant_name <- function(x, col_name){
  x %>%
    dplyr::select(plant_name) %>%
    dplyr::mutate(!!col_name := TRUE) %>%
    dplyr::distinct()
}

#' Make trait matrices global and community level
#'
#' @param plant_traits traits data frame
#' @param abu_frame abundance data frame
#' @param remove_na_traits wether to remove the plants without traits before computation
#' @param remove_na_abu wether to remove the plants without seasonal abundace
#'
#' @return a list of length two. The first correspond to community scale and the
#'   second to global scale
#'
make_trait_matrices <- function(plant_traits, abu_frame, remove_na_traits = TRUE, remove_na_abu = TRUE) {
  
  # first arrange things so that there is a list for community and a list for global
  log_abu <- abu_frame %>%
    dplyr::mutate(flowers = log(flowers)) 
  
  per_community <- plant_traits %>% 
    dplyr::full_join(flower_matrix(log_abu), by = "plant_name")  %>% 
    dplyr::group_by(site_name) %>%
    # not necessary as now we are using absolute abundances (log-transformed)
    # dplyr::mutate_at(dplyr::vars(dplyr::contains("2011"), dplyr::contains("2010")), scale) %>% 
    dplyr::mutate_at(dplyr::vars(dplyr::contains("2011"), dplyr::contains("2010")), as.numeric) %>%
    # dplyr::filter(site_name != "MA_AN_R_1") %>%
    split(.$site_name) 
  
  global <- log_abu %>%
    flower_matrix() %>%
    dplyr::group_by(plant_name) %>%
    dplyr::summarise_if(is.numeric, sum, na.rm = TRUE) %>%
    dplyr::mutate(site_name = "global") %>%
    dplyr::full_join(plant_traits, ., by = "plant_name") %>% 
    # not necessary as now we are using absolute abundances (log-transformed)
    # dplyr::mutate_at(dplyr::vars(dplyr::contains("2011"), dplyr::contains("2010")), scale) %>%
    dplyr::mutate_at(dplyr::vars(dplyr::contains("2011"), dplyr::contains("2010")), as.numeric) %>%
    list(.)

  community_and_global_traits <- list(community = per_community, 
       global = global)
  
  # fix the matrices
  community_and_global_traits %>%
    purrr::map(format_trait_matrices, remove_na_traits, remove_na_abu)
}

#' Fix trait matrix list
#' 
#' Puts factors to numeric in the way that FD wants it
#'
#' @param x trait matrix list 
#' @param remove_na_traits wether to remove the plants without traits before computation
#' @param remove_na_abu wether to remove the plants without seasonal abundace
#'
#' @return a fixed trait matrix
#'
format_trait_matrices <- function(x, remove_na_traits, remove_na_abu) {
  
  if (remove_na_traits) x %<>% purrr::map(dplyr::filter, !is.na(growth_form))
  if (remove_na_abu) x %<>% purrr::map(dplyr::filter, !is.na(`2011-01-01`))
  
  trait_matrices <- x %>%
    purrr::map(function(x) {
      x %<>%
        `class<-`("data.frame") 
      row.names(x) <- as.character(x$plant_name)
      x
    })
  
  sp_names <- trait_matrices %>%
    purrr::map(rownames)
  
  # binary need to be converted to 0s and 1s for FD:dbFD to work
  # data with species-traits needs to have rownmaes 
  trait_matrices %>%
    purrr::map(dplyr::select, -plant_name, -site_name) %>%
    purrr::map(dplyr::mutate_if, is.character, as.factor) %>%
    purrr::map(dplyr::mutate_if, is.factor, function(x) as.numeric(x) - 1) %>% 
    # purrr::map(~ dplyr::select_at(., dplyr::vars(dplyr::contains("2011"), dplyr::contains("2010")))) %>%
    # purrr::map(~ dplyr::select(., growth_form, native, longevity, flower_color)) %>%
    purrr::map2(sp_names, `rownames<-`)
}

#' Get species coordinates in the Functional space
#'
#' @param trait_matrices a list with lists of trait matrics
#' @param corr correction to use, defaults to Cailliez, wich was the one Camille used
#' @param messages wether to print annoying thingis or not
#' @param weighted wether to weight the traits according to the number of columns they use, 
#'
#' @return a list with the same structure and 
#'
get_species_coords <- function(trait_matrices, corr = "cailliez", messages = FALSE, weighted = FALSE) {
  n_traits <- ncol(trait_matrices[[1]][[1]])
  if (!weighted) {
    weights <- rep(1, n_traits)
  } else {
    n_abu_surveys <- 5
    weights <- c(rep(1, n_traits - n_abu_surveys), rep(1/n_abu_surveys, n_abu_surveys))
  }
  trait_matrices %>%
    purrr::map(function(x){
      x %>%
        # calliez correction required to account for NAs, were interested in the principal coordinate analyssis as well so need to tell it to print it out
        purrr::map(FD::dbFD, corr = corr, print.pco = T, messages = messages, w = weights) %>%
        purrr::map(`$`, "x.axes")
    })
}
