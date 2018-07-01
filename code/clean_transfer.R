clean_transfer <- function(file, site_data){
  
  transfer_file <- file
  
  transfer_df <- transfer_file %>%
    read.csv() %>%
    dplyr::rename(
      plant_name = plant,
      fragment_name = fragment, 
      animal_name = insect,
      locality = site) %>% 
    dplyr::mutate(date = as.character(date)) %>%
    dplyr::mutate(
      date = dplyr::if_else(
        stringr::str_detect(date, stringr::regex('[0-9]')),
        date,
        paste(date, "10")),
      date = paste("1", date),
      date = stringr::str_replace(date, "\\.", " "),
      date = stringr::str_replace(date, "Ene", "Jan"),
      date = stringr::str_replace(date, "Dic", "Dec"),
      date = as.Date(date, format = "%d %b %y"),
      fragment_name = stringr::str_replace(fragment_name, "C-alflafa", "C-alfalfa")) %>%
    standardise_name('animal_name') %>%
    standardise_name("plant_name")
  
  foreach(i=1:dplyr::n_distinct(site_data$site_name)) %do% {
    site <- list()
    site$name <- site_data$site_name[i]
    site$locality <- site_data$locality[i]
    site$land_use <- site_data$land_use[i]
    site$fragment <- site_data$fragment[i]
    site$fragment_name <- site_data$fragment_name[i]
    site$transfer <- transfer_df %>% 
      dplyr::filter(fragment_name == site_data$fragment_name[i]) %>%
      dplyr::select(- fragment_name)
    site$study <- "marrero"
    site$sampling <- list(
      from = as.Date("2010-12-01"), 
      to = as.Date("2011-03-01"))
    site
  }
}