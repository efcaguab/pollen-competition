clean_abundance <- function(file, sites) {
  suppressMessages(data <- readr::read_csv(file))
  
  data <- data %>%
    dplyr::rename(fragment_name = fragment) %>%
    dplyr::left_join(sites, by = "fragment_name") %>%
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
    dplyr::rename(plant_name = plant) %>%
    standardise_name('plant_name') %>% 
    dplyr::select(site_name, date, transect, plant_name, flowers) 
  
  site_data <- sites
  
  foreach(i=1:dplyr::n_distinct(site_data$site_name)) %do% {
    site <- list()
    site$name <- site_data$site_name[i]
    site$locality <- site_data$locality[i]
    site$land_use <- site_data$land_use[i]
    site$fragment <- site_data$fragment[i]
    site$fragment_name <- site_data$fragment_name[i]
    site$abundance <- data %>% 
      dplyr::filter(site_name == site_data$site_name[i]) %>%
      dplyr::select(- site_name)
    site$study <- "marrero"
    site
  }
}