# clean visitation data from marrero
# uses sites data frame to standarise localities
clean_visitation_quant <- function(file, sites){
  
  site_char <- sites
  
  suppressMessages(data <- readr::read_csv(file))
  names(data) <- make.names(names(data))
  
  data <- data %>% 
    dplyr::left_join(site_char, by = c("Site" = "fragment_name_alt")) %>%
    dplyr::mutate(
      date = paste(1, date), 
      date = as.Date(date, format = "%d %B %Y"))
  
  plant_data <- data %>%
    dplyr::group_by(Plant.observed) %>%
    dplyr::summarise(plant_family = unique(Plant.family)) %>% 
    dplyr::group_by() %>%
    dplyr::rename(plant_name = Plant.observed) %>%
    standardise_name("plant_name")
  
  plant_abundance <- data %>%
    dplyr::group_by(site_name, date, Plant.observed) %>%
    dplyr::summarise(n_flower_units = round(mean(flower.units.observed.per.plant.species))) %>% 
    dplyr::rename(plant_name = Plant.observed) %>%
    dplyr::group_by() %>%
    standardise_name("plant_name")
  
  animal_data <- data %>%
    # remove records of no visits
    dplyr::filter(
      Flower.visitor.ID != "No visits", 
      Visitor_type != "No visits") %>%
    # correct mistake for Astylus quadrilineatus
    dplyr::mutate(
      Visitor_type = dplyr::if_else(
        Flower.visitor.ID == "Astylus quadrilineatus", 
        "beetle", 
        Visitor_type)) %>%
    dplyr::group_by(Flower.visitor.ID) %>%
    dplyr::summarise(
      animal_family = unique(Flower.visitor_family), 
      animal_order = unique(Flower.visitor_order), 
      animal_type = unique(Visitor_type)) %>%
    dplyr::mutate(animal_type = tolower(animal_type)) %>%
    dplyr::group_by() %>%
    dplyr::rename(animal_name = Flower.visitor.ID) %>%
    standardise_name('animal_name')
  
  interaction_data <- data %>%
    dplyr::filter(Visitation.frequency != 0) %>%
    dplyr::rename(
      plant_name = Plant.observed, 
      animal_name = Flower.visitor.ID, 
      n_visits = Visitation.frequency) %>%
    dplyr::select(site_name, date, plant_name, animal_name, n_visits) %>%
    standardise_name('animal_name') %>%
    standardise_name("plant_name")
  
  data_vis <- foreach(i=1:dplyr::n_distinct(site_char$site_name)) %do% {
    site <- list()
    site$name <- as.character(site_char$site_name[i])
    site$area_sampled <- data %>%
      dplyr::filter(site_name == site_char$site_name[i]) %$%
      total.area_sampled_m2 %>% unique()
    site$fragment_name <- site_char$fragment_name[i]
    dates <- data %>% 
      dplyr::filter(site_name == site_char$site_name[i]) %$%
      date %>% range()
    site$sampling <- list(from = dates[1], 
                          to = dates[2])
    site$plant_data <- plant_data 
    site$plant_abundance <- plant_abundance %>% 
      dplyr::filter(site_name == site_char$site_name[i]) %>%
      dplyr::select(- site_name)
    site$animal_data <- animal_data 
    site$visitation <-  interaction_data %>% 
      dplyr::filter(site_name == site_char$site_name[i]) %>%
      dplyr::select(- site_name)
    site$study <- "marrero"
    site
  }
}