## Clean deposition data from marrero
clean_deposition <- function(file, site_data){
  
  suppressMessages(data <- readr::read_csv(file))
  
  data <- data %>% 
    preformat_data() %>%
    dplyr::rename(
      plant_name = plant, 
      cons = co_grains,
      hete = he_grains,
      n_stigma = stigma) %>%
    dplyr::group_by() %>%
    standardise_name('plant_name')
  
  plant_data <- data %>%
    dplyr::group_by(site_name, plant_name) %>%
    dplyr::summarise(status = unique(status)) %>%
    dplyr::mutate(status = dplyr::if_else(status == "e", "exotic", "native")) %>%
    dplyr::group_by()
  
  data <- data %>%
    dplyr::select(
      site_name, 
      plant_name,
      treatment,
      cons,
      hete,
      n_stigma) %>%
    dplyr::rename(recipient = plant_name) %>%
    dplyr::mutate(plant = 1:nrow(.))
  
  plants <- data %>% 
    dplyr::select(site_name, plant, n_stigma, treatment, recipient)
  
  conspecific <- data %>%
    dplyr::mutate(
      donor = recipient, 
      n_grains = cons) %>% 
    dplyr::select(plant, recipient, donor, n_grains) 
  
  heterospecific <- data %>%
    dplyr::mutate(donor = "UNKNOWN", 
           n_grains = hete) %>% 
    dplyr::select(plant, recipient, donor, n_grains)
  
  data <- dplyr::bind_rows(conspecific, heterospecific) %>% 
    dplyr::left_join(plants) %>% 
    dplyr::mutate(pollen_type = "non_germinated") %>%
    dplyr::select(site_name, plant, n_stigma, recipient, donor, n_grains, pollen_type, treatment)
  
  message("rearranging data as list")
  # save all data in a list of unique sites in a sort of deposition-site object.
  # each site has a list of properties that describe and relate it to other sites.
  # As a minimum it has a name and a deposition table
  data_dep <- foreach(i=1:dplyr::n_distinct(site_data$site_name)) %do% {
    site <- list()
    site$name <- site_data$site_name[i]
    site$locality <- site_data$locality[i]
    site$land_use <- site_data$land_use[i]
    site$fragment <- site_data$fragment[i]
    site$fragment_name <- site_data$fragment_name[i]
    site$plant_data <- plant_data %>% 
      dplyr::filter(site_name == site_data$site_name[i]) %>%
      dplyr::select(- site_name)
    site$deposition <-  data %>% 
      dplyr::filter(site_name == site_data$site_name[i]) %>%
      dplyr::select(- site_name)
    site$study <- "marrero"
    site$sampling <- list(from = as.Date("2010-12-01"), 
                          to = as.Date("2011-03-01"))
    site
  }
  data_dep
}