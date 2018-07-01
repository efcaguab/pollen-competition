## Clean deposition data from marrero
clean_deposition <- function(file, site_data){
  
  suppressMessages(data <- readr::read_csv(file))
  
  data <- data %>% 
    preformat_data() %>%
    rename(plant_name = plant, 
           cons = co_grains,
           hete = he_grains,
           n_stigma = stigma) %>%
    group_by() %>%
    standardise_name('plant_name')
  
  plant_data <- data %>%
    group_by(site_name, plant_name) %>%
    summarise(status = unique(status)) %>%
    mutate(status = if_else(status == "e", "exotic", "native")) %>%
    group_by()
  
  data <- data %>%
    select(site_name, 
           plant_name,
           treatment,
           cons,
           hete,
           n_stigma) %>%
    rename(recipient = plant_name) %>%
    mutate(plant = 1:nrow(.))
  
  plants <- data %>% 
    select(site_name, plant, n_stigma, treatment, recipient)
  
  conspecific <- data %>%
    mutate(donor = recipient, 
           n_grains = cons) %>% 
    select(plant, recipient, donor, n_grains) 
  
  heterospecific <- data %>%
    mutate(donor = "UNKNOWN", 
           n_grains = hete) %>% 
    select(plant, recipient, donor, n_grains)
  
  data <- bind_rows(conspecific, heterospecific) %>% 
    left_join(plants) %>% 
    mutate(pollen_type = "non_germinated") %>%
    select(site_name, plant, n_stigma, recipient, donor, n_grains, pollen_type, treatment)
  
  message("rearranging data as list")
  # save all data in a list of unique sites in a sort of deposition-site object.
  # each site has a list of properties that describe and relate it to other sites.
  # As a minimum it has a name and a deposition table
  data_dep <- foreach(i=1:n_distinct(site_data$site_name)) %do% {
    site <- list()
    site$name <- site_data$site_name[i]
    site$locality <- site_data$locality[i]
    site$land_use <- site_data$land_use[i]
    site$fragment <- site_data$fragment[i]
    site$fragment_name <- site_data$fragment_name[i]
    site$plant_data <- plant_data %>% 
      filter(site_name == site_data$site_name[i]) %>%
      select(- site_name)
    site$deposition <-  data %>% 
      filter(site_name == site_data$site_name[i]) %>%
      select(- site_name)
    site$study <- "marrero"
    site$sampling <- list(from = as.Date("2010-12-01"), 
                          to = as.Date("2011-03-01"))
    site
  }
  data_dep
}