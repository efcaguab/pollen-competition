# clean Hugo's visitation data

library(dplyr)
library(foreach)

# script arguments 
args <- commandArgs(trailingOnly = TRUE)
input_file <- args[1]
dep_file <- args[2]
output_file <- args[3]

# # testing defaults
# input_file <- "./data/datasets/raw/marrero2016/visitation.csv"
# dep_file <- "data/datasets/processed/deposition/marrero.rds"
# output_file <- "./data/datasets/processed/visitation/marrero.rds"

site_char <- readRDS(dep_file) %>%
  plyr::ldply(function(x){
    data.frame(fragment_name = x$fragment_name, 
               # fragment = x$fragment,
               site_name = x$name)
  }) %>% 
  # standarise site names =/
  mutate(fragment_name = paste(substr(site_name, 4, 5), 
                               fragment_name, 
                               sep = "_"), 
         fragment_name = gsub("alfalfa", "alfa", fragment_name),
         fragment_name = gsub("pastura", "pas", fragment_name),
         fragment_name = gsub("1agr", "A-1", fragment_name),
         fragment_name = gsub("2agr", "A-2", fragment_name))

data <- readr::read_csv(input_file)
names(data) <- make.names(names(data))

data <- data %>% 
  left_join(site_char, by = c("Site" = "fragment_name")) %>%
  mutate(date = paste(1, date), 
         date = as.Date(date, format = "%d %B %Y"))

plant_data <- data %>%
  group_by(Plant.observed) %>%
  summarise(plant_family = unique(Plant.family)) %>% 
  group_by() %>%
  rename(plant_name = Plant.observed)

plant_abundance <- data %>%
  group_by(site_name, date, Plant.observed) %>%
  summarise(n_flower_units = round(mean(flower.units.observed.per.plant.species))) %>% 
  rename(plant_name = Plant.observed) %>%
  group_by()

animal_data <- data %>%
  # remove records of no visits
  filter(Flower.visitor.ID != "No visits", 
         Visitor_type != "No visits") %>%
  # correct mistake for Astylus quadrilineatus
  mutate(Visitor_type = if_else(Flower.visitor.ID == "Astylus quadrilineatus", 
                                "beetle", 
                                Visitor_type)) %>%
  group_by(Flower.visitor.ID) %>%
  summarise(animal_family = unique(Flower.visitor_family), 
            animal_order = unique(Flower.visitor_order), 
            animal_type = unique(Visitor_type)) %>%
  mutate(animal_type = tolower(animal_type)) %>%
  group_by() %>%
  rename(animal_name = Flower.visitor.ID)

interaction_data <- data %>%
  filter(Visitation.frequency != 0) %>%
  rename(plant_name = Plant.observed, 
         animal_name = Flower.visitor.ID, 
         n_visits = Visitation.frequency) %>%
  select(site_name, date, plant_name, animal_name, n_visits)

data <- foreach(i=1:n_distinct(site_char$site_name)) %do% {
  site <- list()
  site$name <- site_char$site_name[i]
  site$area_sampled <- data %>%
    filter(site_name == site_char$site_name[i]) %$%
    total.area_sampled_m2 %>% unique()
  site$fragment_name <- site_char$fragment_name[i]
  dates <- data %>% 
    filter(site_name == site_char$site_name[i]) %$%
    date %>% range()
  site$sampling <- list(from = dates[1], 
                        to = dates[2])
  site$plant_data <- plant_data 
  site$plant_abundance <- plant_abundance %>% 
    filter(site_name == site_char$site_name[i]) %>%
    select(- site_name)
  site$animal_data <- animal_data 
  site$visitation <-  interaction_data %>% 
    filter(site_name == site_char$site_name[i]) %>%
    select(- site_name)
  site$study <- "marrero"
  site
}

saveRDS(data, output_file)


