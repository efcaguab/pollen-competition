# clean Emmer's pollen deposition data

library(dplyr)
library(foreach)

# script arguments 
args <- commandArgs(trailingOnly = TRUE)
input_file <- args[1]
output_file <- args[2]

# # testing defaults
# input_file <- "./data/datasets/raw/emer2015/pollen_transfer.csv"
# output_file <- "./data/datasets/processed/deposition/emer.rds"

data <- readr::read_csv(input_file) %>%
  group_by(river, habitat) %>%
  mutate(fragment = as.numeric(as.factor(sitecode))) %>%
  mutate(hab = if_else(habitat == "invaded", "IN", "NI"),
         riv = substr(river, 1, 2), 
         riv = toupper(riv), 
         site_name = paste("EM", riv, hab, fragment, sep = "_"), 
         date = as.Date(day, format = "%d/%m/%y")) %>%
  rename(recipient = stigmaspecies) %>%
  mutate(pollenspecies = gsub("^ni", "UNKNOWN", pollenspecies)) %>%
  rename(plant = code, 
         donor = pollenspecies, 
         n_stigma = nstigmas,
         n_grains = quantity) %>%
  mutate(pollen_type = "non_germinated", 
         treatment = "open",
         donor = if_else(grepl("m[0-9]", donor), toupper(donor), donor)) %>%
  group_by()

site_data <- data %>% 
  group_by(site_name) %>%
  summarise(habitat = unique(habitat), 
            river = unique(river),
            fragment = unique(fragment), 
            date_start = min(date), 
            date_stop = max(date)) %>%
  group_by()

plant_data <- data %>%
  group_by(site_name, recipient) %>%
  summarise(stigmatype = unique(stigmatype)) %>%
  rename(plant_name = recipient,
         stigma_type = stigmatype) %>%
  group_by()

data <- data %>%
  select(site_name, plant, n_stigma, recipient, donor, n_grains, pollen_type, treatment)

data <- foreach(i=1:n_distinct(site_data$site_name)) %do% {
  site <- list()
  site$name <- site_data$site_name[i]
  site$locality <- site_data$river[i]
  site$habitat <- site_data$habitat[i]
  site$fragment <- site_data$fragment[i]
  site$sampling <- list(from = site_data$date_start[i], 
                        to = site_data$date_stop[i])
  site$plant_data <- plant_data %>% 
    filter(site_name == site_data$site_name[i]) %>%
    select(- site_name)
  site$deposition <-  data %>% 
    filter(site_name == site_data$site_name[i]) %>%
    select(- site_name)
  site$study <- "emer"
  site
}

saveRDS(data, output_file)

