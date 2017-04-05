# clean Tur's pollen deposition data

library(dplyr)
library(foreach)

# script arguments 
args <- commandArgs(trailingOnly = TRUE)
input_file <- args[1]
species_info <- args[2]
output_file <- args[3]

# # testing defaults
# input_file <- "./data/datasets/raw/tur2016/Data_Tur_et_al_2016_EcolLet.txt"
# species_info <- "./data/datasets/raw/tur2016/Species_info_Tur_et_al_2016_EcolLet.txt"
# output_file <- "./data/datasets/processed/deposition/tur.rds"

species <- readr::read_delim(species_info, delim = ";") %>%
  mutate(Species = gsub("spX", "sp.", Species),
         Species = gsub("sp", "sp.", Species),
         species_name = paste(Genus, Species), 
         species_name = gsub("\\.\\.", ".", species_name)) %>%
  select(Abbreviation, species_name) %>%
  bind_rows(data.frame(Abbreviation = c("Sen.sp.", "Sen.unk", "Uni.unk"), 
                       species_name = c("Senecio sp.", "Senecio sp.", "UNKNOWN")))

data <- readr::read_delim(input_file, delim = ";") %>%
  mutate(site_name = if_else(altitude == 2000, "HI", as.character(altitude)), 
         site_name = if_else(altitude == 1800, "ME", as.character(site_name)),
         site_name = if_else(altitude == 1600, "LO", as.character(site_name)), 
         site_name = paste("TU", site_name, sep = "_"), 
         date = as.Date(date, format = "%d-%b-%y")) %>%
  left_join(species, by = c("receptor" = "Abbreviation")) %>%
  rename(recipient = species_name) %>%
  left_join(species, by = c("donor" = "Abbreviation")) %>%
  select(-donor) %>%
  rename(donor = species_name) %>%
  mutate(plant = gsub("\\.[0-9]+", "", plant), 
         plant = gsub("[A-Z]+", "", plant))

site_data <- data %>%
  group_by(site_name) %>% 
  summarise(altitude = unique(altitude), 
            date_start = min(date, na.rm = T), 
            date_stop = max(date, na.rm = T))

per_plant_data <- data %>% 
  group_by(site_name, recipient, plant) %>%
  mutate(n_stigma = n_distinct(stigma)) %>%
  group_by(site_name, recipient, donor, plant) %>%
  summarise(n_stigma = unique(n_stigma), 
            germ = sum(germ), 
            non.germ = sum(non.germ)) %>% 
  reshape2::melt(measure.vars = c("germ", "non.germ"), 
                 variable.name = "pollen_type", 
                 value.name = "n_grains") %>%
  mutate(treatment = "open")

# save all data in a list of unique sites in a sort of deposition-site object.
# each site has a list of properties that describe and relate it to other sites.
# As a minimum it has a name and a deposition table
data <- foreach(i=1:n_distinct(site_data$site_name)) %do% {
  site <- list()
  site$name <- site_data$site_name[i]
  site$altitude <- site_data$altitude[i]
  site$sampling <- list(from = site_data$date_start[i], 
                        to = site_data$date_stop[i])
  site$deposition <-  per_plant_data %>% 
    filter(site_name == site_data$site_name[i]) %>%
    select(- site_name)
  site$study <- "tur"
  site
}

saveRDS(data, output_file)


