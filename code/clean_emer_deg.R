# clean Emer's degree data from Lopezaraiza visitation networks

library(dplyr)

# script arguments 
args <- commandArgs(trailingOnly = TRUE)
input_folder <- args[1]
output_file <- args[2]

# # testing defaults
# input_folder <- "./data/datasets/raw/lopezaraiza2007"
# output_file <- "./data/datasets/processed/degree/emer.rds"

files <- list.files(input_folder, full.names = T)
files <- files[grepl(" b", files)]
file_names <- basename(files) %>% 
  tools::file_path_sans_ext() %>%
  gsub(" b", "", .) %>%
  gsub(" ", "_", .)
names(files) <- file_names

deg <- files %>%
  plyr::ldply(read.table, sep = "\t") %>%
  select(V1, V2) %>%
  rename(plant_name = V1) %>%
  group_by(plant_name) %>%
  summarise(degree = n_distinct(V2)) %>%
  group_by()

list(name = "emer",
     study = "emer",
     degree = deg) %>%
  list() %>%
  saveRDS(output_file)
