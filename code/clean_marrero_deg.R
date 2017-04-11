# Compile degree data from visitation networks in Hugo's work

library(dplyr)

# script arguments 
args <- commandArgs(trailingOnly = TRUE)
input_file <- args[1]
output_file <- args[2]

# # testing defaults
# input_file <- "./data/datasets/processed/visitation/marrero.rds"
# output_file <- "./data/datasets/processed/degree/marrero.rds"

vis <- readRDS(input_file)

deg <- plyr::ldply(vis, function(x) x$visitation) %>%
  group_by(plant_name) %>%
  summarise(degree = n_distinct(animal_name)) %>% 
  group_by()

list(name = "marrero", 
     study = "marrero",
     degree = deg) %>%
  list() %>%
  saveRDS(output_file)