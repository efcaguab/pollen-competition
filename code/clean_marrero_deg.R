# Compile degree data from visitation networks in Hugo's work

lapply(c("dplyr", "readr", "tidyr", "stringr"), 
       function(x) library(x, character.only = T))

# script arguments 
args <- commandArgs(trailingOnly = TRUE)
vis_file <- args[1]
qual_vis_file <- args[2]
output_file <- args[3]

# testing defaults
vis_file <- "../data/datasets/processed/visitation/marrero.rds"
qual_vis_file <- "../data/datasets/raw/marrero2016/qualitative_visits.csv" 
output_file <- "../data/datasets/processed/degree/marrero.rds"

vis <- readRDS(vis_file)

qua <- read_csv(qual_vis_file)
qua %>% 
  mutate(plant_name = plant, 
         plant_name = tolower(plant_name),
         # assume that cf is close to the actual species. Currently only
         # ocurring for *Noticastrum acuminatum*
         plant_name = str_replace(plant_name, " sp. cf. ", " "), 
         plant = plant_name) %>%
  arrange(plant_name) %>%
  separate(plant, c("genus", "species"), sep = " ") %>% View

deg <- plyr::ldply(vis, function(x) x$visitation) %>%
  group_by(plant_name) %>%
  summarise(degree = n_distinct(animal_name)) %>% 
  group_by()

list(name = "marrero", 
     study = "marrero",
     degree = deg) %>%
  list() %>%
  saveRDS(output_file)