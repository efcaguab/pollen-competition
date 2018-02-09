library(dplyr)
library(brranching)
library(taxize)
library(tibble)
args <- commandArgs(trailingOnly = TRUE)
dep <- readRDS(args[1])
vis <- readRDS(args[2])
deg <- readRDS(args[3])
output_file <- args[4]
# 
deg <- readRDS("data/datasets/processed/all_degree.rds")
dep <- readRDS("data/datasets/processed/all_deposition.rds")
vis <- readRDS("data/datasets/processed/all_visitation.rds")

dep_species <- dep %>%
  plyr::llply(function(x) as.character(x$deposition$recipient)) %>% 
  do.call(c, .) %>% as.data.frame() %>% rownames_to_column("net") %>%
  mutate(net = gsub("[0-9]+", "", net), 
         net = substr(net, 1, nchar(net)-1)) %>% 
  distinct()

hugo_dep_species <- dep %>%
  plyr::llply(function(x) {
    if(x$study == "marrero") x$deposition$recipient
  }) %>%
  do.call(c, .) %>% unique()
deg_species <- deg %>%
  plyr::llply(function(x) as.character(x$degree$plant_name)) %>% 
  do.call(c, .) %>% as.data.frame() %>% rownames_to_column("net") %>%
  mutate(net = gsub("[0-9]+", "", net), 
         net = paste(net, "deg")) %>% 
  distinct()
vis_species <- vis %>%
  plyr::llply(function(x) as.character(x$visitation$plant_name)) %>% 
  do.call(c, .) %>% unique()

plant_names <- dep_species %>% 
  union(deg_species) %>%
  union(vis_species)

# fix misspelled names
plant_names <- plant_names %>%
  gsub('Descurania argentina', 'Descurainia argentina', .) %>%
  gsub('Chiliotrichium rosmarinifolium', 'Chiliotrichum rosmarinifolium', .) 


pt <- phylomatic(plant_names, get = 'POST')
# in_hugos <- if_else(pt$tip.label %in% gsub(" ", "_", tolower(hugo_dep_species)),
                    # "red", "black")

saveRDS(pt, output_file)