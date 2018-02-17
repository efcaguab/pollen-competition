library(dplyr)
library(foreach)
library(drake)
library(tools)
library(stringr)
library(magrittr)

# load functions
functions_folder <- './functions'
list_files_with_exts(functions_folder, 'R') %>%
  lapply(source) %>% invisible()

# plan to clean data
clean_data <- drake_plan(
  sites = site_data('./data/raw/marrero-estigmatic_pollen.csv'),
  deposition = clean_deposition('./data/raw/marrero-estigmatic_pollen.csv', sites),
  visitation_quant = clean_visitation_quant('./data/raw/marrero-quantitative_visits.csv', sites),
  visitation_qual = clean_visitation_qual('./data/raw/marrero-qualitative_visits.csv', sites),
  transfer = clean_transfer('./data/raw/marrero-pollen_transfer.csv', sites))
)

# set up plan
project_plan <- rbind(clean_data)
project_config <- drake_config(project_plan)
vis_drake_graph_sml0(project_config)

# execute plan
make(clean_data)
# vis_drake_graph_sml0(project_config)
