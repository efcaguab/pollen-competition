# basic R
library(stats)
library(utils)
# additional packages
library(dplyr)
library(foreach)
library(drake)
library(tools)
library(stringr)
library(magrittr)
library(knitr)
library(rmarkdown)
library(xtable)

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
  transfer = clean_transfer('./data/raw/marrero-pollen_transfer.csv', sites),
  abundance = clean_abundance('./data/raw/marrero-abundance.csv', sites),
  armonised_data = armonise_species_names(deposition, visitation_quant, visitation_qual, transfer, abundance)
)

format_data <- drake_plan(
  dep_frame = extract_dep_frame(armonised_data),
  # control_dep_plant = extract_closed_treatment_means(dep_frame, c("season", "site_name", "plant")),
  # control_dep_site = extract_closed_treatment_means(dep_frame, c("season", "site_name")),
  # control_dep_global = extract_closed_treatment_means(dep_frame, NULL), 
  strings_in_dots = 'literals'
)

analysing <- drake_plan(
  consp_self = model_conspecific_self(dep_frame),
  significant_gain_global = mann_withney_part_df(dep_frame, by = 'recipient', var = 'treatment'),
  significant_gain_site = mann_withney_part_df(dep_frame, by = c('recipient', 'site_name'), var = 'treatment'),
  strings_in_dots = 'literals'
)

reporting <- drake_plan(
  'publication/supp_info.pdf' = render('publication/supp_info.Rmd', quiet = TRUE),
  file_targets = TRUE
)

# set up plan
project_plan <- rbind(clean_data, format_data, analysing, reporting)
project_config <- drake_config(project_plan)
vis_drake_graph_sml0(project_config)

# execute plan
make(project_plan)
vis_drake_graph_sml0(project_config)
