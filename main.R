if(!packrat:::isPackratModeOn()) packrat::on()
pkgconfig::set_config("drake::strings_in_dots" = "literals")

# # basic R
# library(stats)
# library(utils)
# # additional packages
# library(foreach)
# library(sjstats)
# library(readr)
# library(MuMIn)
# library(smatr)
# library(drake)
# library(tools)
# library(stringr)
# library(magrittr)
# library(knitr)
# library(rmarkdown)
# library(xtable)
# library(spaa)  # niche overlap
# library(purrr)
# library(tidyr)
# library(bookdown)  # figure and table references in reports
# library(tinytex)
# library(ggplot2)
# library(broom) 
# library(nlme)
# library(tibble)
# library(dplyr)

# load functions
f <- lapply(list.files("code", full.names = T), source)

# plan to clean data
clean_data <- drake_plan(
  sites = site_data('./data/raw/marrero-estigmatic_pollen.csv', './data/raw/site_names.csv'),
  deposition = clean_deposition('./data/raw/marrero-estigmatic_pollen.csv', sites),
  visitation_quant = clean_visitation_quant('./data/raw/marrero-quantitative_visits.csv', sites),
  visitation_qual = clean_visitation_qual('./data/raw/marrero-qualitative_visits.csv', sites),
  transfer = clean_transfer('./data/raw/marrero-pollen_transfer.csv', sites),
  abundance = clean_abundance('./data/raw/marrero-abundance.csv', sites),
  random_effects = read_csv("./data/raw/random_effects.csv"),
  armonised_data = armonise_species_names(deposition, visitation_quant, visitation_qual, transfer, abundance)
)

format_data <- drake_plan(
  dep_frame = extract_dep_frame(armonised_data),
  abu_frame = extract_abu_frame(armonised_data),
  plant_rel_abu = calculate_relative_abundance(abu_frame, dep_frame),
  plant_pheno_overlap = calculate_phenology_overlap(abu_frame, dep_frame),
  vis_frame = extract_vis_frame(armonised_data),
  degree = get_degree(vis_frame, dep_frame),
)

analysing <- drake_plan(
  consp_self = model_conspecific_self(dep_frame),
  significant_gain_global = mann_withney_part_df(filter(dep_frame, pollen_category == 'conspecific'), by = 'recipient', var = 'treatment', conf.int = T),
  significant_gain_site = mann_withney_part_df(filter(dep_frame, pollen_category == 'conspecific'), by = c('recipient', 'site_name'), var = 'treatment', conf.int = T),
)

n_replicates <- 10
transformation <- function(x) log(x + 1)

boot_replicates <- drake_plan(
  rep = data_replicate(dep_frame, 
                       plant_rel_abu,
                       plant_pheno_overlap,
                       degree, 
                       sites, 
                       transformation, N), 
) %>%
  evaluate_plan(rules = list(N = 1:n_replicates)) 

random_models <- drake_plan(
  random_mod = run_random_models(rep_N, random_effects)
) %>%
  evaluate_plan(rules = list(N = 1:n_replicates)) 

glanced_random_models <- random_models %>%
  gather_plan(., gather = "glance_random_models", target = "glanced_random")

random_summaries <- drake_plan(
  best_random = best_random_effect(glanced_random, random_effects)
)

fixed_models <- drake_plan(
  fixed_mod = run_model(rep_N, best_random)
) %>%
  evaluate_plan(rules = list(N = 1:n_replicates)) 

glanced_fixed_models <- fixed_models %>%
    gather_plan(., gather = "glance_fixed_models", target = "glanced_fixed")

tidied_fixed_models <- fixed_models %>%
  gather_plan(., gather = "tidy_fixed_models", target = "tidied_fixed")

model_corr <- fixed_models %>%
  gather_plan(., gather = "get_model_correlations", target = "model_correlations")

het_con_linear_fit <- fixed_models %>%
  gather_plan(., gather = "get_model_linear_fits", target = "model_linear_fits")

fixed_summaries <- drake_plan(
  wilcox_glo_com = global_vs_community(glanced_fixed)
)

predictions <- drake_plan(
  trade_off_predictions = trade_off_pred(tidied_fixed, 
                                         wilcox_glo_com, 
                                         list(plant_rel_abu, plant_pheno_overlap, degree), 
                                         chosen_criteria = "nrmse"), 
)

model_plans <- rbind(
  random_models, glanced_random_models, 
  random_summaries, 
  fixed_models, glanced_fixed_models, tidied_fixed_models, 
  model_corr, het_con_linear_fit,
  fixed_summaries, 
  predictions
)

reporting <- drake_plan(
  'paper/supp_info.tex' = render('paper/supp_info.Rmd', quiet = TRUE),
  'paper/supp_info.pdf' = latexmk('paper/supp_info.tex', clean = FALSE),
  'paper/manuscript.tex' = render('paper/manuscript.Rmd', quiet = TRUE),
  'paper/manuscript.pdf' = latexmk('paper/manuscript.tex', clean = FALSE),
  'paper/questions_observations_todo.pdf' = my_render('paper/questions_observations_todo.Rmd', quiet = TRUE, depends_on = 'paper/manuscript.pdf'),
  file_targets = TRUE
)

# set up plan
project_plan <- rbind(clean_data, format_data,
                      boot_replicates, 
                      model_plans,
                      analysing, reporting)
project_config <- drake_config(project_plan)
vis_drake_graph(project_config, split_columns = T, targets_only = T)

# execute plan
# make(project_plan, parallelism = "parLapply", jobs = 7)
make(project_plan)
