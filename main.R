# Prepare workspace -------------------------------------------------------

if(!packrat:::isPackratModeOn()) packrat::on()
pkgconfig::set_config("drake::strings_in_dots" = "literals")

library(magrittr)
library(foreach)

# load functions
f <- lapply(list.files("code", full.names = T), source)

# Clean data --------------------------------------------------------------

# plan to clean data
clean_data_plan <- drake::drake_plan(
  sites = site_data(drake::file_in('./data/raw/marrero-estigmatic_pollen.csv'), drake::file_in('./data/raw/site_names.csv')),
  deposition = clean_deposition(drake::file_in('./data/raw/marrero-estigmatic_pollen.csv'), sites),
  visitation_quant = clean_visitation_quant(drake::file_in('./data/raw/marrero-quantitative_visits.csv'), sites),
  visitation_qual = clean_visitation_qual(drake::file_in('./data/raw/marrero-qualitative_visits.csv'), sites),
  transfer = clean_transfer(drake::file_in('./data/raw/marrero-pollen_transfer.csv'), sites),
  abundance = clean_abundance(drake::file_in('./data/raw/marrero-abundance.csv'), sites),
  random_effects = readr::read_csv(drake::file_in("./data/raw/random_effects.csv")),
  armonised_data = armonise_species_names(deposition, visitation_quant, visitation_qual, transfer, abundance)
)

format_data_plan <- drake::drake_plan(
  dep_frame = extract_dep_frame(armonised_data),
  abu_frame = extract_abu_frame(armonised_data),
  plant_rel_abu = calculate_relative_abundance(abu_frame, dep_frame),
  # plant_pheno_overlap = calculate_phenology_overlap(abu_frame, dep_frame),
  vis_frame = extract_vis_frame(armonised_data),
  degree = get_degree(vis_frame, dep_frame), 
  tra_frame = extract_tra_frame(armonised_data), 
  pollen_contribution = get_pollen_contribution(tra_frame)
)

traits_plan <- drake::drake_plan(
  plant_traits = read_plant_traits(drake::file_in('data/raw/plant_traits.csv')), 
  trait_matrices = make_trait_matrices(plant_traits, abu_frame, TRUE, TRUE), 
  species_coords = get_species_coords(trait_matrices), 
  unq_frame = get_species_uniqueness(species_coords), 
  org_frame = get_species_originality(species_coords, abu_frame)
)

imputation_plan <- drake::drake_plan(
  imputed_degree = impute_degree(degree), 
  imputed_abundance = impute_abundace(plant_rel_abu), 
  imputed_originality = impute_originality(org_frame), 
  imputed_pollen = impute_pollen(pollen_contribution)
)

# Basic analyses ----------------------------------------------------------

# TODO: Check that the missing plants with deposition are fine
# TODO: Impute the community values with a fit from the global values?

basic_analyses_plan <- drake::drake_plan(
  consp_self = model_conspecific_self(dep_frame),
  significant_gain_global = mann_withney_part_df(
    dplyr::filter(dep_frame, pollen_category == 'conspecific'), 
    by = 'recipient', 
    var = 'treatment', 
    conf.int = T),
  significant_gain_site = mann_withney_part_df(
    dplyr::filter(dep_frame, pollen_category == 'conspecific'), 
    by = c('recipient', 'site_name'), 
    var = 'treatment', 
    conf.int = T)
)

# Bootstrap models --------------------------------------------------------

n_replicates <- 99
transformation <- function(x) log(x + 1)

boot_replicates <- drake::drake_plan(
  rep = data_replicate(
    dep_frame, 
    imputed_abundance,
    imputed_pollen,
    imputed_degree, 
    imputed_originality,
    sites, 
    transformation, 
    N)) %>%
  drake::evaluate_plan(rules = list(N = 1:n_replicates)) 

random_models <- drake::drake_plan(
  random_mod = run_random_models(rep_N, random_effects)
) %>%
  drake::evaluate_plan(rules = list(N = 1:n_replicates)) 
glanced_random_models <- random_models %>%
  drake::gather_plan(., gather = "glance_random_models", target = "glanced_random")
random_summaries <- drake::drake_plan(
  best_random = best_random_effect(glanced_random, random_effects)
)

fixed_models <- drake::drake_plan(
  fixed_mod = run_model(rep_N, best_random)) %>%
  drake::evaluate_plan(rules = list(N = 1:n_replicates)) 
glanced_fixed_models <- fixed_models %>%
    drake::gather_plan(., gather = "glance_fixed_models", target = "glanced_fixed")
tidied_fixed_models <- fixed_models %>%
  drake::gather_plan(., gather = "tidy_fixed_models", target = "tidied_fixed")

aic_plan <- drake::drake_plan(
  model_formula_ranking = get_best_fixed_model_formula(glanced_fixed)
)

model_corr <- fixed_models %>%
  drake::gather_plan(., gather = "get_model_correlations", target = "model_correlations")

het_con_linear_fit <- fixed_models %>%
  drake::gather_plan(., gather = "get_model_linear_fits", target = "model_linear_fits")

best_model_formula <- "pollen_gain ~  abn + poc + deg + org"

fixed_summaries <- drake::drake_plan(
  wilcox_glo_com = global_vs_community(glanced_fixed, model_formula = best_model_formula)
)
predictions <- drake::drake_plan(
  trade_off_predictions = trade_off_pred(
    tidied_fixed, 
    wilcox_glo_com, 
    list(imputed_abundance, imputed_pollen, imputed_degree, imputed_originality), 
    chosen_criteria = "r2c", 
    model_formula = best_model_formula)
)

model_plans <- rbind(
  random_models, glanced_random_models, 
  random_summaries, 
  fixed_models, glanced_fixed_models, tidied_fixed_models, 
  model_corr,
  het_con_linear_fit,
  fixed_summaries, 
  predictions
)

# Plots -------------------------------------------------------------------

figure_plan <- drake::drake_plan(
  fig_model_results_global = make_fig_model_results_global(tidied_fixed), 
  fig_con_hetero_gain = make_fig_con_hetero_gain(tidied_fixed, model_linear_fits), 
  fig_proportion_vs_variables = make_fig_proportion_vs_variables(trade_off_predictions),
  fig_pollen_density = make_fig_pollen_density(dep_frame), 
  fig_pollen_density_diff = make_fig_pollen_density_diff(rep_1), 
  fig_abundance = make_fig_abundance(plant_rel_abu, sites), 
  fig_all_model_results = make_fig_all_model_results(tidied_fixed, sites, model_formula_ranking), 
  fig_community_global_scatter = make_fig_community_global_scatter(plant_rel_abu, org_frame, degree, sites, pollen_contribution)
)

# Reporting ---------------------------------------------------------------

reporting_plan <- drake::drake_plan(
  render_pdf(drake::knitr_in('paper/supp-info.Rmd'), drake::file_out('paper/supp-info.pdf'), clean_md = FALSE),
  render_pdf(drake::knitr_in('paper/manuscript.Rmd'), drake::file_out('paper/manuscript.pdf'), clean_md = FALSE),
  render_pdf(drake::file_in('paper/questions_observations_todo.Rmd'), drake::file_out('paper/questions_observations_todo.pdf'), clean_md = FALSE))

# Make all ----------------------------------------------------------------

# set up plan
project_plan <- rbind(
  clean_data_plan, 
  traits_plan,
  format_data_plan,
  imputation_plan,
  boot_replicates,
  model_plans,
  aic_plan,
  basic_analyses_plan,
  figure_plan,
  reporting_plan
  )

project_config <- drake::drake_config(project_plan)
# drake::vis_drake_graph(project_config, targets_only = T)

# execute plan
drake::make(project_plan, parallelism = "parLapply", jobs = 3)
# drake::make(project_plan)
