# Prepare workspace -------------------------------------------------------

pkgconfig::set_config("drake::strings_in_dots" = "literals")

library(magrittr)
library(foreach)
library(drake)

# load functions
f <- lapply(list.files("code", full.names = T), source)

n_replicates <- 99
transformation <- function(x) log(x + 1)

# Clean data --------------------------------------------------------------

# plan to clean data
clean_data_plan <- drake_plan(
  sites = site_data(file_in('./data/raw/marrero-estigmatic_pollen.csv'), file_in('./data/raw/site_names.csv')),
  deposition = clean_deposition(file_in('./data/raw/marrero-estigmatic_pollen.csv'), sites),
  visitation_quant = clean_visitation_quant(file_in('./data/raw/marrero-quantitative_visits.csv'), sites),
  visitation_qual = clean_visitation_qual(file_in('./data/raw/marrero-qualitative_visits.csv'), sites),
  transfer = clean_transfer(file_in('./data/raw/marrero-pollen_transfer.csv'), sites),
  abundance = clean_abundance(file_in('./data/raw/marrero-abundance.csv'), sites),
  random_effects = readr::read_csv(file_in("./data/raw/random_effects.csv")),
  armonised_data = armonise_species_names(deposition, visitation_quant, visitation_qual, transfer, abundance)
)

format_data_plan <- drake_plan(
  dep_frame = extract_dep_frame(armonised_data),
  abu_frame = extract_abu_frame(armonised_data),
  plant_rel_abu = calculate_relative_abundance(abu_frame, dep_frame),
  # plant_pheno_overlap = calculate_phenology_overlap(abu_frame, dep_frame),
  vis_frame = extract_vis_frame(armonised_data),
  degree = get_degree(vis_frame, dep_frame),
  shar_pol = get_shared_pol(vis_frame),
  tra_frame = extract_tra_frame(armonised_data),
  pollen_dominance = get_pollen_dominance(tra_frame, vis_frame),
  pollen_contribution = get_pollen_contribution(tra_frame)
)

traits_plan <- drake_plan(
  plant_traits = read_plant_traits(file_in('data/raw/plant_traits.csv')),
  trait_matrices = make_trait_matrices(plant_traits, abu_frame, TRUE, TRUE),
  species_coords = get_species_coords(trait_matrices, weighted = TRUE),
  unq_frame = get_species_uniqueness(species_coords),
  org_frame = get_species_originality(species_coords, abu_frame)
)

imputation_plan <- drake_plan(
  imputed_degree_legacy = impute_degree(degree),
  imputed_degree = impute_shared(shar_pol),
  imputed_abundance = impute_abundace(plant_rel_abu),
  imputed_originality = impute_originality(org_frame),
  imputed_pollen = impute_pollen_dominance(pollen_dominance),
  imputed_pollen_legacy = impute_pollen_contrib(pollen_contribution)
)

# Basic analyses ----------------------------------------------------------

# TODO: Check that the missing plants with deposition are fine
# TODO: Impute the community values with a fit from the global values?

basic_analyses_plan <- drake_plan(
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

boot_replicates <- drake_plan(
  rep = data_replicate(
    dep_frame,
    imputed_abundance,
    imputed_pollen,
    imputed_degree,
    imputed_originality,
    sites,
    transformation,
    N)) %>%
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
  fixed_mod = run_model(rep_N, best_random)) %>%
  evaluate_plan(rules = list(N = 1:n_replicates))
glanced_fixed_models <- fixed_models %>%
    gather_plan(., gather = "glance_fixed_models", target = "glanced_fixed")
tidied_fixed_models <- fixed_models %>%
  gather_plan(., gather = "tidy_fixed_models", target = "tidied_fixed")

aic_plan <- drake_plan(
  model_formula_ranking = get_best_fixed_model_formula(glanced_fixed)
)

model_corr <- fixed_models %>%
  gather_plan(., gather = "get_model_correlations", target = "model_correlations")

het_con_linear_fit <- fixed_models %>%
  gather_plan(., gather = "get_model_linear_fits", target = "model_linear_fits")

het_con_linear_fit_sp <- fixed_models %>%
  gather_plan(., gather = "get_model_linear_fits_species", target = "model_linear_fits_species")

best_model_formula <- "pollen_gain ~  abn + poc + deg + org"

fixed_summaries <- drake_plan(
  wilcox_glo_com = global_vs_community(glanced_fixed, model_formula = best_model_formula),
  summary_effects = get_summary_effects(tidied_fixed),
  coefficient_averages = get_coefficient_averages(tidied_fixed, model_formula_ranking, N = 99),
  variable_importance = get_variable_importance(model_formula_ranking),
  r2_values = calc_model_r2_values(model_formula_ranking, glanced_fixed)
)

predictions <- drake_plan(
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
  het_con_linear_fit, het_con_linear_fit_sp,
  fixed_summaries,
  predictions
)

pca_plan <- drake_plan(
  pca_data = get_pca_data(plant_rel_abu, pollen_contribution, degree, org_frame, sites),
  pcas = get_pca(pca_data, imputation_variants = 0:2),
  random_plant_distances = all_randomisations_plant_name(pcas, 99),
  random_site_distances = all_randomisations_site_name(pcas, 99),
  permanova_plant_distances = get_permanova(random_plant_distances, "plant_name"),
  permanova_site_distances = get_permanova(random_site_distances, "site_name"),
  fig_pca = plot_pca(pcas, chosen_threshold = 0),
  fig_distances = plot_permanova_dist(permanova_plant_distances, permanova_site_distances)

)

facilitation_plan <- drake_plan(
  facilitation_models = model_facilitation(dep_frame),
  fig_pca_contrib = plot_pca_variances_and_contributions(pcas, chosen_threshold = 0),
  facilitation_random_effects = extract_random_effects(facilitation_models),
  facilitation_plot_df = get_facilitation_plot_df(dep_frame, facilitation_random_effects),
  fig_random_slopes = plot_random_slopes(facilitation_plot_df)
)

analyses_plan <- rbind(
  clean_data_plan,
  traits_plan,
  format_data_plan,
  imputation_plan,
  boot_replicates,
  model_plans,
  aic_plan,
  basic_analyses_plan,
  pca_plan,
  facilitation_plan
)

# Paper -------------------------------------------------------------------

figure_plan <- drake_plan(
  fig_model_results_global = make_fig_model_results_global(tidied_fixed),
  fig_con_hetero_gain = make_fig_con_hetero_gain(tidied_fixed, model_linear_fits, model_formula_ranking, model_linear_fits_species),
  fig_hetero_con = make_fig_con_hetero_empirical(dep_frame),
  open_bagged_model = model_open_bagged(dep_frame),
  coef_open_bagged = get_coef_open_bagged(open_bagged_model),
  con_con_plot_df = get_con_con_plot_df(coef_open_bagged),
  fig_con_con = plot_bagged_vs_open_conspecific(con_con_plot_df),
  fig_proportion_vs_variables = make_fig_proportion_vs_variables(trade_off_predictions),
  fig_pollen_density = make_fig_pollen_density(dep_frame),
  fig_pollen_density_diff = make_fig_pollen_density_diff(rep_1),
  fig_abundance = make_fig_abundance(plant_rel_abu, sites),
  fig_all_model_results = make_fig_all_model_results(tidied_fixed, sites, model_formula_ranking),
  fig_community_global_scatter = make_fig_community_global_scatter(plant_rel_abu, org_frame, degree, sites, pollen_contribution),
  fig_effect_quant_qual = make_fig_effect_quant_qual(summary_effects, model_formula_ranking),
  fig_coefficient_averages = make_fig_coefficient_avarages(coefficient_averages, variable_importance),
  fig_average_qual_quant = make_fig_average_quant_qual(coefficient_averages),
  fig_correlation = make_fig_correlation(rep_1),
  fig_var_importance = plot_variable_importance(variable_importance),
  fig_coef_avg = plot_coefficient_averages(coefficient_averages, variable_importance)
)

reporting_plan <- drake_plan(
  abstract = readLines(file_in("./paper/abstract.md")),
  keywords = process_keywords(file_in("./paper/keywords.md")),
  acknowledgements = readLines(file_in("./paper/acknowledgements.md")),
  intro_line_number = get_line_number(file_in("paper/manuscript.Rmd"), "# Introduction"),
  abs_wordcount = count_words(file_in("paper/abstract.md")),
  msc_wordcount = count_words(file_in('paper/manuscript.Rmd'), lines_to_ignore = 1:intro_line_number),
  n_references = count_references(file_in('paper/manuscript.Rmd'), lines_to_ignore = 1:intro_line_number, refs_to_exclude = "@ref"),
  n_displays = count_displays(file_in('paper/manuscript.Rmd'), lines_to_ignore = 1:intro_line_number),
  msc_title = get_yaml_title(file_in('paper/manuscript.Rmd')),
  render_pdf(knitr_in('paper/supp-info.Rmd'), file_out('paper/supp-info.pdf'), clean_md = FALSE),
  render_pdf(file_in('paper/draft-info.Rmd'), file_out('paper/draft-info.pdf'), clean_md = FALSE),
  render_pdf(knitr_in('paper/manuscript.Rmd'), file_out('paper/manuscript.pdf'), clean_md = FALSE),
  knitr::knit2pdf(knitr_in("paper/cover-letter.Rnw"), output = file_out("paper/cover-letter.tex"))
  )

paper_plan <- rbind(
  figure_plan,
  reporting_plan
)

# Make all ----------------------------------------------------------------

# set up plan
project_plan <- rbind(
  analyses_plan,
  paper_plan
  )

project_config <- drake_config(project_plan)
# vis_drake_graph(project_config, targets_only = T)

# execute plan
# make(project_plan, parallelism = "parLapply", jobs = 3)
make(project_plan)
