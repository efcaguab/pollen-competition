# Prepare workspace -------------------------------------------------------

if(!packrat:::isPackratModeOn()) packrat::on()
pkgconfig::set_config("drake::strings_in_dots" = "literals")

library(magrittr)

# make sure that drake project is up to date
# system("make")

# load functions
f1 <- lapply(list.files("code", full.names = T), source)
f2 <- lapply(list.files("presentations/abc2018/figures_code", full.names = T), source)

# Plan --------------------------------------------------------------------

getwd()
# load needed data
load_data_plan <- drake::drake_plan(
  abc_tidied_fixed = drake::readd("tidied_fixed", character_only = T), 
  abc_model_linear_fits = drake::readd("model_linear_fits", character_only = T), 
  abc_model_formula_ranking = drake::readd("model_formula_ranking", character_only = T), 
  abc_model_linear_fits_species = drake::readd("model_linear_fits_species", character_only = T)
)

figure_het_con_plan <- drake::drake_plan(
  abc_fig_con_hetero_elements = get_figure_elements(abc_tidied_fixed, abc_model_linear_fits, abc_model_formula_ranking, abc_model_linear_fits_species)
)

abc_figures_plan <- rbind(
  load_data_plan, 
  figure_het_con_plan
)

abc_config <- drake::drake_config(abc_figures_plan)
drake::make(abc_figures_plan)
