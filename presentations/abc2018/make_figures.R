# Prepare workspace -------------------------------------------------------

if(!packrat:::isPackratModeOn()) packrat::on()
pkgconfig::set_config("drake::strings_in_dots" = "literals")

library(magrittr)

# make sure that drake project is up to date
# system("make")

# load needed data
load_data_plan <- drake::drake_plan(
  tidied_fixed = drake::readd("tidied_fixed"), 
  model_linear_fits = drake::readd("model_linear_fits"), 
  model_formula_ranking = drake::readd("model_formula_ranking"), 
  model_linear_fits_species = drake::readd("model_linear_fits_species")
)

abc_figures_plan <- rbind(
  load_data_plan
)

abc_config <- drake::drake_config(abc_figures_plan)
drake::make(abc_figures_plan)

