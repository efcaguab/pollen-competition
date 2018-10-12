# Prepare workspace -------------------------------------------------------

if(!packrat:::isPackratModeOn()) packrat::on()
pkgconfig::set_config("drake::strings_in_dots" = "literals")

library(magrittr)

# make sure that drake project is up to date
# system("make")

# load functions
f1 <- lapply(list.files("code", full.names = T), source)
f2 <- lapply(list.files("presentations/abc2018/figures_code", full.names = T), source)

abc_cache <- drake::recover_cache(".drake_abc")

# Plan --------------------------------------------------------------------

drake::default_cache_path()

# load needed data
tidied_fixed = drake::readd("tidied_fixed", character_only = T)
model_linear_fits = drake::readd("model_linear_fits", character_only = T)
model_formula_ranking = drake::readd("model_formula_ranking", character_only = T)
model_linear_fits_species = drake::readd("model_linear_fits_species", character_only = T)

figure_het_con_plan <- drake::drake_plan(
  fig_con_hetero_elements = get_figure_elements(tidied_fixed, model_linear_fits, model_formula_ranking, model_linear_fits_species),
  fig_canvas = make_fig_het_con_abc(fig_con_hetero_elements, tidied_fixed, rep("white", 2))
)

figures_plan <- rbind(
  figure_het_con_plan
)

abc_config <- drake::drake_config(figures_plan, cache = abc_cache)
drake::make(figures_plan, cache = abc_cache)
