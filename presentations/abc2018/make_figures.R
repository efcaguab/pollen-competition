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
  make_fig_het_con_abc(fig_con_hetero_elements, 
                       tidied_fixed, 
                       colour_pallete = rep("black", 2), 
                       colour_guide = "none",
                       filename = drake::file_out("presentations/abc2018/figures/fig_con_hetero_canvas.png")),
  make_fig_het_con_abc(fig_con_hetero_elements, 
                       tidied_fixed, 
                       colour_pallete = c(rev(RColorBrewer::brewer.pal(4, "OrRd"))[1], NA), 
                       colour_guide = "none",
                       add_smooth = FALSE, 
                       filename = drake::file_out("presentations/abc2018/figures/fig_con_hetero_absolute.png")),
  make_fig_het_con_abc(fig_con_hetero_elements, 
                       tidied_fixed, 
                       colour_pallete = c(rev(RColorBrewer::brewer.pal(4, "OrRd"))[1], NA), 
                       colour_guide = "none",
                       add_smooth = TRUE, 
                       alpha_points_factor = 0.25,
                       filename = drake::file_out("presentations/abc2018/figures/fig_con_hetero_absolute_smooth.png")),
  make_fig_het_con_abc(fig_con_hetero_elements, 
                       tidied_fixed, 
                       colour_pallete = rev(RColorBrewer::brewer.pal(4, "OrRd")), 
                       colour_guide = "legend",
                       add_smooth = TRUE, 
                       alpha_points_factor = 1,
                       filename = drake::file_out("presentations/abc2018/figures/fig_con_hetero_absolute_rel_smooth.png"))
)

coefficient_averages <- drake::readd("coefficient_averages", character_only = T)

figure_quant_qual_plan <- drake::drake_plan(
  make_fig_mean_quant_qual(coefficient_averages, 
                           width = ggplot2::unit(12, "in"),
                           height = ggplot2::unit(4.5, "in"),
                           filename = drake::file_out("presentations/abc2018/figures/fig_quant_qual.png"))
)

figures_plan <- rbind(
  figure_het_con_plan, 
  figure_quant_qual_plan
)

abc_config <- drake::drake_config(figures_plan, cache = abc_cache)
drake::make(figures_plan, cache = abc_cache)
