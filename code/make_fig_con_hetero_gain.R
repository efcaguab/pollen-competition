make_fig_con_hetero_gain <- function(tidied_fixed, model_linear_fits, model_formula_ranking, model_linear_fits_species) {
  require(ggplot2)
  
  model_weights <- model_formula_ranking$aggregated %>%
    # dplyr::filter(scale == "community") %>%
    # dplyr::group_by(pollen_category) %>%
    dplyr::mutate(likelyhood = get_likelyhoods(delta_AIC_median), 
                  weight = get_weights(likelyhood)) %>%
    dplyr::select(fixed_formula, weight)
  
  ab_lines <- model_linear_fits %>%
    dplyr::filter(var_trans == "log", 
                  scale == "community") %>%
    tidyr::spread(data = ., key = sma_parameter, value = value) %>%
    dplyr::inner_join(model_weights, by = "fixed_formula") %>%
    dplyr::group_by(model, scale) %>%
    dplyr::sample_n(size = 1, weight = weight) %>%
    dplyr::group_by()
  
  points_sp <- model_linear_fits_species %>%
    dplyr::filter(var_trans == "log",
                  scale == "community") %>% 
  #   tidyr::spread(data = ., key = sma_parameter, value = value) %>% 
    dplyr::inner_join(model_weights, by = "fixed_formula") %>% 
    dplyr::group_by(site_name, plant_name, scale, model) %>%
    dplyr::sample_n(size = 1, weight = weight) %>%
    dplyr::group_by()
  
  exp_minus_one <- function(x){
    exp(x)-1
  }
  
  points_sp_summarised <- points_sp %>%
    dplyr::group_by(plant_name, site_name) %>%
    dplyr::summarise_at(c("conspecific", "heterospecific"), 
                        dplyr::funs(median, quantile_05,quantile_95)) %>%
    dplyr::mutate_if(is.numeric, I) 

  dplyr::data_frame(x = get_pred_range(tidied_fixed, "heterospecific"),
                   y = get_pred_range(tidied_fixed, "conspecific")) %>%
    ggplot() +
    geom_abline(data = ab_lines,
    aes(slope = slope, intercept = elevation),
    size = 0.1, linetype = 1, colour = "grey",
    alpha = 0.5) +
    geom_errorbar(data = points_sp_summarised,
                  aes(x = heterospecific_median, ymin = conspecific_quantile_05, ymax = conspecific_quantile_95),
                  show.legend = F, alpha = 0.5, colour = "grey") +
    geom_errorbarh(data = points_sp_summarised,
                  aes(x = heterospecific_median,
                      y = conspecific_median,
                      xmin = heterospecific_quantile_05, xmax = heterospecific_quantile_95),
                  show.legend = F, alpha = 0.5, colour = "grey") +
    geom_point(data = points_sp_summarised,
    aes(x = heterospecific_median, y = conspecific_median),
    alpha = 1, show.legend = T, shape = 21, colour = "black", size = 1) +
    geom_abline(data = dplyr::summarise_if(ab_lines, is.numeric, median),
                aes(slope = slope, intercept = elevation),
                size = 0.5, linetype = 1, colour = "black") +
    geom_abline(slope = 1, intercept = 0, size = 0.25, linetype = 2) +
    geom_hline(yintercept = 0, size = 0.25, linetype = 2) +
    coord_cartesian(xlim = c(0, get_pred_range(tidied_fixed, "heterospecific")[2]),
                    ylim = c(0, get_pred_range(tidied_fixed, "conspecific"))) +
    # scale_x_continuous(breaks = seq(0,10, 2)) +
    # scale_y_continuous(breaks = seq(0,10,2)) +
    pub_theme() +
    scale_color_discrete() +
    labs(x = "gain in heterospecific pollen",
         y = "gain in conspecific pollen")
}

# function just to get the range of preditions
get_pred_range <- function(tidied_fixed, x, var_trans = "log", scale = "global"){
  tidied_fixed %>% 
    dplyr::filter(
      pollen_category == x,
      var_trans == var_trans, 
      scale == scale) %$% 
    estimate %>% 
    range()
}
