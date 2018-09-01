make_fig_con_hetero_gain <- function(tidied_fixed, model_linear_fits, model_formula_ranking) {
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
    dplyr::sample_n(size = 1, weight = weight)%>%
    dplyr::group_by()
  
  dplyr::data_frame(x = get_pred_range(tidied_fixed, "heterospecific"),
                   y = get_pred_range(tidied_fixed, "conspecific")) %>%
    ggplot(aes(x, y)) +
    geom_abline(data = ab_lines, 
                aes(slope = slope, intercept = elevation), 
                size = 0.1, linetype = 1, colour = "grey", 
                alpha = 0.5) +
    geom_abline(data = dplyr::summarise_if(ab_lines, is.numeric, median),
    aes(slope = slope, intercept = elevation),
    size = 0.5, linetype = 1, colour = "black") +
    geom_abline(slope = 1, intercept = 0, size = 0.25, linetype = 2) +
    geom_point(alpha = 0) +
    coord_equal(xlim = get_pred_range(tidied_fixed, "heterospecific"),
                ylim = get_pred_range(tidied_fixed, "conspecific")) +
    pub_theme() +
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
