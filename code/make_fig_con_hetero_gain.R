make_fig_con_hetero_gain <- function(tidied_fixed, model_linear_fits) {
  require(ggplot2)
  ab_lines <- model_linear_fits %>%
    dplyr::filter(var_trans == "log",
                  scale == "global") %>%
    tidyr::spread(data = ., key = sma_parameter, value = value)
  
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
