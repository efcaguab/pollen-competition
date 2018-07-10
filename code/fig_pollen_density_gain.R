
make_fig_model_results_global <- function(tidied_fixed) {
  
  require(ggplot2)
  
  tidied_fixed %>%
    dplyr::filter(
      term != "(Intercept)" , 
      var_trans == 'log',
      scale == "imputed") %>%
    dplyr::group_by(pollen_category,scale, model, var_trans, term) %>%
    dplyr::summarise(estimate = dplyr::first(estimate)) %>%
    dplyr::group_by() %>%
    humanize() %>%
    ggplot(aes(
      x = estimate, 
      colour = pollen_category, 
      fill = pollen_category)) +
    geom_vline(xintercept = 0, linetype = 2, size = 0.25) +
    stat_density(
      geom = 'line',
      # alpha = 0.15,
      position = position_identity(),
      # draw_quantiles = c(0.5, 0.95, 0.05), 
      trim = T 
      # scale = "count"
      ) +
    pub_theme() +
    facet_grid( ~ term, scales = "free_x", space = "free_x") +
    scale_color_manual(values = c_scale()) +
    scale_fill_manual(values = c_scale()) +
    scale_x_continuous(breaks = seq(-2,2, by = 0.25)) + 
    theme(legend.position = "top") +
    labs(colour = "", fill = "", x = "estimated effect on pollen gain")

}
