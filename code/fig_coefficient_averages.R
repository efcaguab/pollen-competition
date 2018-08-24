make_fig_coefficient_avarages <- function(coefficient_averages){
  require(ggplot2)
  require(ggridges)
  coefficient_averages %>% 
    dplyr::filter(scale == "community") %>%
    dplyr::group_by(pollen_category, scale, term, sample_n) %>%
    # dplyr::summarise(estimate = mean(estimate)) %>%
    dplyr::group_by() %>%
    humanize() %>%
    dplyr::mutate(term = forcats::fct_relevel(term, c("abundance", "degree"), after = 2), 
                  pollen_category = dplyr::if_else(pollen_category == "conspecific", 
                                                   "conspecific pollen  ", 
                                                   "heterospecific pollen")) %>%
    ggplot(aes(x = estimate,
               y = term,
               colour = pollen_category, 
               # vline_color = pollen_category, 
               fill = pollen_category)) +
    geom_vline(xintercept = 0, linetype = 1, size = 0.25, color = "grey75") +
    geom_density_ridges(alpha = 0.25, 
                        panel_scaling = T, 
                        scale = 1.5, 
                        quantile_lines = F, quantiles = 2, 
                        vline_linetype = 1, 
                        vline_size = 0.25, 
                        rel_min_height = 0) +
    # coord_flip() +
    pub_theme() +
    facet_grid(~ pollen_category, space = "free_x", scales = "free_x") +
    scale_color_manual(values = c_scale()) +
    scale_fill_manual(values = c_scale()) +
    # scale_discrete_manual("vline_color", values = c_scale()) +
    theme(legend.position = "none", 
          legend.direction = "vertical",
          panel.border = element_blank(), 
          panel.grid.major.x = element_line(size = 0.25)) +
    labs(colour = "", fill = "", x = "estimated effect on pollen gain", y = "")

}

c_scale_discrete <- function(x){
  c_scale()[x]
}
