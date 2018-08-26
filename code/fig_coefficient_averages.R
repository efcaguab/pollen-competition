make_fig_coefficient_avarages <- function(coefficient_averages, variable_importance){
  require(ggplot2)
  require(ggridges)
  dist <- coefficient_averages %>% 
    dplyr::filter(scale == "community") %>%
    dplyr::group_by(pollen_category, scale, term, sample_n) %>%
    # dplyr::summarise(estimate = mean(estimate)) %>%
    dplyr::group_by() %>%
    humanize() %>%
    dplyr::mutate(term = forcats::fct_relevel(term, c("abundance", "degree"), after = 2), 
                  pollen_category = dplyr::if_else(pollen_category == "conspecific", 
                                                   "conspecific pollen  ", 
                                                   "heterospecific pollen")) %>%
    # add fake point lower so that I can include the importance labels
    dplyr::group_by(pollen_category) %>%
    dplyr::mutate(estimate = dplyr::if_else(min(estimate) == estimate,
                                            estimate - 0.1, 
                                            estimate))
  
  # to get the x aesthetic for the importance labels
  min_values <- dist %>%
    dplyr::summarise(x = min(estimate)) %>%
    dplyr::mutate(x = dplyr::if_else(pollen_category == "heterospecific pollen", 
                                     x + 0.03, 
                                     x), 
                  title = "relative\nimportance")
  
  imp <- variable_importance %>%
    tidyr::gather(key = "pollen_category", value = "importance", conspecific, heterospecific) %>%
    dplyr::mutate(term = forcats::fct_relevel(term, c("abundance", "degree"), after = 2), 
                  pollen_category = dplyr::if_else(pollen_category == "conspecific", 
                                                   "conspecific pollen  ", 
                                                   "heterospecific pollen"), 
                  importance = round(importance, 2), 
                  label = sprintf("%.2f", importance)) %>%
    dplyr::inner_join(min_values, by = "pollen_category")
  
  
  p <- dist %>%
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
    geom_text(data = imp, 
              aes(label = label, y = as.numeric(term),
                  x = x), 
              fill = "white", 
              size = 2.6, 
              nudge_y = 0.2, 
              fontface = "bold") +
    geom_text(data = min_values, 
              aes(label = title, x = x, y = 4.9), 
              fontface = "bold",
              size = 2.6, 
              nudge_x = -0.05, 
              hjust = "left", 
              lineheight = 0.6) +
    # coord_flip() +
    pub_theme() +
    facet_grid(~ pollen_category, space = "free_x", scales = "free_x") +
    scale_color_manual(values = c_scale()) +
    scale_fill_manual(values = c_scale()) +
    scale_x_continuous(expand = c(0,0), 
                       breaks = seq(-2,2, by = 0.2)) +
    scale_y_discrete(expand = c(0.01,0.1)) +
    # scale_discrete_manual("vline_color", values = c_scale()) +
    theme(legend.position = "none", 
          legend.direction = "vertical",
          panel.border = element_blank(), 
          panel.grid.major.x = element_line(size = 0.25), 
          axis.ticks = element_blank(), 
          strip.text = element_text(size = 7, face = "bold")) +
    labs(colour = "", fill = "", x = "estimated effect on pollen gain", y = "")

  # pdf(width = 6.5, height = 2)
  p
  # dev.off()
}

c_scale_discrete <- function(x){
  c_scale()[x]
}
