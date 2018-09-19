make_fig_coefficient_avarages <- function(coefficient_averages, variable_importance){
  require(ggplot2)
  require(ggridges)
  dist <- coefficient_averages %>% 
    dplyr::filter(scale == "community") %>%
    dplyr::group_by(pollen_category, scale, term, sample_n) %>%
    # dplyr::summarise(estimate = mean(estimate)) %>%
    dplyr::group_by() %>%
    humanize() %>%
    dplyr::mutate(term = forcats::fct_relevel(term, c("func. originality", "abundance", "degree"), after = 2)) %>%
    # add fake point lower so that I can include the importance labels
    dplyr::group_by(pollen_category) %>%
    dplyr::mutate(estimate = dplyr::if_else(max(estimate) == estimate,
                                            estimate + 0.15, 
                                            estimate)) %>%
    dplyr::filter(pollen_category != "heterospecific_abs") %>%
    dplyr::group_by()
  
  # to get the x aesthetic for the importance labels
  min_values <- dist %>%
    dplyr::group_by(pollen_category) %>%
    dplyr::summarise(x = max(estimate)) %>%
    dplyr::mutate(title = "relative var.\nimportance") 
  
  imp <- variable_importance %>%
    tidyr::gather(key = "pollen_category", value = "importance", -term) %>%
    dplyr::mutate(term = forcats::fct_relevel(term, c("func. originality", "abundance", "degree"), after = 2), 
                  # pollen_category = dplyr::if_else(pollen_category == "conspecific", 
                                                   # "conspecific pollen  ", 
                                                   # "heterospecific pollen"), 
                  importance = round(importance, 2), 
                  label = sprintf("%.2f", importance)) %>%
    dplyr::inner_join(min_values, by = "pollen_category") %>%
    dplyr::filter(pollen_category != "heterospecific_abs")
  
  dist %<>% 
    dplyr::inner_join(imp) %>%
    dplyr::group_by() 
  
  p <- dist %>%
    dplyr::filter(estimate != 0) %>%
    ggplot(aes(x = estimate,
               y = term,
               colour = pollen_category, 
               # vline_color = pollen_category, 
               fill = pollen_category)) +
    geom_vline(xintercept = 0, linetype = 2, size = 0.25, color = "grey30") +
    geom_density_ridges(alpha = 0.65, 
                        panel_scaling = F, 
                        scale = 1.5, 
                        quantile_lines = F, quantiles = 2, 
                        vline_linetype = 1, 
                        vline_size = 0.25, 
                        rel_min_height = 0, 
                        bandwidth = 0.015) +
    geom_text(data = imp, 
              aes(label = label, y = as.numeric(term),
                  x = x, size = importance), 
              fill = "white", 
              # size = 2.65, 
              nudge_y = 0.2, 
              nudge_x = -0.04,
              fontface = "bold") +
    geom_text(data = min_values, 
              aes(label = title, x = x, y = 4.9), 
              fontface = "bold",
              size = 2.2, 
              nudge_x = 0.06, 
              hjust = "right", 
              lineheight = 0.7) +
    # coord_flip() +
    pub_theme() +
    facet_grid(~ pollen_category, space = "free_x", scales = "free_x") +
    scale_color_manual(values = rev(RColorBrewer::brewer.pal(4, "OrRd"))) +
    scale_fill_manual(values = rev(RColorBrewer::brewer.pal(4, "OrRd"))) +
    scale_size_continuous(range = c(2.2, 2.65)) +
    # scale_alpha_continuous(range = c(0.7, 0.7)) +
    scale_x_continuous(expand = c(0,0), 
                       breaks = seq(-2,2, by = 0.4)) +
    scale_y_discrete(expand = c(0.01,0.1)) +
    # scale_discrete_manual("vline_color", values = c_scale()) +
    theme(legend.position = "none", 
          legend.direction = "horizontal",
          panel.border = element_blank(), 
          panel.grid.major.x = element_line(size = 0.25), 
          axis.ticks = element_blank(),
          axis.text.y = element_text(size = 7, vjust = 0, colour = "grey20"),
          axis.title.y = element_blank(),
          axis.title.x = element_text(size = 8, colour = "grey20"),
          strip.text = element_text(size = 7, face = "bold", hjust = 0)) +
    labs(colour = "", fill = "", x = "estimated effect on pollen density gain", y = "")

  # pdf(width = 6.5, height = 2)
  p
  # dev.off()
}

c_scale_discrete <- function(x){
  c_scale()[x]
}
