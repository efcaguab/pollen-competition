make_fig_coefficient_avarages <- function(coefficient_averages, variable_importance){
  require(ggplot2)
  require(ggridges)
  dist <- coefficient_averages %>% 
    dplyr::filter(scale == "community", 
                  pollen_category %in% c("conspecific_abs", "heterospecific_abs")) %>%
    dplyr::group_by(pollen_category, scale, term, sample_n) %>%
    # dplyr::summarise(estimate = mean(estimate)) %>%
    dplyr::group_by() %>%
    humanize() %>%
    # dplyr::mutate(term = forcats::fct_relevel(term, c("func. originality", "abundance", "degree"), after = 2)) %>%
    # add fake point lower so that I can include the importance labels
    dplyr::group_by(pollen_category) %>%
    # dplyr::mutate(estimate = dplyr::if_else(max(estimate) == estimate,
                                            # estimate + 0.15, 
                                            # estimate)) %>%
    dplyr::filter(pollen_category != "heterospecific_abs") %>%
    dplyr::group_by()
  
  # to make sure lines extend to the limits
  borders_rows <- dist %>%
    dplyr::mutate(max_max = max(estimate), 
                  min_min = min(estimate)) %>%
    dplyr::group_by(pollen_category) %>%
    dplyr::mutate(max = max(estimate), 
                  min = min(estimate)) %>% 
    tidyr::gather(key = "min_max", "min_max_value", min, max) %>% 
    dplyr::group_by(pollen_category, min_max) %>%
    dplyr::filter(estimate == min_max_value) %>% 
    dplyr::slice(1) %>% 
    dplyr::filter(estimate != min_min & min_max == "min" |
                    estimate != max_max & min_max == "max") %>% 
    dplyr::mutate(estimate = dplyr::if_else(min_max == "min", 
                                            min_min, estimate), 
                  estimate = dplyr::if_else(min_max == "max", 
                                            max_max, estimate)) %>% 
    dplyr::group_by() %>%
    dplyr::select(-min_max, -min_min, -max_max, -min_max_value) 
  
  dist <- dplyr::bind_rows(dist, borders_rows)
  
  # to get the x aesthetic for the importance labels
  min_values <- dist %>%
    dplyr::group_by(pollen_category) %>%
    dplyr::summarise(x = max(estimate)) %>%
    dplyr::mutate(title = "relative var.\nimportance") 
  
  imp <- variable_importance %>%
    tidyr::gather(key = "pollen_category", value = "importance", -term) %>%
    dplyr::mutate(term = forcats::fct_relevel(term, c("abundance", "func. originality", "degree"), after = 2), 
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
    dplyr::mutate(pollen_category = dplyr::if_else(pollen_category == "conspecific (absolute)", 
                                                   "conspecific", 
                                                   pollen_category)) %>%
    dplyr::mutate(term = forcats::fct_relevel(term, c("# shared pol.", "func. originality", "abundance"), after = 0)) %>%
    dplyr::filter(estimate != 0) %>%
    ggplot(aes(x = estimate,
               y = term
               # colour = pollen_category, 
               # vline_color = pollen_category, 
               # fill = pollen_category
               )) +
    geom_vline(xintercept = 0, linetype = 2, size = 0.25, color = "grey30") +
    geom_density_ridges(alpha = 0.8, 
                        panel_scaling = T, 
                        scale = 3, 
                        quantile_lines = F, 
                        quantiles = 2, 
                        fill = "white",
                        vline_linetype = 1, 
                        size = 0.25, 
                        rel_min_height = 0, 
                        bandwidth = 0.015, 
                        colour = "grey30") +
    # geom_text(data = imp, 
    #           aes(label = label, y = as.numeric(term),
    #               x = x, size = importance), 
    #           fill = "white", 
    #           # size = 2.65, 
    #           nudge_y = 0.2, 
    #           nudge_x = -0.04,
    #           fontface = "bold") +
    # geom_text(data = min_values, 
    #           aes(label = title, x = x, y = 4.9), 
    #           fontface = "bold",
    #           size = 2.2, 
    #           nudge_x = 0.06, 
    #           hjust = "right", 
    #           lineheight = 0.7) +
    # coord_flip() +
  # geom_hline(yintercept = 0.5, colour = "red") +
    facet_grid(pollen_category ~ ., scales = "free") +
    # scale_color_manual(values = rev(RColorBrewer::brewer.pal(4, "OrRd"))) +
    # scale_fill_manual(values = rev(RColorBrewer::brewer.pal(4, "OrRd"))) +
    scale_size_continuous(range = c(2.2, 2.65)) +
    # scale_alpha_continuous(range = c(0.7, 0.7)) +
    scale_x_continuous(expand = c(0,0), 
                       breaks = seq(-2,2, by = 0.4)) +
    # scale_y_discrete(expand = c(0,0)) +
    # scale_discrete_manual("vline_color", values = c_scale()) +
    coord_cartesian(clip = "off") +
    pub_theme() +
    theme(legend.position = "none",
          # legend.direction = "horizontal",
          panel.border = element_blank(),
          axis.line.x = element_line(size = 0.25),
          axis.ticks.y = element_blank(),
          strip.placement = "outside", 
          panel.spacing.y = unit(0, "pt"),
          # panel.grid.major.y = element_line(size = 0.25, colour = "grey70"),
          axis.text.y = element_text(size = 7, vjust = 0, colour = "black"),
          axis.title.y = element_blank(),
          # axis.title.x = element_text(size = 8, colour = "grey20"),
          strip.text = element_text(size = 7, face = "plain", hjust = 0)) +
    labs(title = "(b) distribution of effects",
         subtitle = "based on 100 bootstrap repllicates",
         colour = "", fill = "", x = "estimated effect on pollen density gain", y = "")

  # pdf(width = 6.5, height = 2)
  p
  # dev.off()
}

c_scale_discrete <- function(x){
  c_scale()[x]
}
