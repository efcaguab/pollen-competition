make_fig_coefficient_avarages <- function(coefficient_averages, variable_importance){
  require(ggplot2)
  require(ggridges)

  pal <- common_graphic_metrics()$pal_el_green[c(6,8)]

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

  dist %<>%
    dplyr::group_by() %>%
    dplyr::mutate(pollen_category = dplyr::if_else(pollen_category == "conspecific (absolute)",
                                                   "conspecific",
                                                   pollen_category))

  labels <- dist %>%
    dplyr::filter(estimate != 0) %>%
    dplyr::mutate(x = estimate, categ1 = term, categ2 = pollen_category) %>%
    dplyr::mutate(min_min = min(x),
                  max_max = max(x)) %>%
    dplyr::group_by(categ1) %>%
    dplyr::mutate(min_x = min(x),
                  max_x = max(x),
                  inner = min(abs(min_x), abs(max_x)),
                  outer = max(abs(min_x), abs(max_x)),
                  mean_x = mean(unique(c(min_x, max_x)))) %>%
    dplyr::select(categ1, categ2, min_min, max_max, min_x, max_x, inner, outer, mean_x) %>%
    dplyr::distinct() %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      sign_outer = dplyr::if_else(
        which.max(c(abs(min_x), abs(max_x))) == 1,
        sign(min_x), sign(max_x)),
      sign_inner = dplyr::if_else(
        which.min(c(abs(min_x), abs(max_x))) == 1,
        sign(min_x), sign(max_x)),
    x_coord = inner * sign_inner,
    x_coord_labs = mean_x,
    x_coord_title = mean_x,
    x_align = dplyr::if_else(x_coord == max_x,
                             "left", "right"),
    x_align_labs = dplyr::if_else(x_coord == min_x, "left", "right"),
    sign_title = dplyr::if_else(x_align == "right",
                                   -1, 1),
    x_coord = dplyr::if_else(categ2 == "func. originality",
                             outer * sign_outer, x_coord)) %>%
    dplyr::rename(term = categ1, pollen_category = categ2) %>%
    dplyr::group_by() %>%
    dplyr::mutate(term_n = dplyr::case_when(
      term == "# shared pol." ~ 4,
      term == "func. originality" ~ 3,
      term == "abundance" ~ 2,
      TRUE ~ 1
    ))
  p <- dist  %>%
    dplyr::filter(estimate != 0) %>%
    dplyr::mutate(term_n = dplyr::case_when(
      term == "# shared pol." ~ 4,
      term == "func. originality" ~ 3,
      term == "abundance" ~ 2,
      TRUE ~ 1
    ),
                  pollen_category = forcats::fct_relevel(pollen_category, c("conspecific", "heterospecific")),
                  pollen_category = forcats::fct_rev(pollen_category)) %>%
    ggplot() +
    geom_vline(xintercept = 0, linetype = 2, size = 0.25, color = "grey30") +
    geom_density_ridges(aes(x = estimate,
                            y = pollen_category,
                            colour = pollen_category),
                        alpha = 0.8,
                        panel_scaling = F,
                        scale = 4,
                        quantile_lines = F,
                        quantiles = 2,
                        fill = "white",
                        vline_linetype = 1,
                        size = 0.25,
                        rel_min_height = 0,
                        bandwidth = 0.015) +
    geom_text(data = labels,
              aes(x = x_coord + sign_title * 0.075,
                  colour = pollen_category,
                  y = pollen_category,
                  label = abb_col(pollen_category),
                  hjust = x_align),
              nudge_x = 0.01,
            stat = "unique", show.legend = F,
            size = 2) +
    geom_text(data = labels %>% dplyr::select(-pollen_category) %>% dplyr::distinct(),
              aes(x = x_coord_title,
                  y = 0.25,
                  label = term,
                  hjust = "center"),
              fontface = "plain",
              stat = "unique", show.legend = F,
              size = 2.5,
              colour = cgm()$pal_el_green[9]) +
    facet_grid(term_n ~ .) +
    scale_color_manual(values = pal) +
    scale_x_continuous(expand = c(0,0),
                       breaks = seq(-2,2, by = 0.4)) +
    # scale_y_discrete(expand = c(1,0)) +
    # scale_discrete_manual("vline_color", values = c_scale()) +
    coord_cartesian(clip = "off", ylim = c(-0.5,2.25)) +
    pub_theme() +
    theme(legend.position = "none",
          # legend.direction = "horizontal",
          panel.border = element_blank(),
          axis.line.x = element_line(size = 0.25),
          axis.ticks.y = element_blank(),
          strip.placement = "outside",
          panel.spacing.y = unit(0, "pt"),
          # panel.grid.major.y = element_line(size = 0.25, colour = "grey70"),
          # axis.text.y = element_text(size = 7, vjust = 0, colour = "black"),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          plot.subtitle = element_text(margin = margin(b = 15)),
          # axis.title.x = element_text(size = 8, colour = "grey20"),
          strip.text = element_blank(
            #size = 7, face = "plain", hjust = 1, margin = margin(t = - 5)
            )) +
    labs(title = "(b) distribution of effects",
    subtitle = "based on 100 bootstrap repllicates",
    colour = "", fill = "", x = "estimated effect on (log) pollen deposition", y = "")

  # pdf(width = 6.5/5*2, height = 2.29)
  p
  # dev.off()
}

c_scale_discrete <- function(x){
  c_scale()[x]
}
