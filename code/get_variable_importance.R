get_variable_importance <- function(model_formula_ranking){

  # get the terms in the models
  terms <- model_formula_ranking$aggregated$fixed_formula %>%
    stringr::str_remove("pollen_gain ~  ") %>%
    stringr::str_split(stringr::fixed(" + ")) %>%
    unlist() %>%
    unique()
  # remove the null model
  terms <- terms[terms != "1"]

  model_weights <- model_formula_ranking$by_model_set %>%
    dplyr::filter(scale == "community") %>%
    dplyr::group_by(pollen_category) %>%
    dplyr::mutate(likelyhood = get_likelyhoods(delta_AIC_median),
                  weight = get_weights(likelyhood))

  crosses <- purrr::cross2(terms, unique(model_formula_ranking$by_model_set$pollen_category))
  crosses %>%
    purrr::map(~model_weights$weight[stringr::str_detect(model_weights$fixed_formula, .[[1]]) & model_weights$pollen_category == .[[2]]]) %>%
    purrr::map(sum) %>%
    purrr::map2_df(crosses,
                   ~ dplyr::data_frame(
                     var = .y[[1]],
                     pollen_category = .y[[2]],
                     importance = .x)
    ) %>%
    dplyr::mutate(importance = round(importance, digits = 3)) %>%
    dplyr::rename(term = var) %>%
    humanize() %>%
    tidyr::spread(pollen_category, importance) %>%
    dplyr::arrange(dplyr::desc(heterospecific))
}

plot_variable_importance <- function(variable_importance) {
  require(ggplot2)

  plot_importance <- function(x){
    middle_space <- -0.4  # for 1/3 is -0.525
    x %>% ggplot(aes(x = term, y = value)) +
      # geom_segment(aes(xend = term, yend = 0)) +
      # geom_point(shape = 21, fill = cgm()$pal_rb3[2], size = 6) +

      geom_col(fill = cgm()$pal_el_green[1]) +
      # geom_hline(yintercept = 0, linetype = 2, size = 0.25, colour = "grey30") +

      # geom_point(aes(x = 1, y = middle_space), alpha = 0) +
      geom_text(aes(label = paste0(" ", format(round(value, digits = 2)), " "),
                    hjust = text_align),
                size = 2) +
      # geom_text(aes(label = term, y = middle_space),
      #           # fontface = "bold",
      #           size = 2.5,
      #           colour = "black") +
      scale_x_discrete(expand = c(0,0)) +
      coord_flip() +
      pub_theme() +
      theme(axis.title.y = element_blank(),
            legend.position = "none",
            panel.border = element_blank(),
            axis.ticks = element_blank(),
            axis.title = element_blank(),
            axis.text = element_blank())
  }

  plots <- variable_importance %>%
    tidyr::gather("key", "value", `conspecific (absolute)`:heterospecific) %>%
    dplyr::filter(key %in% c("conspecific (absolute)", "heterospecific")) %>%
    dplyr::mutate(term = forcats::fct_reorder(term, value),
                  # text_align = dplyr::if_else(value < 0.1,
                                              # "outward", "inward")) %>%
                  text_align = dplyr::if_else(key == "heterospecific",
                                              "right", "left")) %>%
    split(.$key) %>%
    purrr::map(plot_importance)

  plots <- plots %>%
    # subtitles
    purrr::map2(c("conspecific pollen", "heterospecific pollen"),
              function(x,y) {x + labs(subtitle = y)}) %>%
    purrr::map2(list(margin(r = 0, t = 5.5, b = 5.5, l = 5.5), margin(l = 0, r = 5.5, t = 5.5, b = 5.5)),
                function(x,y) {x + theme(plot.margin = y)})

  plots[[3]] <- variable_importance %>%
    ggplot(aes(y = term)) +
    geom_text(aes(label = term), x = 0.5, size = 2.3) +
    # expansion to match the 90% width of the columns
    scale_y_discrete(expand = c(0,0.45)) +
    theme_minimal() +
    theme(axis.text = element_blank(),
          axis.title = element_blank(),
          panel.grid = element_blank(),
          plot.margin = margin(r = 0, t = 5.5, b = 5.5, l = 0))

  plots[[1]] <- plots[[1]] + labs(title = "(a) relative variable importance")
  # plots[[2]] <- plots[[2]] + scale_x_discrete(position = "top", expand = c(0,0))
  plots[[1]] <- plots[[1]] + scale_y_reverse(expand = c(0,0))
  plots[[2]] <- plots[[2]] + scale_y_continuous(expand = c(0,0))
  plots[[2]] <- plots[[2]] + theme(plot.subtitle = element_text(hjust = 1))
  plots[[1]] <- plots[[1]] + theme(plot.title = element_text(margin = margin(b = 5.5, unit = "pt")))

  # pdf(width = 3.25 , height = 1.25)
  # cowplot::plot_grid(
  #   plots[[1]],
  #   plots[[3]],
  #   plots[[2]],
  #   align = "h",
  #   rel_widths = c(1,0.75,1),
  #   nrow = 1
  # )
  # dev.off()
  plots
}


