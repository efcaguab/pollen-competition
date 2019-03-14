# drake::loadd(tidied_fixed, sites)
# Make the figure that shows all results in one
make_fig_all_model_results <- function(tidied_fixed, sites, model_formula_ranking){
  require(ggplot2)
  # only keep models that make up to 95% of the evidence
  best_models <- model_formula_ranking$by_model_set %>%
    dplyr::group_by(scale, pollen_category) %>%
    dplyr::mutate(likelyhood = get_likelyhoods(delta_AIC),
                  weights = get_weights(likelyhood)) %>%
    dplyr::arrange(delta_AIC) %>%
    dplyr::mutate(cum_weight = cumsum(weights),
                  best_set = dplyr::lag(cum_weight) < 0.99,
                  best_set = dplyr::if_else(is.na(best_set), TRUE, best_set)) %>%
    dplyr::filter(best_set)

  best_model_formulas <- model_formula_ranking$by_model_set %>%
    dplyr::ungroup() %>%
    dplyr::filter(pollen_category %in% c("conspecific_abs", "heterospecific_abs"),
                  delta_AIC_median < 4) %>%
    dplyr::mutate(fixed_formula = forcats::fct_reorder(fixed_formula,
                                                       delta_AIC_median,
                                                       .desc = T)) %>%
    dplyr::arrange(dplyr::desc(fixed_formula)) %$%
    fixed_formula %>%
    unique()

  best_model_formula_short <-
    tibble::data_frame(fixed_formula = best_model_formulas) %>%
    humanize() %$%
    fixed_formula

  col_pal <- cgm()$pal_el_green[c(8,6)]

  tidied_fixed %>%
    dplyr::filter(term != "(Intercept)" ,
                  var_trans == 'log',
                  fixed_formula %in% best_model_formulas,
                  scale == "community") %>%
    dplyr::group_by(pollen_category,scale, model, var_trans, term, fixed_formula) %>%
    dplyr::summarise(estimate = dplyr::first(estimate)) %>%
    dplyr::group_by() %>%
    humanize() %>%
    dplyr::mutate(fixed_formula = forcats::fct_relevel(fixed_formula,
                                                       best_model_formula_short)) %>%
    dplyr::filter(pollen_category %in% c("conspecific (absolute)",
                                         "heterospecific")) %>%
    dplyr::mutate(pollen_category =
                    dplyr::if_else(pollen_category == "conspecific (absolute)",
      "conspecific pollen", "heterospecific pollen")) %>%
    ggplot(aes(x = estimate,
               colour = pollen_category,
               fill = pollen_category)) +
    geom_vline(xintercept = 0, linetype = 2, size = 0.25, colour = "grey30") +
    stat_density(geom = 'line',
                 # alpha = 0.15,
                 position = position_identity(),
                 trim = F,
                 size = 0.5) +
    pub_theme() +
    facet_grid(fixed_formula ~ term, scales = "free_x", space = "free_x") +
    scale_color_manual(values = col_pal) +
    scale_fill_manual(values = col_pal) +
    scale_x_continuous(breaks = seq(-2,2, by = 0.2)) +
    scale_y_continuous(breaks = seq(0,50, by = 10)) +
    theme(legend.position = "top",
          panel.border = element_rect(size = 0.15, colour = "grey50"),
          panel.spacing = unit(1, "mm"),
          # axis.line.x.bottom = element_line(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          strip.text.y = element_text(hjust = 0.5)) +
    labs(colour = "", fill = "", y = "probability density")
}
