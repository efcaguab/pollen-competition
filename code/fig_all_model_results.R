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
                  best_set = dplyr::lag(cum_weight) < 0.95, 
                  best_set = dplyr::if_else(is.na(best_set), TRUE, best_set)) %>%
    dplyr::filter(best_set) 
  
  best_model_formulas <- best_models %$%
    fixed_formula %>%
    unique()
  
  tidied_fixed %>%
    dplyr::filter(term != "(Intercept)" , 
                  var_trans == 'log', 
                  fixed_formula %in% best_model_formulas,
                  scale == "community"
    ) %>%
    dplyr::group_by(pollen_category,scale, model, var_trans, term, fixed_formula) %>%
    dplyr::summarise(estimate = dplyr::first(estimate)) %>%
    dplyr::group_by() %>%
    humanize() %>%
    ggplot(aes(x = estimate, 
               colour = pollen_category, 
               fill = pollen_category)) +
    geom_vline(xintercept = 0, linetype = 2, size = 0.25) +
    stat_density(geom = 'area',
                 alpha = 0.15,
                 position = position_identity(),
                 trim = F) +
    pub_theme() +
    facet_grid(fixed_formula ~ term, scales = "free_x", space = "free_x") +
    scale_color_manual(values = c_scale()) +
    scale_fill_manual(values = c_scale()) +
    scale_x_continuous(breaks = seq(-2,2, by = 0.25)) + 
    theme(legend.position = "top") +
    labs(colour = "", fill = "")
}
