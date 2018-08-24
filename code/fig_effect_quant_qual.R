make_fig_effect_quant_qual <- function(summary_effects, model_formula_ranking){
  require(ggplot2)
  best_models <- model_formula_ranking$by_model_set %>%
    dplyr::group_by(scale, pollen_category) %>%
    dplyr::mutate(likelyhood = get_likelyhoods(delta_AIC), 
                  weights = get_weights(likelyhood)) %>% 
    dplyr::arrange(delta_AIC) %>% 
    dplyr::mutate(cum_weight = cumsum(weights), 
                  best_set = dplyr::lag(cum_weight) < 0.95, 
                  best_set = dplyr::if_else(is.na(best_set), TRUE, best_set)) %>%
    dplyr::filter(scale == "community") 
  
  best_model_formulas <- best_models %$%
    fixed_formula %>%
    unique()
  
  agg_formulas <- model_formula_ranking$aggregated %>%
    dplyr::mutate(len = stringr::str_length(fixed_formula)) %>%
    humanize(formula_long = TRUE) %>%
    dplyr::arrange(len, fixed_formula)
  
  params <- list(
    tile_width = 0.9,
    scale_length = 5
  )
  params$scale = scales::brewer_pal(palette = "RdBu")(params$scale_length)
  
  summary_effects %>%
    dplyr::filter(scale == "community", 
                  fixed_formula %in% best_model_formulas) %>%
    tidyr::gather(key = "response", value = "value", quantity_median, quality_median) %>%
    dplyr::group_by() %>%
    humanize(formula_long = TRUE) %>% 
    split(.$response) %>%
    purrr::map(plot_for_one_response, agg_formulas, params)
}

plot_for_one_response <- function(x, agg_formulas, params){
  x %>%
    dplyr::inner_join(agg_formulas, by = "fixed_formula") %>%
    dplyr::mutate(fixed_formula = forcats::fct_reorder(fixed_formula, len, .desc = FALSE)) %>%
    ggplot(aes(y = fixed_formula, x = term)) + 
    geom_tile(aes(fill = value), height = params$tile_width, width = params$tile_width) +
    geom_text(aes(label = round(value, 2)), size = 2.5) +
    scale_fill_gradient2(high = params$scale[params$scale_length], 
                         low = params$scale[1]) +
    pub_theme() +
    coord_equal() +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))
}
