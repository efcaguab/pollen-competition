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