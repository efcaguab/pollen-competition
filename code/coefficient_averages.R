get_coefficient_averages <- function(tidied_fixed, model_formula_ranking, N = 99){
  model_weights <- model_formula_ranking$by_model_set %>%
    dplyr::group_by(pollen_category, scale) %>%
    dplyr::mutate(l = get_likelyhoods(delta_AIC_median), 
                  w = get_weights(l)) %>% 
    dplyr::select(pollen_category, scale, fixed_formula, w)
    
  1:N %>%
    purrr::map_df(get_coeficient_estimate_sample, tidied_fixed, model_weights, N)
  
}

get_coeficient_estimate_sample <- function(sample_n, tidied_fixed, model_weights, N){
  # consider cases with zero
  
  tidied_fixed %>%
    dplyr::filter(term != "(Intercept)") %>% 
    dplyr::group_by(model, pollen_category, scale, fixed_formula, term) %>%
    dplyr::summarise(estimate = unique(estimate)) %>% 
    dplyr::group_by() %>% 
    # complete cases. The estimate of the term for the models in which it wasn't present is zero
    tidyr::complete(model, pollen_category, scale, fixed_formula, term, fill = list(estimate = 0)) %>% 
    dplyr::inner_join(model_weights, 
                      by = c("pollen_category", "scale", "fixed_formula")) %>% 
    dplyr::group_by(pollen_category, scale, term) %>%
    dplyr::sample_n(size = N, replace = T, weight = w) %>% 
    # dplyr::summarise(estimate = weighted.mean(estimate, w)) %>%
    dplyr::mutate(sample_n = sample_n)
}
