calc_model_r2_values <- function(model_formula_ranking, glanced_fixed){
  model_formula_ranking$by_model_set %>%
    dplyr::filter(grepl("abs", pollen_category)) %>%
    dplyr::group_by(pollen_category) %>%
    dplyr::filter(delta_AIC_median == min(delta_AIC_median)) %>%
    dplyr::select(fixed_formula, pollen_category, scale) %>%
    dplyr::left_join(glanced_fixed) %>%
    dplyr::select(r2m, r2c) %>%
    dplyr::summarise_all(dplyr::funs(min, median, mean, max)) %>%
    dplyr::mutate_if(is.numeric, round, digits = 2)
}
