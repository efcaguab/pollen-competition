#' Determine wether global looks better than community level
#'
#' @param glanced_models_table glanced_XXX
#'
#' @return a data frame with wilcoxong comparisons
#' 
global_vs_community <- function(glanced_models_table){
  
  test_table <- function(x){
    glanced_models_table %>%
      dplyr::rename_at(x, function(x) "metric") %>%
      dplyr::select(pollen_category, model, scale, var_trans, metric) %>% 
      tidyr::spread(scale, metric) %>%
      split(list(.$pollen_category, .$var_trans)) %>%
      purrr::map(~ wilcox.test(.$community, .$imputed, paired = T, conf.int = T, alternative = "two.sided")) %>%
     purrr::map_df(~ broom::tidy(.), .id = "m") %>%
     tidyr::separate(m, c("pollen_category", "var_trans"))
  }
  
  positive_models <- c("rmse", "sigma", "nrmse")
  
  list(sigma = "sigma", r2c = "r2c") %>%
   purrr::map_df(~test_table(.), .id = "quality") %>%
    dplyr::mutate(min_scale = dplyr::case_when(
      estimate < 0 & quality %in% positive_models ~ "community",
      estimate > 0 & quality %in% positive_models ~ "imputed",
      estimate < 0 & !(quality %in% positive_models) ~ "imputed",
      estimate > 0 & !(quality %in% positive_models) ~ "community"
      ))
}

#' Figure out best random effect
#'
#' @param x glanced_random
#' @param random_effects data frame with random effects
#'
#' @return one-row data frame with the best one, name and formula
#' 
best_random_effect <- function(x, random_effects){
  best <- x %>%
    dplyr::group_by(pollen_category, scale, var_trans, model) %>%
    dplyr::mutate(delta_AIC = AIC - min(AIC)) %>%
    dplyr::group_by(pollen_category, scale, var_trans, random_effect) %>%
    dplyr::summarise(AIC_50 = median(delta_AIC)) %>% 
    dplyr::group_by() %>%
    dplyr::right_join(expand.grid(pollen_category = unique(.$pollen_category),
                           scale = unique(.$scale),
                           var_trans = unique(.$var_trans),
                           random_effect = unique(.$random_effect))) %>%
    dplyr::group_by(random_effect) %>%
    dplyr::summarise(AIC_50 = median(AIC_50)) %>%
    dplyr::filter(AIC_50 == min(AIC_50, na.rm = T)) %$%
    random_effect
  
  random_effects %>%
    dplyr::filter(random_effect == best)
}
