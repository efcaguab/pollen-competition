#' Determine wether global looks better than community level
#'
#' @param glanced_models_table glanced_XXX
#'
#' @return a data frame with wilcoxong comparisons
#' 
global_vs_community <- function(glanced_models_table){
  
  test_table <- function(x){
    glanced_models_table %>%
      rename_at(x, function(x) "metric") %>%
      select(pollen_category, model, scale, var_trans, metric) %>% 
      spread(scale, metric) %>%
      split(list(.$pollen_category, .$var_trans)) %>%
      map(~ wilcox.test(.$community, .$global, paired = T, conf.int = T, alternative = "two.sided")) %>%
      map_df(~ tidy(.), .id = "m") %>%
      separate(m, c("pollen_category", "var_trans"))
  }
  
  list(rmse = "rmse", sigma = "sigma", r2 = "r2", o2 = "o2", r2c = "r2c") %>%
    map_df(~test_table(.), .id = "quality") %>%
    mutate(min_scale = if_else(estimate < 0, "community", "global"))
}

best_random_effects <- function(x){
  
}