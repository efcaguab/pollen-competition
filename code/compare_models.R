#' Determine wether global looks better than community level
#'
#' @param glanced_models_table glanced_XXX
#'
#' @return a data frame with wilcoxong comparisons
#' 
global_vs_community <- function(glanced_models_table, model_formula, preferred = "community"){
  
  # determine if imputed has been calculated
  if ("imputed" %in% unique(glanced_models_table$scale)){
    imputed_calculated <- TRUE
  } else {
    imputed_calculated <- FALSE
  }
  
  test_table <- function(x){
    spread_models <- glanced_models_table %>%
      # dplyr::filter(fixed_formula == best_model_formula) %>%
      dplyr::rename_at(x, function(x) "metric") %>%
      dplyr::select(pollen_category, model, scale, var_trans, metric, fixed_formula) %>% 
      tidyr::spread(scale, metric)
    
    # if imputed was not calculated
    if(!imputed_calculated){
      spread_models %<>%
        dplyr::mutate(imputed = community)   
    }
    
    spread_models %>%
      split(list(.$pollen_category, .$var_trans, .$fixed_formula)) %>% 
      purrr::map(~ wilcox.test(.$community, .$imputed, paired = T, conf.int = T, alternative = "two.sided")) %>%
     purrr::map_df(~ broom::tidy(.), .id = "m") %>% 
     tidyr::separate(m, c("pollen_category", "var_trans", "fixed_formula"), sep = "\\.")
  }
  
  positive_models <- c("rmse", "sigma", "nrmse")
  
  glo_com <- list(sigma = "sigma", r2c = "r2c") %>%
   purrr::map_df(~test_table(.), .id = "quality") %>%
    dplyr::mutate(min_scale = dplyr::case_when(
      estimate < 0 & quality %in% positive_models ~ "community",
      estimate > 0 & quality %in% positive_models ~ "imputed",
      estimate < 0 & !(quality %in% positive_models) ~ "imputed",
      estimate > 0 & !(quality %in% positive_models) ~ "community"
      ))
  
  if(!imputed_calculated){
    glo_com %<>%
      dplyr::mutate(min_scale = preferred)
  }
  
  glo_com
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

# Figure out best fixed model formula
get_best_fixed_model_formula <- function(glanced_fixed) {
 by_formula <- glanced_fixed %>%
    dplyr::group_by(model, pollen_category, scale) %>%
    dplyr::mutate(delta_AIC = AIC - min(AIC), 
                  delta_AIC_rank = dplyr::min_rank(delta_AIC)) %>% 
    dplyr::group_by(fixed_formula)
 
 by_formula_and_category <- by_formula %>%
   dplyr::group_by(pollen_category, scale, add = TRUE)
 
 list(by_formula, by_formula_and_category) %>%
   purrr::map(~dplyr::summarise_at(., 
                                   dplyr::vars(delta_AIC, delta_AIC_rank),
                                   dplyr::funs(median, quantile_05, quantile_95, dplyr::n()))) %>% 
   purrr::map(~dplyr::mutate(., delta_AIC = delta_AIC_median, 
                             delta_AIC_rank = delta_AIC_rank_median)) %>%
   `names<-`(c("aggregated", "by_model_set"))
}

quantile_05 <- function(x, na.rm = FALSE){
  quantile(x, 0.05, na.rm = na.rm)
}

quantile_95 <- function(x, na.rm = FALSE){
  quantile(x, 0.95, na.rm = na.rm)
}

get_likelyhoods <- function(x){
  exp(x * 0.5* (-1)) 
}

get_weights <- function(x){
  x / sum(x)
}
