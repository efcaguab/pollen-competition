trade_off_pred <- function(tidied_fixed, 
                           wilcox_glo_com, 
                           variables = NA,
                           chosen_criteria = "r2c",
                           vt = "log", 
                           model_formula){

  # td <- tidied_fixed %>%
    # dplyr::filter(var_trans == "log") 
  
  best_models <- wilcox_glo_com %>%
    dplyr::filter(quality == chosen_criteria,
           var_trans == vt)
  
  model_names <- tidied_fixed %$%
    model %>% unique()
  
  fixed_effect_terms <- tidied_fixed %>%
    dplyr::filter(!grepl("Intercept", term), 
                  fixed_formula == model_formula) %$% 
    term %>% unique()
  
  # trade_off_grid <- seq(-2.5,4.8,length.out = 100)
  trade_off_grid <- rnorm(100, sd = 2)
  
  pred_df <- expand.grid(
    value = trade_off_grid, 
    pollen_category = c("conspecific", "heterospecific")) %>%
    dplyr::mutate(model = sample(model_names, size = nrow(.), replace = TRUE))
  
  predict_trade_off <- function(x, vt = "log", tidied_fixed, best_models){
    
    value <- x$value[1]
    pc <- x$pollen_category[1]
    m <- x$model[1]
    
    model_type <- best_models %>%
      dplyr::filter(pollen_category == pc) %$% 
      min_scale
    
    tidied_fixed %>%
      dplyr::filter(pollen_category == pc, 
             model == m, 
             var_trans == vt, 
             scale == model_type) %>%
      dplyr::group_by(term) %>%
      dplyr::summarise_if(is.numeric, median) %>% 
      dplyr::mutate(intercept = estimate[term == "(Intercept)"], 
             response = estimate * value + intercept) %>%
      dplyr::filter(term != "(Intercept)") %>%
      dplyr::select(term, response) %>%
      tidyr::spread("term", "response")
  }
  
  pred_df <- pred_df %>%
    plyr::ddply(c("value", "pollen_category", "model"),
                predict_trade_off, 
                vt = "log", 
                tidied_fixed = tidied_fixed,
                best_models = best_models)
  
  min_max <- function(x){
    foreach (i = 1:ncol(x), .combine = rbind) %do% {
      if(names(x)[i] %in% fixed_effect_terms){
        dplyr::data_frame(term = names(x)[i], min = min(x[names(x)[i]], na.rm = T), max = max(x[names(x)[i]], na.rm = T))
      }
    }
  }
  
  var_limits <- variables %>%
    # purrr::map(~ dplyr::filter(., plant_name %in% plant_species)) %>%
   purrr::map_df(~ min_max(.)) %>%
    dplyr::arrange(term) %>%
    split(.$term)
  
  predictions <- pred_df %>%
    tidyr::gather("term", "estimate", -value, -pollen_category, -model) %>%
    dplyr::select(-model) %>%
    tidyr::spread(pollen_category, estimate) %>%
    dplyr::mutate(gain = conspecific - heterospecific) %>%
    dplyr::arrange(term) %>%
    split(.$term)
  
  purrr::map2_df(predictions, var_limits, ~ dplyr::filter(.x, value > .y$min & value < .y$max))
}
