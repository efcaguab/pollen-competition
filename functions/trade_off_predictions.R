trade_off_pred <- function(tidied_fixed, 
                           wilcox_glo_com, 
                           variables = NA,
                           chosen_criteria = "rmse",
                           vt = "log"){

  # td <- tidied_fixed %>%
    # filter(var_trans == "log") 
  
  best_models <- wilcox_glo_com %>%
    filter(quality == chosen_criteria,
           var_trans == vt)
  
  model_names <- tidied_fixed %$%
    model %>% unique()
  
  fixed_effect_terms <- tidied_fixed %>%
    filter(!grepl("Intercept", term)) %$% 
    term %>% unique()
  
  trade_off_grid <- seq(-2.5,3,length.out = 100)
  
  pred_df <- expand.grid(value = trade_off_grid, 
                         pollen_category = c("conspecific", "heterospecific")) %>%
    mutate(model = sample(model_names, size = nrow(.), replace = TRUE))
  
  predict_trade_off <- function(x, vt = "log", tidied_fixed, best_models){
    
    value <- x$value[1]
    pc <- x$pollen_category[1]
    m <- x$model[1]
    
    model_type <- best_models %>%
      filter(pollen_category == pc) %$% 
      min_scale
    
    tidied_fixed %>%
      filter(pollen_category == pc, 
             model == m, 
             var_trans == vt, 
             scale == model_type) %>%
      group_by(term) %>%
      summarise_if(is.numeric, median) %>%
      mutate(intercept = estimate[term == "(Intercept)"], 
             response = estimate * value + intercept) %>%
      filter(term != "(Intercept)") %>%
      select(term, response) %>%
      spread("term", "response")
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
        data_frame(term = names(x)[i], min = min(x[names(x)[i]]), max = max(x[names(x)[i]]))
      }
    }
  }
  
  var_limits <- variables %>%
    # map(~ filter(., plant_name %in% plant_species)) %>%
    map_df(~ min_max(.)) %>%
    arrange(term) %>%
    split(.$term)
  
  predictions <- pred_df %>%
    gather("term", "estimate", -value, -pollen_category, -model) %>%
    select(-model) %>%
    spread(pollen_category, estimate) %>%
    mutate(gain = conspecific - heterospecific) %>%
    arrange(term) %>%
    split(.$term)
  
  map2_df(predictions, var_limits, ~ filter(.x, value > .y$min & value < .y$max))
}
