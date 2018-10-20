# PREPARE DATA ------------------------------------------------------------

#' Generate replicate datasets for analysis of open-closed flowers
#'
#' @param de dep_frame
#' @param category wether to generate dataset for 'conspecific' or 'heterospecific' pollen
#' @param transformation function to transform density data
#'
#' @return a data frame with pollen_gain column
#'
data_replicate <- function(de, imputed_abundance, imputed_pollen, imputed_degree, imputed_originality, sites, transformation = I, dummy_id){
  
  # de <- drake::readd(dep_frame)
  # ab <- drake::readd(plant_rel_abu)
  # ph <- calculate_phenology_overlap(readd(abu_frame), de)
  # ab <- readd(plant_rel_abu)
  # ph <- readd(plant_pheno_overlap)
  # k <- readd(degree)
  
  de$pollen_category %>%
    unique() %>%
    purrr::map_df(~ get_deposition_sampled_data(de, ., transformation)) %>% 
    repeat_df("scale", c("community", "imputed")) %>% 
    dplyr::left_join(imputed_abundance, by = c("site_name", "plant_name", "scale")) %>% 
    dplyr::left_join(imputed_pollen, by = c("site_name", "plant_name", "scale")) %>% 
    dplyr::left_join(imputed_degree, by = c("site_name", "plant_name", "scale")) %>% 
    dplyr::left_join(imputed_originality, by = c("site_name", "plant_name", "scale")) %>%
    dplyr::inner_join(sites, by = "site_name") %>% 
    dplyr::mutate(
      fragment = as.character(fragment),
      site_plant = paste(plant_name, site_name, sep = "."), 
      var_trans = "log") %>%
    dplyr::filter(scale == "community") %>%
    dplyr::group_by(scale) %>%
    dplyr::filter(pollen_category != "heterospecific_ctr")
}


repeat_df <- function(x, var_name, var_values){
  var_values %>%
    purrr::map_dfr(~ dplyr::mutate(x, !!var_name := .)) 
}

#' Sample the deposition data to get a boostrap data replicate 
#'
#' @param x dep_frame
#' @param category either 'con' or 'het'
#'
#' @return a dataframe with bootstrap replicates
#' 
get_deposition_sampled_data <- function(x, category, transformation){
  
  abs_ctr <- x %>%
    # only work with one category at a time
    dplyr::filter(pollen_category == category) %>% 
    split(list(.$treatment), drop = T) %>%
    purrr::map_df(~ dplyr::sample_n(., size = nrow(.), replace = T)) %>%
    dplyr::mutate(pollen_gain = transformation(pollen_density), 
                  pollen_category = dplyr::if_else(treatment == "closed", 
                                                   paste(category, "ctr", sep = "_"), 
                                                   paste(category, "abs", sep = "_"))) %>%
    dplyr::select(site_name, plant_name, pollen_gain, pollen_category)
    
    rel <- x %>%
      dplyr::filter(pollen_category == category) %>%
      dplyr::select(plant, site_name, plant_name, treatment, pollen_density) %>%
      dplyr::group_by(site_name, plant_name) %>% 
      # find number of pairs that should be done per species-site combination
      dplyr::mutate(
        n_closed = length(pollen_density[treatment == 'closed']), 
        n_open = length(pollen_density[treatment == 'open']), 
        n_samples = max(n_closed, n_open)) %>% 
      split(list(.$site_name, .$plant_name, .$treatment), drop = T) %>% 
      purrr::map_df(~ dplyr::sample_n(., size = .$n_samples[1], replace = T)) %>% 
    dplyr::group_by(site_name, plant_name, treatment) %>%
    # make a dummy id to pair samples
    dplyr::mutate(dummy_id = 1:n()) %>%  
    dplyr::select(-plant, -n_open, -n_closed, -n_samples) %>% 
    # transform count data if desired
    # dplyr::mutate(pollen_density = transformation(pollen_density)) %>% 
    # pair samples
    tidyr::spread(treatment, pollen_density) %>% 
      dplyr::mutate(pollen_gain = transformation(open) - transformation(closed)) %>%
      dplyr::mutate(pollen_category = category)
    
    dplyr::bind_rows(abs_ctr, rel) 
}

# MODELS ------------------------------------------------------------------

#' Evaluate random effect models
#'
#' @param d rep_##
#' @param method either 'REML' or 'ML'
#'
#' @return a list of models
#' 
run_random_models <- function(d, random_effects, method = "REML"){
  
  random_effects %>%
    split(.$random_effect) %>%
    purrr::map_df(~ dplyr::mutate(d, random_formula = .$random_formula, random_effect = .$random_effect)) %>% 
    split(list(.$pollen_category, .$scale, .$var_trans, .$random_effect)) %>%
    purrr::map(~ try(nlme::lme(pollen_gain ~ abn + poc + deg + org, random = as.formula(.$random_formula[1]), na.action = na.omit, method = method, data = .)))
}

run_model <- function(d, best_random, method = "ML"){
  
  vars <- c("abn", "poc", "deg", "org")
  
  paste("pollen_gain ~ ", combn_formulas(vars)) %>%
    purrr::map_df(~ dplyr::mutate(d, fixed_formula = .)) %>% 
    split(list(.$pollen_category, .$scale, .$var_trans, .$fixed_formula)) %>%
    purrr::map(~ try(nlme::lme(as.formula(.$fixed_formula[1]), random = as.formula(best_random$random_formula[1]), na.action = na.omit, method = method, data = .)))

}

#
combn_formulas <- function(vars, null_model = TRUE, from = 1){
  v <- from:length(vars) %>%
    purrr::map(~ combn(vars, .)) %>%
    purrr::map(apply, 2, paste, collapse = " + ") %>%
    unlist()
  if (null_model) {
    v <- c("1", v)
  }
  return(v)
} 

# AGGREGATE MODELS --------------------------------------------------------

glance_random_models <- function(...){
  models <- list(...)
  # models <- list(random_mod_1 = run_random_models(readd(rep_1)), random_mod_2 = run_random_models(readd(rep_2)))
  gather_models(models, broom::glance, c("pollen_category", "scale", "var_trans", "random_effect")) 
}

glance_fixed_models <- function(...){
  models <- list(...)
  gather_models(models, broom::glance, c("pollen_category", "scale", "var_trans", "fixed_formula"))
}

tidy_fixed_models <- function(...){
  models <- list(...)
  gather_models(models, broom::tidy, c("pollen_category", "scale", "var_trans", "fixed_formula"))
}

gather_models <- function(models, fun, subdivisions){
  if(identical(fun, broom::glance)){
    gather_glance(models, fun, subdivisions)
  } else if(identical(fun, broom::tidy)){
    gather_tidy(models, fun, subdivisions)
  }
  
}

gather_glance <- function(models, fun, subdivisions){
  
  fun_model <- function(x){
    x <- x[purrr::map_chr(x, class) != "try-error"]
    
    
    arrange_df <- function(a, z, y){
      # rmsee <- sjstats::rmse(a)
      cbind(fun(a), 
            n_plants = dplyr::n_distinct(a$data$plant_name), 
            n = a$dims$N, 
            # rmse = rmsee,
            # nrmse = rmsee/(max(a$data$pollen_gain) - min(a$data$pollen_gain)),
            # r2 = y[[1]], 
            # o2 = y[[2]],
            r2m = z[1], 
            r2c = z[2], 
            AICc = y[1])
    }
    
    # R2sjstats <- x %>%
      # purrr::map(~ sjstats::r2(.))
    R2mumin <- x %>%
      purrr::map(~ MuMIn::r.squaredGLMM(.))
    
    AICc <- x %>%
      purrr::map(MuMIn::AICc)
    
    purrr::pmap_df(list(x, R2mumin, AICc), .f = arrange_df, .id = "m") %>%
     tidyr::separate("m", subdivisions, sep = "\\.") %>%
      dplyr::mutate(AIC_nc = AIC, 
                    AIC = AICc) 
  }
  
  models %>% 
   purrr::map_df(~ fun_model(.), .id = 'model')
}

gather_tidy <- function(models, fun, subdivisions){
  
  fun_model <- function(x){
    x <- x[purrr::map_chr(x, class) != "try-error"]
    
    x %>%
   purrr::map_df(~ fun(.),  .id = "m") %>%
     tidyr::separate("m", subdivisions, sep = "\\.")
  }
  
  models %>% 
   purrr::map_df(~ fun_model(.), .id = 'model')
}

get_model_correlations <- function(...){
  models <- list(...)
  
  # remove failed models
  failed <- purrr::map(models, purrr::map_chr, class) %>%
    purrr::map_lgl(~ any("try-error" %in% .)) 
  models <- models[!failed]
  
  pearson_slope <- function(x,y){
    data.frame(pearson_slope = cov(x, y) * sd(y) / sd(x))
  }
  
  pearson_slopes <- function(x){
    x %>%
      purrr::map(~ broom::augment(.)) %>%
      purrr::map(~ dplyr::select(., pollen_category, scale, var_trans, site_name, plant_name, fixed_formula, .fitted)) %>% 
      purrr::map_df(~ dplyr::distinct(.)) %>%  
      tidyr::spread(pollen_category, .fitted) %>% 
      # ggplot(aes(x = conspecific, y = heterospecific, colour = interaction(scale, var_trans))) +
      # geom_point() + geom_smooth(method = "lm") + coord_equal()
      # qplot(conspecific, heterospecific, colour = interaction(scale, var_trans), data = .)
      split(list(.$fixed_formula, .$scale, .$var_trans)) %>%
     purrr::map_df(~ pearson_slope(.$conspecific, .$heterospecific), .id = "m") %>%
     tidyr::separate(m, c("scale", "var_trans", "fixed_formula"), sep = "\\.")
  }
  
  models %>%
   purrr::map_df(~ pearson_slopes(.), .id = "model")
}

get_model_linear_fits <- function(...){
  models <- list(...)
  models <- remove_failed_models(models)
  
  pearson_slopes <- function(x){
    x %>%
      expand_model_predictions() %>% 
      # ggplot(aes(x = conspecific, y = heterospecific, colour = interaction(scale, var_trans))) +
      # geom_point() + geom_smooth(method = "lm") + coord_equal()
      # qplot(conspecific, heterospecific, colour = interaction(scale, var_trans), data = .)
      split(list(.$scale, .$var_trans, .$fixed_formula)) %>% 
      get_sma_conspecific_heterospecific() %>%
     tidyr::separate(m, c("scale", "var_trans", "fixed_formula"), sep = "\\.")
  }
  
  models %>%
   purrr::map_df(~ pearson_slopes(.), .id = "model")
}

remove_failed_models <- function(models){
  # remove failed models
  failed <- purrr::map(models, purrr::map_chr, class) %>%
    purrr::map_lgl(~ any("try-error" %in% .)) 
  models <- models[!failed]
  models
}

expand_model_predictions <- function(x){
  x %>% purrr::map(~ broom::augment(.)) %>% 
    purrr::map(~ dplyr::select(., pollen_category, scale, var_trans, site_name, plant_name, fixed_formula, .fitted)) %>%  
    purrr::map_df(~ dplyr::distinct(.)) %>%  
    tidyr::spread(pollen_category, .fitted)
}

pearson_slope <- function(x,y){
  data.frame(pearson_slope = cov(x, y) * sd(y) / sd(x))
}

get_sma_conspecific_heterospecific <- function(x){
  # one relationship for absolute values of conspecific pollen and one for relative ones
  absolute <- x %>%
    purrr::map(~ smatr::sma(conspecific_abs ~ heterospecific_abs, data = .)) %>%
    # purrr::walk(~ plot(.)) %>%
    purrr::map(~ coef(.)) %>% 
    purrr::map_df(~ tibble::rownames_to_column(as.data.frame(.)), .id = "m") %>%
    dplyr::rename(sma_parameter = rowname,
                  value  = '.') %>%
    dplyr::mutate(rel = "absolute")
  
  relative <- x %>%
    purrr::map(~ smatr::sma(conspecific ~ heterospecific, data = .)) %>%
    # purrr::walk(~ plot(.)) %>%
    purrr::map(~ coef(.)) %>% 
    purrr::map_df(~ tibble::rownames_to_column(as.data.frame(.)), .id = "m") %>%
    dplyr::rename(sma_parameter = rowname,
                  value  = '.') %>%
    dplyr::mutate(rel = "relative")
  
  dplyr::bind_rows(absolute, relative)
}

# linear relationship between conspecific and heterospecific pollen per species
get_model_linear_fits_species <- function(...){
  # models <- list(drake::readd(fixed_mod_1), drake::readd(fixed_mod_2), drake::readd(fixed_mod_3))
  models <- list(...)
  
  # remove failed models
  models <- remove_failed_models(models)
  models %>%
    purrr::map_df(expand_model_predictions, .id = "model") 
}
