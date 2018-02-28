# PREPARE DATA ------------------------------------------------------------

#' Generate replicate datasets for analysis of open-closed flowers
#'
#' @param de dep_frame
#' @param category wether to generate dataset for 'conspecific' or 'heterospecific' pollen
#' @param transformation function to transform density data
#'
#' @return a data frame with pollen_gain column
#'
data_replicate <- function(de, ab, ph, k, sites, transformation = I, dummy_id){
  
  # de <- readd(dep_frame)
  # ab <- calculate_relative_abundance(readd(abu_frame), de)
  # ph <- calculate_phenology_overlap(readd(abu_frame), de)
  # ab <- readd(plant_rel_abu)
  # ph <- readd(plant_pheno_overlap)
  # k <- readd(degree)
  
  de$pollen_category %>%
    unique() %>%
    map_df(~ get_deposition_sampled_data(de, ., transformation)) %>% 
    left_join(ab, by = c("site_name", "plant_name")) %>%
    left_join(ph, by = c("site_name", "plant_name", "var_trans", "scale")) %>%
    left_join(k, by = c("site_name", "plant_name", "var_trans", "scale")) %>%
    inner_join(sites, by = "site_name") %>%
    mutate(fragment = as.character(fragment),
           site_plant = paste(plant_name, site_name, sep = ".")) %>%
    group_by(scale)
}

#' Sample the deposition data to get a boostrap data replicate 
#'
#' @param x dep_frame
#' @param category either 'con' or 'het'
#'
#' @return a dataframe with bootstrap replicates
#' 
get_deposition_sampled_data <- function(x, category, transformation){
  
  x %>%
    # only work with one category at a time
    filter(pollen_category == category) %>%
    select(plant, site_name, plant_name, treatment, pollen_density) %>%
    group_by(site_name, plant_name) %>%
    # find number of pairs that should be done per species-site combination
    mutate(n_closed = length(pollen_density[treatment == 'closed']), 
           n_open = length(pollen_density[treatment == 'open']), 
           n_samples = min(n_closed, n_open)) %>% 
    # sample the number of pairs with replacement
    split(list(.$site_name, .$plant_name, .$treatment), drop = T) %>%
    map_df(~ sample_n(., size = .$n_samples[1], replace = T)) %>%
    group_by(site_name, plant_name, treatment) %>%
    # make a dummy id to pair samples
    mutate(dummy_id = 1:n()) %>% 
    select(-plant, -n_open, -n_closed, -n_samples) %>%
    # transform count data if desired
    mutate(pollen_density = transformation(pollen_density)) %>%
    # pair samples
    spread(treatment, pollen_density) %>% 
    mutate(pollen_gain = open - closed) %>%
    mutate(pollen_category = category)
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
  
  rem <- random_effects %>%
    split(.$random_effect) %>%
    map_df(~ mutate(d, random_formula = .$random_formula, random_effect = .$random_effect)) %>% 
    split(list(.$pollen_category, .$scale, .$var_trans, .$random_effect)) %>%
    map(~ try(lme(pollen_gain ~ rab + tov + k, random = as.formula(.$random_formula[1]), na.action = na.omit, method = method, data = .)))
}

run_model <- function(d, best_random, method = "ML"){
  
  d %>%
    split(list(.$pollen_category, .$scale, .$var_trans)) %>%
    map(~ lme(pollen_gain ~ rab + tov + k, random = as.formula(best_random$random_formula[1]), na.action = na.omit, method = method, data = .))

}

# AGGREGATE MODELS --------------------------------------------------------

glance_random_models <- function(...){
  models <- list(...)
  # models <- list(random_mod_1 = run_random_models(readd(rep_1)), random_mod_2 = run_random_models(readd(rep_2)))
  gather_models(models, glance, c("pollen_category", "scale", "var_trans", "random_effect"))
}

glance_fixed_models <- function(...){
  models <- list(...)
  gather_models(models, glance, c("pollen_category", "scale", "var_trans"))
}

tidy_fixed_models <- function(...){
  models <- list(...)
  gather_models(models, tidy, c("pollen_category", "scale", "var_trans"))
}

gather_models <- function(models, fun, subdivisions){
  if(identical(fun, glance)){
    gather_glance(models, fun, subdivisions)
  } else if(identical(fun, tidy)){
    gather_tidy(models, fun, subdivisions)
  }
  
}

gather_glance <- function(models, fun, subdivisions){
  
  fun_model <- function(x){
    x <- x[map_chr(x, class) != "try-error"]
    
    
    arrange_df <- function(a, y, z){
      cbind(fun(a), 
            n_plants = n_distinct(a$data$plant_name), 
            n = a$dims$N, 
            rmse = sjstats::rmse(a), 
            r2 = y[[1]], 
            o2 = y[[2]],
            r2m = z[1], 
            r2c = z[2])
    }
    
    R2sjstats <- x %>%
      map(~ r2(.))
    R2mumin <- x %>%
      map(~ MuMIn::r.squaredGLMM(.))
    
    pmap_df(list(x, R2sjstats, R2mumin), .f = arrange_df, .id = "m") %>%
      separate("m", subdivisions)
  }
  
  models %>% 
    map_df(~ fun_model(.), .id = 'model')
}

gather_tidy <- function(models, fun, subdivisions){
  
  fun_model <- function(x){
    x <- x[map_chr(x, class) != "try-error"]
    
    x %>%
    map_df(~ fun(.),  .id = "m") %>%
      separate("m", subdivisions)
  }
  
  models %>% 
    map_df(~ fun_model(.), .id = 'model')
}