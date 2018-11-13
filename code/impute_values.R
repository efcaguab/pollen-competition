# Functions to inpute community values with global ones -----------------

impute_abundace <- function(plant_rel_abu) {
  # drake::loadd(plant_rel_abu)
  datasets <- split_dataset(plant_rel_abu, "abu", "rab") 
 models <-  datasets %>%
    purrr::map(~lme4::lmer(community ~ global +  (global | plant_name) + (global | site_name), data = .))
   # extract(2) %>%
    # purrr::map(plot)
 models_as_df(models, datasets, "abn")
}

impute_degree <- function(degree){
  datasets <- split_dataset(degree, "kn", "k")
  models <-  datasets %>%
    purrr::map(~lme4::lmer(community ~ global +  (1 | plant_name), data = .))
  models_as_df(models, datasets, "deg")
}

impute_shared <- function(shar_pol){
  datasets <- split_dataset(shar_pol, c("n_shared_pol", "kn"), "k")
  models <-  datasets %>%
    purrr::map(~ lm(community ~ global , data = .))
  models_as_df(models, datasets, "deg")
}

impute_originality <- function(org_frame){
  datasets <- org_frame %>%
    dplyr::mutate(var_trans = "log") %>%
    split_dataset("originality", "org")
  models <- datasets %>%
    purrr::map(~lme4::lmer(community ~ global +  (1 | plant_name) + (global | site_name), data = .)) 
  models_as_df(models, datasets, "org")
}

impute_pollen_dominance <- function(pollen_contribution) {
 datasets <- pollen_contribution %>%
   split_dataset(c("grain", "pollen_cont"), "poc")
 models <- datasets %>%
   purrr::map(~lme4::lmer(community ~ global +  (1 | plant_name) + (1 | site_name), data = .))
 models_as_df(models, datasets, "poc")
}

impute_pollen_contrib <- function(pollen_contribution) {
  datasets <- pollen_contribution %>%
    split_dataset(c("grain", "pollen_cont"), "poc")
  models <- datasets %>%
    purrr::map(~lme4::lmer(community ~ global +  (1 | plant_name) + (1 | site_name), data = .))
  models_as_df(models, datasets, "poc")
}

# Auxiliary functions to impute variables -------------------------

models_as_df <- function(x, datasets, val){
  x %>%
    purrr::map2_dfr(datasets, ~modelr::add_predictions(.y, .x)) %>% 
    dplyr::mutate(imputed = dplyr::if_else(is.na(community), pred, community)) %>% 
    dplyr::filter(var_trans == "log") %>%
    dplyr::select(-var_trans, -global, -pred) %>% 
    tidyr::gather(key = scale, value = !!val, c(community, imputed)) 
}

split_dataset <- function(x, remove = NULL, spread_var = NULL){
  if (!is.null(remove)) x %<>% dplyr::select(-!!remove) 
  x %>%
    tidyr::spread("scale", spread_var) %>%
    split(.$var_trans)
}
