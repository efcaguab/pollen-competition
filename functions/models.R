model_conspecific_self <- function(x){
  x %>%
    rename(species = recipient, 
           community = site_name) %>%
    glm(pollen_density ~ species * community, 
             data = ., 
             subset = treatment == 'closed', 
             family = 'quasipoisson')
}


mann_withney_part_df <- function(x, by = 'recipient', categ = 'conspecific', ...){
  x %>% 
    filter(pollen_category == categ) %>%
    plyr::dlply(by, function(y){
      try(mann_withney_df(y, ...))
    })
}

# test mann withney inside a data frame column
mann_withney_df <- function(x, var, treatments = c('open', 'closed'), alternative = 'greater', conf.int = FALSE){
  a <- x %>% 
    filter_at(var, any_vars(. == treatments[1])) %$% 
    pollen_density
  b <- x %>% 
    filter_at(var, any_vars(. == treatments[2])) %$%
    pollen_density
  wilcox.test(a, b, alternative, conf.int = conf.int)
}


