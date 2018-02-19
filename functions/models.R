model_conspecific_self <- function(x){
  glm(pollen_density ~ recipient * site_name, 
             data = x, 
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
mann_withney_df <- function(x, var, treatments = c('open', 'closed'), alternative = 'greater'){
  a <- x %>% 
    filter_at(var, any_vars(. == treatments[1])) %$% 
    pollen_density
  b <- x %>% 
    filter_at(var, any_vars(. == treatments[2])) %$%
    pollen_density
  wilcox.test(a, b, alternative)
}


