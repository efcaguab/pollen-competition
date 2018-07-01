#' Model to compare conspecific pollen deposition of closed vs open flowers
#'
#' @param x data frame with deposition data
#'
#' @return a glm model
#' 
model_conspecific_self <- function(x){
  x %>%
    dplyr::rename(
      species = recipient, 
      community = site_name) %>%
    glm(
      pollen_density ~ species * community, 
      data = ., 
      subset = treatment == 'closed', 
      family = 'quasipoisson')
}


#' Calculate multiple Mann Withney tests in a data frame
#' 
#' Each test is performed by grouping by the splitting column. Calls `mann_withney_df` which contains extra parameters for the test
#'
#' @param x a data frame 
#' @param by column 
#' @param ... other arguments to `mann_withney_df` 
#' @param var column with numeric data for the test
#'
#' @return a list of tests
#' 
mann_withney_part_df <- function(x, by = 'recipient', var, ...){
  x %>% 
      plyr::dlply(by, function(y){
      try(mann_withney_df(y, var, ...))
    })
}

#' Test mann withney inside a data frame column
#'
#' @param x data frame
#' @param var column with numeric data for the test
#' @param treatments levels of variable that splits 
#' @param alternative 
#' @param conf.int 
#'
mann_withney_df <- function(x, var, treatments = c('open', 'closed'), alternative = 'greater', conf.int = FALSE){
  a <- x %>% 
    dplyr::filter_at(var, dplyr::any_vars(. == treatments[1])) %$% 
    pollen_density
  b <- x %>% 
    dplyr::filter_at(var, dplyr::any_vars(. == treatments[2])) %$%
    pollen_density
  wilcox.test(a, b, alternative, conf.int = conf.int)
}
