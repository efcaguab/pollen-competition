pub_theme <- function(){
  theme_bw() +
    theme(text = element_text(size = 8), 
          title = element_text(size = 8),
          strip.background = element_blank(),
          strip.text = element_text(hjust = 0),
          legend.title = element_text(hjust = 0.5), 
          legend.key.size = unit(3, "mm"), 
          legend.margin = margin())
}

remove_legend <- function(x){
  require(ggplot2)
  x + theme(legend.position = 'none')
}


#' Change frame contents so that it's ready for rendering
#'
#' @param x a data frame -- usually tidy frame
#' @param sites 
#'
#' @return
#' 
humanize <- function(x, sites = NA, random_effects = NA){
  if ('site_name' %in% names(x)) {
    x <- dplyr::inner_join(x, sites, by = 'site_name')
  }
  if ('term' %in% names(x)) {
    x <- x %>%
      dplyr::mutate(term = dplyr::case_when(
        grepl('org', term) ~ 'trait originality',
        grepl('abn', term) ~ 'relative abundance',
        grepl('rab', term) ~ 'relative abundance',
        grepl('deg', term) ~ 'degree',
        grepl('k', term) ~ 'degree',
        grepl('poc', term) ~ 'share pollen pool',
        TRUE ~ term
      ))
  }
  if ('fixed_formula' %in% names(x)) {
    x <- x %>%
      dplyr::mutate(
        fixed_formula = stringr::str_replace(fixed_formula, "pollen_gain ~", ""),
        fixed_formula = stringr::str_replace(fixed_formula, "abn", "a"),
        fixed_formula = stringr::str_replace(fixed_formula, "poc", "p"),
        fixed_formula = stringr::str_replace(fixed_formula, "org", "t"),
        fixed_formula = stringr::str_replace(fixed_formula, "deg", "k"))
  }
  if ('random_effect' %in% names(x)) {
    x <- dplyr::inner_join(x, random_effects, by = "random_effect")
  }
  x
}

# colour scale values 
c_scale <- function() {
  c("#d7301f", "#fdcc8a", "#fc8d59", "#fef0d9")
}
