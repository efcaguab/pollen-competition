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
#' @param formula_long Should the formula terms be converted to long names?
#'
#' @return
#' 
humanize <- function(x, sites = NA, random_effects = NA, formula_long = FALSE, term_long = TRUE){
  if ('site_name' %in% names(x) & !is.na(sites)) {
    x <- dplyr::inner_join(x, sites, by = 'site_name')
  }
  if ('term' %in% names(x)) {
    x <- x %>%
      dplyr::mutate(term = dplyr::case_when(
        grepl('org', term) ~ 'func. originality',
        grepl('abn', term) ~ 'abundance',
        grepl('rab', term) ~ 'abundance',
        grepl('deg', term) ~ 'degree',
        grepl('k', term) ~ 'degree',
        grepl('poc', term) ~ 'share pollen',
        TRUE ~ term
      ))
  }
  if ('fixed_formula' %in% names(x)) {
    if (formula_long == TRUE) {
      x <- x %>%
        dplyr::mutate(
          fixed_formula = stringr::str_replace(fixed_formula, "pollen_gain ~", ""),
          fixed_formula = stringr::str_replace(fixed_formula, "abn", "abundance"),
          fixed_formula = stringr::str_replace(fixed_formula, "poc", "share pollen"),
          fixed_formula = stringr::str_replace(fixed_formula, "org", "func. originality"),
          fixed_formula = stringr::str_replace(fixed_formula, "deg", "degree"))
    } else {
      x <- x %>%
        dplyr::mutate(
          fixed_formula = stringr::str_replace(fixed_formula, "pollen_gain ~", ""),
          fixed_formula = stringr::str_replace(fixed_formula, "abn", "a"),
          fixed_formula = stringr::str_replace(fixed_formula, "poc", "p"),
          fixed_formula = stringr::str_replace(fixed_formula, "org", "t"),
          fixed_formula = stringr::str_replace(fixed_formula, "deg", "k"))
    }
  }
  if ('random_effect' %in% names(x)) {
    x <- dplyr::inner_join(x, random_effects, by = "random_effect")
  }
  if ('pollen_category' %in% names(x)) {
    x <- x %>%
      dplyr::filter(pollen_category != "heterospecific_abs") %>%
      dplyr::mutate(pollen_category = dplyr::case_when(
        grepl('conspecific_abs', pollen_category) ~ 'conspecific (absolute)',
        grepl('conspecific', pollen_category) ~ 'conspecific (relative)',
        TRUE ~ 'heterospecific'
      ))
  }
  x
}

# colour scale values 
c_scale <- function() {
  c("#d7301f", "#fdcc8a", "#fc8d59", "#fef0d9")
}
