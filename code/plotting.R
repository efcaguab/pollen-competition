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
        grepl('tov', term) ~ 'phenology overlap',
        grepl('rab', term) ~ 'relative abundance',
        grepl('k', term) ~ 'degree',
        TRUE ~ term
      ))
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
