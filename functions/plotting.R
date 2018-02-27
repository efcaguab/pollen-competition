pub_theme <- function(){
  theme_bw() +
    theme(text = element_text(size = 8), 
          title = element_text(size = 8),
          strip.background = element_blank(),
          strip.text = element_text(hjust = 0),
          legend.title = element_text(hjust = 0.5))
}

remove_legend <- function(x){
  x + theme(legend.position = 'none')
}


#' Change frame contents so that it's ready for rendering
#'
#' @param x a data frame -- usually tidy frame
#' @param sites 
#'
#' @return
#' @export
#'
#' @examples
humanize <- function(x, sites = NA){
  if('site_name' %in% names(x)){
    x <- inner_join(x, sites, by = 'site_name')
  }
  if('term' %in% names(x)){
    x <- x %>%
      mutate(term = case_when(
        grepl('tov', term) ~ 'phenology overlap',
        grepl('rab', term) ~ 'relative abundance', 
        TRUE ~ term
      ))
  }
  if('random_effect' %in% names(x)){
    x <- x %>%
      mutate(random_effect = case_when(
        random_effect == 'plantINcommunity' ~ '1 | comunity / plant sp.',
        random_effect == 'plantINfragment' ~ '1 | locality / land use / fragment / plant sp.',
        random_effect == 'plantINlanduse' ~ '1 | locality / land use / plant sp.',
        random_effect == 'plantINlocality' ~ '1 | locality / plant sp.', 
        TRUE ~ '1 | plant sp.'
      ))
  }
  x
}
