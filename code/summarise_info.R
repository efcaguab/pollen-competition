
#' Produce a table with summary statistics
#'
#' @param y a list of models
#' @param fun function to tabulate with a data frame as return. Defaults to 
#'
#' @return a data frame 
#' 
glance_table <- function(y, fun = broom::tidy){
  y %>% plyr::ldply(function(x){
    if(class(x) != "try-error"){
      fun(x)
    } else {
      NULL
    }
  })
}

#' Shorten species names 
#'
#' From Genus species to G. species. Ignores Genus sp.
#'
#' @param x a character vector
#'
#' @return a character vector
#'
shorten_sp_name <- function(x){
  y <- x
  for(i in 1:length(x)){
    if(!grepl('sp\\.', x[i])) {
      y[i] <- mini_dot(x[i])
    } 
  }
  y
}

#' Shorten species names
#'
#' #' From Genus species to G. species. Ignores Genus sp.
#' 
#' @param x a character string
#'
#' @return a character string
#'
mini_dot <- function(x){
  require(stringr)
  space_location <- stringr::str_locate(x, ' ')
  if (!any(is.na(space_location))) {
    str_sub(x, 2, space_location[1]) <- ". "
    x
  } else{
    x
  }
}
