shorten_sp_name <- function(x){
  y <- x
  for(i in 1:length(x)){
    if(!grepl('sp\\.', x[i])) {
      y[i] <- mini_dot(x[i])
    } 
  }
  y
}

mini_dot <- function(x){
  space_location <- stringr::str_locate(x, ' ')
  if(!any(is.na(space_location))){
    stringr::str_sub(x, 2, space_location[1]) <- ". "
    x
  } else{
    x
  }
}
