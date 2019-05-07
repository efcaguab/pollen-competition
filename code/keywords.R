# read keywords and order them alphabetically
process_keywords <- function(filename){
  readLines(filename) %>%
    purrr::discard(~ . == "") %>%
    purrr::map_chr(stringr::str_trim) %>%
    sort() %>%
    glue::collapse(sep = ", ", last = ", and ")
}
