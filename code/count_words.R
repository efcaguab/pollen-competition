# count words in a file
count_words <- function(filename = "", lines_to_ignore = NULL, remove_md_headings = TRUE){
  text_lines <- readLines(filename)
  if (!is.null(lines_to_ignore)) {
    text_lines <- text_lines[-lines_to_ignore]
  }
  if (remove_md_headings) {
    heading_lines <- which(grepl("^#", text_lines))
    if (length(heading_lines) > 0) {
      text_lines <- text_lines[-heading_lines]
    }
  }
  text_to_count <- paste(text_lines, 
                         collapse = " ")
  get_text_stats(text_to_count)
}

# get stats of a text
get_text_stats <- function(text){
  text <- prep_text(text)
  n_char_tot <- sum(stringi::stri_stats_latex(text)[c(1, 3)])
  n_words_stri <- unname(stringi::stri_stats_latex(text)[4])
  wpm <- 200
  reading_time_stri <- ceiling(n_words_stri/wpm)
  return(list(n_char_tot_stri = n_char_tot, n_words_stri = n_words_stri, 
              reading_time_stri = reading_time_stri))
}

prep_text <- function(text) {
    text <- gsub("[\r\n]", " ", text)
    text <- gsub("---.*--- ", "", text)
    text <- gsub("```\\{.+?\\}.+?```", "", text)
    text <- gsub("`r.+?`", "", text)
    text <- gsub("<!--.+?-->", "", text)
    text <- gsub("\\(http.+?\\)", "", text)
    text <- gsub("!\\[.+?\\)", "", text)
    text <- gsub("#*", "", text)
    text <- gsub("<.+?>|</.+?>", "", text)
    text <- gsub("%", "", text)
    if (nchar(text) == 0) {
      stop("You have not selected any text. Please select some text with the mouse and try again")
    }
    else {
      return(text)
    }
}