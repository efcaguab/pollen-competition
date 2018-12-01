pub_theme <- function(){
  theme_bw() +
    theme(text = element_text(size = 8), 
          title = element_text(size = 8),
          axis.title = element_text(size = 7),
          plot.title = element_text(size = 7, face = "bold", margin = margin(b = 0)),
          plot.subtitle = element_text(size = 7),
          plot.tag = element_text(size = 7),
          strip.background = element_blank(),
          strip.text = element_text(hjust = 0),
          legend.title = element_text(hjust = 0.5), 
          legend.key.size = unit(3, "mm"), 
          legend.margin = margin(), 
          axis.ticks.x = element_line(colour = "grey30", size = 0.05),
          axis.ticks.y = element_line(colour = "grey30", size = 0.25),
          # panel.border = element_blank(),
          # axis.line.y = element_line(),
          panel.grid = element_blank(), 
          panel.background = element_blank(), 
          plot.background = element_blank())
}

remove_legend <- function(x){
  require(ggplot2)
  x + theme(legend.position = 'none')
}

common_graphic_metrics <- function(){
  list(
    pal_rb3 = RColorBrewer::brewer.pal(4, "Greys")[c(3,1,2)],
    size_errorbars =  0.25,
    color_errorbars = "grey30", 
    color_errorbars_light = "grey70", 
    log1p_axis_breaks_10 = c(0, 10, 100, 1000, 10000), 
    point_size = 1, 
    color_references = "grey50", 
    size_references = 0.25, 
    fill_rows = "grey95"
  )
}

cgm <- function(){
  common_graphic_metrics()
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
        grepl('originality', term) ~ 'func. originality',
        grepl('abn', term) ~ 'abundance',
        grepl('abu', term) ~ 'abundance',
        grepl('rab', term) ~ 'abundance',
        grepl('deg', term) ~ '# shared pol.',
        grepl('k', term) ~ '# shared pol.',
        grepl('poc', term) ~ 'visit efficacy',
        grepl('pollen_cont', term) ~ 'visit efficacy',
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
      dplyr::filter(pollen_category != "heterospecific") %>%
      dplyr::mutate(pollen_category = dplyr::case_when(
        grepl('conspecific_abs', pollen_category) ~ 'conspecific (absolute)',
        grepl('conspecific_ctr', pollen_category) ~ 'conspecific (control)',
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


# numbers to words
numbers2words <- function(x){
  ## Function by John Fox found here: 
  ## http://tolstoy.newcastle.edu.au/R/help/05/04/2715.html
  ## Tweaks by AJH to add commas and "and"
  helper <- function(x){
    
    digits <- rev(strsplit(as.character(x), "")[[1]])
    nDigits <- length(digits)
    if (nDigits == 1) as.vector(ones[digits])
    else if (nDigits == 2)
      if (x <= 19) as.vector(teens[digits[1]])
    else trim(paste(tens[digits[2]],
                    Recall(as.numeric(digits[1]))))
    else if (nDigits == 3) trim(paste(ones[digits[3]], "hundred and", 
                                      Recall(makeNumber(digits[2:1]))))
    else {
      nSuffix <- ((nDigits + 2) %/% 3) - 1
      if (nSuffix > length(suffixes)) stop(paste(x, "is too large!"))
      trim(paste(Recall(makeNumber(digits[
        nDigits:(3*nSuffix + 1)])),
        suffixes[nSuffix],"," ,
        Recall(makeNumber(digits[(3*nSuffix):1]))))
    }
  }
  trim <- function(text){
    #Tidy leading/trailing whitespace, space before comma
    text=gsub("^\ ", "", gsub("\ *$", "", gsub("\ ,",",",text)))
    #Clear any trailing " and"
    text=gsub(" and$","",text)
    #Clear any trailing comma
    gsub("\ *,$","",text)
  }  
  makeNumber <- function(...) as.numeric(paste(..., collapse=""))     
  #Disable scientific notation
  opts <- options(scipen=100) 
  on.exit(options(opts)) 
  ones <- c("", "one", "two", "three", "four", "five", "six", "seven",
            "eight", "nine") 
  names(ones) <- 0:9 
  teens <- c("ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen",
             "sixteen", " seventeen", "eighteen", "nineteen")
  names(teens) <- 0:9 
  tens <- c("twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty",
            "ninety") 
  names(tens) <- 2:9 
  x <- round(x)
  suffixes <- c("thousand", "million", "billion", "trillion")     
  if (length(x) > 1) return(trim(sapply(x, helper)))
  helper(x)
}

# round numbers to preserve sum (good for percentages)
round_preserve_sum <- function(x, digits = 0) {
  up <- 10 ^ digits
  x <- x * up
  y <- floor(x)
  indices <- tail(order(x-y), round(sum(x)) - sum(y))
  y[indices] <- y[indices] + 1
  y / up
}

abb_col <- function(x){
  x %>% 
    stringr::str_replace("conspecific", "consp.") %>%
    stringr::str_replace("heterospecific", "hetsp.")
}