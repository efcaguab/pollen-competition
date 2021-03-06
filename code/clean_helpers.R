
#' Standardise scientific names
#'
#' Ensures dots after 'sp', ensures space before species 'number', removes
#' trailing and double spaces, dots
#'
#' @param df input data frame
#' @param column column in which to perform the magic
#'
#' @return data frame with updated names in the selected column
#'
#'   
standardise_name <- function(df, column){
  class(df) <- 'data.frame'
  df[, column] = stringr::str_replace(df[, column], stringr::regex('\\s(sp)'), ' sp.')
  cond <- stringr::str_detect(df[, column], stringr::regex('([0-9]+)'))
  df[cond, column] = stringr::str_replace(df[cond, column], stringr::regex('([0-9]+)'), function(x) paste0(' ', x))
  df[, column] = stringr::str_replace_all(df[, column], stringr::regex('\\.+'), '.')
  df[, column] = stringr::str_replace_all(df[, column], stringr::regex('\\s+'), ' ')
  # df[, column] = str_replace_all(df[, column], "Grupo ", '')
  df[, column] = stringr::str_replace(df[, column], stringr::regex('[^(sp)]\\.$'), '')
  df[, column] = trimws(df[, column])
  df[, column] = paste(toupper(substr(df[, column], 1, 1)), substr(df[, column], 2, nchar(df[, column])), sep="")
  df
}


#' Extract data frame with site information 
#' 
#' Uses the cleaned version of the deposition data and it's used to clean other data components
#'
#' @param depostion 
#'
#' @return a data frame with the site information
#'
#'
site_names <- function(depostion) {
  
  extract_fragment_and_site_name <- function(x){
    data.frame(
      fragment_name = x$fragment_name, 
      # fragment = x$fragment,
      site_name = x$name)
  }
  
  deposition %>%
    plyr::ldply(extract_fragment_and_site_name) %>% 
    dplyr::mutate(
      fragment_name = paste(substr(site_name, 4, 5), fragment_name, sep = "_"), 
      fragment_name = gsub("alfalfa", "alfa", fragment_name),
      fragment_name = gsub("pastura", "pas", fragment_name),
      fragment_name = gsub("1agr", "A-1", fragment_name),
      fragment_name = gsub("2agr", "A-2", fragment_name))
}


site_data <- function(file, site_file){
  
  suppressMessages(data <- readr::read_csv(file))
  suppressMessages(site_name <- readr::read_csv(site_file))
  data %>%
    preformat_data() %>%
    dplyr::group_by(site_name) %>%
    dplyr::summarise(
      locality = unique(site), 
      land_use = unique(land_use),
      fragment = unique(fragment),
      fragment_name = unique(fragment_name)) %>%
    dplyr::mutate(land_use = dplyr::if_else(land_use == "a", "agricultural", "reserve")) %>%
    dplyr::group_by()%>%
    # standarise site names =/ (alternative)
    dplyr::mutate(
      fragment_name_alt = paste(substr(site_name, 4, 5), fragment_name, sep = "_"), 
      fragment_name_alt = gsub("alfalfa", "alfa", fragment_name_alt),
      fragment_name_alt = gsub("pastura", "pas", fragment_name_alt),
      fragment_name_alt = gsub("1agr", "A-1", fragment_name_alt),
      fragment_name_alt = gsub("2agr", "A-2", fragment_name_alt)) %>% 
    dplyr::inner_join(site_name, by = 'locality')
  
}

preformat_data <- function(data){
  data %>% 
    dplyr::group_by(site, land_use) %>%
    dplyr::mutate(fragment_name = fragment, 
           fragment_name = gsub("\x92", "i", fragment_name),
           fragment = as.numeric(as.factor(fragment)),
           site_name = paste("MA", site, toupper(land_use), fragment, sep = "_"))
}