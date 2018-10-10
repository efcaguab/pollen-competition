get_figure_elements <- function(tidied_fixed, abc_model_linear_fits, model_formula_ranking, model_linear_fits_species){
  require(ggplot2)
  
  model_weights <- model_formula_ranking$aggregated %>%
    # dplyr::filter(scale == "community") %>%
    # dplyr::group_by(pollen_category) %>%
    dplyr::mutate(likelyhood = get_likelyhoods(delta_AIC_median), 
                  weight = get_weights(likelyhood)) %>%
    dplyr::select(fixed_formula, weight)
  
  ab_lines <- model_linear_fits %>%
    dplyr::filter(var_trans == "log", 
                  scale == "community") %>%
    tidyr::spread(data = ., key = sma_parameter, value = value) %>%
    dplyr::inner_join(model_weights, by = "fixed_formula") %>%
    dplyr::group_by(model, scale) %>%
    dplyr::sample_n(size = 1, weight = weight) %>%
    dplyr::group_by()
  
  ab_lines_summarised <- ab_lines %>%
    dplyr::group_by(rel) %>%
    dplyr::summarise_if(is.numeric, median)
  
  points_sp <- model_linear_fits_species %>%
    dplyr::filter(var_trans == "log",
                  scale == "community") %>% 
    #   tidyr::spread(data = ., key = sma_parameter, value = value) %>% 
    dplyr::inner_join(model_weights, by = "fixed_formula") %>% 
    dplyr::group_by(site_name, plant_name, scale, model) %>%
    dplyr::sample_n(size = 1, weight = weight) %>%
    dplyr::group_by() %>%
    tidyr::gather("con_type", "conspecific", dplyr::contains("conspecific")) 
  
  exp_minus_one <- function(x){
    exp(x)-1
  }
  
  points_sp_summarised <- points_sp %>%
    dplyr::group_by(plant_name, site_name, con_type) %>%
    dplyr::summarise_at(dplyr::vars(dplyr::contains("specific")), 
                        dplyr::funs(median, quantile_05,quantile_95)) %>%
    dplyr::mutate_if(is.numeric, I) %>%
    dplyr::group_by() %>%
    dplyr::rename(rel = "con_type") %>%
    dplyr::mutate(rel = dplyr::case_when(rel == "conspecific" ~ "relative", TRUE ~ "absolute"))
  
  pa <- RColorBrewer::brewer.pal(4, "OrRd")
  major_labs <- c(0,10,100, 1000)
  minor_labs <- c(0, 5, 10, 50, 100, 500, 1000)
  major_breaks <- log(major_labs + 1)
  minor_breaks <- log(minor_labs + 1)
  
  list(model_weights, ab_lines, ab_lines_summarised, points_sp, points_sp_summarised, pa)
}

