make_fig_con_hetero_gain <- function(tidied_fixed, model_linear_fits, model_formula_ranking, model_linear_fits_species) {
  require(ggplot2)
  
  model_weights <- model_formula_ranking$aggregated %>%
    # dplyr::filter(scale == "community") %>%
    # dplyr::group_by(pollen_category) %>%
    dplyr::mutate(likelyhood = get_likelyhoods(delta_AIC_median), 
                  weight = get_weights(likelyhood)) %>%
    dplyr::select(fixed_formula, weight)
  
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
                        dplyr::funs(median, quantile_05,quantile_95), na.rm = T) %>%
    dplyr::mutate_if(is.numeric, I) %>%
    dplyr::group_by() %>%
    dplyr::rename(rel = "con_type") %>%
    dplyr::mutate(rel = dplyr::case_when(rel == "conspecific" ~ "relative", TRUE ~ "absolute"))
  
  pa <- RColorBrewer::brewer.pal(4, "OrRd")
  major_labs <- c(0,10,100, 1000)
  minor_labs <- c(0, 5, 10, 50, 100, 500, 1000)
  major_breaks <- log(major_labs + 1)
  minor_breaks <- log(minor_labs + 1)

  p <- dplyr::data_frame(x = get_pred_range(tidied_fixed, "heterospecific"),
                         y = get_pred_range(tidied_fixed, "conspecific")) %>%
    ggplot() +
    geom_abline(slope = 1, intercept = 0, size = 0.25, linetype = 2) +
    geom_errorbar(data = points_sp_summarised,
                  aes(x = heterospecific_median,
                      ymin = conspecific_quantile_05,
                      ymax = conspecific_quantile_95,
                      colour = rel),
                  show.legend = F, alpha = 0.25) +
    geom_errorbarh(data = points_sp_summarised,
                   aes(x = heterospecific_median,
                       y = conspecific_median,
                       xmin = heterospecific_quantile_05,
                       xmax = heterospecific_quantile_95,
                       colour = rel),
                   show.legend = F, alpha = 0.25) +
    geom_point(data = points_sp_summarised,
               aes(x = heterospecific_median,
                   y = conspecific_median,
                   colour = rel),
               alpha = 1, show.legend = T, shape = 21, size = 1) +
    geom_smooth(data = points_sp,
                aes(x = heterospecific_abs,
                    y = conspecific,
                    colour = con_type, 
                    group = interaction(con_type, model)),
                size = 0.1, 
                linetype = 1,
                alpha = 0.25,
                show.legend = T,
                method = "lm", se = F) +
    geom_smooth(data = points_sp,
                aes(x = heterospecific_abs,
                    y = conspecific,
                    colour = con_type),
                size = 0.5, 
                linetype = 1,
                alpha = 1,
                show.legend = T,
                method = "lm", se = F) +
    scale_x_continuous(breaks = major_breaks, labels = major_labs, minor_breaks = minor_breaks) +
    scale_y_continuous(breaks = major_breaks, labels = major_labs, minor_breaks = minor_breaks) +
    scale_colour_manual(values = rev(RColorBrewer::brewer.pal(4, "OrRd")),
                        labels = c("relative", "absolute", "control")) +
    pub_theme() +
    labs(x = "heterospecific pollen density",
         y = "conspecific pollen density") +
    theme(legend.position = c(0.01,0.98),
          legend.direction = "horizontal",
          legend.justification = c(0,1),
          legend.title = element_blank(),
          legend.background = element_rect(fill = "white"),
          panel.grid.major = element_line(size = 0.25),
          axis.title = element_text(size = 8, colour = "grey20"))
  p

  # pdf(width = 3.25, height = 4.25)
  # p
  # dev.off()
}

# function just to get the range of preditions
get_pred_range <- function(tidied_fixed, x, var_trans = "log", scale = "global"){
  tidied_fixed %>% 
    dplyr::filter(
      pollen_category == x,
      var_trans == var_trans, 
      scale == scale) %$% 
    estimate %>% 
    range()
}
