get_figure_elements <- function(tidied_fixed, model_linear_fits, model_formula_ranking, model_linear_fits_species){
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
    dplyr::rename(rel = "con_type") 
  
  points_sp_summarised %<>%
    dplyr::filter(rel != "conspecific")
  
  points_sp %<>%
    dplyr::filter(con_type != "conspecific")
  
   list(
     model_weights = model_weights,
     points_sp = points_sp, 
     points_sp_summarised = points_sp_summarised)
}

make_fig_het_con_abc <- function(fe, 
                                 tidied_fixed, 
                                 colour_pallete = rev(RColorBrewer::brewer.pal(4, "OrRd")), 
                                 filename, colour_guide = "legend", 
                                 add_smooth = TRUE, 
                                 alpha_points_factor = 1){
  require(ggplot2)
  
  pa <- RColorBrewer::brewer.pal(4, "OrRd")
  major_labs <- c(0,10,100, 1000)
  minor_labs <- c(0, 5, 10, 50, 100, 500, 1000)
  major_breaks <- log(major_labs + 1)
  minor_breaks <- log(minor_labs + 1)
  
  p <- dplyr::data_frame(x = get_pred_range(tidied_fixed, "heterospecific"),
                    y = get_pred_range(tidied_fixed, "conspecific")) %>%
    ggplot() +
    geom_abline(slope = 1, intercept = 0, size = 0.25, linetype = 2, colour = "white") +
    geom_errorbar(data = fe$points_sp_summarised,
                  aes(x = heterospecific_median,
                      ymin = conspecific_quantile_05,
                      ymax = conspecific_quantile_95,
                      colour = rel),
                  show.legend = F, 
                  alpha = 0.5 * alpha_points_factor, 
                  size = 0.5) +
    geom_errorbarh(data = fe$points_sp_summarised,
                   aes(x = heterospecific_median,
                       y = conspecific_median,
                       xmin = heterospecific_quantile_05,
                       xmax = heterospecific_quantile_95,
                       colour = rel),
                   show.legend = F, 
                   alpha = 0.5 * alpha_points_factor) +
    geom_point(data = fe$points_sp_summarised,
               aes(x = heterospecific_median,
                   y = conspecific_median,
                   colour = rel, 
                   fill = rel),
               alpha = 1 * alpha_points_factor,
               show.legend = T, shape = 21, size = 2)
  if (add_smooth) {
    p <- p+
    geom_smooth(data = fe$points_sp,
                aes(x = heterospecific_abs,
                    y = conspecific,
                    colour = con_type, 
                    group = interaction(con_type, model)),
                size = 0.1, 
                linetype = 1,
                alpha = 0.15,
                show.legend = T,
                method = "lm", se = F) +
    geom_smooth(data = fe$points_sp,
                aes(x = heterospecific_abs,
                    y = conspecific,
                    colour = con_type),
                size = 1, 
                linetype = 1,
                alpha = 1,
                show.legend = T,
                method = "lm", se = F) 
    }
  p <- p +
    scale_x_continuous(limits = c(0,8), breaks = major_breaks, labels = major_labs, minor_breaks = minor_breaks) +
    scale_y_continuous(limits = c(0,8), breaks = major_breaks, labels = major_labs, minor_breaks = minor_breaks) +
    scale_colour_manual(values = colour_pallete,
                        labels = c("open", "control", "control")) +
    scale_fill_manual(values = colour_pallete,
                      labels = c("open", "control", "control")) +    
    guides(colour = colour_guide, fill = colour_guide) +
    abc_theme() +
    labs(title = "conspecific vs. heterospecific",
         subtitle = "pollen grains per stigma",
           x = "heterospecific",
         y = "conspecific") +
    theme(legend.position = c(0.98,0.02),
          legend.direction = "vertical",
          legend.justification = c(1,0),
          legend.title = element_blank(),
          panel.grid.major = element_line(size = 0.25)) +
    coord_equal()
  
  ggsave(filename, plot = p, device = "png", bg = "black")
}
