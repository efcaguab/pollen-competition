# model facilitation in Tur style using deposition data
# uses lme4 and has a random slope and intercept for every plant-site combination
model_facilitation <- function(dep_frame){
  dep_frame %>%
    dplyr::filter(treatment == "open") %>%
    dplyr::select(plant, plant_name, site_name, pollen_density, pollen_category) %>%
    tidyr::spread(key = "pollen_category", value = "pollen_density") %>% 
    dplyr::mutate(conspecific = ceiling(conspecific)) %>%
    lme4::glmer(conspecific ~ heterospecific + (heterospecific | plant_name : site_name), family = "poisson", data = .)
}

# extract the random effects from facilitation models 
# also gets the error using the conditional variance stored as an attribute in the model
extract_random_effects <- function(facilitation_models){
  facilitation_models %>%
    lme4::ranef(condVar = TRUE) %>%
    extract2(1) %>% {
      x <- . ; x %>% 
        tibble::rownames_to_column() %>%
        # call helper function
        dplyr::mutate(error = get_error_random_effects(x)) 
    }  %>%
    tidyr::separate("rowname", into = c("plant_name", "site_name"), sep = ":", remove = F) 
}

# helpef function to extract the error from the model
get_error_random_effects <- function(x) {
  x %>%
    attr("postVar") %>%
    extract(2,2,) %>%
    sqrt() 
}

# plot the slopes of the random effects models
plot_random_slopes <- function(facilitation_random_effects){
  pal <- common_graphic_metrics()$pal_rb3
  
  require(ggplot2)
  facilitation_plot_df <- facilitation_random_effects %>%
    dplyr::mutate(plant_name = forcats::fct_reorder(plant_name, heterospecific), 
                  upper = heterospecific + error, 
                  lower = heterospecific - error, 
                  effect_category = dplyr::case_when(
                    0 < lower ~ "positive", 
                    0 > upper ~ "negative", 
                    TRUE ~ "neutral"
                  ), 
                  effect_category = forcats::fct_relevel(
                    effect_category, 
                    c("negative", "neutral", "positive")))
  
  count_plot <- facilitation_plot_df %>%
    dplyr::mutate(effect_category = forcats::fct_rev(effect_category), 
                  label_col = dplyr::if_else(effect_category == "neutral", 
                                             "black", "white"), 
                  label = dplyr::case_when(
                    effect_category == "positive" ~ "+",
                    effect_category == "negative" ~ "-", 
                    TRUE ~ ""
                  ), 
                  fontface = dplyr::case_when(
                    label_col == "black" ~ "plain", 
                    TRUE ~ "bold"
                  )) %>%
    dplyr::mutate(label_height = 
                    rank(dplyr::desc(as.numeric(effect_category)), 
                         ties.method = "average"), 
                  label_height = label_height / n()) %>%
    plot_bar_proportion()
  
  shades <- facilitation_plot_df %$%
    unique(plant_name) %>%
    tibble::data_frame(plant_name = ., 
                       site_name = NA) %>%
    dplyr::mutate(x = 1:n(),
                  alpha = x %% 2)
    
  slope_plot <- facilitation_plot_df %>%
    dplyr::group_by(plant_name) %>%
    dplyr::arrange(plant_name, heterospecific) %>%
    dplyr::mutate(n_sites = dplyr::n_distinct(site_name), 
                  art_site = 1:n_sites[1]) %>%
    dplyr::group_by() %>%
    ggplot() +
    geom_linerange(aes(x = plant_name, 
                       group = art_site, 
                       ymin = lower, ymax = upper), 
                   position = position_dodge(width = 0.5), 
                   size = cgm()$size_errorbars, 
                   color = cgm()$color_errorbars, 
                   alpha = 0) + 
    geom_tile(data = shades, aes(x = x, alpha = alpha,y = 0),
              width = 1, height = Inf, fill = "grey90") +
    geom_hline(aes(yintercept = 0), 
               linetype = 2, 
               size = 0.25, 
               colour = "black") +
    geom_linerange(aes(x = plant_name, 
                       group = art_site, 
                       y = heterospecific, 
                       ymin = lower, ymax = upper), 
                   position = position_dodge(width = 0.75), 
                   size = cgm()$size_errorbars, 
                   color = cgm()$color_errorbars) + 
    geom_point(aes(x = plant_name, y = heterospecific, group = art_site, 
                   fill = effect_category), size = 1, 
               position = position_dodge(0.75), 
               shape = 21, 
               color = cgm()$color_errorbars) +
    scale_fill_manual(values = pal) +
    # scale_color_viridis_d(option = "D", end = 0.8) + 
    labs(y = bquote("slope" ~ beta[i])) + 
    coord_flip() +
    pub_theme() +
    theme(axis.title.y = element_blank(), 
          axis.text.y = element_text(face = "italic"), 
          # panel.grid.major.y = element_line(size = 0.15, 
                                            # colour = "grey80"), 
          legend.position = "none", 
          axis.ticks.y = element_blank()) 
          # axis.line.x = element_line(size = 0.25), )
  slope_plot
  list(slope_plot = slope_plot, 
       count_plot = count_plot)
}

get_label_height <- function(x){
  as.numeric(x)
}

plot_bar_proportion <- function(x){
  pal <- common_graphic_metrics()$pal_rb3
  x  %>% 
    ggplot(aes(x = "effect_category", 
               fill = effect_category)) +
    geom_bar(stat = "count", 
             width = 0.9, 
             position = "fill") +
    geom_text(aes(label = label, 
                  y = label_height,
                  colour = label_col, 
                  size = label_col,
                  fontface = fontface),
              stat = "unique", 
              position = position_identity(), 
              angle = 0) +
    scale_y_continuous(position = "right", 
                       expand = c(0,0), 
                       labels = scales::percent) +
    scale_fill_manual(values = rev(pal)) +
    scale_color_manual(values = c("black", "white")) +
    scale_size_manual(values = c(2,2.5)) +
    pub_theme() +
    theme(legend.position = "none", 
          axis.title = element_blank(), 
          axis.text.x = element_blank(), 
          axis.ticks.x = element_blank(), 
          panel.border = element_blank(), 
          # axis.line.y = element_line(size = 0.25), 
          plot.margin = unit(c(1,1,1,0), "mm")) 
}
