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
    dplyr::rename(rel = "con_type")

  pa <- RColorBrewer::brewer.pal(4, "OrRd")
  major_labs <- c(0,10,100, 1000, 10000)
  minor_labs <- c(0, 5, 10, 50, 100, 500, 1000, 10000)
  major_breaks <- log(major_labs + 1)
  minor_breaks <- log(minor_labs + 1)

  points_sp_summarised %<>%
    dplyr::filter(rel == "conspecific_abs")
  points_sp %<>%
    dplyr::filter(con_type == "conspecific_abs")

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
                   colour = interaction(rel, site_name)),
               alpha = 1, show.legend = T, shape = 21, size = 1) +
    geom_smooth(data = points_sp,
                aes(x = heterospecific_abs,
                    y = conspecific,
                    colour = interaction(con_type, site_name),
                    group = interaction(con_type, model, site_name)),
                size = 0.1,
                linetype = 1,
                alpha = 0.25,
                show.legend = T,
                method = "lm", se = F) +
    geom_smooth(data = points_sp,
                aes(x = heterospecific_abs,
                    y = conspecific,
                    colour = interaction(con_type, site_name)),
                size = 0.5,
                linetype = 1,
                alpha = 1,
                show.legend = T,
                method = "lm", se = F) +
    scale_x_continuous(breaks = major_breaks, labels = major_labs, minor_breaks = minor_breaks) +
    scale_y_continuous(breaks = major_breaks, labels = major_labs, minor_breaks = minor_breaks) +
        # scale_colour_manual(values = rev(RColorBrewer::brewer.pal(4, "OrRd")),
                        # labels = c("relative", "absolute", "control")) +
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


# tinkering species level figures
figures_per_species <- function(){
  points_sp_summarised %>%
    dplyr::group_by(plant_name) %>%
    dplyr::mutate(n = dplyr::n_distinct(site_name)) %>%
    dplyr::filter(n >= 3) %>%
    ggplot(aes(x = heterospecific_median,
               y = conspecific_median,
               color = plant_name,
               linetype = rel,
               shape = rel)) +
    geom_smooth(method = "lm", se = F) +
    geom_point()

  points_sp_summarised_con %>%
    dplyr::group_by(plant_name) %>%
    dplyr::filter(!is.na(heterospecific_median)) %>%
    dplyr::mutate(n = dplyr::n_distinct(site_name)) %>%
    dplyr::filter(n >= 3) %>%
    dplyr::select(site_name, plant_name, heterospecific_median, conspecific_ctr_median, conspecific_abs_median) %>%
    tidyr::gather("type", "density", heterospecific_median, conspecific_ctr_median, conspecific_abs_median) %>%
    ggplot(aes(x = interaction(site_name),
               y = density,
               fill = type,
               colour = type)) +
    geom_point(stat = "identity", position = "dodge") +
    geom_line(aes(group = type)) +
    coord_flip() +
    facet_wrap(~ plant_name, scales = "free_y")

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

model_open_bagged <- function(dep_frame){
  df <- dep_frame %>%
    dplyr::filter(pollen_category == "conspecific")

  # Using a GLMM with the number of grains as a predictor and the treatment
  # (bagged/unbagged) as the predictor. Allow a random itercept and slope for
  # each species nested in the community
  lme4::glmer(n_grains ~
                treatment + (treatment | plant_name : site_name),
              data = df,
              offset = log(n_stigma),
              family = "poisson")
}

# Check wether the difference in conspecific pollen between bagged and
# unbagged flowers is significant
get_coef_open_bagged <- function(open_baged_model){

  open_baged_model %>%
    coef() %>%
    extract2(1) %>%
    tibble::rownames_to_column() %>%
    dplyr::mutate(error = get_error_random_effects(open_baged_model)) %>%
    tidyr::separate("rowname", into = c("plant_name", "site_name"),
                    sep = ":", remove = F) %>%
    dplyr::mutate(closed =  `(Intercept)`,
                  open = closed + treatmentopen,
                  effect_category = dplyr::case_when(
                    open - 2 * error > closed ~ "positive",
                    closed - 2 * error > open ~ "negative",
                    TRUE ~ "neutral"
                  ))

}

get_con_con_plot_df <- function(coef_open_bagged){
  # Check wether the difference in conspecific pollen between bagged and
  # unbagged flowers is significant
  coef_open_bagged %>%
    dplyr::mutate(open_mid = open,
                  open_lower = open - error,
                  open_upper = open + error,
                  closed_mid = closed,
                  closed_lower = closed - error,
                  closed_upper = closed + error) %>%
    dplyr::mutate_at(.vars = dplyr::vars(tidyselect::contains("mid"),
                                         tidyselect::contains("upper"),
                                         tidyselect::contains("lower")),
                     exp)
}

plot_bagged_vs_open_conspecific <- function(con_df){
  require(ggplot2)

  axis_breaks <-  cgm()$log1p_axis_breaks_10
  linesize <- common_graphic_metrics()$size_errorbars
  linecolor <- cgm()$color_errorbars_light
  pal <-common_graphic_metrics()$pal_el_green[c(8,5,1)]
  shape_col <- common_graphic_metrics()$pal_el_green[9]

  scatter_plot <- con_df %>%
    ggplot(aes(x = closed_mid, y = open_mid)) +
    geom_abline(slope = 1, intercept = 0, size = 0.25, linetype = 2) +
    # geom_smooth(method = "lm",
    #             size = 0.5,
    #             color = "black") +
    geom_errorbar(aes(ymin = open_lower, ymax = open_upper),
                  size = linesize,
                  color = linecolor) +
    geom_errorbarh(aes(xmin = closed_lower, xmax = closed_upper),
                   size = linesize,
                   color = linecolor) +
    geom_point(aes(fill = effect_category),
               shape = 21,
               size = 1,
               colour = shape_col) +
    geom_point(aes(x = 0, y = 0), alpha = 0, colour = shape_col) +
    annotate(geom = "text", x = 0, y = Inf,
             label = "facilitation predominates",
             hjust = 0,
             vjust = 1.5,
             size = 2.25,
             colour = "grey20") +
    annotate(geom = "text", x = Inf, y = 0,
             label = "competition predominates",
             hjust = 1.1,
             vjust = 0,
             size = 2.25,
             colour = "grey20") +
    scale_x_continuous(trans = "log1p", breaks = axis_breaks) +
    scale_y_continuous(trans = "log1p", breaks = axis_breaks) +
    scale_fill_manual(values = pal, na.value = "white") +
    labs(x = "grains in bagged flowers",
         y =  "grains in open flowers",
         title = "(c) competition vs. facilitation - criterion #2",
         subtitle = "self- vs. animal-mediated pollination") +
    pub_theme() +
    theme(legend.position = "none")
  #axis.line = element_line(size = 0.25))

  bar_plot <- con_df %>%
    dplyr::mutate(forcats::fct_relevel(effect_category, c("negative", "neutral", "positive")),
                  effect_category = forcats::fct_rev(effect_category),
                  label_col = dplyr::if_else(effect_category == "neutral", "black", "white"),
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
    plot_bar_proportion(pal)

  list(scatter_plot, bar_plot)
}

wilcox_open_closed <- function(pollen_density, treatment){
  l <- unique(treatment)
  x <- pollen_density[treatment == l[2]]
  y <- pollen_density[treatment == l[1]]
  if (length(x) > 1 & length(y) > 1){
    p <- dplyr::case_when(
      wilcox.test(x, y, alternative = "greater")$p.value < 0.05 ~ "positive",
      wilcox.test(y, x, alternative = "greater")$p.value < 0.05 ~ "negative",
      TRUE ~ "neutral"
    )
  } else {
    p <- "neutral"
  }
  p
}

se <- function(x) sqrt(var(x, na.rm = T)/length(x))

make_fig_con_con <- function(model_formula_ranking, model_linear_fits_species){

  require(ggplot2)

  major_labs <- c(0,10,100, 1000, 10000)
  minor_labs <- c(0, 5, 10, 50, 100, 500, 1000, 10000)
  major_breaks <- log(major_labs + 1)
  minor_breaks <- log(minor_labs + 1)

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

  points_sp_con <- points_sp %>%
    tidyr::spread(con_type, conspecific)

  points_sp_summarised_con <- points_sp_con  %>%
    dplyr::group_by(plant_name, site_name) %>%
    dplyr::summarise_at(dplyr::vars(dplyr::contains("specific")),
                        dplyr::funs(median, quantile_05,quantile_95), na.rm = T) %>%
    dplyr::mutate_if(is.numeric, I)

  # conspecific abs vs control
  points_sp_con  %>%
    ggplot() +
    geom_abline(slope = 1, intercept = 0, size = 0.25, linetype = 2) +
    geom_errorbar(data = points_sp_summarised_con,
                  aes(x = conspecific_ctr_median,
                      ymin = conspecific_abs_quantile_05,
                      ymax = conspecific_abs_quantile_95),
                  show.legend = F, alpha = 0.25) +
    geom_errorbarh(data = points_sp_summarised_con,
                   aes(y = conspecific_abs_median,
                       x = conspecific_ctr_median,
                       xmin = conspecific_ctr_quantile_05,
                       xmax = conspecific_ctr_quantile_95),
                   show.legend = F, alpha = 0.25) +
    geom_point(data = points_sp_summarised_con,
               aes(x = conspecific_ctr_median,
                   y = conspecific_abs_median),
               alpha = 1, show.legend = T, shape = 21, size = 1) +
    geom_smooth(aes(x = conspecific_ctr,
                    y = conspecific_abs,
                    group = model),
                method = "lm", se = F,
                size = 0.1,
                alpha = 0.25,
                colour = "black") +
    geom_smooth(aes(x = conspecific_ctr,
                    y = conspecific_abs),
                method = "lm", se = F,
                size = 0.5,
                alpha = 1,
                colour = "black") +
    scale_x_continuous(breaks = major_breaks, labels = major_labs, minor_breaks = minor_breaks) +
    scale_y_continuous(breaks = major_breaks, labels = major_labs, minor_breaks = minor_breaks) +
    pub_theme() +
    labs(x = "control - conspecific pollen density",
         y = "conspecific pollen density") +
    theme(legend.position = c(0.01,0.98),
          legend.direction = "horizontal",
          legend.justification = c(0,1),
          legend.title = element_blank(),
          legend.background = element_rect(fill = "white"),
          panel.grid.major = element_line(size = 0.25),
          axis.title = element_text(size = 8, colour = "grey20"))
}

make_fig_con_hetero_empirical <- function(dep_frame){
  require(ggplot2)
  axis_breaks <-  cgm()$log1p_axis_breaks_10
  shape_col <- common_graphic_metrics()$pal_el_green[9]
  dep_frame %>%
    dplyr::select(site_name, plant, plant_name, treatment, pollen_category, pollen_density) %>%
    # dplyr::mutate(pollen_density = log(pollen_density + 1)) %>%
    tidyr::spread(pollen_category, pollen_density) %>%
    dplyr::group_by(site_name, plant_name) %>%
    dplyr::mutate(heterospecific = fill_heterospecific_closed(heterospecific, treatment)) %>%
    dplyr::group_by(site_name, plant_name, treatment) %>%
    dplyr::summarise_at(c("conspecific", "heterospecific"),
                        dplyr::funs(mid, upper, lower)) %>%
    dplyr::filter(treatment == "open") %>%
    ggplot(aes(x = heterospecific_mid, y = conspecific_mid)) +
    geom_point(aes(x = 0, y = 0), alpha = 0) +
    geom_abline(slope = 1, intercept = 0, size = 0.25, linetype = 2) +
    # geom_smooth(method = "lm", size = 0.5, colour = "black", fill = "grey90", alpha = 1) +
    geom_errorbar(aes(ymin = conspecific_lower, ymax = conspecific_upper),
                  size = cgm()$size_errorbars,
                  color = cgm()$color_errorbars_light) +
    geom_errorbarh(aes(xmin = heterospecific_lower, xmax = heterospecific_upper),
                   size = cgm()$size_errorbars, color = cgm()$color_errorbars_light) +
    geom_point(size = cgm()$point_size,
               shape = 21,
               color = shape_col,
               fill = "white") +
    scale_x_continuous(trans = "log1p", breaks = axis_breaks) +
    scale_y_continuous(trans = "log1p", breaks = axis_breaks, labels = function(x) paste0(" ", x)) +
    labs(x = "heterospecific",
         y = "conspecific",
         title = "hetero- vs. conspecific pollen",
         subtitle = "mean pollen grains per stigma") +
    pub_theme()
}

mid <- function(x){
  mean(x, na.rm  = TRUE)
}

upper <- function(x){
  mid(x) + se(x)
}

lower <- function(x){
  mid(x) - se(x)
}

fill_heterospecific_closed <- function(heterospecific, treatment){
  # if there are  open bags
  if ("open" %in% treatment) {
    # if there is just one open bag
    if (sum("open" == treatment) == 1) {
      heterospecific[treatment == "closed"] <- heterospecific[treatment == "open"]
    } else{
      heterospecific[treatment == "closed"] <-
        sample(x = heterospecific[treatment == "open"],
               size = length(heterospecific[treatment == "closed"]),
               replace = T)
    }
  } else {
    heterospecific[treatment == "closed"] <- NA
  }
  heterospecific
}
