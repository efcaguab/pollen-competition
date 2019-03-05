# model facilitation in Tur style using deposition data
# uses lme4 and has a random slope and intercept for every plant-site combination
model_facilitation <- function(dep_frame){
  d <- dep_frame %>%
    dplyr::filter(treatment == "open") %>%
    dplyr::select(plant, plant_name, site_name, n_grains, n_stigma, pollen_category) %>%
    tidyr::spread(key = "pollen_category", value = "n_grains")
    # dplyr::mutate(conspecific = ceiling(conspecific)) %>%
   lme4::glmer(conspecific ~
                  heterospecific + (heterospecific | plant_name : site_name),
                family = "poisson",
                offset = log(n_stigma),
                data = d)
}

# extract the random effects from facilitation models
# also gets the error using the conditional variance stored as an attribute in the model
extract_random_effects <- function(facilitation_models){
  facilitation_models %>%
    coef() %>%
    extract2(1) %>%
    tibble::rownames_to_column() %>%
    dplyr::mutate(error = get_error_random_effects(facilitation_models)) %>%
    tidyr::separate("rowname", into = c("plant_name", "site_name"), sep = ":", remove = F)
}

# helpef function to extract the error from the model
get_error_random_effects <- function(x) {
  x %>%
    lme4::ranef(condVar = TRUE) %>%
    extract2(1) %>%
    attr("postVar") %>%
    extract(2,2,) %>%
    sqrt()
}

get_facilitation_plot_df <- function(dep_frame, facilitation_random_effects){
  n_obs <- dep_frame %>%
    dplyr::filter(treatment == "open") %>%
    dplyr::group_by(plant_name) %>%
    dplyr::summarise(n = dplyr::n_distinct(plant))

  facilitation_random_effects %>%
    dplyr::full_join(n_obs) %>%
    dplyr::mutate(plant_name = paste0(plant_name, " (", n, ")")) %>%
    dplyr::mutate(plant_name = forcats::fct_reorder(plant_name, heterospecific, .desc = TRUE),
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
}

# plot the slopes of the random effects models
plot_random_slopes <- function(facilitation_plot_df){
  pal <- common_graphic_metrics()$pal_rb3

  require(ggplot2)

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

  symlog <- function(x, C = -1){
    sign(x)*(log10(1+abs(x)/(10^C)))
  }
  symloginv <- function(y, C = -1){
    sign(y)*10^C*(10^(abs(y))-1)
  }
  symlog_trans <- scales::trans_new("symlog", symlog, symloginv)

  nudge_lower <- 0.05
  nudge_upper <- 0.05

  n_plants <- dplyr::n_distinct(facilitation_plot_df$plant_name)
  slope_plot <- facilitation_plot_df %>%
    dplyr::group_by(plant_name) %>%
    dplyr::arrange(plant_name, heterospecific) %>%
    dplyr::mutate(n_sites = dplyr::n_distinct(site_name),
                  art_site = 1:n_sites[1],
                  hete_mean = median(heterospecific)) %>%
    dplyr::group_by() %>%
    dplyr::mutate(y_label = dplyr::if_else(hete_mean > 0,
                                           min(lower) - nudge_lower,
                                           max(upper) + nudge_upper)) %>%
    ggplot() +
    #this linerange is only to start with a discrete scale
    geom_linerange(aes(x = plant_name,
                       group = art_site,
                       ymin = lower, ymax = upper),
                   position = position_dodge(width = 0.5),
                   size = cgm()$size_errorbars,
                   # color = color_errorbars_light,
                   alpha = 0) +
    geom_tile(data = shades,
              aes(x = x, alpha = alpha == 0,y = 0),
              width = 1, height = Inf, fill = cgm()$fill_rows) +
    geom_hline(aes(yintercept = 0),
               linetype = 2,
               size = 0.25,
               colour = cgm()$color_reference) +
    geom_linerange(aes(x = plant_name,
                       group = art_site,
                       y = heterospecific,
                       ymin = lower, ymax = upper),
                   position = position_dodge(width = 0.75),
                   size = cgm()$size_errorbars,
                   color = cgm()$color_errorbars_light) +
    geom_point(aes(x = plant_name, y = heterospecific, group = art_site,
                   fill = effect_category), size = 1,
               position = position_dodge(0.75),
               shape = 21,
               color = cgm()$color_errorbars) +
    geom_text(aes(label = plant_name, x = plant_name,
                  y = y_label),
              stat = "unique",
              size = 2,
              fontface = "italic",
              hjust = "inward") +
    geom_point(aes(y = 0, x = -0.5), alpha = 0) +
    annotate(geom = "text", x = 0, y = 0,
             label = expression(" " %->% plain("facilitation predominates") %->% ""),
             hjust = "left",
             vjust = 0.5,
             size = 2.25,
             colour = "grey20") +
    geom_point(aes(y = 0, x = n_plants + 1.5), alpha = 0) +
    annotate(geom = "text", x = n_plants + 1, y = 0,
             label = expression(" " %<-% plain("competition predominates") %<-% ""),
             hjust = "right",
             vjust = 0.5,
             size = 2.25,
             colour = "grey20") +
    scale_fill_manual(values = pal) +
    scale_y_continuous(trans = symlog_trans,
                       breaks = c(-1, -0.5, -0.25, -0.1,  0, 0.1, 0.25, 0.5, 1, 3, 10),
                       expand = c(0,0.05)) +
    scale_x_discrete(expand = c(0,0)) +
    scale_alpha_manual(values = c(0,1)) +
    labs(y = bquote("slope" ~ beta[i]),
         title = "(a) relationship hetero-conspecific pollen",
         subtitle = "slope of species-community random effects") +
    coord_flip() +
    pub_theme() +
    theme(axis.title.y = element_blank(),
          axis.text.y =element_blank(),
          legend.position = "none",
          axis.ticks.y = element_blank(),
          panel.border = element_blank(),
          axis.line.x = element_line(size = 0.25))
  # slope_plot

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
    labs(y = "% species") +
    pub_theme() +
    theme(legend.position = "none",
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 6),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.border = element_blank(),
          # axis.line.y = element_line(size = 0.25),
          plot.margin = unit(c(1,1,1,0), "mm"))
}
