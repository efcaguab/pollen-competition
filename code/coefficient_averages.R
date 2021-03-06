get_coefficient_averages <- function(tidied_fixed, model_formula_ranking, N = 99){
  model_weights <- model_formula_ranking$by_model_set %>%
    dplyr::group_by(pollen_category, scale) %>%
    dplyr::mutate(l = get_likelyhoods(delta_AIC_median),
                  w = get_weights(l)) %>%
    dplyr::select(pollen_category, scale, fixed_formula, w)

  1:N %>%
    purrr::map_df(get_coeficient_estimate_sample, tidied_fixed, model_weights, N)

}

get_coeficient_estimate_sample <- function(sample_n, tidied_fixed, model_weights, N){
  # consider cases with zero

  tidied_fixed %>%
    dplyr::filter(term != "(Intercept)") %>%
    dplyr::group_by(model, pollen_category, scale, fixed_formula, term) %>%
    dplyr::summarise(estimate = mean(estimate)) %>%
    dplyr::group_by() %>%
    # complete cases. The estimate of the term for the models in which it wasn't present is zero
    tidyr::complete(model, pollen_category, scale, fixed_formula, term, fill = list(estimate = 0)) %>%
    dplyr::inner_join(model_weights,
                      by = c("pollen_category", "scale", "fixed_formula")) %>%
    dplyr::group_by(pollen_category, scale, term) %>%
    dplyr::sample_n(size = N, replace = T, weight = w) %>%
    # dplyr::summarise(estimate = weighted.mean(estimate, w)) %>%
    dplyr::mutate(sample_n = sample_n)
}


# coefficient_averages %>%
#   dplyr::filter(grepl("abs",pollen_category)) %>%
#   # dplyr::filter(estimate != 0) %>%
#   ggplot(aes(x = estimate)) +
#   geom_density(binwidth = 0.05) +
#   facet_grid(pollen_category~term) +
#   scale_y_continuous(trans = "log1p")

plot_coefficient_averages <- function(coefficient_averages, variable_importance){
  # drake::loadd(coefficient_averages)
    require(ggplot2)

    var_imp <- variable_importance %>%
      tidyr::gather("key", "value", `conspecific (absolute)`:heterospecific) %>%
      dplyr::filter(key %in% c("conspecific (absolute)", "heterospecific")) %>%
      dplyr::mutate(term = forcats::fct_reorder(term, value))

    plot_metric_qual_quan <- function(x, this_metric){
      # annotations <- tibble::data_frame(
      #   x = 5,
      #   y = 0,
      #   label = c("\u2665 positive effect →", "negative effect"),
      #   hjust = c(0, 1),
      #   metric = NA
      # )

      abs_smaller <- function(x,y) {if (abs(x) < abs(y)) return(x) ; y}
      abs_larger <-  function(x,y) {if (abs(x) < abs(y)) return(y) ; x}

      line_offset <- 0.25
      p <- x %>%
        dplyr::filter(metric == this_metric) %>%
        dplyr::rowwise() %>%
        dplyr::mutate(label_y = dplyr::if_else(abs(estimate_mid) < 0.25,
                                               abs_larger(mean_estimate_quantile_025, mean_estimate_quantile_975),
                                               abs_smaller(mean_estimate_quantile_025, mean_estimate_quantile_975)),
                      label_hjust = dplyr::if_else(abs(estimate_mid) > 0.25,
                      "inward",
                      "outward")) %>%
        # dplyr::filter(metric == this_metric) %>%
        ggplot(aes(y = estimate_mid, x = term, group = metric)) +
        geom_tile(aes(height = Inf, width = 1, alpha = as.numeric(term) %% 2 == 0),
                  fill = cgm()$fill_rows) +
        geom_hline(yintercept = 0, linetype = 2, size = 0.25, colour = "gray30") +
        geom_text(aes(#label = "..",
          label = paste0(" ", format(round(estimate_mid, 2)), " "),
                      y = label_y,
                      hjust = label_hjust,
                      alpha = metric == this_metric),
                  size = 2,
                  # nudge_x = line_offset,
                  vjust = 0.5) +
        # geom_errorbar(aes(x = as.numeric(term) - line_offset, ymin = estimate_quantile_025,
        #                   ymax = estimate_quantile_975,
        #                   alpha = metric == this_metric),
        #               colour = cgm()$color_errorbars,
        #               size = cgm()$size_errorbars,
        #               width = 0,
        #               position = position_dodge(width = dodge_width)) +
        geom_errorbar(aes(x = term, ymin = mean_estimate_quantile_025,
                          ymax = mean_estimate_quantile_975,
                          alpha = metric == this_metric),
                      colour = cgm()$color_errorbars,
                      size = cgm()$size_errorbars,
                      width = 0,
                      position = position_dodge(width = dodge_width)) +
        geom_point(aes(x = term, alpha = metric == this_metric),
                   colour = cgm()$pal_el_green[9], shape = 21,
                   fill = "white",
                   size = 1,
                   position = position_dodge(width = dodge_width)) +
        # geom_text(data = annotations, aes(x = x, y = y, label = label, hjust = hjust),
                  # size = 2) +
        pub_theme() +
        labs(y = "effect size") +
        scale_x_discrete(expand = c(0,0)) +
        coord_flip(clip = "off") +
        theme(legend.position = "none",
              axis.title = element_blank(),
              axis.ticks.y = element_blank(),
              axis.text.y = element_blank(),
              panel.border = element_blank(),
              axis.line.x = element_line(size = 0.25)) +
        scale_alpha_manual(values = c(0,1))

      if (this_metric == "quality") {
        p <- p + scale_y_continuous(limits = c(-0.24, 0.35))
      }
      p
    }

    dodge_width <- 0
    qua_qua_data <- coefficient_averages  %>%
      dplyr::group_by(scale, pollen_category, term, sample_n) %>%
      dplyr::mutate(sample_n2 = 1:n()) %>%
      dplyr::select(pollen_category, term, sample_n, sample_n2, estimate) %>%
      tidyr::spread(pollen_category, estimate) %>%
      dplyr::mutate(quantity_abs = conspecific_abs,
                    quality_abs = conspecific_abs - heterospecific_abs) %>%
      dplyr::select(scale, term, sample_n, dplyr::contains("qua")) %>%
      tidyr::gather("var", "estimate", dplyr::contains("qua")) %>%
      dplyr::group_by(scale, term, sample_n, var) %>%
      dplyr::mutate(mean_estimate = mean(estimate)) %>%
      dplyr::group_by(scale, term, var) %>%
      # dplyr::sample_n(9, replace = TRUE) %>%
      dplyr::summarise_at(c("estimate", "mean_estimate"), dplyr::funs(mid, lower, upper, quantile_025, quantile_975)) %>%
      tidyr::separate(col = var, into = c("metric", "type"), sep = "_", remove = FALSE) %>%
      dplyr::mutate(type = paste0(type, ".")) %>%
      dplyr::group_by() %>%
      humanize() %>%
      dplyr::mutate(term = factor(term, levels(var_imp$term)))

    plots <- purrr::map(c("quantity", "quality"),
                        ~ plot_metric_qual_quan(qua_qua_data, .)) %>%
      purrr::map2(c("quantity\n(conspecific)", "purity\n(consp. / heterosp.)"),
                  function(x,y) {x + labs(subtitle = y)})

    plots[[1]] <- plots[[1]] +
      theme(axis.text.y = element_blank(),
            plot.margin = margin(t = 5.5, r = 0, b = 5.5, l = 5.5, unit = "pt"),
            plot.title = element_text(margin = margin(b = 5.5, unit = "pt")),
            axis.line.x = element_line(size = 0.25)) +
      labs(title = "(c) mean effect on pollination service")
    plots[[2]] <- plots[[2]] + theme(plot.subtitle = element_text(hjust = 1),
                                     axis.line.x = element_line(size = 0.25),
                                     plot.margin = margin(t = 5.5, r = 5.5, b = 5.5, l = 0, unit = "pt"))

    plots[[3]] <- qua_qua_data %>%
      ggplot(aes(y = term)) +
      geom_text(aes(label = term), x = 0.5, stat = "unique", size = 2.3)+
      # expansion to match the 90% width of the columns
      scale_y_discrete(expand = c(0,0.5)) +
      theme_minimal() +
      theme(axis.text = element_blank(),
            axis.title = element_blank(),
            panel.grid = element_blank(),
            plot.margin = margin(r = 0, t = 5.5, b = 5.5, l = 0))
  #
  #   pdf(width = 3.25, height = 1.5)
  #   cowplot::plot_grid(
  #     plots[[1]],
  #     plots[[3]],
  #     plots[[2]],
  #     ncol = 3,
  #     align = "h",
  #     rel_widths = c(1, 0.75, 1) # for two thirds figure its 1:1.4
  #   )
  # dev.off()
  plots
}
