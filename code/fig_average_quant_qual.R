make_fig_average_quant_qual <- function(coefficient_averages){
  require(ggplot2)
  
  quant_qual <- coefficient_averages %>%
    dplyr::group_by(scale, pollen_category, term, sample_n) %>%
    dplyr::mutate(sample_n2 = 1:n()) %>% 
    dplyr::select(pollen_category, term, sample_n, sample_n2, estimate) %>%
    tidyr::spread(pollen_category, estimate) %>% 
    dplyr::mutate(quantity_rel = conspecific, 
                  quantity_abs = conspecific_abs,
                  quality_rel = conspecific - heterospecific, 
                  quality_abs = conspecific_abs - heterospecific_abs) %>%
    dplyr::select(scale, term, sample_n, dplyr::contains("qua")) %>%
    tidyr::gather("var", "estimate", dplyr::contains("qua")) %>% 
    dplyr::group_by(scale, term, sample_n, var) %>%
    dplyr::summarise_at("estimate", mean) %>% 
    dplyr::group_by(scale, term, var) %>%
    dplyr::summarise_at("estimate", dplyr::funs(mean, quantile_05, quantile_95)) %>%
    tidyr::separate(col = var, into = c("metric", "type"), sep = "_", remove = FALSE) %>%
    dplyr::mutate(type = paste0(type, "."))
  
  params <- list(
    tile_width = 0.95,
    scale_length = 5
  )
  
  params$scale = scales::brewer_pal(palette = "RdYlGn")(params$scale_length)

  p <- c("quantity", "quality") %>%
    purrr::map(~plot_metric(quant_qual, ., params)) %>%
    purrr::map2(c("top", "bottom"), format_qqplot) %>%
    cowplot::plot_grid(plotlist = .,ncol = 1, 
                       rel_heights = c(1.325, 1))
  
  # pdf(width = 3.25, height = 2)
  p
  # dev.off()
}

format_qqplot <- function(x, y){
  if (y == "top") {
    x <- x +
      labs(title = "mean effect on pollination service") 
  } else{
    x <- x +
      theme(axis.text.x = element_blank()) 
  }
  x +
    theme(plot.margin = unit(0.5 * c(1,1,1,1), "mm")) +
    scale_x_discrete(position = y, expand = c(0,0))
}

plot_metric <- function(x, this_metric, params){
  x %>%
    dplyr::filter(scale == "community", 
                  metric == this_metric) %>%
    dplyr::group_by() %>%
    humanize() %>%
    dplyr::mutate(label1 = round(mean, 3), 
                  label2 = paste0("[", round(quantile_05, 3), ", ", round(quantile_95, 3), "]"), 
                  # var = forcats::fct_relevel(var, c("quantity", "quality")), 
                  term = forcats::fct_relevel(term, c("degree", "abundance", "trait originality"), after = 0)) %>%
    ggplot(aes(x = term, y = type)) +
    geom_tile(aes(fill = mean), 
              height = params$tile_width, 
              width = 1-((1-params$tile_width)/2)) +
    geom_text(aes(label = label1, 
                  colour = abs(mean)> 0.4), 
              nudge_y = 0.15, 
              size = 2.6, 
              fontface = "bold") +
    geom_text(aes(label = label2, 
                  colour = abs(mean)> 0.4), 
              nudge_y = -0.15, 
              size = 1.9) +
    # facet_grid(metric ~ .) +
    scale_fill_gradient2(limits = c(-0.88, 0.88), 
                         name = "mean effect", 
                         high = params$scale[params$scale_length], 
                         mid = "white",
                         low = params$scale[1]) +
    scale_color_manual(values = c("grey30", "white")) +
    pub_theme() +
    # coord_flip() +
    scale_y_discrete(expand = c(0,0), 
                     name = paste0(this_metric)) +# , 
    # labels = c("quality of\npollination", "quantity of\npollination")) +
    
    theme(legend.position = "none", 
          panel.border = element_blank(), 
          axis.title.x = element_blank(),
          axis.text.y = element_text(size = 7, angle = 90, hjust = 0.5),
          axis.text.x = element_text(size = 7),
          axis.title.y = element_text(size = 7, face = "bold"),
          axis.ticks = element_blank(), 
          plot.title = element_text(size = 7, face = "bold", hjust = 0))
  
}
