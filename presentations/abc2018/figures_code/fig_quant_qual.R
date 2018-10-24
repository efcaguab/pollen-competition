make_fig_mean_quant_qual <- function(coefficient_averages, filename, ...){
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
    dplyr::summarise_at("estimate", dplyr::funs(mean, min, max)) %>%
    tidyr::separate(col = var, into = c("metric", "type"), sep = "_", remove = FALSE) %>%
    dplyr::mutate(type = paste0(type, ".")) %>%
    dplyr::filter(type == "abs.")
  
  params <- list(
    tile_width = 0.95,
    scale_length = 5
  )
  
  params$scale = scales::brewer_pal(palette = "RdYlGn")(params$scale_length)
  
  
  p <- quant_qual %>%
    plot_metric_abc(params)
  # p
  
  ggsave(filename, plot = p, device = "png", bg = "black", ...)
  
  # pdf(width = 3.25, height = 2)
  # p
  # dev.off()
}

format_qqplot_abc <- function(x, y){
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

plot_metric_abc <- function(x, params){
  x %>%
    dplyr::filter(scale == "community") %>%
    dplyr::group_by() %>%
    humanize() %>%
    dplyr::mutate(label1 = round(mean, 3), 
                  label2 = paste0("[", round(min, 3), ", ", round(max, 3), "]"), 
                  # var = forcats::fct_relevel(var, c("quantity", "quality")), 
                  term = forcats::fct_relevel(term, c("degree", "abundance", "share pollen"), after = 0)) %>%
    ggplot(aes(x = term, y = metric)) +
    geom_tile(aes(fill = mean), 
              height = params$tile_width, 
              width = 1-((1-params$tile_width)/2)) +
    geom_text(aes(label = label1, 
                  colour = abs(mean)> 0.35), 
              nudge_y = 0.15, 
              family = "Space Mono",
              size = 7, 
              fontface = "bold") +
    geom_text(aes(label = label2, 
                  colour = abs(mean)> 0.4), 
              nudge_y = -0.15, 
              family = "Space Mono",
              size = 5) +
    # facet_grid(metric ~ .) +
    scale_fill_gradient2(limits = c(-0.6, 0.6), 
                         name = "mean effect", 
                         high = params$scale[params$scale_length], 
                         mid = "white",
                         low = params$scale[1]) +
    scale_color_manual(values = c("grey30", "white")) +
    abc_theme() +
    # coord_flip() +
    # scale_y_discrete(expand = c(0,0)) +# , 
    # labels = c("quality of\npollination", "quantity of\npollination")) +
    theme(legend.position = "none", 
          panel.border = element_blank(), 
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          plot.subtitle = element_text(margin = margin(b = 15)),
          axis.text.y = element_text(size = 19, 
                                     angle = 90, hjust = 0.5, 
                                     colour = "white", face = "bold"),
          axis.text.x = element_text(size = 19, colour = "white"),
          panel.background = element_rect(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          # axis.text.x = element_text(size = 7),
          # axis.title.y = element_text(size = 7, face = "bold"),
          axis.ticks = element_blank() #, 
          # plot.title = element_text(size = 7, face = "bold", hjust = 0)
          ) +
    labs(title = "mean effect on pollination service", 
         subtitle = "estimated from 100 bootstrap replicates")  +
    scale_x_discrete(position = "top")
}
