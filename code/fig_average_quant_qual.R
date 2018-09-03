make_fig_average_quant_qual <- function(coefficient_averages){
  require(ggplot2)
  
  quant_qual <- coefficient_averages %>%
    dplyr::group_by(scale, pollen_category, term, sample_n) %>%
    dplyr::mutate(sample_n2 = 1:n()) %>% 
    dplyr::select(pollen_category, term, sample_n, sample_n2, estimate) %>%
    tidyr::spread(pollen_category, estimate) %>% 
    dplyr::mutate(quantity = conspecific, 
                  quality = conspecific - heterospecific) %>%
    dplyr::select(scale, term, sample_n, quantity, quality) %>%
    tidyr::gather("var", "estimate", quantity, quality) %>% 
    dplyr::group_by(scale, term, sample_n, var) %>%
    dplyr::summarise_at("estimate", mean) %>% 
    dplyr::group_by(scale, term, var) %>%
    dplyr::summarise_at("estimate", dplyr::funs(mean, quantile_05, quantile_95))
  
  params <- list(
    tile_width = 0.95,
    scale_length = 5
  )
  
  params$scale = scales::brewer_pal(palette = "RdYlGn")(params$scale_length)

  p <- quant_qual %>%
    dplyr::filter(scale == "community") %>%
    dplyr::group_by() %>%
    humanize() %>%
   dplyr::mutate(label1 = round(mean, 3), 
                 label2 = paste0("[", round(quantile_05, 3), ", ", round(quantile_95, 3), "]"), 
                 # var = forcats::fct_relevel(var, c("quantity", "quality")), 
                 term = forcats::fct_relevel(term, c("degree", "abundance", "trait originality"), after = 0)) %>%
    ggplot(aes(x = term, y = var)) +
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
    scale_fill_gradient2(name = "mean effect", 
                         high = params$scale[params$scale_length], 
                         mid = "white",
                         low = params$scale[1]) +
    scale_color_manual(values = c("grey30", "white")) +
    pub_theme() +
    # coord_flip() +
    labs(title = "mean effect") +
   scale_y_discrete(expand = c(0,0), 
                    name = "effect on the", 
                    labels = c("quality of\npollination", "quantity of\npollination")) +
   scale_x_discrete(position = "top", expand = c(0,0)) +
    theme(legend.position = "none", 
          panel.border = element_blank(), 
          axis.title.x = element_blank(),
          axis.text.y = element_text(size = 7),
          axis.title.y = element_blank(), 
          axis.ticks = element_blank(), 
          plot.title = element_text(size = 7, face = "bold", hjust = 0))
  
  # pdf(width = 3.25, height = 1.1)
  p
  # dev.off()
}