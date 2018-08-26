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
    tile_width = 0.9,
    scale_length = 5
  )
  params$scale = scales::brewer_pal(palette = "RdYlGn")(params$scale_length)

  quant_qual %>%
    dplyr::filter(scale == "community") %>%
    dplyr::group_by() %>%
    humanize() %>%
   dplyr::mutate(label1 = round(mean, 3), 
                 label2 = paste0("[", round(quantile_05, 3), ", ", round(quantile_95, 3), "]"), 
                 var = forcats::fct_relevel(var, c("quantity", "quality")), 
                 term = forcats::fct_relevel(term, c("abundance", "degree"), after = 2)) %>%
    ggplot(aes(x = term, y = var)) +
    geom_tile(aes(fill = mean), 
              width = params$tile_width, 
              height = 1-((1-params$tile_width)/3)) +
    geom_text(aes(label = label1), 
              nudge_x = 0.15, 
              size = 2.8, 
              colour = "grey20") +
    geom_text(aes(label = label2), 
              nudge_x = -0.15, 
              size = 2.5) +
    scale_fill_gradient2(name = "mean\neffect", 
                         high = params$scale[params$scale_length], 
                         mid = params$scale[3],
                         low = params$scale[1]) +
    pub_theme() +
    coord_flip() +
   scale_y_discrete(position= "top", name = "effect on pollination", expand = c(0,0)) +
   scale_x_discrete(name = "", expand = c(0,0)) +
    theme(panel.border = element_blank(), 
          axis.title.x = element_text(size = 8, face = "bold"),
          axis.text.x = element_text(size = 8),
          axis.title.y = element_blank(), 
          axis.ticks = element_blank())
}