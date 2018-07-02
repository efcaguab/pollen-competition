make_fig_pollen_density <- function(dep_frame) {
  require(ggplot2)
  p1 <- dep_frame %>%
    dplyr::mutate(pollen_category = paste(pollen_category, "pollen"), 
                  treatment = dplyr::if_else(treatment == 'closed', 'bagged', treatment)) %>%
    ggplot(aes(x = pollen_density, colour = treatment)) + 
    stat_density(geom = "line") +
    facet_wrap( ~ pollen_category, scales = "free") +
    pub_theme() +
    scale_color_manual(values = c_scale()) +
    labs(x = "pollen density (grains per stigma)", 
         colour = "flower treatment") +
    theme(legend.position = "top")
  
  p2 <- p1 +
    scale_x_log10()
  list(p1, p2)
}