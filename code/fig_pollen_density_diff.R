make_fig_pollen_density_diff <- function(rep_1){
  require(ggplot2)
 list(function(x) exp(x) - 1, I) %>%
    plyr::llply(plot_dist, rep_1)
  
}

plot_dist <- function(fun, rep_1){
  rep_1 %>%
    dplyr::filter(scale == 'imputed', 
                  var_trans == 'log') %>%
    dplyr::mutate_at(c('open', 'closed'), fun) %>% 
    ggplot(aes(x = open - closed, colour = pollen_category)) +
    stat_density(position = position_identity(), geom = "line") +
    pub_theme() +
    scale_color_manual(values = c_scale()) +
    theme(legend.position = "top") + 
    labs(x = "gain in pollen density (open-closed)", 
         colour = "pollen type")
}