make_fig_abundance <- function(plant_rel_abu, sites) {
  require(ggplot2)
  
  p1 <- plant_rel_abu %>%
    dplyr::filter(scale == 'community', 
                  var_trans == 'log') %>%
    dplyr::inner_join(sites, by = 'site_name') %>%
    ggplot() +
    stat_density(aes(x = rab, colour = locality_name),
                 position = position_identity(), geom = 'line') +
    # stat_density(aes(x = abu_tot_rel_log), geom = 'line') +
    scale_color_manual(values = c_scale()) +
    pub_theme() +
    # guides(colour = guide_legend(title = 'locality'), linetype = guide_legend(title = 'land use')) +
    labs(colour = 'locality', x = 'relative abundance')
  
  p2 <- plant_rel_abu %>%
    dplyr::filter(scale == 'global', 
                  var_trans == 'log') %>%
    dplyr::distinct() %>%
    dplyr::inner_join(sites, by = 'site_name') %>%
    ggplot(aes(x = abu)) +
    stat_density(position = position_identity(), geom = 'line') +
    scale_color_manual(values = c_scale()) +
    pub_theme() +
    labs(x = 'abundance (floral units)')
  
  list(p1, p2)
}