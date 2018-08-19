
# make figure that shows relationship between global and community variables

make_fig_community_global_scatter <- function(plant_rel_abu, org_frame, degree, sites, pollen_contribution){
  require(ggplot2)
  
  ab <- plant_rel_abu %>%
    dplyr::select(-abu) %>%
    dplyr::rename(value = rab) %>%
    dplyr::mutate(term = "rab") 
  
  or <- org_frame %>%
    dplyr::select(-originality) %>%
    dplyr::rename(value = org) %>%
    dplyr::mutate(term = "org")
  
  k <- degree %>%
    dplyr::select(-kn) %>%
    dplyr::rename(value = k) %>%
    dplyr::mutate(term = "k")
  
  pc <- pollen_contribution %>%
    dplyr::select(-grain) %>%
    dplyr::rename(value = poc) %>%
    dplyr::mutate(term = "poc")
  
  dplyr::bind_rows(ab, or, k, pc) %>%  
    dplyr::filter(var_trans == "log") %>% 
    tidyr::spread(scale, value) %>%
    humanize(sites) %>%
    ggplot(aes(x = community, y = global)) +
    geom_smooth(method = "lm", colour = "black", size = 0.75) +
    geom_hline(yintercept = 0, size = 0.25, linetype = 2) +
    geom_vline(xintercept = 0, size = 0.25, linetype = 2) +
    geom_point(shape = 21, size = 1) +
    facet_wrap( ~ term, scales = "free", ncol = 4) +
    coord_equal() +
    pub_theme()
}
