get_pca <- function(imputed_abundance, imputed_pollen, imputed_degree, imputed_originality, sites){
  
  # get data together
  data <- imputed_abundance %>% 
    dplyr::left_join(imputed_pollen, by = c("site_name", "plant_name", "scale")) %>% 
    dplyr::left_join(imputed_degree, by = c("site_name", "plant_name", "scale")) %>% 
    dplyr::left_join(imputed_originality, by = c("site_name", "plant_name", "scale")) %>%
    dplyr::inner_join(sites, by = "site_name") %>% 
    dplyr::filter(scale == "imputed", 
                  !is.na(abn), 
                  !is.na(poc), 
                  !is.na(deg), 
                  !is.na(org)) 
  
  # principal components analysis
  panal <- data %>%
    dplyr::select(-fragment) %>%
    dplyr::select_if(is.numeric) %>%
    prcomp()
  
  list(acp = panal, data = data)
  
}

make_fig_pca <- function(pc_analysis){
  
  require(ggplot2)
  require(ggfortify)
  
  pca_data <- fortify(pc_analysis$acp, data = pc_analysis$data)
  pca_loadings <- pc_analysis$acp$rotation %>%
    tibble::as_tibble(rownames = "var_name")
  
  hulls <- pca_data %>%
    dplyr::group_by(plant_name) %>%
    dplyr::select_at(dplyr::vars(dplyr::contains("PC"))) %>%
    dplyr::do(.[grDevices::chull(.[, -1L]), ])
  
  pca_data %>%
    dplyr::group_by() %>%
    ggplot(aes(x = PC1, y = PC2)) +
    # geom_point() +
    geom_polygon(data = hulls, 
                 aes(group = plant_name), 
                 colour = "black",
                 linetype = 2, 
                 size = 0.25, 
                 alpha = 0.1) +
    geom_segment(data = pca_loadings, 
                 aes(xend = 0, yend = 0), 
                 color = "red")
  
  panal %>%
    autoplot(data = data, 
             colour = "plant_name", 
             # label.label = "plant_name",
             label = F,
             # label.repel= T,
             loadings = T, 
             loadings.label = T, 
             frame = T, 
             scale = F, 
             frame.type = "convex") +
    pub_theme() +
    theme(legend.position = "none")
}


