
make_fig_pca_abc <- function(species_pca, alpha_poly = 1, alpha_point = 1, alpha_loadings = 1, filename, highlighted_plant = NULL, ...){
  
  pc_analysis <- species_pca
  require(ggplot2)
  require(ggfortify)
  require(ggrepel)
  
  pca_data <- fortify(pc_analysis$acp, data = pc_analysis$data)
  pca_loadings <- pc_analysis$acp$rotation %>%
    tibble::as_tibble(rownames = "var_name") %>%
    dplyr::mutate_if(is.numeric, multiply_by, 3) %>%
    dplyr::rename(term = var_name) %>%
    humanize()
  
  # # scale
  # lam <- pc_analysis$acp$sdev[c(1, 2)]
  # lam <- lam * sqrt(nrow(pca_data))
  # lam <- lam ^ scale
  # pca_data[, c("PC1", "PC2")] <- t(t(pca_data[, c("PC1", "PC2")]) / lam)
  # 
  hulls <- pca_data %>%
    dplyr::group_by(plant_name) %>%
    dplyr::select_at(dplyr::vars(dplyr::contains("PC"))) %>%
    dplyr::do(.[grDevices::chull(.[, -1L]), ])

  if (alpha_poly == 0) {
    colour_poly <- NA
  } else {
    colour_poly <- "white"
  }
  
 
  
  
  set.seed(2)
  p <- pca_data %>%
    dplyr::group_by() %>%
    ggplot(aes(x = PC1, y = PC2)) +
    geom_segment(data = pca_loadings, 
                 aes(xend = 0, yend = 0), 
                 arrow = arrow(ends = "first", angle = 15, 
                               length = unit(0.1, "inches")),
                 color = "grey60", 
                 alpha = 1 * alpha_loadings) +
    geom_text_repel(data = pca_loadings, 
                    aes(label = term), 
                    colour = "white", 
                    family = "Space Mono",
                    size = 6, 
                    alpha = 1 * alpha_loadings) +
    geom_polygon(data = hulls,
                 aes(group = plant_name),
                 colour = colour_poly,
                 linetype = 1,
                 size = 0.25,
                 alpha = 1 * alpha_poly, 
                 fill = "white") +
    geom_point(colour = "white", alpha = 1 * alpha_point, size = 2) +
    # geom_text_repel(aes(label = plant_name), colour = "white", size = 1) +
    abc_theme() +
    labs(title = "plant strategies", 
         subtitle = "principal component decomposition", 
         x = "first principal component", 
         y = "second principal component") +
    coord_equal()
  
  if (!is.null(highlighted_plant)) {
    filtered_pca_data <- pca_data %>%
      dplyr::filter(plant_name %in% highlighted_plant) %>%
      dplyr::group_by() %>%
      dplyr::mutate(plant_name = dplyr::if_else(plant_name == "Mentha pulegium", 
                                                "ZZMentha pulegium", plant_name))
    filtered_hulls <- hulls %>%
      dplyr::filter(plant_name %in% highlighted_plant) %>%
      dplyr::group_by() %>%
      dplyr::mutate(plant_name = dplyr::if_else(plant_name == "Mentha pulegium", 
                                                "ZZMentha pulegium", plant_name))
    
    p <- p +
      geom_polygon(data = filtered_hulls, 
                   aes(fill = plant_name, 
                       color = plant_name), 
                   alpha = 0.5) +
      geom_point(data = filtered_pca_data, 
                 aes(color = plant_name), size = 2) +
      scale_color_brewer(palette = "Set1") + 
      scale_fill_brewer(palette = "Set1") +
      theme(legend.position = "none")
  }
  # p
  
  ggsave(filename, plot = p, device = "png", bg = "black", ...)
  
}
