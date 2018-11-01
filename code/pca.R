# put data together to calculate PCA
get_pca_data <- function(plant_rel_abu, pollen_contribution, degree, org_frame, sites){
  
  pca_data <- plant_rel_abu %>%
    dplyr::full_join(pollen_contribution, by = c("plant_name", "site_name", "var_trans", "scale")) %>%
    dplyr::full_join(degree, by = c("plant_name", "site_name", "var_trans", "scale")) %>%
    dplyr::full_join(org_frame, by = c("plant_name", "site_name", "var_trans", "scale")) %>%
    dplyr::inner_join(sites, by = "site_name") %>%
    dplyr::filter(scale == "community",
                  var_trans == "log") %>% 
    dplyr::select(site_name, plant_name, abu, pollen_cont, kn, originality)  %>% 
    dplyr::mutate_at(dplyr::vars(abu, pollen_cont, kn), function(x) log(x))
  
  # scale variables across the whole study
  # this gives indication of the flexibility of a plant
  across <- pca_data %>%
    dplyr::group_by() %>%
    dplyr::mutate_if(is.numeric, dplyr::funs(scale)) %>%
    dplyr::mutate_if(is.numeric, dplyr::funs(as.numeric)) %>%
    dplyr::mutate(pca_type = "across")
  
  # scale variables within each community
  # this gives indication of the relative separation within a community
  within <- pca_data %>%
    dplyr::group_by(site_name) %>%
    dplyr::mutate_if(is.numeric, dplyr::funs(scale)) %>%
    dplyr::group_by() %>%
    dplyr::mutate(pca_type = "within")
  
  dplyr::bind_rows(across, within) 
}

# calculate several flavors of PCA
get_pca <- function(pca_data, imputation_variants = 1:3){
  pca_data %>%
    split(.$pca_type) %>%
    purrr::cross2(imputation_variants) %>% 
    purrr::map(~ impute_na_values_pca(.[[1]], .[[2]])) %>%
    purrr::map(get_factominer_pca, scale.unit = F, graph = FALSE)
}  

# impute variables for a PCA using rows that have at most `na_threshold` NA values
impute_na_values_pca <- function(x, na_threshold = 0){
  data_filtered <- x %>%
    dplyr::mutate(n_nas = apply(., 1, function(x) sum(is.na(x)))) %>%
    dplyr::filter(n_nas <= na_threshold) %>%
    dplyr::select(-n_nas)
  
  data_numeric <- data_filtered %>% 
    `class<-`("data.frame") %>% 
    dplyr::select_if(is.numeric)
  
  if (na_threshold != 0) {
    ncp <- data_numeric %>%
      missMDA::estim_ncpPCA() %>%
      extract2("ncp")
    
    imputed_data <- data_numeric %>% 
      `class<-`("data.frame") %>%
      dplyr::select_if(is.numeric) %>%
      missMDA::imputePCA(ncp = ncp, nb.init = 1) %>%
      extract2("completeObs") %>%
      as.data.frame()
  } else {
    imputed_data <- data_numeric
  }
  # put imputed numeric columns back into the original data frame
  data_filtered %>%
    dplyr::select_if(function(x) !is.numeric(x)) %>%
    dplyr::bind_cols(imputed_data) %>%
    dplyr::mutate(threshold = as.character(na_threshold))
}

# wrapper arround the factominer PCA function
get_factominer_pca <- function(x, ...) {
  # index of columns with qualitative information
  quali.sup <- lapply(x, is.character) %>% 
    unlist() %>%
    which()
  
  x %>% 
    `class<-`("data.frame") %>% 
    # dplyr::select_if(is.numeric) %>%
    FactoMineR::PCA(quali.sup = quali.sup, ...)
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


