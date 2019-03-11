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
    dplyr::mutate_at(dplyr::vars(abu, kn), function(x) log(x))

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
get_pca <- function(pca_data, imputation_variants = 0:2){
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
      as.matrix() %>%
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

# find convex hull volumes defined by points of a category
convex_hull_volume <- function(this_pca, col, dims = 2){
  this_pca %>%
    get_pc_coords() %>%
    split(.[col]) %>%
    purrr::map(dplyr::select_if, is.numeric) %>%
    # discard groups that do not contain enough points to create a simplex one
    # point more than the dimensions
    purrr::map(dplyr::select, 1:dims) %>%
    purrr::discard(~ nrow(.x) < dims + 1) %>%
    purrr::map(geometry::convhulln, "FA") %>%
    purrr::map_dfr(~ data.frame(vol = extract2(., "vol")), .id = col)
}

# find distances of points in a category (median, mean, min, max)
distance_between_points <- function(this_pca, col){
  this_pca %>%
    get_pc_coords() %>%
    split(.[col]) %>%
    purrr::map(dplyr::select_if, is.numeric) %>%
    purrr::map(dist) %>%
    purrr::map_dfr(~ data.frame(median_dist = median(., na.rm = TRUE),
                                mean_dist = mean(., na.rm = TRUE)), .id = col)
}

# get a data frame with the principal compoment coordinates of a FactoMiner PCA object
get_pc_coords <- function(this_pca){
  this_pca$call$X %>%
    dplyr::select_if(function(x) !is.numeric(x)) %>%
    dplyr::bind_cols(as.data.frame(this_pca$ind$coord))
}

# get both distances and volumes from one call and return results in a data frame
# dims specifies the number of dimensions to be used for the convex hull
get_pca_sizes_df <- function(this_pca, col, dims = 2){
  distance <- distance_between_points(this_pca, col)
  volume <- convex_hull_volume(this_pca, col, dims)
  dplyr::full_join(distance, volume, by = col)
}

# randomise groupings in a FactoMiner::PCA object
# randomises the column `col` in the call
# randomisation can be constrained by `constrain_col`
randomise_pca_groupings <- function(this_pca, col, constrain_col = NULL){
  random_pca <- this_pca
  if (is.null(constrain_col)){
    new_factor <- this_pca$call$X %>%
      extract2(col) %>%
      as.character() %>%
      sample(replace = FALSE) %>%
      as.factor()
  } else {
    new_factor <- this_pca$call$X %>%
      extract(c(col, constrain_col)) %>%
      split(.[constrain_col]) %>%
      purrr::map(extract2, col) %>%
      purrr::map(as.character) %>%
      purrr::map(sample, replace = FALSE) %>%
      unlist() %>%
      `names<-`(NULL) %>%
      as.factor()
  }
  random_pca$call$X[col] <- new_factor
  random_pca
}

# run the randomisations and get the distances/volumes for a FactoMiner PCA object with groupings
# return a list with the randomisations, the empirical and the pca_type, threshold (bad practice?)
run_randomisations_pca <- function(this_pca, col, constrain_col, dims = 2, n_rand = 99){
  random <- 1:n_rand %>%
    purrr::map(~ randomise_pca_groupings(this_pca, col, constrain_col)) %>%
    purrr::map_df(get_pca_sizes_df, col, .id = "n_sample")

  empirical <- get_pca_sizes_df(this_pca, col, dims = dims) %>%
    dplyr::mutate(n_sample = as.character(0))

  list(empirical = empirical,
       random = random,
       pca_type = as.character(this_pca$call$X$pca_type[1]),
       na_threshold = as.character(this_pca$call$X$threshold[1]))
}

# call the randomisations with parameters for the plants
all_randomisations_plant_name <- function(pcas, n_rand){
  pcas %>%
    purrr::keep(function(x) as.character(x$call$X$pca_type[1]) == "across") %>%
    purrr::map(run_randomisations_pca,
               col = "plant_name",
               constrain_col = NULL,
               dim = 2,
               n_rand = n_rand) %>%
    purrr::map_df(randomisation_as_df)
}

# call the randomisations with parameters for the sites
all_randomisations_site_name <- function(pcas, n_rand){
  pcas %>%
    purrr::keep(function(x) as.character(x$call$X$pca_type[1]) == "within") %>%
    purrr::map(run_randomisations_pca,
               col = "site_name",
               constrain_col = "plant_name",
               dim = 2,
               n_rand = n_rand) %>%
    purrr::map_df(randomisation_as_df)
}

# convert the result of a call to a data frame
randomisation_as_df <- function(x){
  x %$%
    dplyr::bind_rows(empirical, random) %>%
    dplyr::mutate(pca_type = x$pca_type, na_threshold = x$na_threshold)
}

# get cumedist ranking for the randomisation results to get the permanova
get_permanova <- function(x, col){
  var <- rlang::syms(col)
  x %>%
  dplyr::group_by(!!var[[1]], na_threshold, pca_type) %>%
    dplyr::mutate_if(is.numeric, dplyr::percent_rank) %>%
    # sample 0 corresponds to the empirical distances
    dplyr::filter(n_sample == 0) %>%
    tidyr::gather(key = "metric", value = "value", dplyr::contains("dist"), dplyr::contains("vol")) %>%
    dplyr::group_by()
}

plot_permanova_dist <- function(permanova_plant_distances,
                                permanova_site_distances){
  require(ggplot2)

  perma_data <- permanova_plant_distances %>%
    dplyr::filter(na_threshold %in% c(0,1)) %>%
    dplyr::group_by(plant_name) %>%
    dplyr::mutate(mean_value = mean(value, na.rm = T)) %>%
    dplyr::group_by() %>%
    dplyr::filter(!is.na(mean_value),
                  metric == "median_dist",
                  na_threshold == 0,
                  !is.na(value)) %>%
    # dplyr::mutate(plant_name = paste0("  ", plant_name, "  ")) %>%
    dplyr::mutate(plant_name = shorten_sp_name(plant_name)) %>%
    dplyr::mutate(plant_name = forcats::fct_reorder(plant_name, value, .desc = F),
                  value_adj = p.adjust(value, method = "BH"))

  nudge <- 0
  perma_data %>%
    dplyr::mutate(x_label = dplyr::if_else(value < 0.05,
                                           max(value) + nudge, min(value) - nudge)) %>%
    ggplot(aes(x = value, y = plant_name)) +
    geom_tile(aes(width = Inf, height = 1,
                  alpha = as.numeric(plant_name) %% 2 == 0),
              fill = cgm()$fill_rows) +
    geom_vline(xintercept = 0.05, linetype = 2,
               size = cgm()$size_references,
               color = cgm()$color_references) +
    geom_segment(aes(xend = 0.05, yend = plant_name), size = 0.25,
                 color = cgm()$pal_el_green[9]) +
    geom_point(shape = 21,
               size = 1,
               fill = "white",
               color = cgm()$pal_el_green[9]) +
    geom_text(aes(label = plant_name, x = x_label),
              fontface = "italic",
              size = 2,
              hjust = "inward") +
    geom_point(aes(y = 13.5, x = 0.05), alpha = 0) +
    annotate(geom = "text", x = 0.05, y = 13,
             label = expression(" " %->% plain("more flexible") %->% ""),
             parse = F,
             hjust = "left",
             size = 2.25,
             colour = "grey20") +
    geom_point(aes(y = -0.5, x = 0.05), alpha = 0) +
    annotate(geom = "text", x = 0.05, y = 0,
             label = expression(" " %<-% plain("less flexible") %<-% ""),
             parse = F,
             hjust = "right",
             size = 2.25,
             colour = "grey20") +
    pub_theme() +
    theme(legend.position = "none",
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          panel.border = element_blank(),
          axis.line.x = element_line(size = 0.25)) +
    labs(x = expression(italic(p) - plain("value")),
         title = "(c) flexibility of plant's strategies",
         subtitle = "median distance between plant strategies vs. randomisations") +
    coord_cartesian(clip = "off") +
    scale_x_continuous(expand = c(0,0.1),
                       breaks = c(0.01, 0.05, 0.5, 0.99),
                       trans = "log") +
    scale_y_discrete(expand = c(0,0)) +
    scale_alpha_manual(values = c(0,1))

}

# Plot the PCA, scaled globally (across communities)
plot_pca <- function(pcas, chosen_threshold){

  require(ggplot2)

  this_pca <- pcas %>%
    purrr::keep(~ .$call$X$pca_type[1] == "across") %>%
    purrr::keep(~ .$call$X$threshold[1] == chosen_threshold) %>%
    extract2(1)

  this_pca_data <- this_pca$call$X %>%
    dplyr::bind_cols(tibble::as_data_frame(this_pca$ind$coord)) %>%
    dplyr::group_by(plant_name) %>%
    dplyr::mutate(n_sites = dplyr::n_distinct(site_name))

  variances_data <- this_pca$eig %>%
    as.data.frame() %>%
    tibble::rownames_to_column() %>%
    dplyr::mutate(rowname = stringr::str_remove(rowname, "comp ")) %>%
    dplyr::rename(dim = rowname)

  hulls <- this_pca_data %>%
    # hulls on;y for plants in more than one site
    dplyr::filter(n_sites > 1) %>%
    dplyr::group_by(plant_name) %>%
    dplyr::select_at(dplyr::vars(dplyr::contains("Dim"))) %>%
    dplyr::do(.[grDevices::chull(.[, -1L]), ]) %>%
    dplyr::filter(plant_name %in% unique(this_pca_data$plant_name))

  # plot only points that are in a hull or belong to single community
  hulls_and_single_points <- hulls %>%
    dplyr::mutate(hull_point = T) %>%
    dplyr::full_join(this_pca_data) %>%
    dplyr::filter(!is.na(hull_point) | n_sites == 1)

  this_pca_data %>%
    ggplot(aes(x = Dim.1, y = Dim.2)) +
    geom_hline(yintercept = 0,
               linetype = 2,
               colour = cgm()$color_references,
               size = cgm()$size_references) +
    geom_vline(xintercept = 0,
               linetype = 2,
               colour = cgm()$color_references,
               size = cgm()$size_references) +
    geom_polygon(data = hulls,
                 aes(group = plant_name),
                 colour = cgm()$color_errorbars,
                 fill = cgm()$pal_el_green[1],
                 size = cgm()$size_errorbars,
                 alpha = 0.25) +
    geom_point(data = hulls_and_single_points,
               aes(fill = n_sites > 1),
               shape = 21,
               colour = cgm()$pal_el_green[9],
               size = 1) +
    pub_theme() +
    scale_fill_manual(values = c("white", cgm()$pal_el_green[1])) +
    theme(legend.position = "none") +
    # coord_equal() +
    labs(x = paste0("1st component (", round(variances_data$`percentage of variance`[1]), "%)"),
         y = paste0("1nd component (", round(variances_data$`percentage of variance`[2]), "%)"),
         title = "(b) plant strategies in PCA space",
         subtitle = "convex hulls of species in two communities or more")

}

plot_pca_variances_and_contributions <- function(pcas, chosen_threshold){
  require(ggplot2)

  this_pca <- pcas %>%
    purrr::keep(~ .$call$X$pca_type[1] == "across") %>%
    purrr::keep(~ .$call$X$threshold[1] == chosen_threshold) %>%
    extract2(1)

  variances_data <- this_pca$eig %>%
    as.data.frame() %>%
    tibble::rownames_to_column() %>%
    dplyr::mutate(rowname = stringr::str_remove(rowname, "comp ")) %>%
    dplyr::rename(dim = rowname) %>%
    dplyr::mutate_if(is.numeric, function(x) x/100) %>%
    dplyr::mutate(group = "components' cumulative percentage of variance")

  contributions_data <- this_pca$var$contrib %>%
    as.data.frame() %>%
    tibble::rownames_to_column() %>%
    tidyr::gather(key = "dim", "contribution", dplyr::contains("Dim")) %>%
    dplyr::mutate(dim = stringr::str_remove(dim, "Dim."),
                  contribution = contribution/100) %>%
    dplyr::rename(term = rowname)

  contributions_data %>%
    humanize(term_long = F) %>%
    dplyr::mutate(term = forcats::fct_relevel(term, "visit effectiv.", "abundance", "func. originality")) %>%
    ggplot(aes(x = as.numeric(dim), y = contribution)) +
    geom_col(aes(fill = term), alpha = 0.75,
             position = position_stack(reverse = TRUE)) +
    geom_line(data = variances_data,
              aes(y = `cumulative percentage of variance`,
                  linetype = group),
              size = 0.5,
              colour = cgm()$pal_el_green[c(9)]) +
    geom_point(data = variances_data,
               aes(y = `cumulative percentage of variance`,
                   shape = group),
               size = 1,
               # shape = 21,
               fill = cgm()$pal_rb3[2],
               colour = cgm()$pal_el_green[c(9)]) +
    scale_x_continuous(breaks =1:4, trans = "reverse", labels = scales::ordinal(1:4)) +
    scale_y_continuous(labels = scales::percent, expand = c(0,0)) +
    scale_fill_manual(values = cgm()$pal_el_green[c(8,6,4,2)]) +
    scale_shape_manual(values = 21) +
    scale_linetype_manual(values = 1) +
    guides(fill = guide_legend(order = 2, title = ""),
           shape = guide_legend(order = 1, title = ""),
           linetype = guide_legend(order = 1, title = "")) +
    pub_theme() +
    coord_flip(clip = "off") +
    labs(x = "component",
         title = "(a) components' variance and variable contributions",
         subtitle = "principal component analysis of ecological variables") +
    theme(panel.border = element_blank(),
          axis.line.x = element_line(size = 0.25),
          legend.position = "top",
          axis.ticks.y = element_blank(),
          legend.box = "vertical",
          legend.spacing = unit(1, "pt"),
          plot.margin = margin(t = 5.5, r = 8, b = 5.5, l = 5.5, unit = "pt"),
          legend.text = element_text(margin = margin(r = 0, l = -1, unit = "pt")),
          legend.box.just = "right",
          legend.margin = margin(),
          legend.box.margin = margin(b = -10))

}

# Evaluate competition of alternative strategies
tinkering <- function(glanced_fixed, tidied_fixed, include_random = FALSE) {
  loadd(glanced_fixed)
  loadd(tidied_fixed)

  model_weights <- glanced_fixed %>%
    dplyr::group_by(model, pollen_category, scale, var_trans) %>%
    dplyr::mutate(delta_AIC = AICc - min(AICc),
                  model_likelihood = get_likelyhoods(delta_AIC),
                  model_weight = get_weights(model_likelihood)) %>%
    dplyr::group_by(fixed_formula, add = TRUE) %>%
    dplyr::select(model_weight) %>%
    dplyr::ungroup()


  a <- dplyr::inner_join(tidied_fixed, model_weights,
                         by = c("model", "pollen_category", "scale",
                                "var_trans", "fixed_formula")) %>%
    # Using the regex code \.\b so that in cases like Plant sp..MA_SC_R_1 the
    # string gets splited in the dot that is next to the second word boundary
    tidyr::separate(col = level,
                    into = c("plant_name", "site_name"),
                    sep = "\\.\\b") %>%
    dplyr::group_by(model, pollen_category, scale, var_trans, term) %>%
    dplyr::group_by(site_name, plant_name, add = TRUE) %>%
    dplyr::summarise(estimate = weighted.mean(x = estimate,
                                              w = model_weight),
                     n_formulas = dplyr::n_distinct(fixed_formula))
}
