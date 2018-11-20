# model facilitation in Tur style using deposition data
# uses lme4 and has a random slope and intercept for every plant-site combination
model_facilitation <- function(dep_frame){
  dep_frame %>%
    dplyr::filter(treatment == "open") %>%
    dplyr::select(plant, plant_name, site_name, pollen_density, pollen_category) %>%
    tidyr::spread(key = "pollen_category", value = "pollen_density") %>% 
    dplyr::mutate(conspecific = ceiling(conspecific)) %>%
    lme4::glmer(conspecific ~ heterospecific + (heterospecific | plant_name : site_name), family = "poisson", data = .)
}

# extract the random effects from facilitation models 
# also gets the error using the conditional variance stored as an attribute in the model
extract_random_effects <- function(facilitation_models){
  facilitation_models %>%
    lme4::ranef(condVar = TRUE) %>%
    extract2(1) %>% {
      x <- . ; x %>% 
        tibble::rownames_to_column() %>%
        # call helper function
        dplyr::mutate(error = get_error_random_effects(x)) 
    }  %>%
    tidyr::separate("rowname", into = c("plant_name", "site_name"), sep = ":", remove = F) 
}

# helpef function to extract the error from the model
get_error_random_effects <- function(x) {
  x %>%
    attr("postVar") %>%
    extract(2,2,) %>%
    sqrt() 
}

# plot the slopes of the random effects models
plot_random_slopes <- function(facilitation_random_effects){
  require(ggplot2)
  facilitation_random_effects %>%
    dplyr::mutate(plant_name = forcats::fct_reorder(plant_name, heterospecific), 
                  upper = heterospecific + error, 
                  lower = heterospecific - error) %>%
    ggplot(aes(x = plant_name, y = heterospecific, group = site_name)) +
    geom_point(aes(group = site_name), size = 1, 
               position = position_dodge(width = 0.5), 
               shape = 21) +
    geom_hline(aes(yintercept = 0), 
               linetype = 2, 
               size = 0.5, 
               colour = "grey40") +
    geom_linerange(aes(ymin = lower, ymax = upper), 
                   position = position_dodge(width = 0.5), 
                   size = 0.25) + 
    labs(y = "Slope") + 
    coord_flip() +
    pub_theme() +
    theme(axis.title.y = element_blank(), 
          axis.text.y = element_text(face = "italic"), 
          panel.grid.major.y = element_line())
}
