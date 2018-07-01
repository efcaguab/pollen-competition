make_fig_proportion_vs_variables <- function(trade_off_predictions) {
  require(ggplot2)
  trade_off_predictions %>%
    dplyr::mutate_at(c("conspecific", "heterospecific"), exp) %>%
    dplyr::mutate(gain = conspecific / (conspecific + heterospecific)) %>%
    dplyr::filter(!(value > 2.2 & term == "k")) %>%
    humanize() %>%
    ggplot(aes(x = value, y = gain, colour = term)) +
    geom_hline(yintercept = 0.5, size = 0.25, linetype = 2) + 
    geom_point(shape = 21, alpha = 0.5, size = 1) +
    geom_smooth(aes(fill = term), 
                se = F, size = 0.5, method = "glm", 
                method.args = list(family = "binomial"), alpha = 0.1)+
    pub_theme() +
    scale_color_manual(values = c_scale()) +  
    scale_fill_manual(values = c_scale()) +
    scale_y_continuous(labels = scales::percent) +
    theme(legend.position = "top") +
    labs(colour = "", fill = "", linetype = "", 
         y = "proportion conspecific pollen gain", 
         x = "scaled variable value")
}