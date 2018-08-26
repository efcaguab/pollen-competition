 make_fig_correlation <- function(rep_1){
   require(ggplot2)
   
   r <- rep_1 %>%
     dplyr::group_by() %>%
     dplyr::filter(scale == "community") %>%
     dplyr::select(abn, poc, deg, org)
   # tidyr::gather(key = "var", value = "value", abn, poc, deg, org) %>% 
   
   cols <- expand.grid(x = c("abn", "poc", "deg", "org"), y = c("abn", "poc", "deg", "org")) %>%
     dplyr::mutate_all(as.character) %>%
     dplyr::filter(x != y)
   
   pl <-  plyr::ddply(cols, c("x", "y"), function(x, r){
     tibble::data_frame(
       x_values = r[x$x][[1]], 
       y_values = r[x$y][[1]])
   }, r) %>% 
     dplyr::rename(term = x) %>%
     humanize() %>%
     dplyr::rename(x = term, 
                   term = y) %>%
   humanize() %>%
   dplyr::rename(y = term) 
 
 cors <- pl %>%
   dplyr::group_by(x, y) %>%
   dplyr::summarise(cor = cor(x_values, y_values, use = "pairwise")) %>%
   dplyr::mutate(cor = paste0("Cor: ", round(cor, 3)))
 
 pl %>% 
   ggplot(aes(x = x_values, y = y_values)) +
   geom_point(shape = 21, size = 1) +
   geom_smooth(method = "lm", colour = "black", size = 0.5, se = FALSE) +
   geom_text(data = cors, aes(label = cor), x = -1.65, y = 2.5, size = 2.5) +
   facet_grid(x ~ y) + 
   pub_theme() +
   labs(x = "", y = "")
 }
