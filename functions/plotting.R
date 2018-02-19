pub_theme <- function(){
  theme_bw() +
    theme(text = element_text(size = 8), 
          title = element_text(size = 8),
          strip.background = element_blank(),
          strip.text = element_text(hjust = 0),
          legend.title = element_text(hjust = 0.5))
}
