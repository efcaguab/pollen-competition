# theme for abc presentation figures
abc_theme <- function(){
    theme(text = element_text(size = 18, colour = "white", family = "Space Mono"), 
          axis.text = element_text(colour = "grey75"),
          plot.title = element_text(face = "bold"),
          rect = element_rect(fill = "black"),
          line = element_line(colour = "grey10"),
          panel.background = element_rect(fill = "black", colour = "grey75"),
          plot.background = element_rect(colour = "black"),
          panel.grid.major = element_line(colour = "grey30"),
          panel.grid.minor = element_line(colour = "grey20"),
          strip.background = element_blank(),
          strip.text = element_text(hjust = 0),
          legend.title = element_text(hjust = 0.5), 
          legend.key.size = unit(3, "mm"), 
          legend.margin = margin())
}
