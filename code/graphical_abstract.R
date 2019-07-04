export_graphical_abstract <- function(fig_distances, filename, dpi){
  suppressPackageStartupMessages({
    library(ggplot2)
  })
  p <- fig_distances +
    labs(title = "Most plants have flexible trade-off strategy",
         subtitle = "Plants use differentstrategies depending on the community they belong to",
         caption = "Median distance between plant niches and randomisations")

  ggsave(filename, p, device = "png", width = 4, height = 3.2, dpi = dpi)
}
