vis_drake_graph_sml0 <- function(config){
  df <- dataframes_graph(config, split_columns = F)
  nodes_to_remove <- df$nodes %>%
    filter(level == 0,
           type != 'file') %$% id
  df$nodes <- df$nodes %>%
    filter(!(id %in% nodes_to_remove))
  df$edges <- df$edges %>%
    filter(!(from %in% nodes_to_remove))
  render_drake_graph(df)
}
