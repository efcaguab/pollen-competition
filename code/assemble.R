# assemble all disparate data into one file

library(foreach)

args <- commandArgs(trailingOnly = TRUE)

# # testing defaults
# args <- list.files("data/datasets/processed/deposition", full.names = T)

input_files <- c("./data/datasets/processed/degree/marrero.rds", 
                 "data/datasets/processed/deposition/marrero.rds", 
                "data/datasets/processed/visitation/marrero.rds")
output_file <- args[length(args)]

data <- foreach(i=1:length(input_files), .combine = c) %do% {
  readRDS(args[i])
}

site_names <- foreach(i=1:length(input_files), .combine = c) %do% {
  lapply(readRDS(args[i]), function(x) x$name)
}

names(data) <- unlist(site_names)

saveRDS(data, output_file)
