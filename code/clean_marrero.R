# clean Hugo's pollen deposition data

library(dplyr)
library(foreach)

# # script arguments 
# args <- commandArgs(trailingOnly = TRUE)
# input_file <- args[1]
# output_file <- args[2]


# DEPOSITION --------------------------------------------------------------

# testing defaults
input_file <- "./data/datasets/raw/marrero2016/estigmatic_pollen.csv"
# output_file <- "./data/datasets/processed/deposition/marrero.rds"

data <- readr::read_csv(input_file)

data <- data %>% 
  group_by(site, land_use) %>%
  mutate(fragment_name = fragment, 
         fragment_name = gsub("\x92", "i", fragment_name),
         fragment = as.numeric(as.factor(fragment)),
         site_name = paste("MA", site, toupper(land_use), fragment, sep = "_"))	%>%
  rename(plant_name = plant, 
         cons = co_grains,
         hete = he_grains,
         n_stigma = stigma) %>%
  group_by()

site_data <- data %>%
  group_by(site_name) %>%
  summarise(locality = unique(site), 
            land_use = unique(land_use),
            fragment = unique(fragment),
            fragment_name = unique(fragment_name)) %>%
  mutate(land_use = if_else(land_use == "a", "agricultural", "reserve")) %>%
  group_by()

plant_data <- data %>%
  group_by(site_name, plant_name) %>%
  summarise(status = unique(status)) %>%
  mutate(status = if_else(status == "e", "exotic", "native")) %>%
  group_by()

data <- data %>%
  select(site_name, 
         plant_name,
         treatment,
         cons,
         hete,
         n_stigma) %>%
  rename(recipient = plant_name) %>%
  mutate(plant = 1:nrow(.))

plants <- data %>% 
  select(site_name, plant, n_stigma, treatment, recipient)

conspecific <- data %>%
  mutate(donor = recipient, 
         n_grains = cons) %>% 
  select(plant, recipient, donor, n_grains) 

heterospecific <- data %>%
  mutate(donor = "UNKNOWN", 
         n_grains = hete) %>% 
  select(plant, recipient, donor, n_grains)

data <- bind_rows(conspecific, heterospecific) %>% 
  left_join(plants) %>% 
  mutate(pollen_type = "non_germinated") %>%
  select(site_name, plant, n_stigma, recipient, donor, n_grains, pollen_type, treatment)

# save all data in a list of unique sites in a sort of deposition-site object.
# each site has a list of properties that describe and relate it to other sites.
# As a minimum it has a name and a deposition table
data_dep <- foreach(i=1:n_distinct(site_data$site_name)) %do% {
  site <- list()
  site$name <- site_data$site_name[i]
  site$locality <- site_data$locality[i]
  site$land_use <- site_data$land_use[i]
  site$fragment <- site_data$fragment[i]
  site$fragment_name <- site_data$fragment_name[i]
  site$plant_data <- plant_data %>% 
    filter(site_name == site_data$site_name[i]) %>%
    select(- site_name)
  site$deposition <-  data %>% 
    filter(site_name == site_data$site_name[i]) %>%
    select(- site_name)
  site$study <- "marrero"
  site$sampling <- list(from = as.Date("2010-12-01"), 
                        to = as.Date("2011-03-01"))
  site
}


# VISITATION (QUANT) --------------------------------------------------------------

# clean Hugo's visitation data

# # testing defaults
vis_file <- "./data/datasets/raw/marrero2016/visitation.csv"
# dep_file <- "data/datasets/processed/deposition/marrero.rds"
# output_file <- "./data/datasets/processed/visitation/marrero.rds"

site_char <- data_dep %>%
  plyr::ldply(function(x){
    data.frame(fragment_name = x$fragment_name, 
               # fragment = x$fragment,
               site_name = x$name)
  }) %>% 
  # standarise site names =/
  mutate(fragment_name = paste(substr(site_name, 4, 5), 
                               fragment_name, 
                               sep = "_"), 
         fragment_name = gsub("alfalfa", "alfa", fragment_name),
         fragment_name = gsub("pastura", "pas", fragment_name),
         fragment_name = gsub("1agr", "A-1", fragment_name),
         fragment_name = gsub("2agr", "A-2", fragment_name))

data <- readr::read_csv(vis_file)
names(data) <- make.names(names(data))

data <- data %>% 
  left_join(site_char, by = c("Site" = "fragment_name")) %>%
  mutate(date = paste(1, date), 
         date = as.Date(date, format = "%d %B %Y"))

plant_data <- data %>%
  group_by(Plant.observed) %>%
  summarise(plant_family = unique(Plant.family)) %>% 
  group_by() %>%
  rename(plant_name = Plant.observed)

plant_abundance <- data %>%
  group_by(site_name, date, Plant.observed) %>%
  summarise(n_flower_units = round(mean(flower.units.observed.per.plant.species))) %>% 
  rename(plant_name = Plant.observed) %>%
  group_by()

standardize_animal_name <- . %>%
  # standarise animal names for ease
  mutate(animal_name = stringr::str_replace(animal_name, stringr::regex('\\s(sp)'), ' sp.'), 
         animal_name = if_else(stringr::str_detect(animal_name, stringr::regex('([0-9]+)')),
                               stringr::str_replace(animal_name, stringr::regex('([0-9]+)'), function(x) paste0(' ', x)), 
                               animal_name),
         animal_name = stringr::str_replace_all(animal_name, stringr::regex('\\.+'), '.'), 
         animal_name = stringr::str_replace_all(animal_name, stringr::regex('\\s+'), ' '),
         animal_name = stringr::str_replace(animal_name, stringr::regex('[^(sp)]\\.$'), ''),
         animal_name = trimws(animal_name)) 

animal_data <- data %>%
  # remove records of no visits
  filter(Flower.visitor.ID != "No visits", 
         Visitor_type != "No visits") %>%
  # correct mistake for Astylus quadrilineatus
  mutate(Visitor_type = if_else(Flower.visitor.ID == "Astylus quadrilineatus", 
                                "beetle", 
                                Visitor_type)) %>%
  group_by(Flower.visitor.ID) %>%
  summarise(animal_family = unique(Flower.visitor_family), 
            animal_order = unique(Flower.visitor_order), 
            animal_type = unique(Visitor_type)) %>%
  mutate(animal_type = tolower(animal_type)) %>%
  group_by() %>%
  rename(animal_name = Flower.visitor.ID) %>%
  standardize_animal_name()



interaction_data <- data %>%
  filter(Visitation.frequency != 0) %>%
  rename(plant_name = Plant.observed, 
         animal_name = Flower.visitor.ID, 
         n_visits = Visitation.frequency) %>%
  select(site_name, date, plant_name, animal_name, n_visits) %>%
  standardize_animal_name()

data_vis <- foreach(i=1:n_distinct(site_char$site_name)) %do% {
  site <- list()
  site$name <- as.character(site_char$site_name[i])
  site$area_sampled <- data %>%
    filter(site_name == site_char$site_name[i]) %$%
    total.area_sampled_m2 %>% unique()
  site$fragment_name <- site_char$fragment_name[i]
  dates <- data %>% 
    filter(site_name == site_char$site_name[i]) %$%
    date %>% range()
  site$sampling <- list(from = dates[1], 
                        to = dates[2])
  site$plant_data <- plant_data 
  site$plant_abundance <- plant_abundance %>% 
    filter(site_name == site_char$site_name[i]) %>%
    select(- site_name)
  site$animal_data <- animal_data 
  site$visitation <-  interaction_data %>% 
    filter(site_name == site_char$site_name[i]) %>%
    select(- site_name)
  site$study <- "marrero"
  site
}


# ARMONIZE DEPOSITION AND VISITATION --------------------------------------

# change names from to to in all columns specified in affected_col
change_names <- function(df, affected_col, from, to){
  for(i in 1:length(affected_col)){
    if(affected_col[i] %in% names(df)){
      class(df) <- "data.frame"
      # message("evaluated column is ", affected_col[i])
      df[, affected_col[i]] <- as.character(df[, affected_col[i]])
      if(any(stringr::str_detect(df[, affected_col[i]], from))) {
        # message("found instance of change")
      }
      df[, affected_col[i]] <- stringr::str_replace(df[, affected_col[i]], from, to)
    }
  }
  tibble::as.tibble(df)
}

# change names in data_frames arranged in a list
change_names_site <- function(li, affected_col, from, to){
  for(i in 1:length(li)){
    # message("evaluating site ", li$name, ", field ", names(li)[i])
    if(any(affected_col %in% names(li[[i]]))){
      li[[i]] <- change_names(li[[i]], affected_col, from, to)
    }
  }
  li
}

# Correct species names
# Bacharis sp: En SC y en LC es Baccharis pingraea y en AN  Baccharis ulicina.
data_dep <- plyr::llply(data_dep, function(x){
  if(x$locality %in% c("SC", "LC")){
    change_names_site(x, 
                      affected_col = c("plant_name", "donor", "recipient"),
                      from = "Baccharis sp.", 
                      to = "Baccharis pingraea")
  } else if(x$locality %in% c("AN")){
    change_names_site(x, 
                      affected_col = c("plant_name", "donor", "recipient"),
                      from = "Baccharis sp.", 
                      to = "Baccharis ulicina")
  }
})

data_dep <- plyr::llply(data_dep, change_names_site, 
            affected_col = c("plant_name", "donor", "recipient"),
            from = "Adesmia sp", 
            to = "Adesmia bicolor")

data_dep <- plyr::llply(data_dep, change_names_site, 
                        affected_col = c("plant_name", "donor", "recipient"),
                        from = "Dipsacus sp.", 
                        to = "Dipsacus sativus")

data_dep <- plyr::llply(data_dep, change_names_site, 
                        affected_col = c("plant_name", "donor", "recipient"),
                        from = "Descurania argentina", 
                        to = "Descurainia argentina")
data_vis <- plyr::llply(data_vis, change_names_site, 
                        affected_col = c("plant_name", "donor", "recipient"),
                        from = "Descuraina argentina", 
                        to = "Descurainia argentina")

data_dep <- plyr::llply(data_dep, change_names_site, 
                        affected_col = c("plant_name", "donor", "recipient"),
                        from = "Glandularia sp.", 
                        to = "Glandularia hookeriana")

data_dep <- plyr::llply(data_dep, change_names_site, 
                        affected_col = c("plant_name", "donor", "recipient"),
                        from = "Thelesperma sp.", 
                        to = "Thelesperma megapotamicum")

data_vis <- plyr::llply(data_vis, change_names_site, 
                        affected_col = c("plant_name", "donor", "recipient"),
                        from = "Oxalis sp", 
                        to = "Oxalis violeta")

data_dep <- plyr::llply(data_dep, change_names_site, 
                        affected_col = c("plant_name", "donor", "recipient"),
                        from = "Nothoscordum euosimum", 
                        to = "Nothoscordum nudicaule")

# VISITATION (QUALI) ------------------------------------------------------------------

qual_vis_file <- "data/datasets/raw/marrero2016/qualitative_visits.csv" 

qua <- readr::read_csv(qual_vis_file)
qua %<>%
  rename(plant_name = plant,
         fragment_name = fragment, 
         animal_name = insect,
         locality = site) %>% 
  mutate(date = paste("1", date),
         date = stringr::str_replace(date, "\\.", " "),
         date = stringr::str_replace(date, "Ene", "Jan"),
         date = stringr::str_replace(date, "Dic", "Dec"),
         date = as.Date(date, format = "%d %b %y"),
         fragment_name = stringr::str_replace(fragment_name, "C-alflafa", "C-alfalfa")) %>%
  # standarise animal names for ease
  standardize_animal_name()

# save all data in a list of unique sites in a sort of deposition-site object.
# each site has a list of properties that describe and relate it to other sites.
# As a minimum it has a name and a deposition table
data_vis_qual <- foreach(i=1:n_distinct(site_data$site_name)) %do% {
  site <- list()
  site$name <- site_data$site_name[i]
  site$locality <- site_data$locality[i]
  site$land_use <- site_data$land_use[i]
  site$fragment <- site_data$fragment[i]
  site$fragment_name <- site_data$fragment_name[i]
  site$visitation <- qua %>% 
    filter(fragment_name == site_data$fragment_name[i]) %>%
    select(- fragment_name)
  site$study <- "marrero"
  site$sampling <- list(from = as.Date("2010-12-01"), 
                        to = as.Date("2011-03-01"))
  site
}

# ARMONISE DEPOSITION AND QUALITATIVE VISITS ------------------------------

data_vis_qual <- plyr::llply(data_vis_qual, change_names_site, 
                        affected_col = c("plant_name", "donor", "recipient"),
                        from = "Descurania argentina", 
                        to = "Descurainia argentina")

data_vis_qual <- plyr::llply(data_vis_qual, change_names_site, 
                        affected_col = c("plant_name", "donor", "recipient"),
                        from = "Dipsacus sp.", 
                        to = "Dipsacus sativus")

data_vis_qual <- plyr::llply(data_vis_qual, change_names_site, 
                        affected_col = c("plant_name", "donor", "recipient"),
                        from = "Nothoscordum euosimum", 
                        to = "Nothoscordum nudicaule")

data_vis_qual <- plyr::llply(data_vis_qual, change_names_site, 
                        affected_col = c("plant_name", "donor", "recipient"),
                        from = "Thelesperma sp.", 
                        to = "Thelesperma megapotamicum")

# ARMONISE VISITS AND QUAL VISITS -----------------------------------------

data_vis_qual <- plyr::llply(data_vis_qual, change_names_site, 
                             affected_col = c("animal_name"),
                             from = "Ligaeidae sp. 1", 
                             to = "Lygaeidae sp. 1")

data_vis_qual <- plyr::llply(data_vis_qual, change_names_site,
                             affected_col = c("animal_name"),
                             from = "Blaesoxipha \\(Acanthodotheca\\) sp. 1",
                             to = "Blaesoxipha (Acanthodotheca) sp.")

data_vis_qual <- plyr::llply(data_vis_qual, change_names_site,
                             affected_col = c("animal_name"),
                             from = "Graehomya auriceps",
                             to = "Graphomya auriceps")

data_vis <- plyr::llply(data_vis, change_names_site,
                             affected_col = c("animal_name"),
                             from = "Exomalopsis sp. 1",
                             to = "Exomalopsis sp.")

vis_animal <- plyr::ldply(data_vis, function(x) x$animal_data) %>%
  distinct()
vis_qual_animal <- plyr::ldply(data_vis_qual, function(x) x$visitation %>% group_by(animal_name) %>% summarise(a=1)) %>%
  distinct()

full_join(vis_animal, vis_qual_animal, by = "animal_name") %>%
  arrange(animal_name) %>% View

vis_animal <- plyr::ldply(data_vis, function(x) x$animal_data) %>%
  arrange(animal_name) %>% View
# qua %>% arrange(date, locality,fragment_name) %>% filter(locality == "LC") %>% View()

dep_plants <- plyr::ldply(data_dep, function(x) x$plant_data) %>%
  distinct()
vis_plants <- plyr::ldply(data_vis_qual, function(x) x$visitation %>% group_by(plant_name) %>% summarise(a=1)) %>%
  distinct()

full_join(dep_plants, vis_plants, by = "plant_name") %>%
  arrange(plant_name) %>% View

saveRDS(data, output_file)






# saveRDS(data, output_file)
