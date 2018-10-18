
# NAME MATCHING CHECKING --------------------------------------------------

loadd(armonised_data)

# plant_names

dep <- plyr::ldply(armonised_data$deposition, function(x) x$plant_data) %>% 
  distinct() %>%
  select(plant_name) %>%
  mutate(deposition = 'deposition')

vil <- plyr::ldply(armonised_data$visitation_qual, function(x) x$visitation) %>% 
  group_by(plant_name) %>%
  summarise(n = 1) %>%
  distinct() %>%
  select(plant_name) %>%
  mutate(visitation_quali = 'visitation_qual')

vin <- plyr::ldply(armonised_data$visitation_quant, function(x) x$visitation) %>% 
  group_by(plant_name) %>%
  summarise(n = 1) %>%
  distinct() %>%
  select(plant_name) %>%
  mutate(visitation_quant = 'visitation_quant')

tra <- plyr::ldply(armonised_data$transfer, function(x) x$transfer) %>% 
  group_by(plant_name) %>%
  summarise(n = 1) %>%
  distinct() %>%
  select(plant_name) %>%
  mutate(transfer = 'transfer')

abu <- plyr::ldply(armonised_data$abundance, function(x) x$abundance) %>%
  group_by(plant_name) %>%
  summarise(n = 1) %>%
  distinct() %>%
  select(plant_name) %>%
  mutate(abundance = 'abundance')

all_plants <- dep %>%
  full_join(vin) %>%
  full_join(vil) %>%
  full_join(tra) %>%
  full_join(abu) %>%
  arrange(plant_name)

View(all_plants)

inspect_family <- function(x, file){
  fam <- read.csv(file) %>%
    mutate(plant_name = paste(Genus, Species)) 
  gen <- fam$Genus %>%
    unique() %>%
    plyr::ldply(function(y) filter(x, grepl(as.character(y), plant_name)))
  spp <- filter(x, plant_name %in% fam$plant_name)
  rbind(gen, spp) %>% distinct()
}

inspect_family(all_plants, file = "./data/raw/poaceae.csv") # none ...
inspect_family(all_plants, file = './data/raw/compositae.csv') # too many... useless
inspect_family(all_plants, file = './data/raw/malvaceae.csv') # none with deposition...
inspect_family(all_plants, file = './data/raw/rubiaceae.csv') # this would work... but no deposition 


# Imputed pollen 17-08-18 -------------------------------------------------

drake::loadd(imputed_pollen)

imputed_pollen %>%
  tidyr::spread(scale, poc) %>% View
  ggplot(aes(community, imputed)) +
  geom_point() +
  geom_smooth(method = "lm")

# Checking match between transfer and visitation network ------------------

drake::loadd(tra_frame, vis_frame)

animals_transfer <- tra_frame %>%
  dplyr::select(site_name, animal_name) %>%
  dplyr::distinct() %>%
  dplyr::mutate(in_tra = 1) %>%
  dplyr::group_by(site_name)

animals_visitation <- vis_frame %>%
  dplyr::select(site_name, animal_name) %>%
  dplyr::distinct() %>%
  dplyr::mutate(in_vis = 1) %>%
  dplyr::group_by(site_name)

# there seems to be a relatively good agreement, all species with transfer also have registered visits, although not all insects with visits have been registered for transfer
dplyr::anti_join(animals_transfer, animals_visitation) %T>% View %$% 
  unique(site_name)

vis_for_tra <- vis_frame %>%
  dplyr::mutate(n_visits = dplyr::if_else(survey_type == "quantitative", n_visits, 1)) %>%
  dplyr::group_by(site_name, plant_name, animal_name) %>%
  dplyr::summarise(n_visits = sum(n_visits))

tra_frame %>%
  dplyr::group_by(site_name, plant_name, animal_name) %>%
  dplyr::summarise(grain = mean(grain)) %>% 
  dplyr::left_join(vis_for_tra) %>% View
  