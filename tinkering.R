
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
  dplyr::mutate(n_visits = dplyr::if_else(survey_type == "quantitative", n_visits, 1L)) %>%
  dplyr::group_by(site_name, plant_name, animal_name) %>%
  dplyr::summarise(n_visits = sum(n_visits))

pollen_share_per_int <- tra_frame %>%
  dplyr::group_by(site_name, plant_name, animal_name) %>%
  dplyr::summarise(grain = mean(grain)) %>% 
  dplyr::left_join(vis_for_tra) %>% 
  dplyr::mutate(n_visits = dplyr::if_else(is.na(n_visits), 1L, n_visits)) %>%
  dplyr::group_by(site_name, animal_name) %>%
  dplyr::mutate(n_visits_sp = sum(n_visits, na.rm = T), 
                prop_visits = n_visits/n_visits_sp)

pollen_share_per_int %>% 
  ggplot(aes(x = log(grain * prop_visits))) +
  geom_density()
  
pollen_share_per_int %>%
  dplyr::group_by(site_name, plant_name) %>%
  dplyr::summarise(share = log(sum(prop_visits * grain))) %$%
  hist(share)

# Number of shared pollinators

drake::loadd(vis_frame)

nets <- vis_frame %>%
  split(.$site_name) %>%
  purrr::map(~ xtabs(n_visits~ animal_name + plant_name, data = ., sparse = T)) %>%
  purrr::map(as.matrix) %>% 
  as.matrix() 



find_interacting_species <- function(x, sp_name, type, int_col_name){
  type <- rlang::sym(type)
  # int_col_name <- rlang::enquo(int_col_name)
  x %>%
    dplyr::filter(!!type == sp_name) %>%
    extract2(int_col_name) %>%
    unique()
}

find_interacting_animals <- function(x, plant_name){
  find_interacting_species(x, plant_name, "plant_name", "animal_name")
}

find_n_interacting_plants <- function(x, animal_name){
  find_interacting_species(x, animal_name, "animal_name", "plant_name") %>%
    dplyr::n_distinct()
}

is_animal_shared <- function(x, animal_name) {
  find_n_interacting_plants(x, animal_name) > 1
}

n_shared_animals <- function(x, plant_name){
  find_interacting_animals(x, plant_name) %>%
    purrr::map_lgl(is_animal_shared, x = x) %>%
    sum()
}

n_shared_pol_community <- function(x){
  x %$%
    unique(plant_name) %>% 
    {`names<-`(., .)} %>%
    purrr::map_df(~ dplyr::data_frame(n_shared_pol = n_shared_animals(x, plant_name = .)), .id = "plant_name")
}

n_shared_df <- vis_frame %>%
  dplyr::filter(animal_name != "X") %>%
  split(.$site_name) %>%
  purrr::map_df(n_shared_pol_community, .id = "site_name") 

drake::loadd(degree)
require(ggplot2)

degree %>%
  dplyr::filter(var_trans == "lin", 
                scale == "community") %>%
  dplyr::full_join(n_shared_df) %>% View
  dplyr::mutate(n_shared_pol = dplyr::if_else(is.na(n_shared_pol), 
                                              0L, n_shared_pol)) %>% 
  ggplot(aes(x = kn, y = n_shared_pol)) +
  geom_point(shape = 21, stat = "sum") +
  geom_smooth(method = "lm") +
  scale_x_continuous(trans = "log1p") +
  scale_y_continuous(trans = "log1p")
