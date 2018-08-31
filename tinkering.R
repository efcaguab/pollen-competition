
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
