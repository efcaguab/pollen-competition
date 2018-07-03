
#'Read & clean plant traits data
#'
#'Reads the plant trait data and manually changes the names of species so that
#'they all match with the species names in the armonised data names. They might
#'be incorrect but at least they match...
#'
#'@param file file with the csv raw plant trait data
#'
#'@return a data frame
#'
read_plant_traits <- function(traits_file) {
  suppressMessages(readr::read_csv(traits_file, skip = 1)) %>% 
    dplyr::rename(plant_name = `plant species`,
                  growth_form = `habit (tree/shrub/herb/climber/parasite, etc)`, 
                  native = `native in study region (yes/no)`,
                  longevity = `life cycle (annual/perennial)`, 
                  flower_color = `flower colour (as seen by humans)`) %>%
    dplyr::select(plant_name, growth_form, native, longevity, flower_color) %>%
    change_names("plant_name", "Adesmia sp", "Adesmia sp.") %>%
    change_names("plant_name", "Anagallis arvensis", "Anagalis sp.") %>%
    change_names("plant_name", "Cuscuta sp", "Cuscuta sp.") %>%
    change_names("plant_name", "Descuraina argentina", "Descurania argentina")  %>%
    change_names("plant_name", "Dipsacus sativus", "Dipsacus sp.") %>%
    change_names("plant_name", "Gamochaeta sp", "Gamochaeta sp.") %>%
    change_names("plant_name", "Geranium sp", "Geranium sp.") %>%
    change_names("plant_name", "Hydrocotyle sp", "Hydrocotyle sp.") %>%
    change_names("plant_name", "Hypochaeris radicata", "Hypochoeris radicata") %>%
    change_names("plant_name", "Hypochaeris pampasica", "Hypochoeris pampasica") %>%
    change_names("plant_name", "Lythrum hyssopifolia", "Lythrum sp.") %>%
    change_names("plant_name", "Nothoscordum nudicaule", "Nothoscordum euosimum") %>%
    change_names("plant_name", "Oenothera sp", "Oenothera sp.") %>%
    change_names("plant_name", "Oxalis sp", "Oxalis amarillo") %>%
    change_names("plant_name", "Oxalis amarillo cf.  O. articulata", "Oxalis violeta") %>%
    change_names("plant_name", "Panphalea bupleurifolia", "Pamphalea bupleurifolia") %>%
    change_names("plant_name", "Polygala linoides", "Poligala linoides") %>%
    change_names("plant_name", "Portulaca oleracea", "Portulaca oleracea oleracea") %>%
    change_names("plant_name", "Salsola Kali", "Salsola kali") %>%
    change_names("plant_name", "Scutellaria racemosa", "Scutelaria racemosa") %>%
    change_names("plant_name", "Sonchus sp", "Sonchus sp.") %>%
    change_names("plant_name", "Vicia linearifolia", "Vicia linealifolia") %>%
    change_names("plant_name", "Hydrocotyle sp.", "Hydrocotile sp.")
}

