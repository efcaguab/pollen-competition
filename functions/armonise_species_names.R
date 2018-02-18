armonise_species_names <- function(deposition, 
                                   visitation_quant,
                                   visitation_qual,
                                   transfer, 
                                   abundance){
  # Correct plant's species names
  # Bacharis sp: En SC y en LC es Baccharis pingraea y en AN  Baccharis ulicina.
  deposition <- plyr::llply(deposition, function(x){
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
  
  datasets <- list(deposition = deposition, 
       visitation_quant = visitation_quant, 
       visitation_qual = visitation_qual,
       transfer = transfer, 
       abundance = abundance)
  
  datasets %>% 
    # Family to species changes --- not controversial
    change("Vicia sp.", "Vicia linealifolia") %>%
    # Typos
    change("Vicia linialifolia", "Vicia linealifolia") %>%
    change("Salsola Kali", "Salsola kali") %>%
    change("Glicine max", "Glycine max") %>%
    change("Picrocia longifolia", "Picrosia longifolia") %>%
    change("Glandularia perubiana", "Glandularia peruviana") %>%
    change("Nothoscordum nudicaule", "Nothoscordum euosimum") %>%
    change("Glandularia sp.", "Glandularia hookeriana") %>%
    change("Crepis cetosa", "Crepis setosa") %>%
    change("Grupo Crepis-Hypochoeris", "Crepis setosa") %>%
    # different names used in different locations. 
    change("Verbenaceae", "Verbena sp.") %>%
    # there are two verbena species only overlap in the visitation_quali
    # dataset. Verbena intermedia seems to be better represented in other
    # datasets for the same locations so just gonna assign it there
    # plyr::llply(visitation_qual, function(x) filter(x$visitation, grepl("Verbena", plant_name)))
    # plyr::llply(deposition, function(x) filter(x$deposition, grepl("Verbena", donor)))
    # plyr::llply(transfer, function(x) filter(x$transfer, grepl("Verbena", plant_name)))
    change("Verbena sp.", "Verbena intermedia") %>%
    # Viola sp. was used in the abundance dataset
    change("Viola sp.","Viola arvensis") %>%
    # there is only one Turnera species so seems fine
    change("Turnera sp.", "Turnera sidioides") %>%
    # there is only one species of Thelesperma so seems fine too
    change("Thelesperma sp.", "Thelesperma megapotamicum") %>%
    # there is only one Solidago species 
    change("Solidago sp.", "Solidago chilensis") %>%
    # there is only one species of this too
    change("Taraxacum sp.", "Taraxacum oficinale") %>%
    change("Portulaca", "Portulaca oleracea") %>%
    change("Noticastrum sp. cf. Acuminatum", "Noticastrum acuminatum") %>%
    change("Rubiaceae", "Mitracarpus sp.") %>%
    ### POTENTIALLY POLLEMIC
    # Inconlclusive about trifolium
    # plyr::llply(visitation_qual, function(x) filter(x$visitation, grepl("Trifolium", plant_name)))
    # plyr::llply(deposition, function(x) filter(x$deposition, grepl("Trifolium", donor)))
    # plyr::llply(transfer, function(x) filter(x$transfer, grepl("Trifolium", plant_name)))
    # plyr::llply(abundance, function(x) filter(x$abundance, grepl("Trifolium", plant_name)))
    # Changing solanum sp to solanum sisymbriifolium based on viistation qualitative information, shady
    # plyr::llply(visitation_qual, function(x) filter(x$visitation, grepl("Solanum", plant_name)))
    # plyr::llply(deposition, function(x) filter(x$deposition, grepl("Solanum", donor)))
    # plyr::llply(transfer, function(x) filter(x$transfer, grepl("Solanum", plant_name)))
    # plyr::llply(abundance, function(x) filter(x$abundance, grepl("Solanum", plant_name)))
    change("Solanum sp.", "Solanum sisymbriifolium") %>%
    change("Senecio sp.", "Senecio pulcher") %>%
    change("Physalis sp.", "Physalis viscosa") %>%
    change("Oxalis sp.", "Oxalis violeta") %>%
    change("Grupo Baccharis sp.p.", "Baccharis pingraea") %>%
    change("Grupo Carduus-Cirsium", "Carduus acanthoides")
  
}

change <- function(x, from, to, affected_col = c("plant_name", "donor", "recipient")){
  # function to change in each dataset
  change_in_dataset <- function(y, from, to, affected_col){
    plyr::llply(y, change_names_site, 
                affected_col = affected_col, 
                from = from, 
                to = to)
  }
  # make the changes across
  plyr::llply(x, change_in_dataset, from = from, to = to, affected_col = affected_col)
}

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