#' Filter the species list to the groups of interest.
#'
#' @description This function filters the species provided in WildTrax reports to only the groups of interest. The groups available for filtering are mammal, bird, amphibian, abiotic, insect, and unknown. Zero-filling functionality is available to ensure all surveys are retained in the dataset if no observations of the group of interest are available.
#'
#' @param data Dataframe of WildTrax observations, for example the main or tag report.
#' @param remove Character; groups to filter from the report ("mammal", "bird", "amphibian", "abiotic", "insect", "unknown"). Defaults to retaining bird group only.
#' @param zerofill Logical; indicates if zerofilling should be completed. If TRUE, unique surveys with no observations after filtering are added to the dataset with "NONE" as the value for species_code and/or species_common_name. If FALSE, only surveys with observations of the retained groups are returned. Default is TRUE.
#' @import dplyr
#' @export
#'
#' @examples
#' \dontrun{
#' dat.tmtt <- replace_tmtt(dat, remove=c("mammal", "amphibian", "abiotic", "insect", "unknown"), zerofill = TRUE)
#' }
#' @return A dataframe identical to input with observations of the specified groups removed.


wt_tidy_species <- function(dat, remove=c("mammal", "amphibian", "abiotic", "insect", "unknown"), zerofill = TRUE){

  #Convert to the sql database labels for species class
  remove <- case_when(remove=="mammal" ~ "Mammalia",
                      remove=="amphibian" ~ "Amphibia",
                      remove=="abiotic" ~ "Abiotic",
                      remove=="insect" ~ "Insecta",
                      remove=="bird" ~ "Aves",
                      !is.na(remove) ~ remove)

  #Get the sql lookup table
  .species <- read.csv(system.file("lu_species.csv"), package="wildRtrax")

  #Get the species codes for what you want to filter out
  species.remove <- .species %>%
    dplyr::filter(species_class %in% remove)

  #add the unknowns if requested
  if("unknown" %in% remove){
    species.remove <- .species %>%
      dplyr::filter(str_sub(species_common_name, 1, 12)=="Unidentified") %>%
      rbind(species.remove)
  }

  #Remove those codes from the data
  filtered <- dplyr::filter(dat, !species_code %in% species.remove$species_code)

  #if you don't need nones, remove other NONEs & return the filtered object
  if(zerofill==FALSE){

    filtered.sp <- dplyr::filter(filtered, species_code!="NONE")

    return(filtered.sp)
  }

  #if you do need nones, add them
  if(zerofill==TRUE){

    #first identify the unique visits (replace this with task_id in the future)
    visit <- dat %>%
      dplyr::select(-species_code, -species_common_name, -species_class, -individual_appearance_order, -tag_start_s, -vocalization, -abundance, -species_comments, -is_verified) %>%
      unique()

    #see if there are any that have been removed
    none <- suppressMessages(anti_join(visit, filtered)) %>%
      mutate(species_code = "NONE",
             species_common_name = "NONE")

    #add to the filtered data
    filtered.none <- suppressMessages(full_join(filtered, none)) %>%
      arrange(organization, project_name, location, recording_date, tag_start_s, individual_appearance_order)

    #return the filtered object with nones added
    return(filtered.none)

  }

}
