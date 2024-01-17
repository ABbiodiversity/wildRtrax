#' General convenience functions
#'
#' @description Takes input latitude and longitudes and computes the distances between each set of valid points
#'
#' @param input_from_tibble Use a tibble constructed with a distinct list of location names, latitude and longitude
#' @param input_from_file Use a file downloaded from either an organization or project
#'
#' @import dplyr tibble tidyr sf
#' @export
#'
#' @examples
#' \dontrun{
#' df <- wt_location_distances(input = my_location_tibble, input_from_file)
#' }
#'
#' @return A three-column tibble with the distances between each location

wt_location_distances <- function(input_from_tibble = NULL, input_from_file = NULL) {

  if (is.null(input_from_tibble) & is.null(input_from_file)) {
    stop(
      "Please supply either a tibble or a path to the location list.",
      call. = TRUE
    )
  } else if (!is.null(input_from_tibble) & !is.null(input_from_file)) {
    stop("Please only supply one of tibble or file.", call. = TRUE)
  }

  if (is.null(input_from_file)) {
    inp <- input_from_tibble
  } else
    inp <- readr::read_csv(input_from_file)

  l <- nrow(inp)

  locs <- inp %>%
    dplyr::filter(!is.na(latitude) | !is.na(longitude))

  m <- nrow(locs)

  n <- m - l

  if (n > 0) {
    message(n, 'X rows were skipped as they did not contain a latitude or longitude value.')
  } else {
    message('All rows have a latitude and longitude! Creating the matrix...')
  }

  locs <- locs %>%
    dplyr::select(location, latitude, longitude) %>%
    dplyr::distinct() %>%
    sf::st_as_sf(., coords = c("longitude","latitude"), crs = 4326) %>%
    dplyr::select(location, geometry) %>%
    dplyr::mutate(id = row_number())

  distances <- sf::st_distance(locs, locs)

  location_ids <- locs %>%
    tibble::as_tibble() %>%
    dplyr::select(location, id) %>%
    dplyr::relocate(id)

  final_distances <- distances %>%
    tibble::as_tibble() %>%
    tibble::rownames_to_column(var = "location_from") %>%
    tidyr::pivot_longer(cols = -location_from, names_to = "distance_to", values_to = "distance") %>%
    dplyr::mutate(distance_to = str_replace(distance_to, "V","")) %>%
    dplyr::mutate_at(vars(location_from, distance, distance_to), as.numeric) %>%
    dplyr::filter(!distance == 0) %>%
    dplyr::left_join(., location_ids, by = c("location_from" = "id")) %>%
    dplyr::left_join(., location_ids, by = c("distance_to" = "id")) %>%
    dplyr::select(location.x, location.y, distance) %>%
    dplyr::rename("location_from" = 1) %>%
    dplyr::rename("distance_to" = 2) %>%
    dplyr::select(location_from, distance_to, distance)

  return(final_distances)

}

#' Filter species from a report
#'
#' @description This function filters the species provided in WildTrax reports to only the groups of interest. The groups available for filtering are mammal, bird, amphibian, abiotic, insect, and unknown. Zero-filling functionality is available to ensure all surveys are retained in the dataset if no observations of the group of interest are available.
#'
#' @param data WildTrax main report or tag report from the `wt_download_report()` function.
#' @param remove Character; groups to filter from the report ("mammal", "bird", "amphibian", "abiotic", "insect", "unknown"). Defaults to retaining bird group only.
#' @param zerofill Logical; indicates if zerofilling should be completed. If TRUE, unique surveys with no observations after filtering are added to the dataset with "NONE" as the value for species_code and/or species_common_name. If FALSE, only surveys with observations of the retained groups are returned. Default is TRUE.
#' @param sensor Character; can be one of "ARU" or "PC"
#'
#' @import dplyr
#' @export
#'
#' @examples
#' \dontrun{
#' dat.tidy <- wt_tidy_species(dat,
#' remove=c("mammal", "amphibian", "abiotic", "insect", "unknown"),
#' zerofill = TRUE)
#' }
#' @return A dataframe identical to input with observations of the specified groups removed.

wt_tidy_species <- function(data,
                            remove = c("mammal", "amphibian", "abiotic", "insect", "unknown"),
                            zerofill = TRUE,
                            sensor = c("ARU","PC")){

  #Rename fields if PC
  if(sensor=="PC"){
    data <- data %>%
      rename(task_id=survey_id,
             recording_date_time = survey_date)
  }

  #Convert to the sql database labels for species class
  remove <- dplyr::case_when(remove=="mammal" ~ "MAMMALIA",
                      remove=="amphibian" ~ "AMPHIBIA",
                      remove=="abiotic" ~ "ABIOTIC",
                      remove=="insect" ~ "INSECTA",
                      remove=="bird" ~ "AVES",
                      !is.na(remove) ~ remove)

  .species <- wt_get_species()

  #Get the species codes for what you want to filter out
  species.remove <- .species %>%
    dplyr::filter(species_class %in% remove)

  #Add the unknowns if requested
  if("unknown" %in% remove){
    species.remove <- .species %>%
      dplyr::filter(str_sub(species_common_name, 1, 12)=="Unidentified") %>%
      rbind(species.remove)
  }

  #Remove those codes from the data
  filtered <- dplyr::filter(data, !species_code %in% species.remove$species_code)

  #if you don't need nones, remove other NONEs & return the filtered object
  if(zerofill==FALSE){

    filtered.sp <- dplyr::filter(filtered, species_code!="NONE")

    #Translate point count field names back
    if(sensor=="PC"){
      filtered.sp <- filtered.sp %>%
        rename(survey_id=task_id,
               survey_date = recording_date_time)
    }

    return(filtered.sp)
  }

  #if you do need nones, add them
  if(zerofill==TRUE){

    #first identify the unique visits (replace this with task_id in the future)
    visit <- data %>%
      dplyr::select(organization, project_id, location_id, recording_date_time, task_id) %>%
      dplyr::distinct()

    #see if there are any that have been removed
    none <- suppressMessages(anti_join(visit, filtered)) %>%
      dplyr::mutate(species_code = "NONE",
             species_common_name = "NONE")

    #add to the filtered data
    filtered.none <- suppressMessages(full_join(filtered, none)) %>%
      dplyr::arrange(organization, project_id, location, recording_date_time)

    #Translate point count field names back
    if(sensor=="PC"){
      filtered.none <- filtered.none %>%
        rename(survey_id=task_id,
               survey_date = recording_date_time)
    }

    #return the filtered object with nones added
    return(filtered.none)

  }

}

#' Replace 'TMTT' abundance with model-predicted values
#'
#' @description This function uses a lookup table of model-predicted values to replace 'TMTT' entries in listener-processed ARU data from WildTrax. The model-predicted values were produced using estimated abundances for 'TMTT' entries in mixed effects model with a Poisson distribution and random effects for species and observer.
#'
#' @param data Dataframe of WildTrax observations, for example the summary report.
#' @param calc Character; method to convert model predictions to integer ("round", "ceiling", or "floor"). See `?round()` for details.
#' @import dplyr
#' @export
#'
#' @examples
#' \dontrun{
#' dat.tmtt <- wt_replace_tmtt(dat, calc="round")
#' }
#' @return A dataframe identical to input with 'TMTT' entries in the abundance column replaced by integer values.

wt_replace_tmtt <- function(data, calc="round"){

  #check if it's ARU data
  if(!"recording_date_time" %in% colnames(data)){
    stop("The `wt_replace_tmmtt` function only works on data from the ARU sensor")
  }

  #load tmtt lookup table
  .tmtt <- readRDS(system.file("extdata", "tmtt_predictions.rds", package="wildRtrax"))

  #wrangle to tmtts only
  dat.tmtt <- data %>%
    dplyr::filter(individual_count=="TMTT")

  #replace values with random selection from bootstraps
  if(nrow(dat.tmtt) > 0){
    dat.abun <- dat.tmtt %>%
      mutate(species_code = ifelse(species_code %in% .tmtt$species_code, species_code, "species"),
             observer_id = as.integer(ifelse(observer_id %in% .tmtt$observer_id, observer_id, 0))) %>%
    data.frame() %>%
      inner_join(.tmtt %>% select(species_code, observer_id, pred), by=c("species_code", "observer_id")) %>%
      mutate(individual_count = case_when(calc == "round" ~ round(pred),
                                          calc == "ceiling" ~ ceiling(pred),
                                          calc == "floor" ~ floor(pred),
                                          TRUE ~ NA_real_)) %>%
      select(-pred)
  } else { dat.abun <- dat.tmtt }

  #join back to data
  out <- data %>%
    dplyr::filter(individual_count!="TMTT") %>%
    rbind(., dat.abun)

  #return the unmarked object
  return(out)

  #remove the lookup table
  rm(.tmtt)

}

#' Convert to a wide survey by species dataframe
#'
#' @description This function converts a long-formatted report into a wide survey by species dataframe of abundance values. This function is best preceded by the`wt_tidy_species` and `wt_replace_tmtt` functions  to ensure 'TMTT' and amphibian calling index values are not converted to zeros.
#'
#' @param data WildTrax main report or tag report from the `wt_download_report()` function.
#' @param sound Character; vocalization type(s) to retain ("all", "Song", "Call", "Non-vocal"). Can be used to remove certain types of detections. Defaults to "all" (i.e., no filtering). Note this functionality is only available for the ARU sensor.
#' @param sensor Character; can be one of "ARU" or "PC"
#'
#' @import dplyr
#' @export
#'
#' @examples
#' \dontrun{
#' dat.tidy <- wt_tidy_species(dat)
#' dat.tmtt <- wt_replace_tmtt(dat.tidy)
#' dat.wide <- wt_make_wide(dat.tmtt, sound="all")
#' }
#' @return A dataframe identical to input with observations of the specified groups removed.

wt_make_wide <- function(data, sound="all", sensor="ARU"){

  #Steps for ARU data
  if(sensor=="ARU"){

    #Filter to first detection per individual
    summed <- data %>%
      dplyr::group_by(organization, project_id, location, recording_date_time, task_method, aru_task_status, observer_id, species_code, species_common_name, individual_order) %>%
      dplyr::mutate(first = max(detection_time)) %>%
      dplyr::ungroup() %>%
      dplyr::filter(detection_time==first)

    #Remove undesired sound types
    if(!"all" %in% sound){
      sound <- str_to_title(sound)
      summed <- dplyr::filter(summed, vocalization %in% sound)
    }

    #Make it wide
    wide <- summed %>%
      dplyr::mutate(individual_count = case_when(grepl("^C",  individual_count) ~ NA_character_, TRUE ~ individual_count) %>% as.numeric()) %>%
      dplyr::filter(!is.na(individual_count)) %>% # Filter out things that aren't "TMTT" species. Fix for later.
      tidyr::pivot_wider(id_cols = organization:task_method,
                  names_from = "species_code",
                  values_from = "individual_count",
                  values_fn = sum,
                  values_fill = 0,
                  names_sort = TRUE)

  }

  #Steps for point count data
  if(sensor=="PC"){

    #Make it wide and return field names to point count format
    wide <- data %>%
      dplyr::mutate(individual_count = as.numeric(individual_count)) %>%
      dplyr::filter(!is.na(individual_count)) %>% # Filter out things that aren't "TMTT" species. Fix for later.
      tidyr::pivot_wider(id_cols = organization:survey_duration_method,
                  names_from = "species_code",
                  values_from = "individual_count",
                  values_fn = sum,
                  values_fill = 0,
                  names_sort = TRUE)

  }

  return(wide)

}

#' Format WildTrax report for occupancy modelling
#'
#' @description This function formats the summary report from the `wt_download_report()` function into an unmarked object for occupancy modelling. The current version only includes formatting for the ARU sensor and for single species single season models.
#'
#' @param data Summary report of WildTrax observations from the `wt_download_report()` function. Currently only functioning for the ARU sensor.
#' @param species Character; four-letter alpha code for the species desired for occupancy modelling.
#' @param siteCovs Optional dataframe of site covariates. Must contain a column with the same values as the location field in the data, with one row per unique value of location (i.e., one row per site).
#' @param sensor Character; can be one of "ARU" or "PC"
#'
#' @import dplyr lubridate unmarked
#' @export
#'
#' @examples
#' \dontrun{
#' dat.occu <- wt_format_occupancy(dat, species="CONI", siteCovs=NULL)
#' mod <- occu(~ 1 ~ 1, dat.occu)
#' }
#' @return An object of class unmarkedFrameOccu. See `?unmarked::unmarkedFrameOccu` for details.

wt_format_occupancy <- function(data,
                                species,
                                siteCovs=NULL,
                                sensor="ARU"){

  #Rename fields if PC
  if(sensor=="PC"){
    data <- data %>%
      rename(task_id=survey_id,
             recording_date_time = survey_date,
             observer_id = observer)
  }

  #Wrangle observations and observation covariates for the species of interest
  visits <- data %>%
    dplyr::filter(species_code==species) %>%
    dplyr::select(location, recording_date_time) %>%
    dplyr::distinct() %>%
    dplyr::mutate(occur=1) %>%
    dplyr::right_join(data %>%
                 dplyr::select(location, recording_date_time, observer_id, task_method) %>%
                 dplyr::distinct(),
               by=c("location", "recording_date_time")) %>%
    dplyr::mutate(occur = ifelse(is.na(occur), 0, 1),
           doy = yday(recording_date_time),
           hr = as.numeric(hour(recording_date_time) + minute(recording_date_time)/60)) %>%
    dplyr::group_by(location) %>%
    dplyr::arrange(recording_date_time) %>%
    dplyr::mutate(visit = row_number()) %>%
    dplyr::ungroup()

  #Create location X recording dataframe of observations (1 for detected, 0 for undetected)
  y <- visits %>%
    dplyr::select(location, visit, occur) %>%
    tidyr::pivot_wider(id_cols = location, names_from = visit, values_from = occur) %>%
    dplyr::arrange(location) %>%
    dplyr::select(-location) %>%
    data.frame()

  #Create location X recording dataframes for observation covariates (doy = day of year, hr = hour of day, method = processing method, observer = observer ID)
  doy <- visits %>%
    dplyr::select(location, visit, doy) %>%
    tidyr::pivot_wider(id_cols = location, names_from = visit, values_from = doy) %>%
    dplyr::arrange(location) %>%
    dplyr::select(-location) %>%
    data.frame()

  doy2 <- visits %>%
    dplyr::mutate(doy2 = doy^2) %>%
    dplyr::select(location, visit, doy2) %>%
    tidyr::pivot_wider(id_cols = location, names_from = visit, values_from = doy2) %>%
    dplyr::arrange(location) %>%
    dplyr::select(-location) %>%
    data.frame()

  hr <- visits %>%
    dplyr::select(location, visit, hr) %>%
    tidyr::pivot_wider(id_cols = location, names_from = visit, values_from = hr) %>%
    dplyr::arrange(location) %>%
    dplyr::select(-location) %>%
    data.frame()

  hr2 <- visits %>%
    dplyr::mutate(hr2 = hr^2) %>%
    dplyr::select(location, visit, hr2) %>%
    tidyr::pivot_wider(id_cols = location, names_from = visit, values_from = hr2) %>%
    dplyr::arrange(location) %>%
    dplyr::select(-location) %>%
    data.frame()

  method <- visits %>%
    dplyr::select(location, visit, task_method) %>%
    dplyr::mutate(task_method = as.factor(task_method)) %>%
    tidyr::pivot_wider(id_cols = location, names_from = visit, values_from = task_method) %>%
    dplyr::arrange(location) %>%
    dplyr::select(-location) %>%
    data.frame()

  observer <- visits %>%
    dplyr::select(location, visit, observer_id) %>%
    dplyr::mutate(observer = as.factor(observer_id)) %>%
    tidyr::pivot_wider(id_cols = location, names_from = visit, values_from = observer) %>%
    dplyr::arrange(location) %>%
    dplyr::select(-location) %>%
    data.frame()

  #Create a list of the observation covariates
  obsCovs <- list(doy=doy, doy2=doy2, hr=hr, hr2=hr2, observer=observer)

  #Order site covs dataframe if one is provided
  if(!is.null(siteCovs)){

    #Check length of siteCovs object, remove if incorrect
    locs <- length(unique(data$location))

    if(nrow(siteCovs)!=locs){
      siteCovs <- NULL
      warning('Length of siteCovs dataframe does not match observation data')
    }

    else{
      #Arrange by location so that matches the location X recording dataframes
      siteCovs <- siteCovs %>%
        arrange(location)
    }
  }

  #Put together as an unmarked object for single species occupancy models
  options(warn=-1)
  if(is.null(siteCovs)){
    umf <- unmarked::unmarkedFrameOccu(y=y, siteCovs=NULL, obsCovs=obsCovs)
  } else {
    umf <- unmarked::unmarkedFrameOccu(y=y, siteCovs=siteCovs, obsCovs=obsCovs)
  }
  options(warn=0)

  #return the unmarked object
  return(umf)

}

#' Get QPAD offsets
#'
#' @description This function calculates statistical offsets that account for survey-specific and species-specific variation in availability for detection and perceptibility of birds. This function requires download of the `QPAD` R package and should be used on the output of the `wt_format_wide` function
#'
#' @param data Dataframe output from the `wt_make_wide()` function.
#' @param species Character; species for offset calculation. Can be a list of 4-letter AOU codes (e.g., c("TEWA", "OSFL", "OVEN")) or "all" to calculate offsets for every species in the input dataframe for which offsets are available. Defaults to "all".
#' @param version Numeric; version of QPAD offsets to use (2, or 3). Defaults to 3.
#' @param together Logical; whether or not offsets should be bound to the input dataframe or returned as a separate object.
#' @param sensor Character; can be one of "ARU" or "PC"
#'
#' @references Solymos et al. 2013. Calibrating indices of avian density from non-standardized survey data: making the most of a messy situation. Methods in Ecology and Evolution, 4, 1047-1058.
#'
#' @import QPAD dplyr
#' @export
#'
#' @examples
#' \dontrun{
#' devtools::install_github("borealbirds/QPAD")
#'
#' dat.clean <- wt_tidy_species(dat)
#' dat.tmtt <- wt_replace_tmtt(dat.clean)
#' dat.wide <- wt_make_wide(dat.tmtt, sound="all")
#' dat.qpad <- wt_qpad_offsets(dat.wide, species="all", version=3, together = TRUE)
#' }
#' @return An object of class unmarkedFrameOccu. See `unmarked::unmarkedFrameOccu` for details.

wt_qpad_offsets <- function(data, species = c("all"), version = 3, together=FALSE, sensor="ARU"){

  #Rename fields if PC
  if(sensor=="PC"){
    data <- data %>%
      rename(task_id=survey_id,
             recording_date_time = survey_date,
             observer_id = observer) %>%
      rowwise() %>%
      mutate(durationMethod = ifelse(str_sub(survey_duration_method, -1, -1)=="+",
                                     str_sub(survey_duration_method, -100, -2),
                                     survey_duration_method),
             chardur = str_locate_all(durationMethod, "-"),
             chardurmax = max(chardur),
             task_duration = as.numeric(str_sub(durationMethod, chardurmax+1, -4))*60,
             chardis = str_locate_all(survey_distance_method, "-"),
             chardismax = max(chardis),
             distance1 = str_sub(survey_distance_method, chardismax+1, -2),
             task_distance = ifelse(distance1 %in% c("AR", "IN"), Inf, as.numeric(distance1))) %>%
      ungroup()
  }

  #Make prediction object
  cat("Extracting covariates for offset calculation. This may take a moment.")
  x <- .make_x(data)

  #Load QPAD estimates
  cat("\nLoading QPAD estimates... ")
  load_BAM_QPAD(version)

  #Make the species list
  if("all" %in% species) spp <- sort(intersect(getBAMspecieslist(), colnames(data))) else spp <- species

  #Set up the offset loop
  cat("\nCalculating offsets...")
  off <- matrix(0, nrow(x), length(spp))
  colnames(off) <- spp

  #Make the offsets
  for (i in 1:length(spp)){
    cat("\n", spp[i])
    o <- .make_off(spp[i], x)
    off[,i] <- o$offset
  }

  #Return output as dataframe if separate output requested
  if(together==FALSE){
    return(data.frame(off))
  }

  #Put together if requested
  if(together==TRUE){
    out <- cbind(data,
                 data.frame(off) %>%
                   rename_with(.fn=~paste0(.x, ".off")))

    #Translate point count field names back
    if(sensor=="PC"){
      out <- out %>%
        rename(survey_id=task_id,
               survey_date = recording_date_time,
               observer = observer_id) %>%
        dplyr::select(-durationMethod, -chardur, -chardurmax, -task_duration, -chardis, -chardismax, -distance1, -task_distance)
    }

    return(out)
  }

  cat("\nDone!")

}
