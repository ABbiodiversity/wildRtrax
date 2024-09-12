#' General convenience functions
#'
#' @description Takes input latitude and longitudes and computes the distances between each set of valid points
#'
#' @param input_from_tibble Use a tibble constructed with a distinct list of location names, latitude and longitude
#' @param input_from_file Use a file downloaded from either an organization or project
#'
#' @import dplyr tibble tidyr sf
#' @importFrom readr read_csv
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
#' @param remove Character; groups to filter from the report ("mammal", "bird", "amphibian", "abiotic", "insect", "human", "unknown"). Defaults to retaining bird group only.
#' @param zerofill Logical; indicates if zerofilling should be completed. If TRUE, unique surveys with no observations after filtering are added to the dataset with "NONE" as the value for species_code and/or species_common_name. If FALSE, only surveys with observations of the retained groups are returned. Default is TRUE.
#'
#' @import dplyr
#' @export
#'
#' @examples
#' \dontrun{
#' dat.tidy <- wt_tidy_species(dat, remove=c("mammal", "unknown"), zerofill = T)
#' }
#' @return A dataframe identical to input with observations of the specified groups removed.

wt_tidy_species <- function(data,
                            remove = "",
                            zerofill = TRUE) {

  if (is.null(remove)) {
    stop('Not removing any species')
  }

  if (any(!(remove %in% c("mammal", "bird", "amphibian", "abiotic", "insect", "human", "unknown")))) {
    stop("Select remove options from bird, mammal, amphibian, abiotic, insect, human or unknown.")
  }

  #Rename fields if PC
  if("survey_url" %in% colnames(data)){
    data <- data %>%
      rename(task_id=survey_id,
             recording_date_time = survey_date)
  }

  if('bird' %in% remove){
    message('Note: By removing birds, you will not be able to use wt_qpad_offsets since QPAD offsets are only available for birds.')
  }

  #Convert to the sql database labels for species class
  remove <- dplyr::case_when(remove=="mammal" ~ "MAMMALIA",
                      remove=="amphibian" ~ "AMPHIBIA",
                      remove=="abiotic" ~ "ABIOTIC",
                      remove=="insect" ~ "INSECTA",
                      remove=="bird" ~ "AVES",
                      remove=="human" ~ "HUMAN ACTIVITY",
                      remove=="unknown" ~ "unknown",
                      remove=="" ~ remove)

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
    if("survey_url" %in% colnames(data)){
      filtered.sp <- filtered.sp %>%
        rename(survey_id=task_id,
               survey_date = recording_date_time)
    }

    return(filtered.sp)
  }

  #if you do need nones, add them
  if(zerofill==TRUE){

    #first identify the unique visits (task_id) ensure locations are included for proper join
    visit <- data %>%
      dplyr::select(organization, project_id, location, latitude, longitude, location_id, recording_id, recording_date_time, task_id) %>%
      dplyr::distinct()

    #see if there are any that have been removed
    none <- suppressMessages(anti_join(visit, filtered)) %>%
      dplyr::mutate(species_code = "NONE",
             species_common_name = "NONE",
             species_scientific_name = "NONE")

    #add to the filtered data
    filtered.none <- suppressMessages(full_join(filtered, none)) %>%
      dplyr::arrange(organization, project_id, location, recording_date_time)

    #Translate point count field names back
    if("survey_url" %in% colnames(data)){
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
#'
#' @import dplyr tidyr
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
    stop("The `wt_replace_tmtt` function only works on data from the ARU sensor")
  }

  check_none <- data |>
    select(species_code) |>
    distinct() |>
    pull()

  if (length(check_none) == 1 && check_none == 'NONE') {
    stop('There are no species in this project')
  }

  #load tmtt lookup table
  .tmtt <- readRDS(system.file("extdata", "tmtt_predictions.rds", package="wildrtrax"))

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
#' @description This function converts a long-formatted report into a wide survey by species dataframe of abundance values.
#'
#' @param data WildTrax main report or tag report from the `wt_download_report()` function.
#' @param sound Character; vocalization type(s) to retain ("all", "Song", "Call", "Non-vocal"). Can be used to remove certain types of detections. Defaults to "all" (i.e., no filtering).
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

wt_make_wide <- function(data, sound="all"){

  #Steps for ARU data
  if(!"survey_url" %in% colnames(data)){

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
      dplyr::mutate(individual_count = case_when(grepl("^C",  individual_count) ~ NA_character_, TRUE ~ as.character(individual_count)) %>% as.numeric()) %>%
      dplyr::filter(!is.na(individual_count)) %>% # Filter out things that aren't "TMTT" species. Fix for later.
      tidyr::pivot_wider(id_cols = organization:task_method,
                  names_from = "species_code",
                  values_from = "individual_count",
                  values_fn = sum,
                  values_fill = 0,
                  names_sort = TRUE)

  }

  #Steps for point count data
  if("survey_url" %in% colnames(data)){

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
#'
#' @import dplyr unmarked
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
                                siteCovs=NULL){

  #Rename fields if PC
  if("survey_url" %in% colnames(data)){
    data <- data %>%
      rename(task_id=survey_id,
             recording_date_time = survey_date,
             observer_id = observer,
             task_method = survey_duration_method)
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
#' @description This function calculates statistical offsets that account for survey-specific and species-specific variation in availability for detection and perceptibility of birds. This function requires download of the `QPAD` R package and should be used on the output of the `wt_make_wide()` function
#'
#' @param data Dataframe output from the `wt_make_wide()` function.
#' @param species Character; species for offset calculation. Can be a list of 4-letter AOU codes (e.g., c("TEWA", "OSFL", "OVEN")) or "all" to calculate offsets for every species in the input dataframe for which offsets are available. Defaults to "all".
#' @param version Numeric; version of QPAD offsets to use (2, or 3). Defaults to 3.
#' @param together Logical; whether or not offsets should be bound to the input dataframe or returned as a separate object.
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
#' @return A dataframe containing the QPAD values either by themselves or with the original wide data if `together = TRUE`

wt_qpad_offsets <- function(data, species = c("all"), version = 3, together=FALSE) {

  #Rename fields if PC
  if("survey_url" %in% colnames(data)){
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
    if("survey_url" %in% colnames(data)){
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

#' Intersect locations to add a GRTS ID
#'
#' @description This function intersects location data with the GRTS ID provided by [NABat](https://www.nabatmonitoring.org/)
#'
#' @param data Data containing locations
#' @param group_locations_in_cell Option to provide distinct location names if points are found in the same cell. Sequentially provides a number for each GRTS ID e.g. 3-1, 3-2, etc.
#'
#' @import dplyr sf
#' @importFrom tidyr separate
#' @importFrom readr read_csv
#' @export
#'
#' @examples
#' \dontrun{
#'
#' dat.grts <- wt_download_report(reports = "location")
#' grts.data <- wt_add_grts(dat.grts)
#' }
#' @return A dataframe with the additional GRTS IDs

wt_add_grts <- function(data, group_locations_in_cell = FALSE) {

  if(!all(c("location","latitude","longitude") %in% names(data))){
    stop('Data must contains columns for location, latitude and longitude')
  }

  if(anyNA(data$latitude) | anyNA(data$longitude)){
    stop('Some latitdues and longitudes are missing. Cannot find GRTS cells without a latitude and longitude.')
  }

  if (any(data$latitude < -90 | data$latitude > 90 | data$longitude < -180 | data$longitude > 180)) {
    stop('Some latitudes or longitudes are not within the correct coordinate system.')
  }

  # Filter things down a bit by bbox so the intersection doesn't take too long
  points_sf <- sf::st_as_sf(data, coords = c("longitude", "latitude"), crs = 4326)
  points_bbox <- sf::st_as_sfc(st_bbox(points_sf))
  bbox_sf <- sf::st_sf(geometry = points_bbox)
  # Define bounding boxes for each region
  bbox_canada <- sf::st_set_crs(sf::st_sf(sf::st_as_sfc(sf::st_bbox(c(xmin = -173.179, ymin = 34.43, xmax = -16.35, ymax = 85.17)), crs = 4326)), 4326)
  bbox_alaska <-  sf::st_set_crs(sf::st_sf(sf::st_as_sfc(sf::st_bbox(c(xmin = -179.99863, ymin = 51.214183, xmax = -129.9795, ymax = 71.538800)), crs = 4326)),4326)
  bbox_contig <- sf::st_set_crs(sf::st_sf(sf::st_as_sfc(sf::st_bbox(c(xmin = -127.94485, ymin = 22.91700, xmax = -65.26265, ymax = 51.54421)), crs = 4326)),4326)

  # Initialize an empty list to collect the datasets
  grts_list <- list()

  # Check for intersection
  if (nrow(data) > 0) {
    message('Downloading Canada data from NABAT...')
    grts_list[[length(grts_list) + 1]] <- readr::read_csv('https://code.usgs.gov/fort/nabat/nabatr/-/raw/dffbf6afda4d390dbe4d2bf8c51e854b960a33dd/data/GRTS_coords_Canada.csv', show_col_types = FALSE)
  }

  if (nrow(data) > 0) {
    message('Downloading Alaska data from NABAT...')
    grts_list[[length(grts_list) + 1]] <- readr::read_csv('https://code.usgs.gov/fort/nabat/nabatr/-/raw/dffbf6afda4d390dbe4d2bf8c51e854b960a33dd/data/GRTS_coords_Alaska.csv', show_col_types = FALSE)
  }

  # Check for intersection with contiguous US
  if (nrow(data) > 0) {
    message('Downloading contiguous US data from NABAT...')
    grts_list[[length(grts_list) + 1]] <- readr::read_csv('https://code.usgs.gov/fort/nabat/nabatr/-/raw/dffbf6afda4d390dbe4d2bf8c51e854b960a33dd/data/GRTS_coords_CONUS.csv', show_col_types = FALSE)
  }

  # If any datasets were downloaded, bind them together
  if (length(grts_list) > 0) {
    grts_chosen <- dplyr::bind_rows(grts_list)
  } else {
    stop('No overlaps could be found with this data.')
    grts_chosen <- NULL
  }

  # Optional: Check if grts_chosen is NULL before proceeding
  if (is.null(grts_chosen)) {
    stop('No intersected data to proceed with.')
  }

  # Convert grid cells to sf polygons
  grid_cells_sf <- grts_chosen %>%
    tidyr::separate(lowerleft, into = c("lowerleft_lat", "lowerleft_lon"), sep = ",", convert = TRUE) %>%
    tidyr::separate(upperleft, into = c("upperleft_lat", "upperleft_lon"), sep = ",", convert = TRUE) %>%
    tidyr::separate(upperright, into = c("upperright_lat", "upperright_lon"), sep = ",", convert = TRUE) %>%
    tidyr::separate(lowerright, into = c("lowerright_lat", "lowerright_lon"), sep = ",", convert = TRUE) %>%
    tidyr::separate(center, into = c("center_lat", "center_lon"), sep = ",", convert = TRUE) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      geometry = list(sf::st_polygon(list(matrix(
        c(
          lowerleft_lon, lowerleft_lat,
          upperleft_lon, upperleft_lat,
          upperright_lon, upperright_lat,
          lowerright_lon, lowerright_lat,
          lowerleft_lon, lowerleft_lat
        ),
        ncol = 2,
        byrow = TRUE
      ))))
    ) %>%
    dplyr::ungroup() %>%
    sf::st_as_sf(crs = 4326) %>%
    dplyr::select(GRTS_ID, geometry)

  # If CRS are different, transform bbox_sf to match the CRS of grid_cells_sf
  if (!identical(st_crs(grid_cells_sf), st_crs(bbox_sf))) {
    bbox_sf <- st_transform(sf::st_make_valid(bbox_sf), st_crs(sf::st_make_valid(grid_cells_sf)))
  }

  grid_cells_filtered <- grid_cells_sf %>% suppressWarnings(sf::st_intersection(bbox_sf))

  # Perform spatial join to find which polygon each point falls into
  if(nrow(grid_cells_filtered) == 0){
    stop('There are no grid cells that intersect the points')
  } else {
    result <- suppressWarnings(sf::st_join(points_sf, grid_cells_filtered, join = sf::st_within))
  }

  # Convert back to tibble and select relevant columns
  new_data <- result %>%
    tibble::as_tibble() %>%
    dplyr::relocate(GRTS_ID, .after = location) %>%
    sf::st_drop_geometry() %>%
    dplyr::mutate(longitude = unlist(purrr::map(.$geometry, 1)),
                  latitude = unlist(purrr::map(.$geometry, 2))) %>%
    dplyr::relocate(latitude, .after = GRTS_ID) %>%
    dplyr::relocate(longitude, .after = latitude) %>%
    dplyr::select(-geometry)

  if (group_locations_in_cell == TRUE) {
    new_data2 <- new_data %>%
      dplyr::group_by(GRTS_ID) %>%
      dplyr::mutate(GRTS_suffix = paste0(GRTS_ID, "-", row_number())) %>%
      dplyr::ungroup() %>%
      dplyr::relocate(GRTS_suffix, .after = GRTS_ID)

    return(new_data2)
  } else {
    return(new_data)
  }
}

#' Format data for a specified portal
#'
#' @description This function takes the WildTrax reports and converts them to the desired format
#' `r lifecycle::badge("experimental")`
#'
#' @param input A report containing locations from `wt_download_report()`
#' @param format A format i.e. 'FWMIS'
#'
#' @import dplyr
#' @export
#'
#' @examples
#' \dontrun{
#'
#' dat <- wt_download_report(reports = c("main","visit","equipment")) |>
#' wt_format_data(format = 'FWMIS')
#' }
#' @return A tibble with the formatted report

wt_format_data <- function(input, format = 'FWMIS'){

  # User agent
  u <- getOption("HTTPUserAgent")
  if (is.null(u)) {
    u <- sprintf("R/%s; R (%s)",
                 getRversion(),
                 paste(getRversion(), R.version$platform, R.version$arch, R.version$os))
  }

  # Add wildrtrax version information:
  u <- paste0("wildrtrax ", as.character(packageVersion("wildrtrax")), "; ", u)

  spp_fwmis <- httr::POST(
    httr::modify_url("https://dev-api.wildtrax.ca", path = "/bis/get-species-fwmis-map"),
    accept = "application/json",
    httr::add_headers(Authorization = NULL),
    httr::user_agent(u)
  )

  spps <- httr::content(spp_fwmis)
  spps_tibble <- map_dfr(spps, ~ tibble(species_id = .x$sfw_species_id, sfw_name = .x$sfw_name)) %>%
    inner_join(., wt_get_species() %>% select(species_id, species_common_name), by = ("species_id"))

  org_id = 5
  #org_id <- input %>% select(organization) %>% distinct() %>% pull()

  loceq_payload <- list(
    limit = 2e9,
    orderBy = "deploymentDate",
    orderDirection = "DESC",
    organizationId = org_id,
    page = 1)

  location_equipment <- httr::POST(
    httr::modify_url("https://dev-api.wildtrax.ca", path = "/bis/get-location-visit-equipment-summary"),
    accept = "application/json",
    httr::add_headers(
      Authorization = paste("Bearer", ._wt_auth_env_$access_token),
      Origin = "https://dev.wildtrax.ca",
      Pragma = "no-cache",
      Referer = "https://dev.wildtrax.ca"
    ),
    httr::user_agent(u),
    body = loceq_payload,
    encode = "json"
  )

  loceq <- httr::content(location_equipment)
  loceq <- map_dfr(loceq[[2]], ~ tibble(location = .x$locationName,
                                   deployment_date = .x$deploymentDate,
                                   retrieval_date = .x$retrieveDate,
                                   equipment_type = .x$typeId,
                                   equipment_code = .x$code,
                                   serial_number = .x$serialNo,
                                   equipment_make = .x$make,
                                   equipment_model = .x$model,
                                   equipment_condition = .x$conditionId,
                                   equipment_direction = .x$directionDegree,
                                   equipment_mount = .x$mountId,
                                   equipment_target = .x$targetId,
                                   stake_distance = .x$stakeDistance,
                                   parent_equipment = .x$parentEquipment))

   visit_payload <- list(
    limit = 2e9,
    orderBy = "locationName",
    orderDirection = "DESC",
    organizationId = org_id,
    page = 1)

  visits <- httr::POST(
    httr::modify_url("https://dev-api.wildtrax.ca", path = "/bis/get-location-visits"),
    accept = "application/json",
    httr::add_headers(
      Authorization = paste("Bearer", ._wt_auth_env_$access_token),
      Origin = "https://dev.wildtrax.ca",
      Pragma = "no-cache",
      Referer = "https://dev.wildtrax.ca"
    ),
    httr::user_agent(u),
    body = visit_payload,
    encode = "json"
  )

  visits <- httr::content(visits)
  visits <- map_dfr(visits[[2]], ~ tibble(location = .x$locationName,
                                          latitude = .x$latitude,
                                          longitude = .x$longitude,
                                          visit_date = .x$date,
                                          snow_depth_m = .x$snowDepth,
                                          water_depth_cm = .x$waterDepth,
                                          bait = .x$baitId,
                                          crew = .x$crewName,
                                          access_method = .x$accessMethodId,
                                          distance_to_clutter = .x$distanceToClutter,
                                          distance_to_water = .x$distanceToWater,
                                          clutter_percent = .x$clutterPercent,
                                          sunrise = .x$sunRise,
                                          sunset = .x$sunSet,
                                          timezone = .x$timeZone,
                                          land_features = .x$landFeatureIds))

  output <- input %>%
    inner_join(spps_tibble, by = c("species_common_name"))

  output <- output %>%
    inner_join(., visits %>% select(location, visit_date, crew, land_features), by = c("location" = "location"))

  if (nrow(output) == 0) {stop('There were no visits to join for this project. Enter visits in your Organization.')}

  output <- output %>%
    inner_join(., loceq %>% select(location, deployment_date, retrieval_date, equipment_condition, equipment_direction, equipment_mount, stake_distance), by = c("location" = "location"))

  if (nrow(output) == 0) {stop('There was no location equipment for this project. Enter your equipment and visits in your Organization.')}

  if (any(grepl("image", names(output), ignore.case = TRUE))) {
    # DO CAMERA STUFF
    output <- output %>%
      select(location, latitude, longitude, location_buffer_m, visit_date, deployment_date, retrieval_date, image_date_time, sfw_name, individual_count, age_class, sex_class) %>%
      distinct()
    return(output)
    # FORMAT AND OUTPUT
  } else {
    # DO ARU STUFF
    output <- output %>%
      select(organization, location, latitude, longitude, location_buffer_m, recording_date_time, deployment_date, retrieval_date, visit_date, sfw_name, individual_order, individual_count) %>%
      distinct() %>%
      mutate(`sc_SURVEYTYPE.domainCodeIdSurveyType` = "Breeding -BREEDING", .before = organization) %>%
      rename("wi_stakeholderInSurveyCrew" = organization,
             "sd_effectiveDate" = deployment_date,
             "sd_terminationDate" = retrieval_date) %>%
      relocate(sd_effectiveDate, .after = `sc_SURVEYTYPE.domainCodeIdSurveyType`) %>%
      relocate(sd_terminationDate, .after = sd_effectiveDate)

    new_columns <- list(
      sc_TAXONOMIC.targetSpecies = NA,
      ss_sensitiveFlag = NA,
      sc_SNOWCOVER.domainCodeIdSnowCoverCondition = NA,
      sc_PRECIPITTN.domainCodeIdPrecipitation = NA,
      si_cloudCover = NA,
      sf_temperature = NA,
      si_windSpeed = NA,
      sc_WINDDIRCTN.domainCodeIdWindDirection = NA,
      ss_comments = NA,
      wc_OBSEXPTISE.domainCodeExpertiseLevel = NA,
      ws_comments = NA,
      li_parentLocationNumber = NA,
      ls_locationNumber = NA,
      ls_inAlbertaFlag = NA,
      lf_startUtmEasting = NA,
      lf_startUtmNorthing = NA,
      lc_GISREFMER.domainCodeIdStartReferenceMeridian = NA,
      lc_LOCTYPE.domainCodeIdLocationType = NA,
      li_blockNumber = NA,
      lc_GISSOURCE.domainCodeIdMeasurementSource = NA,
      lc_GISDATUM.domainCodeIdDatum = NA,
      li_startAtsLegalSubdivision = NA,
      lc_ATSQUARTER.domainCodeIdStartAtsQuarter = NA,
      li_startAtsSection = NA,
      li_startAtsTownship = NA,
      li_startAtsRange = NA,
      lc_ATSMERIDIA.domainCodeIdStartAtsMeridian = NA,
      lf_endLatitude = NA,
      lf_endLongitude = NA,
      lf_endUtmEasting = NA,
      lf_endUtmNorthing = NA,
      lc_GISREFMER.domainCodeIdEndReferenceMeridian = NA,
      li_endAtsLegalSubdivision = NA,
      lc_ATSQUARTER.domainCodeIdEndAtsQuarter = NA,
      li_endAtsSection = NA,
      li_endAtsTownship = NA,
      li_endAtsRange = NA,
      lc_ATSMERIDIA.domainCodeIdEndAtsMeridian = NA,
      lc_GEOADMTYP.WMU.geoAdminWmuId = NA,
      li_waterbodyId = NA,
      ls_waterbodyOfficialName = NA,
      li_transectLength = NA,
      ls_comments = NA,
      cs_sampleNumber = NA,
      cs_offTransect = NA,
      cc_SPECHEALTH.domainCodeHealth = NA,
      pc_OBSDISTANC.Distance_from_Observer.surveyParameterId = NA,
      pc_BIRD.Age_Categories.surveyParameterId = NA,
      ds_sampleNumber = NA,
      dt_effectiveTimestamp = NA,
      dt_terminationTimestamp = NA,
      dc_TAXONOMIC.targetSpecies = NA,
      dc_AGE_GROUP.domainCodeAgeGroup = NA,
      ds_gender = NA,
      dc_SPECMARKTP.domainCodeMarking = NA,
      ds_dissected = NA,
      dc_SPECHEALTH.domainCodeHealth = NA,
      ds_healthDescription = NA,
      dc_DEATHCAUSE.domainCodeCauseOfDeath = NA,
      ds_contaminants = NA,
      ds_enforNumber = NA,
      ds_comments = NA,
      ds_initialCapture = NA,
      IndividualSpeciesId = NA,
      `1s_internalFlag` = NA,
      `1c_SPECIDTYPE.domainCodeIdIdType` = NA,
      `1c_SPECIDCOLR.domainCodeIdIdColor` = NA,
      `1i_idNumber` = NA,
      `1i_radioFrequency` = NA,
      `1i_radioChannel` = NA,
      `1i_radioCode` = NA,
      UseTag1Flag = NA,
      `2s_internalFlag` = NA,
      `2c_SPECIDTYPE.domainCodeIdIdType` = NA,
      `2c_SPECIDCOLR.domainCodeIdIdColor` = NA,
      `2i_idNumber` = NA,
      `2i_radioFrequency` = NA,
      `2i_radioChannel` = NA,
      `2i_radioCode` = NA,
      UseTag2Flag = NA,
      `3s_internalFlag` = NA,
      `3c_SPECIDTYPE.domainCodeIdIdType` = NA,
      `3c_SPECIDCOLR.domainCodeIdIdColor` = NA,
      `3i_idNumber` = NA,
      `3i_radioFrequency` = NA,
      `3i_radioChannel` = NA,
      `3i_radioCode` = NA,
      UseTag3Flag = NA,
      as_subsampleId1 = NA,
      as_subsampleId2 = NA,
      as_subsampleId3 = NA,
      ac_SUBSAMPTYP.domainCodeIdSubsampleType = NA,
      ac_BODYSOURCE.domainCodeIdBodySource = NA,
      ac_TESTTYPE.domainCodeIdTestType = NA,
      ac_TESTRESULT.domainCodeIdTestResult = NA,
      as_testResultChar = NA,
      at_testTimestamp = NA,
      as_testOrganization = NA,
      as_storeLocation = NA,
      at_letterSentTimestamp = NA,
      as_comments = NA,
      tc_BIRD.Age_Categories.surveyParameterId = NA,
      hi_siteNumber = NA,
      ht_effectiveTimestamp = NA,
      hi_transectNumber = NA,
      hi_stationNumber = NA,
      hi_depth = NA,
      hs_comments = NA,
      bc_HABGENERAL.Primary_Habitat.surveyParameterId = NA,
      bc_HABGENERAL.Secondary_Habitat.surveyParameterId = NA,
      bc_HABGENERAL.Topography.surveyParameterId = NA,
      fc_SITFEATURE.domainCodeFeature = NA,
      fs_sampleNumber = NA,
      ft_effectiveTimestamp = NA,
      fc_CONFDNTLVL.domainCodeConfidence = NA,
      fc_TAXONOMIC.targetSpecies = NA,
      fs_comments = NA,
      gc_FEATURE.Feature_Status.surveyParameterId = NA,
      gc_BIRD.Lek_Status.surveyParameterId = NA,
      gc_FEATURE.Feature_Count.surveyParameterId = NA,
      cc_DEATHCAUSE.domainCodeCauseOfDeath = NA,
      cc_CONFDNTLVL.domainCodeConfidence = NA,
      cc_EVIDENCE.domainCodeEvidence = NA,
      cc_SPECMARKTP.domainCodeMarking = NA,
      ct_effectiveTimestamp = NA,
      cc_AGE_GROUP.domainCodeAgeGroup = NA,
      cs_gender = NA
    )

    output <- output %>%
      mutate(!!!new_columns) %>%  # Add new columns
      relocate(any_of(names(new_columns)), .after = sd_terminationDate) %>%
      rename('cs_comments' = location,
             'lf_startLatitude' = latitude,
             'lf_startLongitude' = longitude,
             'lf_gisPrecision' = location_buffer_m,
             'ct_effectiveTimestamp' = recording_date_time,
             'ct_terminationTimestamp' = recording_date_time,
             'cc_TAXONOMIC.targetSpecies' = sfw_name,
             'ci_totalCount' = individual_order,
             'cc_ABUNDANCE.domainCodeAbundance' = individual_count)

    new_column_order <- c(
      "sc_SURVEYTYPE.domainCodeIdSurveyType", "sd_effectiveDate", "sd_terminationDate", "sc_TAXONOMIC.targetSpecies",
      "ss_sensitiveFlag", "sc_SNOWCOVER.domainCodeIdSnowCoverCondition", "sc_PRECIPITTN.domainCodeIdPrecipitation",
      "si_cloudCover", "sf_temperature", "si_windSpeed", "sc_WINDDIRCTN.domainCodeIdWindDirection", "ss_comments",
      "ss_comments", "wi_stakeholderInSurveyCrew", "wc_OBSEXPTISE.domainCodeExpertiseLevel", "ws_comments",
      "li_parentLocationNumber", "ls_locationNumber", "lc_LOCTYPE.domainCodeIdLocationType", "li_blockNumber",
      "lc_GISSOURCE.domainCodeIdMeasurementSource", "lc_GISDATUM.domainCodeIdDatum", "lf_gisPrecision",
      "ls_inAlbertaFlag", "lf_startLatitude", "lf_startLongitude", "lf_startUtmEasting", "lf_startUtmNorthing",
      "lc_GISREFMER.domainCodeIdStartReferenceMeridian", "li_startAtsLegalSubdivision", "lc_ATSQUARTER.domainCodeIdStartAtsQuarter",
      "li_startAtsSection", "li_startAtsTownship", "li_startAtsRange", "lc_ATSMERIDIA.domainCodeIdStartAtsMeridian",
      "lf_endLatitude", "lf_endLongitude", "lf_endUtmEasting", "lf_endUtmNorthing", "lc_GISREFMER.domainCodeIdEndReferenceMeridian",
      "li_endAtsLegalSubdivision", "lc_ATSQUARTER.domainCodeIdEndAtsQuarter", "li_endAtsSection", "li_endAtsTownship",
      "li_endAtsRange", "lc_ATSMERIDIA.domainCodeIdEndAtsMeridian", "lc_GEOADMTYP.WMU.geoAdminWmuId", "li_waterbodyId",
      "ls_waterbodyOfficialName", "li_transectLength", "ls_comments", "ls_comments", "cs_sampleNumber",
      "ct_effectiveTimestamp", "ct_terminationTimestamp", "cc_TAXONOMIC.targetSpecies", "ci_totalCount",
      "cc_ABUNDANCE.domainCodeAbundance", "cc_AGE_GROUP.domainCodeAgeGroup", "cs_gender", "cs_offTransect",
      "cc_SPECHEALTH.domainCodeHealth", "cc_DEATHCAUSE.domainCodeCauseOfDeath", "cc_CONFDNTLVL.domainCodeConfidence",
      "cc_EVIDENCE.domainCodeEvidence", "cc_SPECMARKTP.domainCodeMarking", "cs_comments", "pc_OBSDISTANC.Distance_from_Observer.surveyParameterId",
      "pc_BIRD.Age_Categories.surveyParameterId", "ds_sampleNumber", "dt_effectiveTimestamp", "dt_terminationTimestamp",
      "dc_TAXONOMIC.targetSpecies", "dc_AGE_GROUP.domainCodeAgeGroup", "ds_gender", "dc_SPECMARKTP.domainCodeMarking",
      "ds_dissected", "dc_SPECHEALTH.domainCodeHealth", "ds_healthDescription", "dc_DEATHCAUSE.domainCodeCauseOfDeath",
      "ds_contaminants", "ds_enforNumber", "ds_comments", "ds_initialCapture", "IndividualSpeciesId", "1s_internalFlag",
      "1c_SPECIDTYPE.domainCodeIdIdType", "1c_SPECIDCOLR.domainCodeIdIdColor", "1i_idNumber", "1i_radioFrequency",
      "1i_radioChannel", "1i_radioCode", "UseTag1Flag", "2s_internalFlag", "2c_SPECIDTYPE.domainCodeIdIdType",
      "2c_SPECIDCOLR.domainCodeIdIdColor", "2i_idNumber", "2i_radioFrequency", "2i_radioChannel", "2i_radioCode",
      "UseTag2Flag", "3s_internalFlag", "3c_SPECIDTYPE.domainCodeIdIdType", "3c_SPECIDCOLR.domainCodeIdIdColor",
      "3i_idNumber", "3i_radioFrequency", "3i_radioChannel", "3i_radioCode", "UseTag3Flag", "as_subsampleId1",
      "as_subsampleId2", "as_subsampleId3", "ac_SUBSAMPTYP.domainCodeIdSubsampleType", "ac_BODYSOURCE.domainCodeIdBodySource",
      "ac_TESTTYPE.domainCodeIdTestType", "ac_TESTRESULT.domainCodeIdTestResult", "as_testResultChar", "at_testTimestamp",
      "as_testOrganization", "as_storeLocation", "at_letterSentTimestamp", "as_comments", "tc_BIRD.Age_Categories.surveyParameterId",
      "hi_siteNumber", "ht_effectiveTimestamp", "hi_transectNumber", "hi_stationNumber", "hi_depth", "hs_comments",
      "bc_HABGENERAL.Primary_Habitat.surveyParameterId", "bc_HABGENERAL.Secondary_Habitat.surveyParameterId",
      "bc_HABGENERAL.Topography.surveyParameterId", "fc_SITFEATURE.domainCodeFeature", "fs_sampleNumber",
      "ft_effectiveTimestamp", "fc_CONFDNTLVL.domainCodeConfidence", "fc_TAXONOMIC.targetSpecies", "fs_comments",
      "gc_FEATURE.Feature_Status.surveyParameterId", "gc_BIRD.Lek_Status.surveyParameterId", "gc_FEATURE.Feature_Count.surveyParameterId"
    )

    output <- output %>%
      select(all_of(new_column_order))

    return(output)
  }
}


