#' Format WildTrax report for occupancy modelling.
#'
#' @description This function formats the summary report from the `wt_download_report` function into an unmarked object for occupancy modelling. The current version only includes formatting for the ARU sensor and for single species single season models.
#'
#' @param data Summary report of WildTrax observations from the `wt_download_report` function. Currently only functioning for the ARU sensor.
#' @param species Character; four-letter alpha code for the species desired for occupancy modelling.
#' @param siteCovs Optional dataframe of site covariates. Must contain a column with the same values as the location field in the data, with one row per unique value of location (i.e., one row per site).
#' @import dplyr lubridate
#' @importFrom unmarked unmarkedFrameOccu
#' @export
#'
#' @examples
#' \dontrun{
#' dat.occu <- wt_format_occupancy(dat, species="CONI", siteCovs=NULL)
#' mod <- occu(~ 1 ~ 1, dat.occu)
#' }
#' @return An object of class unmarkedFrameOccu. See `?unmarked::unmarkedFrameOccu` for details.

wt_occu_singlesp_singleseas <- function(data,
                                        species,
                                        siteCovs=NULL){

  #Wrangle observations and observation covariates for the species of interest
  visits <- dat %>%
    dplyr::filter(species_code==species) %>%
    dplyr::select(location, recording_date) %>%
    unique() %>%
    mutate(occur=1) %>%
    right_join(dat %>%
                 dplyr::select(location, recording_date, observer, method) %>%
                 unique(),
               by=c("location", "recording_date")) %>%
    mutate(occur = ifelse(is.na(occur), 0, 1),
           recording_date = ymd_hms(recording_date),
           doy = yday(recording_date),
           hr = as.numeric(hour(recording_date) + minute(recording_date)/60)) %>%
    group_by(location) %>%
    arrange(recording_date) %>%
    mutate(visit = row_number()) %>%
    ungroup()

  #Create location X recording dataframe of observations (1 for detected, 0 for undetected)
  y <- visits %>%
    dplyr::select(location, visit, occur) %>%
    pivot_wider(id_cols = location, names_from = visit, values_from = occur) %>%
    arrange(location) %>%
    dplyr::select(-location) %>%
    data.frame()

  #Create location X recording dataframes for observation covariates (doy = day of year, hr = hour of day, method = processing method, observer = observer ID)
  doy <- visits %>%
    dplyr::select(location, visit, doy) %>%
    pivot_wider(id_cols = location, names_from = visit, values_from = doy) %>%
    arrange(location) %>%
    dplyr::select(-location) %>%
    data.frame()

  doy2 <- visits %>%
    mutate(doy2 = doy^2) %>%
    dplyr::select(location, visit, doy2) %>%
    pivot_wider(id_cols = location, names_from = visit, values_from = doy2) %>%
    arrange(location) %>%
    dplyr::select(-location) %>%
    data.frame()

  hr <- visits %>%
    dplyr::select(location, visit, hr) %>%
    pivot_wider(id_cols = location, names_from = visit, values_from = hr) %>%
    arrange(location) %>%
    dplyr::select(-location) %>%
    data.frame()

  hr2 <- visits %>%
    mutate(hr2 = hr^2) %>%
    dplyr::select(location, visit, hr2) %>%
    pivot_wider(id_cols = location, names_from = visit, values_from = hr2) %>%
    arrange(location) %>%
    dplyr::select(-location) %>%
    data.frame()

  method <- visits %>%
    dplyr::select(location, visit, method) %>%
    mutate(method = as.factor(method)) %>%
    pivot_wider(id_cols = location, names_from = visit, values_from = method) %>%
    arrange(location) %>%
    dplyr::select(-location) %>%
    data.frame()

  observer <- visits %>%
    dplyr::select(location, visit, observer) %>%
    mutate(observer = as.factor(observer)) %>%
    pivot_wider(id_cols = location, names_from = visit, values_from = observer) %>%
    arrange(location) %>%
    dplyr::select(-location) %>%
    data.frame()

  #Create a list of the observation covariates
  obsCovs <- list(doy=doy, doy2=doy2, hr=hr, hr2 = hr2, method=method, observer=observer)

  #Order site covs dataframe if one is provided
  if(!is.null(siteCovs)){

    #Check length of siteCovs object, remove if incorrect
    locs <- length(unique(dat$location))

    if(nrow(siteCovs)!=locs){
      siteCovs <- NULL
      warning('length of siteCovs dataframe does not match observation data, removing from unmarked object')
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
    umf <- unmarkedFrameOccu(y=y, siteCovs=NULL, obsCovs=obsCovs)
  } else {
    umf <- unmarkedFrameOccu(y=y, siteCovs=siteCovs, obsCovs=obsCovs)
  }
  options(warn=0)

  #return the unmarked object
  return(umf)

}
