#' Authenticate into WildTrax
#'
#' @description Obtain Auth0 credentials using WT_USERNAME and WT_PASSWORD stored as environment variables
#'
#' @param force Logical; whether or not the force re-authentication even if token has not expired. Defaults to FALSE.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Authenticate first:
#' wt_auth(force = FALSE)
#' }
#'
#'

wt_auth <- function(force = FALSE) {

  if (!exists("._wt_auth_env_"))
    stop("Cannot find the correct environment.", call. = TRUE)

  if (force || .wt_auth_expired())
    .wt_auth()

  invisible(NULL)

}

#' Get download summary
#'
#' @description Obtain a table listing projects that the user is able to download data for
#'
#' @param sensor_id Can be one of "ARU", "CAM", or "PC"
#'
#' @importFrom httr content
#' @importFrom dplyr select mutate across everything
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Authenticate first:
#' wt_auth()
#' wt_get_download_summary(sensor_id = "ARU")
#' }
#'
#' @return a dataframe listing the projects that the user can download data for, including: project name, id, year, number of tags, and status.
#'
wt_get_download_summary <- function(sensor_id) {

  sens <- c("PC", "ARU", "CAM")

  # Stop call if sensor id value is not within range of possible values
  if(!sensor_id %in% sens) {
    stop("A valid value for sensor_id must be supplied. See ?wt_get_download_summary for a list of possible values", call. = TRUE)
  }

  r <- .wt_api_pr(
    path = "/bis/get-download-summary",
    sensorId = sensor_id,
    sort = "fullNm",
    order = "asc"
  )

  x <- data.frame(do.call(rbind, httr::content(r)$results)) |>
       dplyr::select(organization_id = organizationId, organization = organizationName,
                     project = fullNm, project_id = id, sensor = sensorId, tasks, status) |>
    mutate(across(everything(), unlist))

}

#' Download Reports
#'
#' @description Download ARU, Camera, or Point Count data from a project
#'
#' @param project_id Numeric; the project ID number that you would like to download data for. Use `wt_get_download_summary()` to retrieve these IDs.
#' @param sensor_id Character; Can either be "ARU", "CAM", or "PC".
#' @param reports Character; The report type to be returned. Multiple values are accepted as a concatenated string.
#' @param weather_cols Logical; Do you want to include weather information for your stations? Defaults to TRUE.
#' @details Valid values for argument \code{report} when \code{sensor_id} = "CAM" currently are:
#' \itemize{
#'  \item main
#'  \item project
#'  \item location
#'  \item image_report
#'  \item image_set
#'  \item tag
#'  \item megadetector
#'  \item megaclassifier
#'  \item definitions
#' }
#' @details Valid values for argument \code{report} when \code{sensor_id} = "ARU" currently are:
#' \itemize{
#'  \item main
#'  \item project
#'  \item location
#'  \item recording
#'  \item tag
#'  \item birdnet
#'  \item definitions
#' }
#' @details Valid values for argument \code{report} when \code{sensor_id} = "PC" currently are:
#' \itemize{
#'  \item main
#'  \item project
#'  \item location
#'  \item point count
#'  \item definitions
#' }
#'
#' @import httr purrr dplyr
#' @importFrom utils read.csv
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Authenticate first:
#' wt_auth()
#' a_camera_project <- wt_download_report(
#' project_id = 397, sensor_id = "CAM", reports = c("tag", "image_set"),
#' weather_cols = TRUE)
#'
#' an_aru_project <- wt_download_report(
#' project_id = 47, sensor_id = "ARU", reports = c("main", "birdnet"),
#' weather_cols = TRUE)
#' }
#'
#' @return If multiple report types are requested, a list object is returned; if only one, a dataframe.
#'

wt_download_report <- function(project_id, sensor_id, reports, weather_cols = TRUE) {

  # Check if authentication has expired:
  if (.wt_auth_expired())
    stop("Please authenticate with wt_auth().", call. = FALSE)

  # Check if the project_id is valid:
  i <- wt_get_download_summary(sensor_id = sensor_id)
  i <- unlist(i$project_id)

  if (!project_id %in% i) {
    stop("The project_id you specified is not among the projects you are able to download for.", call. = TRUE)
  }

  # Make sure report is specified
  if(missing(reports)) {
    stop("Please specify a report type (or multiple) using the `report` argument. Use ?wt_download_report to view options.",
         call. = TRUE)
  }

  # Allowable reports for each sensor

  cam <- c("main", "project", "location", "image_set", "image_report", "tag", "megadetector", "megaclassifier", "definitions")
  aru <- c("main", "project", "location", "birdnet", "recording", "tag", "definitions")
  pc <- c("main", "project", "location", "point_count", "definitions")

  # Check that the user supplied a valid report type depending on the sensor
  if(sensor_id == "CAM" & !all(reports %in% cam)) {
    stop("Please supply a valid report type. Use ?wt_download_report to view options.", call. = TRUE)
  }

  if(sensor_id == "ARU" & !all(reports %in% aru)) {
    stop("Please supply a valid report type. Use ?wt_download_report to view options.", call. = TRUE)
  }

  if(sensor_id == "PC" & !all(reports %in% pc)) {
    stop("Please supply a valid report type. Use ?wt_download_report to view options.", call. = TRUE)
  }

  # User agent
  u <- getOption("HTTPUserAgent")
  if (is.null(u)) {
    u <- sprintf("R/%s; R (%s)",
                 getRversion(),
                 paste(getRversion(), R.version$platform, R.version$arch, R.version$os))
  }

  # Add wildRtrax version information:
  u <- paste0("wildRtrax ", as.character(packageVersion("wildRtrax")), "; ", u)

  # Create query list
  query_list <- list(
    projectIds = project_id,
    sensorId = sensor_id,
    mainReport = FALSE,
    projectReport = FALSE,
    recordingReport = FALSE,
    pointCountReport = FALSE,
    locationReport = FALSE,
    tagReport = FALSE,
    imageReport = FALSE,
    imageSetReport = FALSE,
    birdnetReport = FALSE,
    megaDetectorReport = FALSE,
    megaClassifierReport = FALSE,
    includeMetaData = FALSE,
    splitLocation = FALSE
  )

  # Create the list of objects
  if ("main" %in% reports) query_list$mainReport <- TRUE
  if ("project" %in% reports) query_list$projectReport <- TRUE
  if ("recording" %in% reports) query_list$recordingReport <- TRUE
  if ("point_count" %in% reports) query_list$pointCountReport <- TRUE
  if ("location" %in% reports) query_list$locationReport <- TRUE
  if ("tag" %in% reports) query_list$tagReport <- TRUE
  if ("image_report" %in% reports) query_list$imageReport <- TRUE
  if ("image_set" %in% reports) query_list$imageSetReport <- TRUE
  if ("birdnet" %in% reports) query_list$birdnetReport <- TRUE
  if ("megadetector" %in% reports) query_list$megaDetectorReport <- TRUE
  if ("megaclassifier" %in% reports) query_list$megaClassifierReport <- TRUE

  query_list$includeMetaData <- TRUE
  query_list$splitLocation <- TRUE

  # Prepare temporary file:
  tmp <- tempfile(fileext = ".zip")
  # tmp directory
  td <- tempdir()

  # Create POST request
  r <- httr::GET(
    httr::modify_url("https://www-api.wildtrax.ca", path = "/bis/download-report"),
    query = query_list,
    accept = "application/zip",
    httr::add_headers(Authorization = paste("Bearer", ._wt_auth_env_$access_token)),
    httr::user_agent(u),
    httr::write_disk(tmp),
    httr::progress()
    )


  # Stop if an error or bad request occurred
  if (httr::http_error(r))
    stop(sprintf(
      "Authentication failed [%s]\n%s",
      httr::status_code(r),
      httr::content(r)$message),
      call. = FALSE)

  # Unzip
  unzip(tmp, exdir = td)

  # Remove abstract file
  abstract <- list.files(td, pattern = "*_abstract.csv", full.names = TRUE, recursive = TRUE)
  file.remove(abstract)

  # List data files, read into R as a list
  files <- gsub(".csv", "", list.files(td, pattern = ".csv", recursive = TRUE))
  files.full <- list.files(td, pattern = ".csv", full.names = TRUE, recursive = TRUE)
  x <- purrr::map(.x = files.full, .f = ~ read.csv(., fileEncoding = "UTF-8-BOM")) %>%
    purrr::set_names(files)

  # Remove weather columns, if desired
  if(weather_cols) {
    x
  } else {
    x <- purrr::map(.x = x, .f = ~ (.x[, !grepl("^daily|^hourly", colnames(.x))]))
  }

  # Return the requested report(s)
  report <- paste(reports, collapse = "|")
  x <- x[grepl(report, names(x))]
  # Return a dataframe if only 1 element in the list (i.e., only 1 report requested)
  if (length(x) == 1) {
    x <- x[[1]]
  } else {
    x
  }

  # Delete csv files
  file.remove(files.full)
  # Delete tmp
  file.remove(tmp)

  return(x)

}


#' Get the WildTrax species table
#'
#' @description Request for the WildTrax species table
#'
#' @import dplyr httr readr jsonlite
#' @export
#'
#' @examples
#'  \dontrun{
#'  wt_species <- wt_get_species()
#'  }
#' @return A tibble of the WildTrax species table
#'

wt_get_species <- function(){

  # Check if authentication has expired:
  if (.wt_auth_expired())
    stop("Please authenticate with wt_auth().", call. = FALSE)

  if (exists("wt_spp_table")) {
    user_input <- readline("Do you want to overwrrite the current species table in your environment? (Y/N): ")

    if (user_input == "N") {
      stop("Stopping.")
    }
  }

  # User agent
  u <- getOption("HTTPUserAgent")
  if (is.null(u)) {
    u <- sprintf("R/%s; R (%s)",
                 getRversion(),
                 paste(getRversion(), R.version$platform, R.version$arch, R.version$os))
  }

  # Add wildRtrax version information:
  u <- paste0("wildRtrax ", as.character(packageVersion("wildRtrax")), "; ", u)

  spp <- httr::POST(
    httr::modify_url("https://www-api.wildtrax.ca", path = "/bis/get-all-species"),
    accept = "application/json",
    httr::add_headers(Authorization = paste("Bearer", ._wt_auth_env_$access_token)),
    httr::user_agent(u),
    httr::progress()
  )

  if (spp$status_code == 200) {
  } else {
    stop("The species table could not be downloaded.")
  }

  spps <- httr::content(spp)

  spp_table <- tibble(
    species_id = map_dbl(spps, ~ ifelse(!is.null(.x$id), .x$id, NA)),
    species_code = map_chr(spps, ~ ifelse(!is.null(.x$code), .x$code, NA)),
    species_common_name = map_chr(spps, ~ ifelse(!is.null(.x$commonName), .x$commonName, NA)),
    species_class = map_chr(spps, ~ ifelse(!is.null(.x$className), .x$className, NA)),
    species_order = map_chr(spps, ~ ifelse(!is.null(.x$order), .x$order, NA)),
    species_scientific_name = map_chr(spps, ~ ifelse(!is.null(.x$scientificName), .x$scientificName, NA))
  )

  message("Successfully downloaded the species table!")

 return(spp_table)

}


#' Download acoustic tags
#'
#' @description Downloads acoustic tags (mp3 and jpg) in an organized format for other purposes.
#'
#' @param input A data frame or tibble of the tag report i.e. wt_download_report(reports = "tag")
#' @param output Directory to store the tags
#' @param clip_type Character; either spectrogram or audio clips

#'
#' @import dplyr tibble readr
#' @export
#'
#' @examples
#' \dontrun{
#' dat.report <- wt_download_report(reports = "tag")
#' wt_download_tags(input = my_tag_data)
#' }
#'
#' @return An organized folder of clips and tags in the output directory. Assigning wt_download_tags to an object will return the table form of the data with the functions returning the after effects in the output directory

wt_download_tags_new <- function(input, output, clip_type = c("spectrogram","audio")) {

  input_data <- input

  # Assuming input_data is a data frame or a matrix
  if (!("tag_spectrogram_url" %in% colnames(input_data)) | !("clip_url" %in% colnames(input_data))) {
    stop("Required columns 'tag_spectrogram_url' and 'clip_url' are missing in input_data. Use wt_download_report(reports = 'tag').")
  }

  if (!dir.exists(output)) {
    stop("This directory doesn't exist.")
  }

  if (clip_type == "audio") {

    input_audio_only <- input_data %>%
      mutate(file_type = tools::file_ext(clip_url)) %>%
      select(organization, location, recording_date_time, species_code, individual_order, detection_time, clip_url, file_type) %>%
      mutate(detection_time = as.character(detection_time), detection_time = gsub("\\.", "_", detection_time)) %>%
      # Create the local file name
      mutate(clip_file_name = paste0(output, "/", organization,"_",location, "_", format(parse_date_time(recording_date_time,"%Y-%m-%d %H:%M:%S"), "%Y%m%d_%H%M%S"),"__", species_code,"__",individual_order,"__",detection_time,".",file_type))

    input_audio_only %>%
      furrr::future_walk2(.x = .$clip_url, .y = .$clip_file_name, .f = ~ download.file(.x, .y))

    return(input_audio_only)

  } else if (clip_type == "spectrogram") {

    input_spec_only <- input_data %>%
      select(organization, location, recording_date_time, species_code, individual_order, detection_time, tag_spectrogram_url) %>%
      mutate(detection_time = as.character(detection_time), detection_time = gsub("\\.", "_", detection_time)) %>%
      # Create the local file name
      mutate(clip_file_name = paste0(output, "/", organization,"_",location, "_", format(parse_date_time(recording_date_time,"%Y-%m-%d %H:%M:%S"), "%Y%m%d_%H%M%S"),"__", species_code,"__",individual_order,"__",detection_time,".jpeg"))

    input_spec_only %>%
      furrr::future_walk2(.x = .$tag_spectrogram_url, .y = .$clip_file_name, .f = ~ download.file(.x, .y))

    return(input_spec_only)

  } else if (clip_type == "spectrogram" & clip_type == "audio") {

    input_both <- input_data %>%
      mutate(file_type = tools::file_ext(clip_url)) %>%
      select(organization, location, recording_date_time, species_code, individual_order, detection_time, tag_spectrogram_url, clip_url) %>%
      mutate(detection_time = as.character(detection_time), detection_time = gsub("\\.", "_", detection_time)) %>%
      # Create the local file name
      mutate(clip_file_name_spec = paste0(output, "/", organization,"_",location, "_", format(parse_date_time(recording_date_time,"%Y-%m-%d %H:%M:%S"), "%Y%m%d_%H%M%S"),"__", species_code,"__",individual_order,"__",detection_time,".jpeg"))
      mutate(clip_file_name_audio = paste0(output, "/", organization,"_",location, "_", format(parse_date_time(recording_date_time,"%Y-%m-%d %H:%M:%S"), "%Y%m%d_%H%M%S"),"__", species_code,"__",individual_order,"__",detection_time,".",file_type))

    #Download spec first
    input_both %>%
      furrr::future_walk2(.x = .$tag_spectrogram_url, .y = .$clip_file_name, .f = ~ download.file(.x, .y))

    input_both %>%
      furrr::future_walk2(.x = .$tag_clip_url, .y = .$clip_file_name, .f = ~ download.file(.x, .y))

    return(input_both)

  } else {
    stop("Need to define what, either spectrogram, audio or both")
  }

}

