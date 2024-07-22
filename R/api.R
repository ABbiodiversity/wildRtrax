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

wt_auth <- function(force = FALSE) {

  if (!exists("._wt_auth_env_"))
    stop("Cannot find the correct environment.", call. = TRUE)

  if(is.null("._wt_auth_env_")){
    message("Would you like to authorize to your WildTrax account?")
  }

  if (force || .wt_auth_expired())
    .wt_auth()

  invisible(NULL)

}

#' Get a download summary from WildTrax
#'
#' @description Obtain a table listing projects that the user is able to download data for
#'
#' @param sensor_id Can be one of "ARU", "CAM", or "PC"
#'
#' @import httr dplyr
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
#' @return A dataframe listing the projects that the user can download data for, including: project name, id, year, number of tasks, a geographic bounding box and project status.
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
       dplyr::select(organization_id = organizationId,
                     organization = organizationName,
                     project = fullNm,
                     project_id = id,
                     sensor = sensorId,
                     tasks,
                     status) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), unlist))

}

#' Download formatted reports from WildTrax
#'
#' @description Download various ARU, camera, or point count data from projects across WildTrax
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
#'  \item point_count
#'  \item definitions
#' }
#'
#' @import httr purrr dplyr
#' @importFrom readr read_csv col_character col_logical
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
  i <- wt_get_download_summary(sensor_id = sensor_id) %>%
    tibble::as_tibble() %>%
    dplyr::select(project_id, sensor)

  sensor_value <- i %>%
    dplyr::rename('id' = 1) %>%
    dplyr::filter(id %in% project_id) %>%
    dplyr::pull(sensor)

  if (!project_id %in% i$project_id) {
    stop("The project_id you specified is not among the projects you are able to download for.", call. = TRUE)
  }

  # Make sure report is specified
  if(missing(reports)) {
    stop("Please specify a report type (or multiple) using the `report` argument. Use ?wt_download_report to view options.",
         call. = TRUE)
  }

  # Allowable reports for each sensor
  cam <- c("main", "project", "location", "image_set", "image_report", "tag", "megadetector", "megaclassifier", "daylight", "definitions")
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

  # Add wildrtrax version information:
  u <- paste0("wildrtrax ", as.character(packageVersion("wildrtrax")), "; ", u)

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
    daylightReport = FALSE,
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
  if ("daylight" %in% reports) query_list$daylightReport <- TRUE

  # Include metadata
  query_list$includeMetaData <- TRUE
  query_list$splitLocation <- TRUE

  # Prepare temporary file:
  tmp <- tempfile(fileext = ".zip")
  # tmp directory
  td <- tempdir()

  # Create GET request
  r <- httr::POST(
    httr::modify_url("https://www-api.wildtrax.ca", path = "/bis/download-report"),
    query = query_list,
    accept = "application/zip",
    httr::add_headers(Authorization = paste("Bearer", ._wt_auth_env_$access_token)),
    httr::user_agent(u),
    httr::write_disk(tmp)
    )

  print(r)

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

  # Remove special characters
  list.files(td, pattern = "*.csv", full.names = TRUE) %>% map(~ {
    directory <- dirname(.x)
    old_filename <- basename(.x)
    new_filename <- gsub("[:()?!~;]", "", old_filename)
    new_path <- file.path(directory, new_filename)
    file.rename(.x, new_path)
  })

  # Index column types. Add more columns and their types as needed
  col_types_index <- list(
    abundance = readr::col_character(),
    image_fire = readr::col_logical()
  )

  files.full <- list.files(td, pattern= "*.csv", full.names = TRUE)
  files.less <- basename(files.full)
  x <- purrr::map(.x = files.full, .f = ~ suppressWarnings(readr::read_csv(., show_col_types = F,
                                                                           skip_empty_rows = T, col_types = list(abundance = readr::col_character(),
                                                                                                                 image_fire = readr::col_logical())))) %>%
    purrr::set_names(files.less)


  # Remove weather columns, if desired
  if(weather_cols) {
    x
  } else {
    x <- purrr::map(.x = x, .f = ~ (.x[, !grepl("^daily|^hourly", colnames(.x))]))
  }

  # Return the requested report(s)
  report <- paste(paste0("_",reports), collapse = "|")
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
#'
#' @import dplyr httr
#' @importFrom readr read_csv
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

  # User agent
  u <- getOption("HTTPUserAgent")
  if (is.null(u)) {
    u <- sprintf("R/%s; R (%s)",
                 getRversion(),
                 paste(getRversion(), R.version$platform, R.version$arch, R.version$os))
  }

  # Add wildrtrax version information:
  u <- paste0("wildrtrax ", as.character(packageVersion("wildrtrax")), "; ", u)

  spp <- httr::POST(
    httr::modify_url("https://www-api.wildtrax.ca", path = "/bis/get-all-species"),
    accept = "application/json",
    httr::add_headers(Authorization = paste("Bearer", ._wt_auth_env_$access_token)),
    httr::user_agent(u)
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

#' Download acoustic media
#'
#' @description Download acoustic media in batch
#'
#' @param input The report data
#' @param output The output folder
#' @param type Either recording, tag_clip_spectrogram or tag_clip_audio
#'
#' @import dplyr tibble purrr
#' @importFrom curl curl_download
#' @export
#'
#' @examples
#' \dontrun{
#' dat.report <- wt_download_report() |>
#' wt_download_media(output = "my/output/folder")
#' }
#'
#' @return An organized folder of media. Assigning wt_download_tags to an object will return the table form of the data with the functions returning the after effects in the output directory

wt_download_media <- function(input, output, type = c("recording","tag_clip_audio","tag_clip_spectrogram")) {

  input_data <- input

  # Check if input_data is provided and in the correct format
  if (missing(input_data) || !is.data.frame(input_data) && !is.matrix(input_data)) {
    stop("Input data must be provided as a data frame or matrix.")
  }

  # Check if output directory exists
  if (!dir.exists(output)) {
    stop("Output directory does not exist.")
  }

  # Check if type is valid
  valid_types <- c("recording","tag_clip_audio","tag_clip_spectrogram")
  if (!type %in% valid_types) {
    stop("Invalid type. Valid types are 'recording', 'tag_clip_spectrogram', or 'tag_clip_audio'.")
  }

  # Process based on type
  if (type == "recording" & "recording_url" %in% colnames(input_data)) {
    output_data <- input_data %>%
      mutate(
        file_type = sub('.*\\.(\\w+)$', '\\1', basename(recording_url)),
        clip_file_name = paste0(output, "/", location, "_", format(recording_date_time, "%Y%m%d_%H%M%S"), ".", file_type)
      ) %>%
      { purrr::map2_chr(.$recording_url, .$clip_file_name, ~ curl::curl_download(.x, .y, mode = "wb")) }

  } else if (type == "tag_clip_spectrogram" & "spectrogram_url" %in% colnames(input_data)) {
    output_data <- input_data %>%
      mutate(
        detection_time = gsub("\\.", "_", as.character(detection_time)),
        clip_file_name = file.path(output, paste0(
          organization, "_", location, "_", format(parse_date_time(recording_date_time, "%Y-%m-%d %H:%M:%S"), "%Y%m%d_%H%M%S"), "__",
          species_code, "__", individual_order, "__", detection_time, ".jpeg"
        ))
      ) %>%
      { purrr::map2_chr(.$spectrogram_url, .$clip_file_name, ~ curl::curl_download(.x, .y, mode = "wb")) }

  } else if (all(c("spectrogram_url", "clip_url") %in% colnames(input_data)) & any(type %in% c("tag_clip_spectrogram", "tag_clip_audio"))) {
    output_data <- input_data %>%
      mutate(
        detection_time = gsub("\\.", "_", as.character(detection_time)),
        audio_file_type = sub('.*\\.(\\w+)$', '\\1', clip_url),
        clip_file_name_spec = file.path(output, paste0(
          organization, "_", location, "_", format(parse_date_time(recording_date_time, "%Y-%m-%d %H:%M:%S"), "%Y%m%d_%H%M%S"), "__",
          species_code, "__", individual_order, "__", detection_time, ".jpeg"
        )),
        clip_file_name_audio = file.path(output, paste0(
          organization, "_", location, "_", format(parse_date_time(recording_date_time, "%Y-%m-%d %H:%M:%S"), "%Y%m%d_%H%M%S"), "__",
          species_code, "__", individual_order, "__", detection_time, ".", audio_file_type
        ))
      ) %>%
      {
        purrr::map2_chr(.$spectrogram_url, .$clip_file_name_spec, ~ curl::curl_download(.x, .y, mode = "wb"))
        purrr::map2_chr(.$clip_url, .$clip_file_name_audio, ~ curl::curl_download(.x, .y, mode = "wb"))
      }

  } else {
    stop("Required columns are either 'recording_url', 'spectrogram_url', or 'clip_url'. Use wt_download_report(reports = 'recording' or 'tag') to get the correct media.")
  }

  return(output_data)
}


#' Download data from Data Discover
#'
#' @description Download Data Discover results from projects across WildTrax
#'
#' @param sensor  The sensor you wish to query from either 'ARU', 'CAM' or 'PC'
#' @param species The species you want to search for (e.g. 'WTSP'). Multiple species can be included.
#' @param boundary The custom boundary you want to use. Defined as at least a four vertex polygon. Definition can also be a bbox
#'
#' @import dplyr tibble httr
#' @export
#'
#' @examples
#' \dontrun{
#'
#' aoi <- list(
#' c(-110.85438, 57.13472),
#' c(-114.14364, 54.74858),
#' c(-110.69368, 52.34150),
#' c(-110.854385, 57.13472)
#' )
#'
#' dd <- wt_dd_summary(sensor = 'ARU', species = 'White-throated Sparrow', boundary = aoi)
#' }
#'
#' @return Return

wt_dd_summary <- function(sensor = c('ARU','CAM','PC'), species = NULL, boundary = NULL) {

  # Set user agent
  u <- getOption("HTTPUserAgent")
  if (is.null(u)) {
    u <- sprintf("R/%s; R (%s)",
                 getRversion(),
                 paste(getRversion(), R.version$platform, R.version$arch, R.version$os))
  }
  u <- paste0("wildrtrax ", as.character(packageVersion("wildrtrax")), "; ", u)

  # Determine whether public or user
  if (!exists("access_token", envir = ._wt_auth_env_)) {
    message("Currently searching as a public user, access to data will be limited. Use wt_auth() to login.")
    tok_used <- NULL

    ddspp <- httr::POST(
      httr::modify_url("https://www-api.wildtrax.ca", path = "/bis/dd-get-species"),
      accept = "application/json",
      httr::add_headers(
        Authorization = NULL,
        Origin = "https://discover.wildtrax.ca",
        Pragma = "no-cache",
        Referer = "https://discover.wildtrax.ca/"
      ),
      httr::user_agent(u),
      body = list(sensorId = sensor),
      encode = "json" # Specify that the payload should be encoded as JSON
    )

    spp_t <- httr::content(ddspp)

    species_tibble <- purrr::map_df(spp_t, ~{
      # Check if each column exists in the list
      commonName <- if ("commonName" %in% names(.x)) .x$commonName else NA
      speciesId <- if ("speciesId" %in% names(.x)) .x$speciesId else NA
      sciName <- if ("sciName" %in% names(.x)) .x$sciName else NA
      tibble(species_common_name = commonName, species_id = speciesId, species_scientific_name = sciName)
    })

    # Fetch species if provided
    if (!is.null(species)) {
      spp <- species_tibble |>
        filter(species_common_name %in% species) |>
        pull(species_id)

      if (is.null(spp)) {
        stop("No species were found.")
      }
    }

  } else {
    if (.wt_auth_expired()) {
      stop("Please authenticate with wt_auth().", call. = FALSE)
    } else {
      tok_used <- paste("Bearer", ._wt_auth_env_$access_token)
      species_tibble <- suppressMessages(wt_get_species())
    }
  }

  # Test for coordinate system
  if (inherits(boundary, "bbox")) {
    lat_valid <- boundary["ymin"] >= -90 & boundary["ymax"] <= 90
    long_valid <- boundary["xmin"] >= -180 & boundary["xmax"] <= 180
    if (!lat_valid | !long_valid) {
      stop("Coordinate system for boundary or bbox must be in latitude and longitude")
    }
  } else if (inherits(boundary, "list")) {
    lat_valid <- all(sapply(boundary, function(coord) coord[2] >= -90 & coord[2] <= 90))
    long_valid <- all(sapply(boundary, function(coord) coord[1] >= -180 & coord[1] <= 180))
    if (!lat_valid | !long_valid) {
      stop("Coordinate system for boundary or bbox must be in latitude and longitude")
    }
  }

  # Test for bbox
  if (inherits(boundary,"bbox")){
    boundary <- list(
      c(boundary['xmin'], boundary['ymin']),
      c(boundary['xmax'], boundary['ymin']),
      c(boundary['xmax'], boundary['ymax']),
      c(boundary['xmin'], boundary['ymax']),
      c(boundary['xmin'], boundary['ymin']) # Closing the polygon
    )
  }

  # Validate boundary if provided
  if (!is.null(boundary)) {
    # Check the number of vertices
    if (length(boundary) < 4) {
      stop("Error: Boundary must have at least four vertices.")
    }

    # Check for closure
    if (!identical(boundary[[1]], boundary[[length(boundary)]])) {
      # If the first and last points are not identical, check if they are 'close enough'
      if (!all(abs(unlist(boundary[[1]]) - unlist(boundary[[length(boundary)]])) < 1e-6)) {
        stop("Error: Boundary must form a closed polygon.")
      }
    }

    if (length(unique(boundary[-c(1, length(boundary))])) != length(boundary[-c(1, length(boundary))])) {
      stop("Error: Boundary contains duplicate vertices (excluding the first and last).")
    }

    # Check for valid coordinates
    if (!all(sapply(boundary, function(coord) all(is.numeric(coord) & length(coord) == 2)))) {
      stop("Error: Each coordinate pair must consist of valid latitude and longitude values.")
    }
  }

  # Here's da earth. Dat is a sweet earth you might say.
  full_bounds <- list(
    `_sw` = list(
      lng = -180.0,
      lat = -90
    ),
    `_ne` = list(
      lng = 180,
      lat = 90
    )
  )

  # Initialize lists to store results
  all_rpps_tibble <- list()
  all_result_tables <- list()

  # Iterate over each species
  for (sp in spp) {

    # Construct payload for httr::POST request
    payload <- list(
      isSpeciesTab = FALSE,
      zoomLevel = 20,
      bounds = full_bounds,
      sensorId = sensor,
      polygonBoundary = boundary,
      organizationIds = NULL,
      projectIds = NULL,
      speciesIds = list(sp)  # Wrap the integer value in a list to make it an array
    )

    # Make request to get-data-discoverer-long-lat-summary endpoint
    rr <- httr::POST(
      httr::modify_url("https://www-api.wildtrax.ca", path = "/bis/get-data-discoverer-long-lat-summary"),
      accept = "application/json",
      httr::add_headers(
        Authorization = tok_used,
        Origin = "https://discover.wildtrax.ca",
        Pragma = "no-cache",
        Referer = "https://discover.wildtrax.ca/"
      ),
      httr::user_agent(u),
      body = payload,
      encode = "json" # Specify that the payload should be encoded as JSON
    )

    # Construct payload for second httr::POST request
    payload_small <- list(
      isSpeciesTab = FALSE,
      zoomLevel = 20,
      bounds = full_bounds,
      sensorId = sensor,
      polygonBoundary = boundary,
      speciesIds = list(sp)
    )

    # Make request to get-data-discoverer-map-and-projects endpoint
    rr2 <- httr::POST(
      httr::modify_url("https://www-api.wildtrax.ca", path = "/bis/get-data-discoverer-map-and-projects"),
      accept = "application/json",
      httr::add_headers(
        Authorization = tok_used,
        Origin = "https://discover.wildtrax.ca",
        Pragma = "no-cache",
        Referer = "https://discover.wildtrax.ca/"
      ),
      httr::user_agent(u),
      body = payload_small,
      encode = "json" # Specify that the payload should be encoded as JSON
    )

    # Extract content from second request
    mapproj <- httr::content(rr2)

    # Extract features from response
    features <- mapproj$map$features

    # Initialize empty vectors to store data
    count <- c()
    longitude <- c()
    latitude <- c()

    # Iterate over features and extract data
    for (feature in features) {
      count <- c(count, feature$properties$count)
      longitude <- c(longitude, feature$geometry$coordinates[[1]])
      latitude <- c(latitude, feature$geometry$coordinates[[2]])
    }

    back_species <- species_tibble |>
      filter(species_id %in% sp) |>
      select(species_common_name)

    # Create tibble for result table
    result_table <- tibble(
      species_common_name = back_species$species_common_name,
      count = count,
      longitude = longitude,
      latitude = latitude
    )

    # Extracting data from second response
    rpps <- httr::content(rr)
    orgs <- purrr::map(rpps$organizations, ~pluck(., "organizationName")) |> map_chr(~ ifelse(is.null(.x), "", .x))
    counts <- purrr::map_dbl(rpps$projects, pluck, "count")
    projectNames <- map(rpps$projects, ~pluck(., "projectName")) |> map_chr(~ ifelse(is.null(.x), "", .x))
    projectIds <- map(rpps$projects, ~pluck(., "projectId")) |> map_int(~ ifelse(is.null(.x), NA_integer_, .x))

    # Create tibble for project summary
    rpps_tibble <- tibble(
      projectId = projectIds,
      project_name = projectNames,
      species_id = sp,
      count = counts
    ) |>
      inner_join(species_tibble, by = "species_id") |>
      select(projectId, project_name, count, species_common_name, species_scientific_name) |>
      distinct()

    # Add results to lists
    all_rpps_tibble[[length(all_rpps_tibble) + 1]] <- rpps_tibble
    all_result_tables[[length(all_result_tables) + 1]] <- result_table
  }

  # Combine results
  combined_rpps_tibble <- bind_rows(all_rpps_tibble)
  combined_result_table <- bind_rows(all_result_tables)

  # Check if any results found
  if (nrow(combined_rpps_tibble) == 0) {
    stop("No results were found on any of the search layers. Broaden your search and try again.")
  }

  if (nrow(combined_result_table) == 0) {
    stop("No results were found on any of the search layers. Broaden your search and try again.")
  }

  # Return list containing combined project summaries and result tables
  return(list(combined_rpps_tibble, combined_result_table))
}



