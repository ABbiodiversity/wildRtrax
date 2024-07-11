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

  # Include metadata
  query_list$includeMetaData <- TRUE
  query_list$splitLocation <- TRUE

  # Prepare temporary file:
  tmp <- tempfile(fileext = ".zip")
  # tmp directory
  td <- tempdir()

  # Create GET request
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

  # Remove special characters
  list.files(td, pattern = "*.csv", full.names = TRUE) %>% map(~ {
    directory <- dirname(.x)
    old_filename <- basename(.x)
    new_filename <- gsub("[:()?!~;]", "", old_filename)
    new_path <- file.path(directory, new_filename)
    file.rename(.x, new_path)
  })
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
