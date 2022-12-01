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

  if (force || .wt_auth_expired())
    .wt_auth()

  invisible(NULL)

}

#' Get download summary
#'
#' @description Obtain a table listing projects that the user is able to download data for
#'
#' @param sensor_id Can either be "ARU" or "CAM"
#'
#' @importFrom httr content
#' @importFrom dplyr select %>%
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

  r <- .wt_api_pr(
    path = "/bis/get-download-summary",
    sensorId = sensor_id,
    sort = "fullNm",
    order = "asc"
  )

  x <- data.frame(do.call(rbind, httr::content(r)$results)) %>%
       dplyr::select(project = fullNm, project_id = id, sensorId, tasks, status)

  return(x)

}

#' Download Reports
#'
#' @description Download ARU, Camera, or Point Count data from a project
#'
#' @param project_id Numeric; the project ID number that you would like to download data for. Use `wt_get_download_summary()` to retrieve these IDs.
#' @param sensor_id Character; Can either be "ARU", "CAM", or "PC".
#' @param report Character; The report type to be returned. Multiple values are accepted as a concatenated string.
#' @param weather_cols Logical; Do you want to include weather information for your stations? Defaults to TRUE.
#' @details Valid values for argument \code{report} when \code{sensor_id} = "CAM" currently are:
#' \itemize{
#'  \item image
#'  \item tag
#'  \item megadetector
#'  \item definitions
#' }
#' @details Valid values for argument \code{report} when \code{sensor_id} = "ARU" currently are:
#' \itemize{
#'  \item summary
#'  \item birdnet
#'  \item task
#'  \item tag
#'  \item definitions
#' }
#' @details Valid values for argument \code{report} when \code{sensor_id} = "PC" currently are:
#' \itemize{
#'  \item report
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
#' wt_download_report(project_id = 397, sensor_id = "CAM", report = c("tag", "image"), weather_cols = TRUE)
#' }
#'
#' @return If multiple report types are requested, a list object is returned; if only one, a dataframe.
#'

wt_download_report <- function(project_id, sensor_id, report, weather_cols = TRUE) {

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
  if(missing(report)) {
    stop("Please specify a report type (or multiple) using the `report` argument. Use ?wt_download_report to view options.",
         call. = TRUE)
  }

  # Allowable reports for each sensor
  cam <- c("image", "tag", "megadetector", "definitions")
  aru <- c("summary", "birdnet", "task", "tag", "definitions")
  pc <- c("report", "definitions")

  # Check that the user supplied a valid report type depending on the sensor
  if(sensor_id == "CAM" & !all(report %in% cam)) {
    stop("Please supply a valid report type. Use ?wt_download_report to view options.", call. = TRUE)
  }

  if(sensor_id == "ARU" & !all(report %in% aru)) {
    stop("Please supply a valid report type. Use ?wt_download_report to view options.", call. = TRUE)
  }

  if(sensor_id == "PC" & !all(report %in% pc)) {
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

  # Prepare temporary file:
  tmp <- tempfile(fileext = ".zip")
  # tmp directory
  td <- tempdir()

  # Create POST request
  r <- httr::POST(
    httr::modify_url("https://www-api.wildtrax.ca", path = "/bis/download-report"),
    query = list(
      projectIds = project_id,
      sensorId = sensor_id,
      splitLocation = FALSE
    ),
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
  abstract <- list.files(td, pattern = "*_abstract.csv", full.names = TRUE)
  file.remove(abstract)

  # List data files, read into R as a list
  files <- gsub(".csv", "", list.files(td, pattern = ".csv"))
  files.full <- list.files(td, pattern = ".csv", full.names = TRUE)
  x <- purrr::map(.x = files.full, .f = ~ read.csv(., fileEncoding = "UTF-8-BOM")) |>
    purrr::set_names(files)

  # Remove weather columns, if desired
  if(weather_cols) {
    x
  } else {
    x <- purrr::map(.x = x, .f = ~ (.x[, !grepl("^daily|^hourly", colnames(.x))]))
  }

  # Return the requested report(s)
  report <- paste(report, collapse = "|")
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






