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
#' @return
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
       dplyr::select(project = fullNm, project_id = id, sensorId, year, tags, status)

  return(x)

}

#' Get download summary
#'
#' @description Obtain a table listing projects that the user is able to download data for
#'
#' @param project_id Numeric; the project ID number that you would like to download data for. Use `wt_get_download_summary() to retrieve these IDs.`
#' @param sensor_id Can either be "ARU" or "CAM"
#' @param cols_def Logical; Do you want to include the column definitions? Defaults to FALSE
#' @param weather_cols Logical; Do you want to include weather information for your stations? Defaults to TRUE
#'
#' @import httr purrr dplyr
#' @importFrom readr read_csv
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
#' @return If cols_def is TRUE, a list is returned with three elements: the data, english, and french column definitions; if FALSE, the data is returned as a dataframe.
#'
wt_download_report <- function(project_id, sensor_id, cols_def = FALSE, weather_cols = TRUE) {

  # Check if authentication has expired:
  if (.wt_auth_expired())
    stop("Please authenticate with wt_auth().", call. = FALSE)

  # Check if the project_id is valid:
  i <- wt_get_download_summary(sensor_id = sensor_id)
  i <- unlist(i$project_ids)

  if (!project_id %in% i) {
    stop("The project_id you specified is not among the projects you are able to download for.", call. = TRUE)
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
  httr::POST(
    httr::modify_url("https://www-api.wildtrax.ca", path = "/bis/download-report"),
    query = list(
      projectIds = project_id,
      sensorId = sensorId,
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
      status_code(r),
      content(r)$message),
      call. = FALSE)

  # Unzip
  unzip(tmp, exdir = td)

  files <- list.files(td, pattern = ".csv")
  files.full <- list.files(td, pattern = ".csv", full.names = TRUE)

  x <- purrr::map(.x = files.full, .f = readr::read_csv) %>%
    purrr::set_names(files)

  # Remove weather columns, if desired
  if(weather_cols) {
    x
  } else {
    x <- purrr::map(.x = x, .f = ~ (.x[, !grepl("^daily|^hourly", colnames(.x))]))
  }

  # Remove english/french column definitions, if desired
  if (cols_def) {
    # Return a list object
    x
  } else {
    # Return a dataframe
    x <- x[!grepl("^english|^french", names(x))]
    x <- x[[1]]
  }

  # Delete csv files
  file.remove(files.full)
  # Delete tmp
  file.remove(tmp)

  return(x)

}
