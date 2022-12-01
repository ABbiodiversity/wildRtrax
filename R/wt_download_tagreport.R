#' Download report of tags.
#'
#' @description This function downloads a report of all tags or annotations for an ARU project. The report contains urls for downloading clips of those annotations. This function should be used prior to using the `wt_download_clips()` function.
#'
#' @param project_id Numeric; the project ID number that you would like to download data for. Use `wt_get_download_summary()` to retrieve these IDs.
#' @import httr purrr dplyr
#' @importFrom utils read.csv
#' @export
#'
#' @examples
#' \dontrun{
#' # Authenticate first:
#' wt_auth()
#'
#' tags <- wt_download_tagreport(project_id = 397)
#' }
#'
#' @return Dataframe of annotations and associated metadata.
#'

wt_download_tagreport <- function(project_id) {

  # Check if authentication has expired:
  if (.wt_auth_expired())
    stop("Please authenticate with wt_auth().", call. = FALSE)

  # Check if the project_id is valid:
  i <- wt_get_download_summary(sensor_id = "ARU")
  i <- unlist(i$project_id)

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
  r <- httr::POST(
    httr::modify_url("https://www-api.wildtrax.ca", path = "/bis/download-tag-clips"),
    query = list(
      projectId = project_id
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

  # List data files, read into R as a list
  files <- gsub(".csv", "", list.files(td, pattern = ".csv"))
  files.full <- list.files(td, pattern = ".csv", full.names = TRUE)
  x <- purrr::map(.x = files.full, .f = ~ read.csv(., fileEncoding = "UTF-8-BOM")) |>
    purrr::set_names(files)

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
