#' Evaluate independent detections in your camera data
#'
#' @description Create independent detections dataframe using camera data from WildTrax
#'
#' @param x A dataframe of camera data; preferably, the output of `wt_download_report()`.
#' @param threshold Numeric; time interval to parse out independent detections.
#' @param units The threshold unit. Can be one of three values, "seconds", "minutes", "hours".
#' @param datetime_col Defaults to `date_detected`; The column indicating the timestamp of the image.
#' @param remove_human Logical; Should human and human-related tags (e.g. vehicles) be removed? Defaults to TRUE.
#' @param remove_domestic Logical; Should domestic animal tags (e.g. cows) be removed? Defaults to TRUE.
#'
#' @import dplyr lubridate
#' @importFrom stringr str_detect
#' @export
#'
#' @examples
#' \dontrun{
#' detections <- wt_ind_detect(x = df, threshold = 30, units = "minutes")
#' }
#'
#' @return A dataframe of independent detections in your camera data, based on the threshold you specified. The df wil include information about the duration of each detection, the number of images, the average number of individual animals per image, and the max number of animals in the detection.
#'
wt_ind_detect <- function(x, threshold, units = "minutes",  datetime_col = date_detected, remove_human = TRUE, remove_domestic = TRUE) {

  # Check that x is a dataframe
  if (!is.data.frame(x)) {
    stop("The first argument must supply a dataframe.")
  }

  # Ensure that datetime_col is of class POSIXct; if not, try to convert.
  name <- enquo(datetime_col) %>% quo_name()
  if(!inherits(x[[name]], c("POSIXct"))) {
    x <- x %>% mutate({{datetime_col}} := lubridate::as_datetime({{datetime_col}}))
    message("Your datetime_col has been converted to a Date.")
  }

  # Check if x contains the required columns - standard output from WildTrax. Probably should make this more flexible.
  req_cols <- c("project", "location", "field_of_view", "scientific_name", "common_name", "number_individuals")
  if (!all(req_cols %in% colnames(x))) {
    stop("Important columns are missing from the data you have supplied.")
  }

  # Check that the units argument is either seconds, minutes, or hours
  if (!units %in% c("seconds", "minutes", "hours")) {
    stop("Please use 'seconds', 'minutes', or 'hours' as your threshold unit.")
  }

  # Convert threshold to seconds
  if (units == "minutes") {
    threshold <- threshold  * 60
  } else if (units == "hours") {
    threshold <- threshold * 60 * 60
  } else {
    threshold
  }

  # Tags to discard - not sure if UNKNOWN is ever wanted?
  t <- c("NONE", "STAFF/SETUP", "UNKNOWN")
  if (remove_human) {
    # Standard WildTrax tags that refer to human(ish) objects
    t <- c(t, "Human", "Vehicle", "Unknown Vehicle", "All Terrain Vehicle", "Train", "Heavy Equipment")
  }
  x <- filter(x, !common_name %in% t)
  if (remove_domestic) {
    # All tags in WildTrax that refer to domestic animals begin with 'Domestic __'
    x <- filter(x, !str_detect(common_name, "^Domestic"))
  }

  # Create ordered dataframe, and calculate time interval between images.
  x1 <- x %>%
    # Remove images outside of the camera field-of-view
    filter(field_of_view == "WITHIN") %>%
    # Sometimes VNA sneaks in here
    mutate(number_individuals = as.numeric(ifelse(number_individuals == "VNA", 1, number_individuals))) %>%
    # Amalgamate tags of same species in same image; currently broken into two separate rows
    group_by(location, {{datetime_col}}, common_name) %>%
    mutate(number_individuals = sum(number_individuals)) %>%
    distinct(location, {{datetime_col}}, common_name, number_individuals, .keep_all = TRUE) %>%
    ungroup() %>%
    # Order the dataframe
    arrange(project, location, {{datetime_col}}, common_name) %>%
    group_by(project, location, common_name) %>%
    # Calculate the time difference between subsequent images
    mutate(interval = int_length({{datetime_col}} %--% lag({{datetime_col}}))) %>%
    # Is this considered a new detection?
    mutate(new_detection = ifelse(is.na(interval) | abs(interval) >= threshold, TRUE, FALSE)) %>%
    ungroup() %>%
    # Number independent detections
    mutate(detection = c(1, cumsum(new_detection[-1]) + 1))

  # Summarise detections
  x2 <- x1 %>%
    group_by(detection, project, location, common_name, scientific_name) %>%
    summarise(start_time = min({{datetime_col}}),
              end_time = max({{datetime_col}}),
              total_duration_seconds = int_length(start_time %--% end_time),
              n_images = n(),
              avg_animals = mean(number_individuals),
              max_animals = max(number_individuals))

  # Return x2
  return(x2)

}
