#' Set of analysis functions
#'
#' @section Summarise camera data by location, time interval, and species.
#'
#' @description This function takes your independent detection data and summarises it by location, specified time interval, and species.
#'
#' @param detect_data Detection data generated from `wt_ind_det()`.
#' @param raw_data The raw camera tag data, which is used to infer the effort (i.e. date ranges of operation) for each camera. Optionally, can supply effort_data directly instead.
#' @param time_interval Character; Can be either "full", "month", "week", or "day".
#' @param variable Character; Can be either "presence", "detections", "counts", or "all" (if you want all three).
#' @param output_format Character; The format of the dataframe returned to you. Can be either "wide" or "long".
#' @param species_col Defaults to `common_name`. The column referring to species. Use to switch between common and scientific names of species, if you have both.
#' @param effort_data Optionally supply your own effort data.
#' @param project_col The column referring to project in your effort data.
#' @param station_col The column referring to each individual camera station/location in your effort data.
#' @param start_col The column indicating the start date of the camera location
#' @param end_col The column indicating the end date of the camera location
#'
#' @import dplyr lubridate tidyr
#' @export
#'
#' @examples
#' \dontrun{
#' summary <- wt_summarise_cam(
#' x, y, time_interval = "day", variable = "detections", output_format = "wide"
#' )
#' }
#'
#' @return A dataframe summarising your camera data by location, time interval, and species.

wt_summarise_cam <- function(detect_data, raw_data, time_interval = "day", variable = "detections", output_format = "wide",
                             species_col = species_common_name, effort_data = NULL, project_col = NULL, station_col = NULL,
                             start_col = NULL, end_col = NULL) {

  # Make sure one of raw_data or effort_data is supplied
  if (rlang::is_missing(raw_data) & is.null(effort_data)) {
    stop("Please supply a value for one of `raw_data` or `effort_data`.")
  }

  # Check that only one is supplied
  if(!rlang::is_missing(raw_data) & !is.null(effort_data)) {
    stop("Please only supply a value for one of `raw_data` or `effort_data`.")
  }

  # Parse the raw or effort data to get time ranges for each camera deployment.
  if (!rlang::is_missing(raw_data)) {
    x <- raw_data %>%
      mutate(image_date_time = ymd_hms(image_date_time)) %>%
      group_by(project_id, location) %>%
      summarise(start_date = as.Date(min(image_date_time)),
                end_date = as.Date(max(image_date_time))) %>%
      ungroup()
  } else {
    x <- effort_data %>%
      select(project_id = {{project_col}},
             location = {{station_col}},
             start_date = {{start_col}},
             end_date = {{end_col}}) %>%
      ungroup()
  }

  # Expand the time ranges into individual days of operation (smallest unit)
  x <- x %>%
    group_by(project_id, location) %>%
    mutate(day = list(seq.Date(start_date, end_date, by = "day"))) %>%
    unnest(day) %>%
    select(project_id, location, day)

  int <- c("day", "week", "month", "full")
  if (!time_interval %in% int) {
    stop("Please select a valid time interval: 'day', 'week', 'month', or 'full'.")
  }

  # Based on the desired timeframe, assess when each detection occurred
  if (time_interval == "day" | time_interval == "full") {
    y <- detect_data %>%
      mutate(day = as.Date(start_time))
  } else if (time_interval == "week") {
    y <- detect_data %>%
      mutate(week = isoweek(start_time))
  } else if (time_interval == "month") {
    y <- detect_data %>%
      mutate(month = month(start_time, label = TRUE, abbr = FALSE))
  }

  # Summarise variable of interest
  y <- y %>%
    group_by(across(c(2:3, {{species_col}}, last_col()))) %>%
    summarise(detections = n(),
              counts = sum(max_animals)) %>%
    ungroup() %>%
    mutate(presence = ifelse(detections > 0, 1, 0))

  # Species present in the data
  sp <- y %>% select({{species_col}}) %>% distinct()

  # Create long df object of all species x location x timeframe combos
  if (time_interval == "day") {
    z <- x %>%
      mutate(n_days_effort = 1) %>%
      crossing(sp) %>%
      left_join(y) %>%
      mutate(across(6:8, ~ replace_na(.x, 0)))
  } else if (time_interval == "week") {
    x <- x %>%
      mutate(week = isoweek(day)) %>%
      group_by(project_id, location, week) %>%
      tally(name = "n_days_effort") %>%
      ungroup()
    z <- x %>%
      crossing(sp) %>%
      left_join(y) %>%
      mutate(across(6:8, ~ replace_na(.x, 0)))
  } else if (time_interval == "month") {
    x <- x %>%
      mutate(month = month(day, label = TRUE, abbr = FALSE)) %>%
      group_by(project_id, location, month) %>%
      tally(name = "n_days_effort") %>%
      ungroup()
    z <- x %>%
      crossing(sp) %>%
      left_join(y) %>%
      mutate(across(6:8, ~ replace_na(.x, 0)))
  } else if (time_interval == "full") {
    z <- x %>%
      crossing(sp) %>%
      left_join(y) %>%
      mutate(across(5:7, ~ replace_na(.x, 0))) %>%
      group_by(project_id, location, {{species_col}}) %>%
      summarise(detections = sum(detections),
                counts = sum(counts),
                presence = ifelse(any(presence == 1), 1, 0)) %>%
      ungroup()
  }

  if (variable == "all") {
    variable <- c("detections", "counts", "presence")
  }

  # Make wide if desired, using
  if (output_format == "wide") {
    z <- z %>%
      pivot_wider(id_cols = 1:4, names_from = {{species_col}}, values_from = {{variable}}, names_sep = ".")
  } else if (output_format == "long") {
    z <- z %>% select(1:5, {{variable}}) %>%
      pivot_longer(cols = {{variable}}, names_to = "variable", values_to = "value")
  } else {
    z
    # If neither 'wide' or 'long' is specified, just return z without pivoting plus a message.
    message("Please specify `wide` or `long` in the output_format argument.")
  }

  return(z)

}

#' Evaluate independent camera detections
#'
#' @description Create an independent detections dataframe using camera data from WildTrax
#'
#' @param x A dataframe of camera data; preferably, the main report from `wt_download_report()`.
#' @param threshold Numeric; time interval to parse out independent detections.
#' @param units The threshold unit. Can be one of three values, "seconds", "minutes", "hours".
#' @param datetime_col Defaults to `image_date_time`; The column indicating the timestamp of the image.
#' @param remove_human Logical; Should human and human-related tags (e.g. vehicles) be removed? Defaults to TRUE.
#' @param remove_domestic Logical; Should domestic animal tags (e.g. cows) be removed? Defaults to TRUE.
#'
#' @import dplyr lubridate
#' @export
#'
#' @examples
#' \dontrun{
#' detections <- wt_ind_detect(x = df, threshold = 30, units = "minutes")
#' }
#'
#' @return A dataframe of independent detections in your camera data, based on the threshold you specified. The df wil include information about the duration of each detection, the number of images, the average number of individual animals per image, and the max number of animals in the detection.

wt_ind_detect <- function(x, threshold, units = "minutes",  datetime_col = image_date_time, remove_human = TRUE, remove_domestic = TRUE) {

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
  req_cols <- c("project_id", "location", "species_common_name", "individual_count")
  if (!all(req_cols %in% colnames(x))) {
    stop("Important columns are missing from the data you have supplied. All of `project_id`, `location`, `species_common_name`, and `individual_count` are required.")
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
  x <- filter(x, !species_common_name %in% t)
  if (remove_domestic) {
    # All tags in WildTrax that refer to domestic animals begin with 'Domestic __'
    x <- filter(x, !stringr::str_detect(species_common_name, "^Domestic"))
  }

  # Create ordered dataframe, and calculate time interval between images.
  x1 <- x %>%
    # Sometimes VNA sneaks in here
    mutate(individual_count = as.numeric(ifelse(individual_count == "VNA", 1, individual_count))) %>%
    # Amalgamate tags of same species in same image; currently broken into two separate rows
    group_by(location, {{datetime_col}}, species_common_name) %>%
    mutate(individual_count = sum(individual_count)) %>%
    distinct(location, {{datetime_col}}, species_common_name, individual_count, .keep_all = TRUE) %>%
    ungroup() %>%
    # Order the dataframe
    arrange(project_id, location, {{datetime_col}}, species_common_name) %>%
    group_by(project_id, location, species_common_name) %>%
    # Calculate the time difference between subsequent images
    mutate(interval = int_length({{datetime_col}} %--% lag({{datetime_col}}))) %>%
    # Is this considered a new detection?
    mutate(new_detection = ifelse(is.na(interval) | abs(interval) >= threshold, TRUE, FALSE)) %>%
    ungroup() %>%
    # Number independent detections
    mutate(detection = c(1, cumsum(new_detection[-1]) + 1))

  # Summarise detections
  x2 <- x1 %>%
    group_by(detection, project_id, location, species_common_name) %>%
    summarise(start_time = min({{datetime_col}}),
              end_time = max({{datetime_col}}),
              total_duration_seconds = int_length(start_time %--% end_time),
              n_images = n(),
              avg_animals_per_image = mean(individual_count),
              max_animals = max(individual_count)) %>%
    ungroup()

  # Return x2
  return(x2)

}
