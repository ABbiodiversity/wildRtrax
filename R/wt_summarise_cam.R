#' Summarise your camera data by location, time interval, and species.
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
#' @importFrom stringr str_detect
#' @importFrom rlang is_missing
#' @export
#'
#' @examples
#' \dontrun{
#' summary <- wt_summarise_cam(x, y, time_interval = "day", variable = "detections", output_format = "wide")
#' }
#'
#' @return A dataframe summarising your camera data by location, time interval, and species.
#'
wt_summarise_cam <- function(detect_data, raw_data, time_interval = "day", variable = "detections", output_format = "wide",
                             species_col = common_name, effort_data = NULL, project_col = NULL, station_col = NULL,
                             start_col = NULL, end_col = NULL) {

  # Make sure one of raw_data or effort_data is supplied
  if (is_missing(raw_data) & is.null(effort_data)) {
    stop("Please supply a value for one of `raw_data` or `effort_data`.")
  }

  # Check that only one is supplied
  if(!is_missing(raw_data) & !is.null(effort_data)) {
    stop("Please only supply a value for one of `raw_data` or `effort_data`.")
  }

  # Parse the raw or effort data to get time ranges for each camera deployment.
  if (!is_missing(raw_data)) {
    x <- raw_data %>%
      mutate(date_detected = ymd_hms(date_detected)) %>%
      group_by(project, location) %>%
      summarise(start_date = as.Date(min(date_detected)),
                end_date = as.Date(max(date_detected))) %>%
      ungroup()
  } else {
    x <- effort_data %>%
      select(project = {{project_col}},
             location = {{station_col}},
             start_date = {{start_col}},
             end_date = {{end_col}}) %>%
      ungroup()
  }

  # Expand the time ranges into individual days of operation (smallest unit)
  x <- x %>%
    group_by(project, location) %>%
    mutate(day = list(seq.Date(start_date, end_date, by = "day"))) %>%
    unnest(day) %>%
    select(project, location, day)

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
      group_by(project, location, week) %>%
      tally(name = "n_days_effort") %>%
      ungroup()
    z <- x %>%
      crossing(sp) %>%
      left_join(y) %>%
      mutate(across(6:8, ~ replace_na(.x, 0)))
  } else if (time_interval == "month") {
    x <- x %>%
      mutate(month = month(day, label = TRUE, abbr = FALSE)) %>%
      group_by(project, location, month) %>%
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
      group_by(project, location, {{species_col}}) %>%
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
