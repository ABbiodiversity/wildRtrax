#' Set of analysis functions
#'
#'
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

#'
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

#' Use a series of multivariate analyses to determine observer differences in acoustic processed data
#'
#' @section `wt_ord` details:
#'
#' @description Uses a series of multivariate analyses to determine differences in multiobserver data

#' @param input Character; A wt_download_report tibble
#' @param min_obs Numeric; The minimum number of replicates you want to use. wt_ord will omit
#' @param confidence Numeric; The confidence of the ellipses in the RDA
#'
#' @import lubridate fitdistrplus furrr purrr scales vegan pipeR vctrs extraDistr gamlss tools
#' @export
#'
#' @examples
#' \dontrun{
#' res <- wt_prd(input = data, min_obs = 10, confidence = 0.67)
#' }
#'
#' @return A list containing the following:

wt_ord <- function(input = x, min_obs, confidence) {

  # Filter by the minimum amount of observers. Less takes longer to run
  multi <- input %>%
    group_by(location, recording_date) %>%
    mutate(unique_times = n_distinct(observer)) %>%
    ungroup() %>%
    filter(unique_times >= min_obs) %>%
    select(-unique_times)

  # Set up the data
  multi <- multi %>%
    mutate(abundance = case_when(abundance == "TMTT" ~ "4", abundance == "CI 1" ~ "1", abundance == "CI 2" ~ "2", abundance == "CI 3" ~ "3", TRUE ~ abundance),
           abundance = as.numeric(abundance)) %>%
    group_by(project_name, location, recording_date, species_code, observer) %>%
    mutate(indvs = max(individual_appearance_order)) %>%
    ungroup() %>%
    dplyr::select(project_name, location, recording_date, species_code, observer, indvs, abundance) %>%
    distinct() %>%
    group_by(project_name, location, recording_date, species_code, observer) %>%
    summarise(abundance = case_when(max(abundance) > max(indvs) ~ max(abundance), TRUE ~ max(indvs))) %>%
    ungroup()

  # Create the species matrix
  multi2 <- multi %>%
    dplyr::select(project_name, location, recording_date, observer, species_code, abundance) %>%
    distinct() %>%
    group_by(project_name, location, recording_date, observer, species_code) %>%
    mutate(abundance = as.numeric(max(abundance))) %>%
    ungroup() %>%
    arrange(location, observer, species_code) %>%
    group_by(species_code, project_name, location, recording_date, observer) %>%
    distinct() %>%
    ungroup() %>%
    filter(!species_code %in% abiotic_codes) %>%
    arrange(species_code, project_name, location, recording_date, observer) %>%
    pivot_wider(names_from = species_code, values_from = abundance, values_fill = 0) %>%
    mutate_if(is.integer, as.numeric) %>%
    replace(is.na(.), 0) %>%
    rowwise() %>%
    mutate(total = sum(c_across(`ALFL`:`YRWA`))) %>%
    ungroup() %>%
    filter(!total < 1) %>%
    select(-total)

  # Create the groups
  multi_type <- multi2 %>%
    dplyr::select(location, recording_date, observer) %>%
    distinct()

  if (length(unique(multi2$location)) == 1) {
    stop(print("You need at least two recordings duplicated in order to generate the analysis"))
  }

  # Run the RDA
  ordination <- rda(multi2[,-c(1:4)] ~ observer + location + recording_date, data = multi_type)
  ordination_obs <- rda(multi2[,-c(1:4)] ~ observer, data = multi_type)
  ordination_loc <- rda(multi2[,-c(1:4)] ~ location, data = multi_type)
  ordination_date <- rda(multi2[,-c(1:4)] ~ recording_date, data = multi_type)
  ordination_obs_loc <- rda(multi2[,-c(1:4)] ~ observer + location, data = multi_type)
  ordination_recording <- rda(multi2[,-c(1:4)] ~ location + recording_date, data = multi_type)
  ordination_obs_date <- rda(multi2[,-c(1:4)] ~ observer + recording_date, data = multi_type)

  # Getting R2 for models
  firstmodel <- RsquareAdj(ordination)$adj.r.squared
  obsmodel <- RsquareAdj(ordination_obs)$adj.r.squared
  locmodel <- RsquareAdj(ordination_loc)$adj.r.squared
  datemodel <- RsquareAdj(ordination_date)$adj.r.squared
  obs_plus_locmodel <- RsquareAdj(ordination_obs_loc)$adj.r.squared
  recordingmodel <- RsquareAdj(ordination_recording)$adj.r.squared
  obs_plus_datemodel <- RsquareAdj(ordination_obs_date)$adj.r.squared

  # Print the results of a permutation test for the constrained ordination
  step <- ordistep(ordination, direction = "both")
  u <- ordination$CCA$u %>% as.data.frame()

  showvarparts(2, bg = c("hotpink","skyblue"))
  # Partioning the variance of the RDA
  mod <- varpart(multi2[,-c(1:4)] %>% as.data.frame(), as.factor(multi2$location), as.factor(multi2$observer), transfo="hel")
  ## Use fill colours
  plot(mod, bg = c("hotpink","skyblue"))
  # Alternative way of to conduct this partitioning

  # Set up the output - scores first
  ordination_scores <- scores(ordination_obs, display = "sites") %>%
    as.data.frame() %>%
    rownames_to_column("site") %>%
    bind_cols(., multi_type)

  # Then eigenvectors
  ordination_vect <- scores(ordination_obs, display = "species") %>%
    as.data.frame()

  # Now plot everything for the ordination
  plot_RDA <- ggplot(data = ordination_scores, aes(x = RDA1, y = RDA2)) +
    geom_point(data = ordination_scores, aes(x = RDA1, y = RDA2, colour = observer), alpha = 0.6) +
    stat_ellipse(data = ordination_scores, aes(colour = observer), linetype = 4, type = 'norm', level = 0.67) +
    geom_vline(xintercept = c(0), color = "#A19E99", linetype = 2) +
    geom_hline(yintercept = c(0), color = "#A19E99", linetype = 2) +
    geom_segment(data = ordination_vect, aes(x = 0, y = 0, xend = RDA1, yend = RDA2), arrow = arrow(length = unit(0.2, "cm"))) +
    geom_text(data = ordination_vect, aes(x = RDA1, y = RDA2, label = rownames(ordination_vect))) +
    # Apply ABMI themes
    labs(x = paste0("RDA1 ", round(ordination$CA$eig[[1]],2), '%'),
         y = paste0("RDA2 ", round(ordination$CA$eig[[2]],2), '%'),
         title = paste0("RDA of observer detections constrained by recording")) +
    theme_bw() +
    guides(fill="none") +
    scale_colour_viridis_d()


  # Time to first detection data setup
  dd <- data %>%
    select(location, recording_date,
           observer, species_code, individual_appearance_order,
           tag_start_s, tag_duration_s,  min_tag_freq, max_tag_freq, vocalization, abundance) %>%
    distinct() %>%
    group_by(location, recording_date) %>%
    mutate(unique_times = n_distinct(observer)) %>%
    ungroup() %>%
    filter(unique_times >= 11) %>%
    select(-unique_times) %>%
    relocate(abundance, .after=individual_appearance_order) %>%
    mutate_at(vars(min_tag_freq, max_tag_freq), ~as.numeric(str_replace(.,"kHz",""))) %>%
    group_by(location, recording_date, observer) %>%
    mutate(detection_order = row_number()) %>%
    ungroup() %>%
    mutate(freq_diff = max_tag_freq - min_tag_freq) %>%
    mutate(Unknown = case_when(grepl('^U',species_code) ~ "Unknown", TRUE ~ "Species"))

  # Plot the results
  d <- ggplot(dd, aes(x=tag_start_s,y=detection_order,colour=Unknown,shape=Unknown)) +
    geom_point(alpha = 0.2) +
    geom_smooth(alpha = 0.6) +
    ylim(0,50) +
    facet_wrap(~observer, scales="free_x") +
    ggtitle("Time to first detection of species and unknown tags") +
    xlab("Tag start time (seconds)") + ylab("Detections per recording") +
    scale_colour_viridis_d() +
    theme_bw()

  #Return all objects
  return(list(plot_RDA, # Ordination
              d, # Time to first detection plot
              ordination, # Ordination
              ordination_scores %>% as_tibble(), # Ordination scores
              vegan::anova.cca(ordination, step = 1000, by = "term"), # Permutation Test for RDA (ANOVA-like)
              vegan::adonis(multi2[,-c(1:4)] ~ observer + location + recording_date, data = multi_type, distance = "jaccard"), # PERMANOVA
              vegan::adonis2(multi2[,-c(1:4)] ~ observer + location + recording_date, data = multi_type, distance = "jaccard")))
}

