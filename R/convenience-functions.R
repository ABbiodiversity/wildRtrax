#' # Set of general convenience functions
#'
#' @section `wt_location_distances` details:
#'
#' @description
#'
#' @param input_from_tibble Use a tibble constructed with a distinct list of location names, latitude and longitude
#' @param input_from_file Use a file downloaded from either an organization or project
#'
#' @import dplyr tibble tidyr sf %>%
#' @export
#'
#' @examples
#' \dontrun{
#' df <- wt_location_distances(input = my_location_tibble, input_from_file)
#' }
#'
#' @return A three-column titble with the distances between each location


wt_location_distances <- function(input_from_tibble = NULL, input_from_file = NULL) {

  if (is.null(input_from_tibble) & is.null(input_from_file)) {
    stop(
      "Please supply either a tibble or a path to the location list.",
      call. = TRUE
    )
  } else if (!is.null(input_from_tibble) & !is.null(input_from_file)) {
    stop("Please only supply one of tibble or file.", call. = TRUE)
  }

  if (is.null(input_from_file)) {
    inp <- input_from_tibble
  } else
    inp <- readr::read_csv(input_from_file)

  l <- nrow(inp)

  locs <- inp %>%
    dplyr::filter(!is.na(latitude) | !is.na(longitude))

  m <- nrow(locs)

  n <- m - l

  if (n > 0) {
    message(n, 'X rows were skipped as they did not contain a latitude or longitude value.')
  } else {
    message('All rows have a latitude and longitude! Creating the matrix...')
  }

  locs <- locs %>%
    dplyr::select(location, latitude, longitude) %>%
    dplyr::distinct() %>%
    sf::st_as_sf(., coords = c("longitude","latitude"), crs = 4326) %>%
    dplyr::select(location, geometry) %>%
    dplyr::mutate(id = row_number())

  distances <- sf::st_distance(locs, locs)

  location_ids <- locs %>%
    tibble::as_tibble() %>%
    dplyr::select(location, id) %>%
    dplyr::relocate(id)

  final_distances <- distances %>%
    tibble::as_tibble() %>%
    tibble::rownames_to_column(var = "location_from") %>%
    tidyr::pivot_longer(cols = -location_from, names_to = "distance_to", values_to = "distance") %>%
    mutate(distance_to = str_replace(distance_to, "V","")) %>%
    dplyr::mutate_at(vars(location_from, distance, distance_to), as.numeric) %>%
    dplyr::filter(!distance == 0) %>%
    dplyr::left_join(., location_ids, by = c("location_from" = "id")) %>%
    dplyr::left_join(., location_ids, by = c("distance_to" = "id")) %>%
    dplyr::select(location.x, location.y, distance) %>%
    dplyr::rename("location_from" = 1) %>%
    dplyr::rename("distance_to" = 2) %>%
    dplyr::select(location_from, distance_to, distance)

  return(final_distances)

}

#'
#' @section `wt_chop` details:
#'
#' @description "Chops" up a wav file into many smaller files of a desired duration
#'
#' @param input A tibble; A single row from a \code(`wt_audio_scanner`) tibble
#' @param segment_length Numeric; Segment length in seconds. Modulo recording will be exported should there be any trailing time left depending on the segment length used
#' @param output_folder Character; output path to where the segments will be stored
#'
#' @import tuneR future furrr lubridate %>% dplyr pipeR
#' @export
#'
#' @examples
#' \dontrun{
#' wt_chop(input = my_audio_tibble %>% slice(1), segment_length = 60, output_folder "/where/i/store/my/chopped/files")
#'
#' df %>%
#'    dplyr::rowwise() %>%
#'    ~purrr::map_lgl(.x = ., ~wt_chop(., segment_length = 60, output_folder = "where/i/store/my/chopped/files")
#
#' }
#'
#' @return Segmented files written to the output_folder
#'

wt_chop <- function(input = NULL, segment_length = NULL, output_folder = NULL) {

  future::plan(multisession)

  # if (!file.exists(input)) {
  #   stop('There is no file.')
  # }

  outroot <- output_folder

  if (!dir.exists(outroot)) {
    stop('The output directory does not exist.')
  }

  inp <- input %>%
    dplyr::select(file_path,
                  recording_date_time,
                  location,
                  file_type,
                  length_seconds)


  length_sec <- inp %>% pluck('length_seconds')

  if (segment_length > length_sec){
    stop('Segment is longer than duration')
  }

  start_times = seq(0, length_sec - segment_length, by = segment_length)
  val <- max(start_times) + segment_length

  if (val < length_sec){
    inp %>>%
      "Chopping the modulo recording" %>>%
      furrr::future_pmap(
        ..1 = .$file_path,
        ..2 = .$recording_date_time,
        ..3 = .$location,
        ..4 = .$file_type,
        ..5 = .$length_seconds,
        .f = ~ tuneR::writeWave(tuneR::readWave(..1, from = val, to = ..5, units = "seconds"),
                                filename = paste0(outroot, ..3, "_", format(..2 + lubridate::seconds(val), "%Y%m%d_%H%M%S"), ".", ..4),
                                extensible = T),
        .options = furrr::furrr_options(seed = T)
      )
  } else {
    message("No modulos found. Chopping the regular segments")
  }

  for (i in seq_along(start_times)) {
       inp %>>%
       "Chopping the regular segments" %>>%
         furrr::future_pmap(
          ..1 = .$file_path,
          ..2 = .$recording_date_time,
           ..3 = .$location,
          ..4 = .$file_type,
         .f = ~ tuneR::writeWave(tuneR::readWave(..1, from = start_times[[i]], to = start_times[[i]] + segment_length, units = "seconds"),
                                    filename = paste0(outroot, ..3, "_", format(..2 + lubridate::seconds(start_times[[i]]), "%Y%m%d_%H%M%S"), ".", ..4),
                                    extensible = T),
         .options = furrr::furrr_options(seed = T)
         )
  }
}


