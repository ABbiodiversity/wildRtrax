#' Scan and extract metadata from audio data
#'
#' @description Scans directories of audio data and returns the file path, file name, file size, date, time, location name,
#' sample rate, length (seconds) and number of channels to be used as filters for other uses
#'
#' @param path Character; The path to the directory with audio files you wish to scan. Can be done recursively.
#' @param file_type Character; Takes one of three values: wav, wac, or both. Use "both" if your directory contains both types of files.
#' @param tz Character; Forces a timezone to each of the recording files; if the time falls into a daylight savings time break, wt_audio_scanner will assume the next valid time
#'
#' @import future fs furrr tibble dplyr tidyr stringr tools pipeR tuneR purrr
#' @importFrom lubridate year ymd_hms yday force_tz
#' @importFrom rlang env_has current_env
#' @export
#'
#' @examples
#' \dontrun{
#' df <- wt_audio_scanner(path = "C:/Users/me/path/to/audio/files", file_type = "both", tz = "US/Mountain")
#' }
#'
#' @return A dataframe with a summary of your audio files

wt_audio_scanner <- function(path, file_type, tz = "") {

  # Create regex for file_type
  if (file_type == "wav" || file_type == "WAV") {
    file_type_reg <- "\\.wav$|\\.WAV$"
  } else if (file_type == "wac") {
    file_type_reg <- "\\.wac$"
  } else if (file_type == "both") {
    file_type_reg <- "\\.wav$|\\.wac$|\\.WAV$"
  } else {
    # Throw error if the file_type is not set to wav, wac, or both.
    stop ("For now, this function can only be used for wav and/or wac files. Please specify either 'wac', 'wav', or 'both' with the file_type argument.")
  }

  # Plan how to resolve a future
  future::plan(multisession)

  # Scan files, gather metadata
  df <- fs::dir_ls(path = path,
                   recurse = TRUE,
                   regexp = file_type_reg,
                   fail = FALSE) %>>%
    "Scanning audio files in path ..." %>>%
    furrr::future_map_dbl(., .f = ~ fs::file_size(.), .progress = TRUE, .options = furrr_options(seed = TRUE)) %>%
    tibble::enframe() %>%
    # Convert file sizes to megabytes
    dplyr::mutate(size_Mb = round(value / 10e5, digits = 2)) %>%
    dplyr::select(file_path = name, size_Mb) %>%
    dplyr::mutate(file_name = stringr::str_replace(basename(file_path), "\\..*", "")) %>%
    dplyr::mutate(file_type = tools::file_ext(file_path)) %>%
    # Parse location and recording date time
    tidyr::separate(
      file_name,
      into = c("location", "recording_date_time"),
      sep = "(?:_0\\+1_|_|__0__|__1__)", # Strips Wildlife Acoustics SM3 file naming convention for channels
      extra = "merge",
      remove = FALSE
    ) %>%
    dplyr::mutate(recording_date_time = str_remove(recording_date_time,'.+?(?:__)')) %>%
    # Create date/time fields
    dplyr::mutate(
      #Apply the timezone if necessary
      recording_date_time = case_when(tz == "" ~ lubridate::ymd_hms(recording_date_time),
                                      TRUE ~ lubridate::force_tz(lubridate::ymd_hms(recording_date_time), tzone = tz, roll = TRUE)),
      julian = lubridate::yday(recording_date_time),
      year = lubridate::year(recording_date_time),
      gps_enabled = dplyr::case_when(
        grepl('\\$', file_name) ~ TRUE),
      year = lubridate::year(recording_date_time)
    ) %>%
    dplyr::arrange(location, recording_date_time) %>%
    # Create time index
    dplyr::group_by(location, year, julian) %>%
    dplyr::mutate(time_index = dplyr::row_number()) %>%
    dplyr::ungroup()

  # Check whether anything was returned
  if (nrow(df) == 0) {
    stop ("There were no files of the type specified in file_path in the directory path specified.")
  }

  # wav files first
  if ("wav" %in% df$file_type) {
  df_wav <- df %>>%
    "Working on wav files..." %>>%
    dplyr::filter(file_type == "wav",
                  size_Mb > 0) %>%
    dplyr::mutate(data = furrr::future_map(.x = file_path,
                                           .f = ~ tuneR::readWave(.x, from = 0, to = Inf, units = "seconds", header = TRUE),
                                           .progress = TRUE,
                                           .options = furrr_options(seed = TRUE))) %>%
    dplyr::mutate(length_seconds = purrr::map_dbl(.x = data, .f = ~ round(purrr::pluck(.x[["samples"]]) / purrr::pluck(.x[["sample.rate"]]))),
                  sample_rate = purrr::map_dbl(.x = data, .f = ~ purrr::pluck(.x[["sample.rate"]])),
                  n_channels = purrr::map_dbl(.x = data, .f = ~ purrr::pluck(.x[["channels"]]))) %>%
    dplyr::select(-data)
  }

  if("wac" %in% df$file_type) {
    df_wac <- df %>>%
    "Working on wac files..." %>>%
      dplyr::filter(file_type == "wac",
                    size_Mb > 0) %>%
      dplyr::mutate(info = purrr::map(.x = file_path, .f = ~ wt_wac_info(.x)),
                    sample_rate = purrr::map_dbl(.x = info, .f = ~ purrr::pluck(.x[["sample_rate"]])),
                    length_seconds = purrr::map_dbl(.x = info, .f = ~ purrr::pluck(.x[["length_seconds"]])),
                    n_channels = purrr::map_dbl(.x = info, .f = ~ purrr::pluck(.x[["n_channels"]]))) %>%
      dplyr::select(-info)
  }

  # Stitch together
  if (!rlang::env_has(rlang::current_env(), "df_wav")) {
    df_final <- df_wac
  } else if (!rlang::env_has(rlang::current_env(), "df_wac")) {
    df_final <- df_wav
  } else {
    df_final <- dplyr::bind_rows(df_wac, df_wav)
  }

  # Return final data frame
  return(df_final)

}

#' Scrape relevant information from wac (Wildlife Acoustics) file
#'
#' @param path Character; The wac file path
#'
#' @import tools
#'
#' @return a list with relevant information

wt_wac_info <- function(path) {

  if(tools::file_ext(path) != "wac") {
    stop("file type not supported by this function.")
  }

  f <- file(path, open = "rb")
  on.exit(close(f))

  name <- readChar(f, 4)
  version <- readBin(con = f, what = integer(), size = 1, endian = "little")
  n_channels <- readBin(con = f, what = integer(), size = 1, endian = "little")
  frame_size <- readBin(con = f, what = integer(), size = 2, endian = "little")
  block_size <-  readBin(con = f, what = integer(), size = 2, endian = "little")
  flags <-  readBin(con = f, what = integer(), size = 2, endian = "little")
  sample_rate <-  readBin(con = f, what = integer(), size = 4, endian = "little")
  samples <- readBin(con = f, what = integer(), size = 4, endian = "little")

  if(n_channels == 1) {
    stereo <- FALSE
  } else {
    stereo <- TRUE
  }

  length_seconds = samples / sample_rate

  return(out = list(sample_rate = sample_rate,
                    n_channels = n_channels,
                    length_seconds = length_seconds))

}
