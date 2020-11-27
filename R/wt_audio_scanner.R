#' Scan and extract metadata from audio data
#'
#' @description Scans directories of audio data and returns the file path, file name, file size, date, time, location name,
#' sample rate, length (seconds) and number of channels to be used as filters for other uses
#'
#' @param path Character; The path to the directory with audio files you wish to scan. Can be done recursively.
#' @param file_type Character; Takes one of three values: wav, wac, or both. Use "both" if your directory contains both types of files.
#'
#' @import future fs furrr tibble dplyr tidyr stringr tools lubridate pipeR tuneR bioacoustics purrr
#' @importFrom rlang env_has current_env
#' @export
#'
#' @examples
#' \dontrun{
#' df <- wt_audio_scanner(path = "C:/Users/me/path/to/audio/files", file_type = "both")
#' }
#'
#' @return A dataframe with a summary of your audio files

wt_audio_scanner <- function(path, file_type) {

  # Create regex for file_type
  if (file_type == "wav") {
    file_type_reg <- "\\.wav$"
  } else if (file_type == "wac") {
    file_type_reg <- "\\.wac$"
  } else if (file_type == "both") {
    file_type_reg <- "\\.wav$|\\.wac$"
  } else {
    # Throw error if the file_type is not set to wav, wac, or both.
    stop ("For now, this function can only be used for wav and/or wac files. Please specify either 'wac', 'wav', or 'both' with the file_type argument.")
  }

  # Plan how to resolve a future
  future::plan(multisession)

  # Scan files, gather metadata
  df <- fs::dir_ls(path = path,
                   recurse = TRUE,
                   regexp = file_type_reg) %>>%
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
    # Create date/time fields
    dplyr::mutate(
      recording_date_time = lubridate::ymd_hms(recording_date_time),
      julian = lubridate::yday(recording_date_time),
      year = lubridate::year(recording_date_time),
      gps_enabled = dplyr::case_when(
        grepl('$', file_name) ~ TRUE,
        TRUE ~ FALSE)
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
  df_wav <- df %>%
    dplyr::filter(file_type == "wav") %>%
    dplyr::mutate(data = furrr::future_map(.x = file_path,
                                           .f = ~ tuneR::readWave(.x, from = 0, to = Inf, units = "seconds", header = TRUE),
                                           .progress = TRUE,
                                           .options = future_options(seed = TRUE))) %>%
    dplyr::mutate(length_seconds = purrr::map_dbl(.x = data, .f = ~ round(purrr::pluck(.x[["samples"]]) / purrr::pluck(.x[["sample.rate"]]))),
                  sample_rate = purrr::map_dbl(.x = data, .f = ~ purrr::pluck(.x[["sample.rate"]])),
                  n_channels = purrr::map_dbl(.x = data, .f = ~ purrr::pluck(.x[["channels"]]))) %>%
    dplyr::select(-data)
  }

  # wac files next
  if ("wac" %in% df$file_type) {
  df_wac <- df %>%
    dplyr::filter(file_type == "wac") %>>%
    "Obtaining sampling rate from wac files ..." %>>%
    dplyr::mutate(sample_rate = furrr::future_map_dbl(.x = file_path,
                                                      .f = ~ bioacoustics::read_audio(.x, from = 0, to = Inf)@samp.rate,
                                                      .progress = TRUE,
                                                      .options = furrr_options(seed = TRUE))) %>>%
    "Retrieve number of channels from wac files ..." %>>%
    dplyr::mutate(n_channels = furrr::future_map_dbl(.x = file_path,
                                                     .f = ~ bioacoustics::read_audio(.x, from = 0, to = Inf)@stereo,
                                                     .progress = TRUE,
                                                     .options = furrr_options(seed = TRUE))) %>>%
    "Calculate sample rate from each wac file ... " %>>%
    dplyr::mutate(samples = furrr::future_map_dbl(.x = file_path,
                                                  .f = ~ length(bioacoustics::read_audio(.x, from = 0, to = Inf)@left),
                                                  .progress = TRUE,
                                                  .options = furrr_options(seed = TRUE))) %>%
    dplyr::mutate(length_seconds = samples / sample_rate) %>%
    dplyr::select(-samples)
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
























