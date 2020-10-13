#' Scans directories of audio data and returns the filepath, filename, file size, date, time, station key, sample rate, length /(s)/ and number of channels to be used as filters for wt_aru_assign or other uses
#'
#' @param path The path to the directory with audio files you wish to scan (character)
#' @param file_type Can be either wac or wav file types (character)
#'
#' @import future fs furrr tibble dplyr tidyr stringr tools lubridate pipeR tuneR bioacoustics purrr
#' @export
#'
#' @examples
#' \dontrun{
#' df <- wt_audio_scanner(path = "my_path", file_type = "\\.wav$|\\.wac$")
#' }
#'
#' @return A dataframe with a summary of your audio files

wt_audio_scanner <- function(path, file_type) {

  # Plan how to resolve a future
  future::plan(multisession)

  df <- fs::dir_ls(path = path,
                   recurse = TRUE,
                   regexp = file_type) %>>%
    "Scanning audio files in path ..." %>>%
    furrr::future_map_dbl(., .f = ~ fs::file_size(.), .progress = TRUE) %>%
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
      sep = "(?:_0\\+1_|_)",
      extra = "merge",
      remove = FALSE
    ) %>%
    dplyr::mutate(
      recording_date_time = lubridate::ymd_hms(recording_date_time),
      julian = lubridate::yday(recording_date_time),
      year = lubridate::year(recording_date_time)
    ) %>%
    dplyr::arrange(location, recording_date_time) %>%
    # Create time index
    dplyr::group_by(location, year, julian) %>%
    dplyr::mutate(time_index = row_number()) %>%
    dplyr::ungroup()

  df_wav <- df %>%
    dplyr::filter(file_type == "wav") %>%
    dplyr::mutate(data = furrr::future_map(.x = file_path,
                                           .f = ~ tuneR::readWave(.x, from = 0, to = Inf, units = "seconds", header = TRUE),
                                           .progress = TRUE)) %>%
    dplyr::mutate(length_seconds = purrr::map_dbl(.x = data, .f = ~ round(purrr::pluck(.x[["samples"]]) / purrr::pluck(.x[["sample.rate"]]))),
                  sample_rate = purrr::map_dbl(.x = data, .f = ~ purrr::pluck(.x[["sample.rate"]])),
                  n_channels = purrr::map_dbl(.x = data, .f = ~ purrr::pluck(.x[["channels"]]))) %>%
    dplyr::select(-data)

  df_wac <- df %>%
    dplyr::filter(file_type == "wac") %>>%
    "Obtaining sampling rate from wac files ..." %>>%
    dplyr::mutate(sample_rate = furrr::future_map_dbl(.x = file_path,
                                               .f = ~ bioacoustics::read_audio(.x, from = 0, to = Inf)@samp.rate,
                                               .progress = TRUE,
                                               .options = future_options(seed = TRUE))) %>>%
    "Retrieve number of channels from wac files ..." %>>%
    dplyr::mutate(sample_rate = furrr::future_map_dbl(.x = file_path,
                                                      .f = ~ bioacoustics::read_audio(.x, from = 0, to = Inf)@stereo,
                                                      .progress = TRUE,
                                                      .options = future_options(seed = TRUE))) %>>%
    "Calculate sample rate from each wac file ... " %>>%
    dplyr::mutate(sample_rate = furrr::future_map_dbl(.x = file_path,
                                                      .f = ~ bioacoustics::read_audio(.x, from = 0, to = Inf)@left,
                                                      .progress = TRUE,
                                                      .options = future_options(seed = TRUE))) %>%
    dplyr::mutate(length_seconds = samples / sample_rate) %>%
    dplyr::select(-samples)

  df_bind <- dplyr::bind_rows(df_wav, df_wac)

  return(df_bind)

}
