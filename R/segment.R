library(seewave)
library(tidyverse)
library(tuneR)
library(fs)
library(lubridate)

aseg <- dir_ls(path = "/users/alexandremacphail/desktop/wav2",
               regexp = "*.wav") %>%
  as_tibble() %>%
  mutate(rw = map(.x = value, .f = ~readWave(.x, header = T)),
         length_seconds = map_dbl(.x = rw, .f = ~ round(purrr::pluck(.x[["samples"]]) / purrr::pluck(.x[["sample.rate"]]), 2)),
         breaks = length_seconds/60,
         break_length = length_seconds / breaks,
         filename = tools::file_path_sans_ext(basename(as.character(value)))) %>%
  tidyr::separate(filename, into = c("location", "recording_date_time"), sep = "(?:_0\\+1_|_|__0__|__1__)", extra = "merge", remove = FALSE) %>%
  dplyr::mutate(recording_date_time = str_remove(recording_date_time, '.+?(?:__)'),
                recording_date_time = ymd_hms(recording_date_time, tz = "US/Mountain")) %>%
  # Create date/time fields
  dplyr::mutate(julian = lubridate::yday(recording_date_time),
                year = lubridate::year(recording_date_time))



