#' # Pre-process acoustic data for use in WildTrax
#'
#' The following set of functions help to pre-process and organize audio and corresponding metadata.
#' \code(`wt_audio_scanner`) scans a directory of audio files and prepares them in a tibble with WildTrax formatted columns.
#' \code(`wt_run_ap`) allows you to generate acoustic indices and false-colour spectrograms from a \code(`wt_audio_scanner`)
#' tibble. \code(`wt_signal_level`) detects signals in audio based on amplitude thresholds. In conjunction, these tools allow
#' you to select recordings parameterized to a specific study design. `vignette("linking-media-to-wildtrax"` will allow you to d
#' dive deeper in how to link the data to the WildTrax platform.
#'
#' @section `wt_audio_scanner` details:
#'
#' @description Scans directories of audio data and returns the file path, file name, file size, date, time, location name,
#' sample rate, length (seconds) and number of channels to be used as filters for other uses
#'
#' @param path Character; The path to the directory with audio files you wish to scan. Can be done recursively.
#' @param file_type Character; Takes one of four values: wav, wac, flac or all. Use "all" if your directory contains many types of files.
#' @param extra_cols Boolean; Default set to FALSE for speed. If TRUE, returns additional columns for file duration, sample rate and number of channels.
#' @param tz Character; Forces a timezone to each of the recording files; if the time falls into a daylight savings time break, `wt_audio_scanner` will assume the next valid time. Use `OlsonNames()` to get a list of valid names.
#'
#' @import future fs furrr tibble dplyr tidyr stringr tools pipeR tuneR purrr seewave
#' @importFrom lubridate year ymd_hms yday with_tz
#' @importFrom rlang env_has current_env
#' @export
#'
#' @examples
#' \dontrun{
#' df <- wt_audio_scanner(path = "C:/Users/me/path/to/audio/files", file_type = "all", extra_cols = FALSE, tz = "US/Mountain")
#' }
#'
#' @return A tibble with a summary of your audio files

wt_audio_scanner <- function(path, file_type, extra_cols = F, tz = "") {
  # Create regex for file_type
  if (file_type == "wav" || file_type == "WAV") {
    file_type_reg <- "\\.wav$|\\.WAV$"
  } else if (file_type == "wac") {
    file_type_reg <- "\\.wac$"
  } else if (file_type == "flac") {
    file_type_reg <- "\\.flac$"
  } else if (file_type == "all") {
    file_type_reg <- "\\.wav$|\\.wac$|\\.WAV$|\\.flac$"
  } else {
    # Throw error if the file_type is not set to wav, wac, or both.
    stop (
      "For now, this function can only be used for wav, wac and/of flac files. Please specify either 'wac', 'wav', 'flac' or 'all' with the file_type argument."
    )
  }

  # Scan files, gather metadata
  df <- tibble::as_tibble(x = path)

  progressr::with_progress({
    p <- progressr::progressor(steps = nrow(df))
    df <- df %>>%
      "Scanning files in path..." %>>%
      dplyr::mutate(file_path = furrr::future_map(.x = value, .f = ~ fs::dir_ls(path = .x, regexp = file_type_reg, recurse = T, fail = F), .progress = TRUE, .options = furrr_options(seed = TRUE)))
   })

    df <- df %>%
    tidyr::unnest(file_path) %>%
    dplyr::mutate(file_size = furrr::future_map_dbl(.x = file_path, .f = ~ fs::file_size(.x), .progress = TRUE, .options = furrr_options(seed = TRUE))) %>%
    dplyr::mutate(file_path = as.character(file_path)) %>%
    dplyr::mutate(size_Mb = round(file_size / 10e5, digits = 2)) %>% # Convert file sizes to megabytes
    dplyr::select(-file_size) %>%
    dplyr::mutate(unsafe = dplyr::case_when(size_Mb <= 0.5 ~ "Unsafe", TRUE ~ "Safe")) %>% #Create safe scanning protocol, pretty much based on file size
    dplyr::select(file_path, size_Mb, unsafe) %>%
    dplyr::mutate(file_name = stringr::str_replace(basename(file_path), "\\..*", "")) %>%
    dplyr::mutate(file_type = tolower(tools::file_ext(file_path))) %>%
    # Parse location and recording date time
    tidyr::separate(file_name, into = c("location", "recording_date_time"), sep = "(?:_0\\+1_|_|__0__|__1__)", extra = "merge", remove = FALSE) %>% # Strips Wildlife Acoustics SM3 file naming convention for channels
    dplyr::mutate(recording_date_time = str_remove(recording_date_time, '.+?(?:__)')) %>%
    # Create date/time fields
    dplyr::mutate(recording_date_time = case_when(tz == "" ~ lubridate::ymd_hms(recording_date_time), TRUE ~ lubridate::with_tz(lubridate::ymd_hms(recording_date_time), tzone = tz)),
                  julian = lubridate::yday(recording_date_time),
                  year = lubridate::year(recording_date_time),
                  gps_enabled = dplyr::case_when(grepl('\\$', file_name) ~ TRUE),
                  year = lubridate::year(recording_date_time)) %>%
    dplyr::arrange(location, recording_date_time) %>%
    # Create time index
    dplyr::group_by(location, year, julian) %>%
    dplyr::mutate(time_index = dplyr::row_number()) %>% # This serves as a ordered count of recordings per day.
    dplyr::ungroup()

  # Check whether anything was returned
  if (nrow(df) == 0) {
    stop (
      "There were no files of the type specified in file_path in the directory path specified."
    )
  }

  if (extra_cols == FALSE) {
    df_final_simple <- df # Omit the extra columns if chosen
  } else {

    df_unsafe <- df %>%
      filter(unsafe == "Unsafe")
    df <- df %>%
      filter(unsafe == "Safe")

    # wav files first
    if ("wav" %in% df$file_type) {
      with_progress({p <- progressr::progressor(steps = nrow(df))
      df_wav <- df %>>%
        "Working on wav files..." %>>%
        dplyr::filter(file_type == "wav") %>% #Make sure things are safe if needed
        dplyr::mutate(data = furrr::future_map(.x = file_path, .f = ~ tuneR::readWave(.x, from = 0, to = Inf, units = "seconds", header = TRUE), .progress = TRUE, .options = furrr_options(seed = TRUE))) %>%
        dplyr::mutate(length_seconds = purrr::map_dbl(.x = data, .f = ~ round(purrr::pluck(.x[["samples"]]) / purrr::pluck(.x[["sample.rate"]]), 2)),
                      sample_rate = purrr::map_dbl(.x = data, .f = ~ round(purrr::pluck(.x[["sample.rate"]]), 2)),
                      n_channels = purrr::map_dbl(.x = data, .f = ~ purrr::pluck(.x[["channels"]]))) %>%
        dplyr::select(-c(data, unsafe))
      })
    }

    #Then wac files
    if ("wac" %in% df$file_type) {
      with_progress({p <- progressr::progressor(steps = nrow(df))
      df_wac <- df %>>%
        "Working on wac files..." %>>%
        dplyr::filter(file_type == "wac") %>%
        dplyr::mutate(info = furrr::future_map(.x = file_path, .f = ~ wt_wac_info(.x), .progress = TRUE, .options = furrr_options(seed = TRUE)),
                      sample_rate = purrr::map_dbl(.x = info, .f = ~ purrr::pluck(.x[["sample_rate"]])),
                      length_seconds = purrr::map_dbl(.x = info, .f = ~ round(purrr::pluck(.x[["length_seconds"]]), 2)),
                      n_channels = purrr::map_dbl(.x = info, .f = ~ purrr::pluck(.x[["n_channels"]]))) %>%
        dplyr::select(-c(info, unsafe))
      })
    }

    #Finally flac
    if ("flac" %in% df$file_type) {
      with_progress({p <- progressr::progressor(steps = nrow(df))
      df_flac <- df %>>%
        "Working on flac files..." %>>%
        dplyr::filter(file_type == "flac") %>%
        dplyr::mutate(data = furrr::future_map(.x = file_path, .f = ~ seewave::wav2flac(.x, reverse = TRUE), .progress = TRUE, .options = furrr_options(seed = TRUE)),
                      sample_rate = purrr::map_dbl(.x = data, .f = ~ purrr::pluck(.x[["sample_rate"]])),
                      length_seconds = purrr::map_dbl(.x = data, .f = ~ round(purrr::pluck(.x[["length_seconds"]]), 2)),
                      n_channels = purrr::map_dbl(.x = data, .f = ~ purrr::pluck(.x[["n_channels"]]))) %>%
        dplyr::select(-c(data, unsafe))
      })
    }
  }

  # Stitch together
  if (rlang::env_has(rlang::current_env(), "df_final_simple")) {
    df_final <- df_final_simple
  } else if (exists("df_wav") & !exists("df_wac") & !exists("df_flac")) {
    df_final <- dplyr::bind_rows(df_wav, df_unsafe)
  } else if (exists("df_wav") & exists("df_wac") & !exists("df_flac")) {
    df_final <- dplyr::bind_rows(df_wav, df_wac, df_unsafe)
  } else if (exists("df_wav") & !exists("df_wac") & exists("df_flac")) {
    df_final <- dplyr::bind_rows(df_wav, df_flac, df_unsafe)
  } else if (!exists("df_wav") & exists("df_wac") & !exists("df_flac")) {
    df_final <- dplyr::bind_rows(df_wac, df_unsafe)
  } else if (!exists("df_wav") & !exists("df_wac") & exists("df_flac")) {
    df_final <- dplyr::bind_rows(df_flac, df_unsafe)
  } else if (!exists("df_wav") & exists("df_wac") & exists("df_flac")) {
    df_final <- dplyr::bind_rows(df_wac, df_flac, df_unsafe)
  } else if (exists("df_wav") & exists("df_wac") & exists("df_flac")) {
    df_final <- dplyr::bind_rows(df_wac, df_wav, df_flac, df_unsafe)
  }

  # Return final data frame
  return(df_final)

}

#' @section `wt_wac_info` details:
#'
#' @description Scrape relevant information from wac (Wildlife Acoustics) file
#'
#' @param path Character; The wac file path
#'
#' @import tools
#' @export
#'
#' @return a list with relevant information

wt_wac_info <- function(path) {
  if (tools::file_ext(path) != "wac") {
    stop("This is not a wac file.")
  }

  f <- file(path, open = "rb")
  on.exit(close(f))

  name <- readChar(f, 4)
  version <-
    readBin(
      con = f,
      what = integer(),
      size = 1,
      endian = "little"
    )
  n_channels <-
    readBin(
      con = f,
      what = integer(),
      size = 1,
      endian = "little"
    )
  frame_size <-
    readBin(
      con = f,
      what = integer(),
      size = 2,
      endian = "little"
    )
  block_size <-
    readBin(
      con = f,
      what = integer(),
      size = 2,
      endian = "little"
    )
  flags <-
    readBin(
      con = f,
      what = integer(),
      size = 2,
      endian = "little"
    )
  sample_rate <-
    readBin(
      con = f,
      what = integer(),
      size = 4,
      endian = "little"
    )
  samples <-
    readBin(
      con = f,
      what = integer(),
      size = 4,
      endian = "little"
    )

  if (n_channels == 1) {
    stereo <- FALSE
  } else {
    stereo <- TRUE
  }

  length_seconds = samples / sample_rate

  return(
    out = list(
      sample_rate = sample_rate,
      n_channels = n_channels,
      length_seconds = length_seconds
    )
  )

}

#' @section `wt_run_ap` for generating acoustic indices and false-colour spectrograms using QUT Ecoacoustics **A**nalysis **P**rograms software
#'
#' @description See \url{https://github.com/QutEcoacoustics/audio-analysis} for information about usage and installation of the AP software.
#' Note that this function relies on having this software installed locally.
#'
#' This function will batch calculate summary and spectral acoustic indices and generate false-colour spectrograms for a folder of audio files using the Towsey.Acoustic configuration (yml) file from the AP software.
#' You can use the output from \code{`wt_audio_scanner`} in the function, or define a local folder with audio files directly.
#'
#' @param x (optional) A data frame or tibble; must contain the absolute audio file path and file name. Use output from \code{`wt_audio_scanner`}.
#' @param fp_col If x is supplied, the column containing the audio file paths. Defaults to file_path.
#' @param audio_dir (optional) Character; path to directory storing audio files.
#' @param output_dir Character; path to directory where you want outputs to be stored.
#' @param path_to_ap Character; file path to the AnalysisPrograms software package. Defaults to "C:\\AP\\AnalysisPrograms.exe".
#'
#' @import dplyr
#' @importFrom stringr str_detect
#' @importFrom furrr future_map
#' @importFrom progress progressor
#' @importFrom shiny with_progress
#' @export
#'
#' @examples
#' \dontrun{
#  wt_run_ap(x = scanner_output, output_dir = '/user/output_folder', path_to_ap = "C:\\AP\\AnalysisPrograms.exe")
#' }
#'
#' @return Output will return to the specific root directory

wt_run_ap <- function(x = NULL, fp_col = file_path, audio_dir = NULL, output_dir, path_to_ap = "C:\\AP\\AnalysisPrograms.exe") {
    # Make sure at least (and only) one of x or audio_folder has been supplied
    if (is.null(x) & is.null(audio_dir)) {
      stop(
        "Please supply either a dataframe with the x argument, or a path to a directory of audio files with the audio_dir argument.",
        call. = TRUE
      )
    } else if (!is.null(x) & !is.null(audio_dir)) {
      stop("Please only supply one of x or audio_dir", call. = TRUE)
    }

    # Check if output_dir is supplied
    if (missing(output_dir)) {
      stop(
        "Please specify a path to a local directory where you would like outputs to be stored.",
        call. = TRUE
      )
    }

    # Supported AP audio formats
    supported_formats <-
      "\\.wav$|\\.mp3$|\\.ogg$|\\.flac$|\\.wv$|\\.webm$|\\.wma$"

    # Will support wac in 1.0.1
    convert <-
      "\\.wac$"

    # List audio files for analysis (vector)
    if (!is.null(x)) {
      # Ensure fp_col is a column name of x
      column <- dplyr::enquo(fp_col) %>%
        dplyr::quo_name()
      if (!column %in% names(x)) {
        stop("The value in fp_col does not refer to a column in x.")
      }
      files <- x %>%
        dplyr::filter(stringr::str_detect({{fp_col}}, supported_formats)) %>%
        dplyr::select({{fp_col}}) %>%
        dplyr::pull()
    } else {
      files <- list.files(audio_dir, pattern = supported_formats, full.names = TRUE)
    }

    future::plan(multisession, workers = 2)

    files <- files %>%
      tibble::as_tibble() %>%
      dplyr::rename("file_path" = 1)

    progressr::with_progress({
      p <- progressr::progressor(steps = nrow(files))

      files <- files %>%
        furrr::future_map(.x = .$file_path, .f = ~ system2(path_to_ap, sprintf('audio2csv "%s" "Towsey.Acoustic.yml" "%s" "-p"', .x, output_dir), invisible = T), furrr_options(seed = T))

    })

    return(message('Done!'))

}

#' @section `wt_glean_ap` for retrieving data from the AP output
#'
#' @description
#'
#' @param
#'
#' @import
#' @export
#'
#' @examples
#' \dontrun{}
#'
#' @return

wt_glean_ap <- function(merge_to_files = x) {}

#' @section `wt_signal_level` to extract relative sound level from a wav file using amplitude thresholds
#'
#' @description Signal level uses amplitude and frequency thresholds in order to detect a signal.
#'
#' @param path The path to the wav file
#' @param fmin The frequency minimum
#' @param fmax The frequency maximum
#' @param threshold The desired threshold
#' @param channel Choose "left" or "right" channel
#' @param aggregate Aggregate detections by this number of seconds, if desired
#'
#' @import tuneR dplyr
#' @importFrom seewave spectro
#' @export
#'
#' @examples
#' \dontrun{
#' df <- wt_signal_level(path = "")
#' }
#'
#' @return A list object containing the following four elements: output (dataframe), aggregated (boolean), channel (character), and threshold (numeric)
#'
wt_signal_level <- function(path, fmin = 500, fmax = NA, threshold, channel = "left", aggregate = NULL) {
    # Load wav object from path
    wav_object <- tuneR::readWave(path)

    # Sampling frequency
    sampling_frequency <- wav_object@samp.rate
    # Recording duration
    recording_duration <-
      length(wav_object@left) / sampling_frequency

    # Check that channel is set to either left or right
    if (!(channel == "left" | channel == "right")) {
      stop('Please specify "left" or "right" channel.')
    }

    if (channel == "left") {
      wav_object <- wav_object@left
    } else {
      if (length(wav_object@right) %in% c(0, 1)) {
        stop('Channel set to "right", but no right channel')
      }
      wav_object <- wav_object@right
    }

    # Remove DC offset
    wav_object <- wav_object - mean(wav_object)
    # Set breaks
    breaks <- seq(0, recording_duration, 300)
    if (breaks[length(breaks)] != recording_duration) {
      breaks[length(breaks) + 1] <- recording_duration
    }

    samps <- breaks * sampling_frequency
    samps[1] <- 1

    times = c()
    rsl.out <- c()

    for (i in 2:length(breaks)) {
      print(paste0('Calculating segment ', i - 1, ' out of ', length(breaks) - 1))
      s <- seewave::spectro(
        wav_object[samps[i - 1]:samps[i]],
        f = sampling_frequency,
        wn = "hamming",
        wl = 512,
        ovlp = 50,
        plot = FALSE,
        norm = FALSE
      )
      # Filter spectrogram
      subset <- which(s$freq >= fmin / 1000)
      if (!is.na(fmax)) {
        subset <- which(s$freq >= fmin / 1000 & s$freq <= fmax / 1000)
      }
      s$freq <- s$freq[subset]
      s$amp <- s$amp[subset,]
      # Calculate max RSL for each window
      rsl <- apply(s$amp, 2, max)
      # Edit times for the chunk
      s$time <- s$time + breaks[i - 1]
      times <- c(times, s$time[rsl > threshold])
      rsl.out <- c(rsl.out, rsl[rsl > threshold])
    }

    if (length(times) > 0) {
      sl <- data.frame(time = times, rsl = rsl.out)
    } else {
      sl <- NA
    }

    # Aggregate (if desired)
    if (!is.null(aggregate)) {
      if (!is.na(sl)) {
        sl <- sl %>%
          dplyr::mutate(
            time_lag = dplyr::lag(time),
            new_detection = ifelse((time - time_lag) >= aggregate, 1, 0),
            detection = c(0, cumsum(new_detection[-1])) + 1
          ) %>%
          dplyr::group_by(detection) %>%
          dplyr::summarise(
            mean_rsl = mean(rsl),
            start_time_s = min(time),
            end_time_s = max(time)
          ) %>%
          dplyr::ungroup() %>%
          mutate(detection_length = end_time_s - start_time_s)
        aggregated <- TRUE
      } else {
        sl
        aggregated <- FALSE
        warning("No signals met the threshold criteria. Output not aggregated.")
      }
    } else {
      if (!is.na(sl)) {
        sl
        aggregated <- FALSE
      } else {
        sl
        aggregated <- FALSE
        warning("No signals met the threshold critera.")
      }
    }

    # Create list object
    d <- list(
      output = sl,
      aggregated = aggregated,
      channel = channel,
      threshold = threshold
    )

    return(d)

  }
