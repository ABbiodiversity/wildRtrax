#' Scan acoustic data to a standard format
#'
#' @description Scans directories of audio data and returns the standard naming conventions
#'
#' @param path Character; The path to the directory with audio files you wish to scan. Can be done recursively.
#' @param file_type Character; Takes one of four values: wav, wac, flac or all. Use "all" if your directory contains many types of files.
#' @param extra_cols Boolean; Default set to FALSE for speed. If TRUE, returns additional columns for file duration, sample rate and number of channels.
#' @param tz Character; Forces a timezone to each of the recording files; if the time falls into a daylight savings time break, `wt_audio_scanner` will assume the next valid time. Use `OlsonNames()` to get a list of valid names.
#'
#' @import future fs furrr tibble dplyr tidyr stringr tools pipeR tuneR purrr seewave progressr
#' @importFrom lubridate year ymd_hms yday with_tz
#' @importFrom rlang env_has current_env
#' @export
#'
#' @examples
#' \dontrun{
#' wt_audio_scanner(path = ".", file_type = "wav", extra_cols = T, tz = "US/Mountain")
#' }
#'
#' @return A tibble with a summary of your audio files.

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
    # Throw error if the file_type is not set to wav, wac, flac, or all..
    stop (
      "For now, this function can only be used for wav, wac and/or flac files. Please specify either 'wac', 'wav', 'flac' or 'all' with the file_type argument."
    )
  }
  
  # Scan files, gather metadata
  df <- tibble::as_tibble(x = path)
  
  # Track progress of file reading
  progressr::with_progress({
    p <- progressr::progressor(steps = nrow(df))
    df <- df %>>%
      "Scanning files in path..." %>>%
      dplyr::mutate(file_path = furrr::future_map(.x = value, .f = ~ fs::dir_ls(path = .x, regexp = file_type_reg, recurse = T, fail = F), .options = furrr_options(seed = TRUE)))
  })
  
  # Create the main tibble
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
    dplyr::group_by(location, year, julian) %>%
    dplyr::mutate(time_index = dplyr::row_number()) %>% # Create time index - this is an ordered list of the recording per day.
    dplyr::ungroup()
  
  # Check if nothing was returned
  if (nrow(df) == 0) {
    stop (
      "There were no files of the type specified in file_path in the directory path specified."
    )
  }
  
  if (extra_cols == FALSE) {
    df_final_simple <- df # Omit the extra columns if chosen
  } else {
    
    # Filter out the unsafe recordings - re-append later
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
        dplyr::mutate(wac_info = furrr::future_map(.x = file_path, .f = ~ wt_wac_info(.x), .progress = TRUE, .options = furrr_options(seed = TRUE)),
                      sample_rate = purrr::map_dbl(.x = wac_info, .f = ~ purrr::pluck(.x[["sample_rate"]])),
                      length_seconds = purrr::map_dbl(.x = wac_info, .f = ~ round(purrr::pluck(.x[["length_seconds"]]), 2)),
                      n_channels = purrr::map_dbl(.x = wac_info, .f = ~ purrr::pluck(.x[["n_channels"]]))) %>%
        dplyr::select(-c(wac_info, unsafe))
      })
    }
    
    #Finally flac
    if ("flac" %in% df$file_type) {
      with_progress({p <- progressr::progressor(steps = nrow(df))
      df_flac <- df %>>%
        "Working on flac files..." %>>%
        dplyr::filter(file_type == "flac") %>%
        dplyr::mutate(flac_info = furrr::future_map(.x = file_path, .f = ~ wt_flac_info(.x), .options = furrr_options(seed = TRUE)),
                      sample_rate = purrr::map_dbl(.x = flac_info, .f = ~ purrr::pluck(.x[["sample_rate"]])),
                      length_seconds = purrr::map_dbl(.x = flac_info, .f = ~ round(purrr::pluck(.x[["length_seconds"]]), 2)),
                      n_channels = purrr::map_dbl(.x = flac_info, .f = ~ purrr::pluck(.x[["n_channels"]]))) %>%
        dplyr::select(-c(flac_info, unsafe))
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
    df_final <- dplyr::bind_rows(df_wac, df_wav, df_flac, df_unsafe) %>%
      select_if(~any(!is.na(unsafe)))
  }
  
  df_final <- df_final %>%
    select(-unsafe)
  
  # Return final data frame
  return(df_final)
  
}

#' Extract relevant metadata from a wac file
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

#' Extract relevant metadata from a flac file
#'
#' @description Scrape relevant information from flac file
#'
#' @param path Character; The flac file path
#'
#' @import tools seewave tuneR
#' @export
#'
#' @return a list with relevant information

wt_flac_info <- function(path) {

  if (tools::file_ext(path) != "flac") {
    stop("This is not a flac file.")
  }

  newfile <- gsub(".flac$", ".wav", input)

  seewave::wav2flac(input, reverse = T)

  info <- tuneR::readWave(newfile, from = 0, to = Inf, units = "seconds", header = T)

  file.remove(newfile)

  return(
    out = list(
      sample_rate = info$sample.rate,
      n_channels = info$n_channels,
      length_seconds = info$samples / info$sample.rate
    )
  )

}

#' Get a variety of acoustic index output from audio
#'
#' @description For generating acoustic indices and false-colour spectrograms using QUT Ecoacoustics **A**nalysis **P**rograms software. See \url{https://github.com/QutEcoacoustics/audio-analysis} for information about usage and installation of the AP software.
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
#' @import dplyr stringr furrr progressr
#' @export
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

  # Plan how to resolve futures
  future::plan(multisession, workers = 2)

  # Track progress of run
  # progressr::with_progress({
  #   p <- progressr::progressor(steps = nrow(files))
    files <- files %>>%
      "Starting AP run - this may take a while depending on your machine..." %>>%
      tibble::as_tibble() %>%
      dplyr::rename("file_path" = 1) %>%
      furrr::future_map(.x = .$file_path, .f = ~ suppressMessages(system2(path_to_ap, sprintf('audio2csv "%s" "Towsey.Acoustic.yml" "%s" "-p"', .x, output_dir)), furrr_options(seed = T)))
  # })
  return(message('Done!'))

}

#' Extract and plot relevant acoustic index metadata and LDFCs
#'
#' @description This function will use a list of media files from a `wt_*` work flow and outputs from `wt_run_ap`
#' in order to generate summary plots of acoustic indices and long-duration false-colour spectrograms. This can
#' be viewed as the "final step" in interpreting acoustic index and LDFC values from your recordings.
#'
#' @param x A data frame or tibble; must contain the file name. Use output from \code{`wt_audio_scanner`}.
#' @param input_dir Character; A folder path where outputs from \code{`wt_run_ap`} are stored.
#' @param purpose Character; type of filtering you can choose from
#'
#' @import tidyverse lubridate magick
#' @export
#'
#' @examples
#' \dontrun{
#' wt_glean_ap(x = wt_audio_scanner_data, input_dir = "/path/to/my/files")
#' }
#'
#' @return Output will return the merged tibble with all information, the summary plots of the indices and the LDFC

wt_glean_ap <- function(x = NULL, input_dir, purpose = c("quality","abiotic","biotic")) {

  # Check to see if the input exists and reading it in
  files <- x

  #Purpose lists
  if (purpose == "quality") {
    purpose_list <- c("Snr","BackgroundNoise")
  } else if (purpose == "abiotic") {
    purpose_list <- c("ClippingIndex","TemporalEntropy","Ndsi")
  } else if (purpose == "biotic") {
    purpose_list <- c("HighFreqCover","MidFreqCover","LowFreqCover","AcousticComplexity","Ndsi")
  } else if (is.null(purpose)) {
    purpose_list <- list_all
  }


  # Check to see if the input exists and reading it in
  if (dir.exists(input_dir)) {
    ind <-
      fs::dir_ls(input_dir, regexp = "*.Indices.csv", recurse = T) %>%
      map_dfr( ~ read_csv(., show_col_types = F)) %>%
      relocate(c(FileName, ResultMinute)) %>%
      select(-c(ResultStartSeconds, SegmentDurationSeconds,RankOrder,ZeroSignal)) %>%
      pivot_longer(!c(FileName, ResultMinute),
                   names_to = "index_variable",
                   values_to = "index_value")

    ldfcs <-
      fs::dir_info(input_dir, regexp = "*__2Maps.png", recurse = T) %>%
      select(path) %>%
      rename("image" = 1) %>%
      mutate(file_name = str_replace(basename(image), '__2Maps.png', ''))

  } else {
    stop("Cannot find this directory")
  }

  # Join the indices and LDFCs to the media
  joined <- files %>%
    inner_join(., ind, by = c("file_name" = "FileName")) %>%
    inner_join(., ldfcs, by = c("file_name" = "file_name")) %>>%
    "Files joined!"

  joined_purpose <- joined %>%
    filter(index_variable %in% purpose_list)

  # Plot a summary of the indices
  plotted <- joined_purpose %>%
    ggplot(., aes(x=julian, y=index_value, group=julian, fill=index_variable)) +
    geom_boxplot() +
    scale_fill_viridis_d() +
    theme_bw() +
    facet_wrap(~index_variable, scales = "free_y") +
    theme(legend.position="right", legend.box = "horizontal") +
    guides(fill = guide_legend(title="New Legend Title")) +
    guides(fill = guide_legend(nrow = 25, ncol = 1)) +
    xlab("Julian Date") +
    ylab("Index value") +
    ggtitle("Summary of indices")

  # Plot the LDFC
  ldfc <- joined_purpose %>%
    select(image) %>%
    distinct() %>%
    map(function(x){magick::image_read(x)}) %>%
    do.call("c", .) %>%
    magick::image_append()

  return(list(joined,plotted,ldfc))
}

#' Get signals from specific windows of audio
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

#' Segment a large audio file
#'
#' @description "Chops" up a wav file into many smaller files of a desired duration
#'
#' @param input A tibble; A single row from a \code{`wt_audio_scanner`} tibble
#' @param segment_length Numeric; Segment length in seconds. Modulo recording will be exported should there be any trailing time left depending on the segment length used
#' @param output_folder Character; output path to where the segments will be stored
#'
#' @import tuneR future furrr lubridate dplyr pipeR
#' @export
#'
#' @examples
#' \dontrun{
#' wt_chop(input = my_audio_tibble %>% slice(1),
#'  segment_length = 60, output_folder "/where/i/store/my/chopped/files")
#' }
#'
#' @return Segmented files written to the output_folder
#'

wt_chop <- function(input = NULL, segment_length = NULL, output_folder = NULL) {

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

  if (segment_length > length_sec) {
    stop('Segment is longer than duration. Choose a shorter segment length.')
  }

  start_times = seq(0, length_sec - segment_length, by = segment_length)
  val <- max(start_times) + segment_length

  if (val < length_sec) {
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
    message("No modulo recordings found. Chopping the regular segments")
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

#' Linking media to WildTrax
#'
#' Prepare media and data for upload to WildTrax
#'
#' The following suite of functions will help you wrangle media and data together
#' in order to upload them to WildTrax. You can make tasks(https://www.wildtrax.ca/home/resources/guide/projects/aru-projects.html)
#' and tags(https://www.wildtrax.ca/home/resources/guide/acoustic-data/acoustic-tagging-methods.html) using the results from a
#' `wt_audio_scanner` tibble or the hits from one of two Wildlife Acoustics programs Songscope() and Kaleidoscpe().
#'
#' Creating tasks from media
#'
#' @section `wt_make_aru_tasks`
#'
#' @description `wt_make_aru_tasks` uses a `wt_audio_scanner` input tibble to create a task template to upload to a WildTrax project.
#'
#' @param input Character; An input `wt_audio_scanner` tibble. If not a `wt_audio_scanner` tibble, the data must contain at minimum the location, recording_date_time and file_path as columns.
#' @param output Character; Path where the output task csv file will be stored
#' @param task_method Character; Method type of the task. Options are 1SPM, 1SPT and None. See Methods(https://www.wildtrax.ca/home/resources/guide/acoustic-data/acoustic-tagging-methods.html) in WildTrax for more details.
#' @param task_length Numeric; Task length in seconds. Must be between 1 - 1800 and can be up to two decimal places.
#'
#' @import dplyr tidyr readr pipeR stringr lubridate tibble
#' @importFrom lubridate ymd_hms with_tz
#' @export
#'
#' @examples
#' \dontrun{
#' wt_make_tasks(input = my_audio_tibble, output = tasks.csv, task_method = "1SPT", task_length = 180)
#' }
#'
#' @return A csv formatted as a WildTrax task template
#'
#' It's important that if the media hasn't been uploaded to WildTrax, that you do that first before trying to generate tasks in a project.
#' In parallel, you can select the files you want and upload and generate tasks in a project.

wt_make_aru_tasks <- function(input, output=NULL, task_method = c("1SPM","1SPT","None"), task_length) {

  task_prep <- input

  req_cols <- c("file_path","location","recording_date_time")

  if (!any(names(task_prep) %in% req_cols)){
    stop("Missing certain columns")
  }

  req_methods <- c("1SPM","1SPT","None")

  if (!(task_method %in% req_methods)) {
    stop("This isn't an accepted method.")
  }

  if ((is.numeric(task_length) & task_length >= 1 & task_length < 1800)==FALSE) {
    stop("task_length must be a number and between 1 and 1800 seconds.")
  }

  tasks <- task_prep %>%
    dplyr::select(location, recording_date_time, length_seconds) %>%
    dplyr::distinct() %>%
    dplyr::mutate(taskLength = case_when(length_seconds < task_length ~ NA_real_, TRUE ~ task_length)) %>% #Make sure recording length is long enough
    dplyr::select(-length_seconds) %>%
    #Add the necessary task columns
    tibble::add_column(method = task_method, .after = "recording_date_time") %>%
    tibble::add_column(status = "New", .after = "taskLength") %>%
    tibble::add_column(transcriber = "", .after = "status") %>%
    tibble::add_column(rain = "", .after = "transcriber") %>%
    tibble::add_column(wind = "", .after = "rain") %>%
    tibble::add_column(industryNoise = "", .after = "wind") %>%
    tibble::add_column(otherNoise = "", .after = "industryNoise") %>%
    tibble::add_column(audioQuality = "", .after = "otherNoise") %>%
    tibble::add_column(taskComments = "", .after = "audioQuality") %>%
    tibble::add_column(internal_task_id = "", .after = "taskComments")

  no_length <- tasks %>%
    filter(is.na(taskLength))

  warning(nrow(no_length), ' rows are shorter than the desired task length')

  if (!is.null(tasks)) {
    message("Converted list of recordings to WildTrax tasks. Go to your WildTrax organization > Recordings Tab > Manage > Upload Recordings.
        Then go to your WildTrax project > Manage > Upload Tasks to upload the csv of tasks.")
  }

  if (is.null(output)) {
    return(tasks)
  } else {
    return(write.csv(tasks, output, row.names = F))
  }
}

#' Convert Kaleidoscope output to tags
#'
#'
#' @description `wt_kaleidoscope_tags` Takes the classifier output from Wildlife Acoustics Kaleidoscope and converts them into a WildTrax tag template for upload
#'
#' @param input Character; The path to the input csv
#' @param output Character; Path where the output file will be stored
#' @param tz Character; Assigns a timezone to the recording files. Use `OlsonNames()` to get a list of valid names.
#' @param freq_bump Boolean; Set to TRUE to add a buffer to the frequency values exported from Kaleidoscope. Helpful for getting more context around a signal in species verification
#'
#' @import dplyr tidyr readr pipeR stringr lubridate tibble
#' @importFrom lubridate ymd_hms with_tz
#' @export
#'
#' @examples
#' \dontrun{
#' wt_kaleidoscope_tags(input = input.csv, output = tags.csv, tz = "", freq_bump = T)
#' }
#'
#' @return A csv formatted as a WildTrax tag template

wt_kaleidoscope_tags <- function (input, output, tz, freq_bump = T) {

  #Check to see if the input exists and reading it in
  if (file.exists(input)) {
    in_tbl <- readr::read_csv(input, col_names = TRUE, na = c("", "NA"), col_types = cols())
  } else {
    stop ("File cannot be found")
  }

  #Cleaning things up for the tag template
  in_tbl_wtd <- in_tbl %>%
    dplyr::select(INDIR, `IN FILE`, DURATION, OFFSET, Dur, DATE, TIME, `AUTO ID*`, Fmin, Fmax) %>%
    tidyr::separate(`IN FILE`, into = c("location", "recording_date_time"), sep = "(?:_0\\+1_|_|__0__|__1__)", extra = "merge", remove = F) %>%
    dplyr::select(-(DATE:TIME)) %>%
    dplyr::relocate(location) %>%
    dplyr::relocate(recording_date_time, .after = location) %>%
    dplyr::mutate(recording_date_time = stringr::str_remove(recording_date_time,'.+?(?:__)')) %>%
    # Create date/time fields
    dplyr::mutate(recording_date_time = lubridate::with_tz(lubridate::ymd_hms(recording_date_time), tzone = tz)) %>% #Apply a time zone if necessary
    dplyr::rename("taskLength" = 5,
                  "startTime" = 6,
                  "tagLength" = 7,
                  "species" = 8,
                  "minFreq" = 9,
                  "maxFreq" = 10) %>%
    dplyr::select(-(INDIR:`IN FILE`)) %>%
    # Updating names to WildTrax species codes
    dplyr::mutate(species = case_when(species == "NoID" ~ "UBAT",
                                      species == "H_freq_Bat" ~ "HighF",
                                      species == "L_freq_Bat" ~ "LowF",
                                      TRUE ~ species),
                  startTime = dplyr::case_when(startTime == 0 ~ 0.1, TRUE ~ startTime)) %>% #Adjusting startTime parameter
    tibble::add_column(method = "1SPT", .after = "recording_date_time") %>%
    tibble::add_column(transcriber = "Not Assigned", .after = "taskLength") %>%
    dplyr::group_by(location, recording_date_time, taskLength, species) %>%
    dplyr::mutate(speciesIndividualNumber = row_number()) %>%
    dplyr::ungroup() %>%
    tibble::add_column(vocalization = "", .after = "speciesIndividualNumber") %>%
    tibble::add_column(abundance = 1, .after= "vocalization") %>%
    dplyr::mutate(vocalization = case_when(species == "Noise" ~ "Non-vocal", TRUE ~ "Call")) %>%
    tibble::add_column(internal_tag_id = "", .after = "maxFreq") %>%
    dplyr::mutate(recording_date_time = as.character(recording_date_time)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(tagLength = dplyr::case_when(tagLength > taskLength ~ taskLength, TRUE ~ tagLength)) %>%
    dplyr::mutate(tagLength = dplyr::case_when(is.na(tagLength) ~ taskLength - startTime, TRUE ~ tagLength),
                  minFreq = dplyr::case_when(is.na(minFreq) ~ 12000, TRUE ~ minFreq * 1000),
                  maxFreq = dplyr::case_when(is.na(maxFreq) ~ 96000, TRUE ~ maxFreq * 1000)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate_at(vars(taskLength,minFreq,maxFreq), ~round(.,2)) %>%
    #Apply the frequency bump (+/- 10000 Hz)
    dplyr::mutate(minFreq = dplyr::case_when(freq_bump == TRUE ~ minFreq - 10000, TRUE ~ minFreq),
                  maxFreq = dplyr::case_when(freq_bump == TRUE ~ maxFreq + 10000, TRUE ~ maxFreq)) %>%
    dplyr::relocate(taskLength, .after = method) %>%
    dplyr::relocate(startTime, .after = abundance) %>%
    dplyr::relocate(tagLength, .after = startTime) %>%
    dplyr::relocate(minFreq, .after = tagLength) %>%
    dplyr::relocate(maxFreq, .after = minFreq) %>%
    dplyr::relocate(internal_tag_id, .after = maxFreq)

  #Write the file
  return(write.csv(in_tbl_wtd, file = output, row.names = F))

  print("Converted to WildTrax tags. Go to your WildTrax project > Manage > Upload Tags.")

}

#' Convert Songscope output to tags
#'
#' @param input Character; The path to the input csv
#' @param output Character; Path where the output file will be stored
#' @param my_output_file Character; Path of the output file
#' @param species_code Character; Short-hand code for the species (see wt_get_species)
#' @param vocalization_type Character; The vocalization type from either Song, Call, Non-Vocal, Night flight and Feeding Buzz
#' @param method Character; Include options from 1SPT, 1SPM or None
#' @param score_filter Numeric; Filter the detections by score
#' @param task_length Numeric; length of the task in seconds
#'
#' @import dplyr tidyr readr pipeR stringr lubridate tibble
#' @export
#'
#' @return A csv formatted as a WildTrax tag template

wt_songscope_tags <- function (input, output = c("env","csv"),
                               my_output_file=NULL, species_code, vocalization_type,
                               score_filter, method = c("USPM","1SPT"), task_length) {

  #Check to see if the input exists and reading it in
  if (file.exists(input)) {
    in_tbl <- readr::read_table(input, col_names = F)
  } else {
    stop ("File cannot be found")
  }

  if ((output == "csv") & is.null(my_output_file)) {
    stop("Specify an output file name for the tag csv")
  } else if (output == "env") {
    print("Reading file...")
  }

  #Cleaning things up for the tag template
  in_tbl_wtd <- in_tbl %>%
    rename("file_path" = 1) %>%
    rename("startTime" = 2) %>%
    rename("tagLength" = 3) %>%
    rename("level" = 4) %>%
    rename("Quality" = 5) %>%
    rename("Score" = 6) %>%
    rename("recognizer" = 7) %>%
    rename("comments"= 8) %>%
    mutate(file_name = tools::file_path_sans_ext(gsub("^.*(\\\\|/)", "", file_path))) %>%
    tidyr::separate(file_name, into = c("location", "recordingDate"),
                    sep = "(?:_0\\+1_|_|__0__|__1__)", extra = "merge", remove = F) %>%
    dplyr::mutate(startTime = as.numeric(startTime)) %>%
    dplyr::mutate(recordingDate = str_remove(recordingDate, '.+?(?:__)')) %>%
    dplyr::mutate(recordingDate = lubridate::ymd_hms(recordingDate))

  if (method == "USPM") {
    in_tbl_wtd <- in_tbl_wtd %>%
      tibble::add_column(method = "USPM", .after = "recordingDate") %>%
      tibble::add_column(taskLength = task_length, .after = "method") %>%
      tibble::add_column(transcriber = "Not Assigned", .after = "taskLength") %>%
      tibble::add_column(species = species_code, .after = "transcriber") %>%
      dplyr::group_by(location, recordingDate, taskLength, species) %>%
      dplyr::mutate(speciesIndividualNumber = row_number()) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(vocalization = vocalization_type) %>%
      tibble::add_column(abundance = 1, .after= "vocalization") %>%
      relocate(startTime, .after = abundance) %>%
      relocate(tagLength, .after = startTime) %>%
      tibble::add_column(minFreq = "", .after= "tagLength") %>%
      tibble::add_column(maxFreq = "", .after= "minFreq") %>%
      tibble::add_column(internal_tag_id = "", .after = "maxFreq") %>%
      select(location, recordingDate, method, taskLength, transcriber, species,
             speciesIndividualNumber, vocalization, abundance, startTime, tagLength,
             minFreq, maxFreq, internal_tag_id, Quality, Score) %>%
      filter(Score >= score_filter)
  } else if (method == "1SPT") {
    in_tbl_wtd <- in_tbl_wtd %>%
      tibble::add_column(method = "1SPT", .after = "recordingDate") %>%
      tibble::add_column(taskLength = task_length, .after = "method") %>%
      tibble::add_column(transcriber = "Not Assigned", .after = "taskLength") %>%
      tibble::add_column(species = species_code, .after = "transcriber") %>%
      dplyr::group_by(location, recordingDate, taskLength, species) %>%
      dplyr::mutate(speciesIndividualNumber = row_number()) %>%
      dplyr::ungroup() %>%
      dplyr::filter(!speciesIndividualNumber > 1) %>%
      dplyr::mutate(vocalization = vocalization_type) %>%
      tibble::add_column(abundance = 1, .after= "vocalization") %>%
      relocate(startTime, .after = abundance) %>%
      relocate(tagLength, .after = startTime) %>%
      tibble::add_column(minFreq = "", .after= "tagLength") %>%
      tibble::add_column(maxFreq = "", .after= "minFreq") %>%
      tibble::add_column(internal_tag_id = "", .after = "maxFreq") %>%
      select(location, recordingDate, method, taskLength, transcriber, species,
             speciesIndividualNumber, vocalization, abundance, startTime, tagLength,
             minFreq, maxFreq, internal_tag_id, Quality, Score) %>%
      filter(Score >= score_filter)
  } else {
    stop("Only USPM and 1SPT uploads are supported at this time")
  }

  if (max(in_tbl_wtd$startTime > task_length)) {
    print("A heads up there are tags outside the length of the chosen task...")
  }

  #Write the file
  if (output == "env") {
    return(in_tbl_wtd)
    print("Converted to WildTrax tags. Review the output then go to your WildTrax project > Manage > Upload Tags.")
  } else if (output == "csv") {
    return(list(in_tbl_wtd, write.csv(in_tbl_wtd, file = my_output_file, row.names = F)))
    print("Converted to WildTrax tags. Go to your WildTrax project > Manage > Upload Tags.")
  }

}
