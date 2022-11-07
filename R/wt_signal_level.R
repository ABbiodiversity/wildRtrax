#' Extract relative sound level from a wav file
#'
#' @description Extract relative sound level from a wav file
#'
#' @param path The path to the wav file
#' @param fmin The frequency minimum
#' @param fmax The frequency maximum
#' @param threshold The desired threshold
#' @param channel Choose "left" or "right"
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
  recording_duration <- length(wav_object@left) / sampling_frequency

  # Check that channel is set to either left or right
  if(!(channel == "left" | channel == "right")) {
    stop('Please specify "left" or "right" channel.')
  }

  if(channel == "left") {
    wav_object <- wav_object@left
  } else {
    if(length(wav_object@right) %in% c(0, 1)) {
      stop('Channel set to "right", but no right channel')
    }
    wav_object <- wav_object@right
  }

  # Remove DC offset
  wav_object <- wav_object - mean(wav_object)
  # Set breaks
  breaks <- seq(0, recording_duration, 300)
  if(breaks[length(breaks)] != recording_duration) {
    breaks[length(breaks) + 1] <- recording_duration
  }

  samps <- breaks * sampling_frequency
  samps[1] <- 1

  times = c()
  rsl.out <- c()

  for(i in 2:length(breaks)) {
    print(paste0('Calculating segment ', i-1, ' out of ', length(breaks) - 1))
    s <- seewave::spectro(wav_object[samps[i - 1]:samps[i]],
                          f = sampling_frequency,
                          wn = "hamming",
                          wl = 512,
                          ovlp = 50,
                          plot = FALSE,
                          norm = FALSE)
    # Filter spectrogram
    subset <- which(s$freq >= fmin / 1000)
    if(!is.na(fmax)) {
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

  if(length(times) > 0) {
    sl <- data.frame(time = times, rsl = rsl.out)
  } else {
    sl <- NA
  }

  # Aggregate (if desired)
  if(!is.null(aggregate)) {
    if(!is.na(sl)) {
    sl <- sl %>%
      dplyr::mutate(time_lag = dplyr::lag(time),
             new_detection = ifelse((time - time_lag) >= aggregate, 1, 0),
             detection = c(0, cumsum(new_detection[-1])) + 1) %>%
      dplyr::group_by(detection) %>%
      dplyr::summarise(mean_rsl = mean(rsl),
                       start_time_s = min(time),
                       end_time_s = max(time)) %>%
      dplyr::ungroup() %>%
      mutate(detection_length = end_time_s - start_time_s)
    aggregated <- TRUE
    } else {
      sl
      aggregated <- FALSE
      warning("No signals met the threshold criteria. Output not aggregated.")
    }
  } else {
    if(!is.na(sl)) {
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

