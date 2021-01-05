#' Generate acoustic indices for a folder of audio files with QUT Ecoacoustics **A**nalysis **P**rograms software
#'
#' @description  See \url{https://github.com/QutEcoacoustics/audio-analysis} for information about usage and installation of the AP software. Note that this function relies on having this software installed locally.
#'
#' This function will batch calculate summary and spectral acoustic indices for a folder of audio files using the Towsey.Acoustic configuration (yml) file from the AP software. You can use the output from \code{wt_audio_scanner} in the function, or define a local folder with audio files directly.
#'
#' @param x (optional) A data frame or tibble; must contain the absolute audio file path and file name. Use output from \code{wt_audio_scanner}.
#' @param fp_col If x is supplied, the column containing the audio file paths. Defaults to file_path.
#' @param audio_dir (optional) Character; path to directory storing audio files.
#' @param output_dir Character; path to directory where you want outputs to be stored.
#' @param path_to_ap Character; file path to the AnalysisPrograms software package. Defaults to "C:\\AP\\AnalysisPrograms.exe".
#'
#' @import dplyr
#' @importFrom stringr str_detect
#' @importFrom foreach foreach %dopar%
#' @importFrom doParallel registerDoParallel
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
  if(is.null(x) & is.null(audio_dir)) {
    stop("Please supply either a dataframe with the x argument, or a path to a directory of audio files with the audio_dir argument.",
         call. = TRUE)
  } else if(!is.null(x) & !is.null(audio_dir)) {
    stop("Please only supply one of x or audio_dir", call. = TRUE)
  }

  # Check if output_dir is supplied
  if(missing(output_dir)) {
    stop("Please specify a path to a local directory where you would like outputs to be stored.", call. = TRUE)
  }

  # Supported AP audio formats
  supported_formats <- "\\.wav$|\\.mp3$|\\.ogg$|\\.flac$|\\.wv$|\\.webm$|\\.wma$"

  # List audio files for analysis (vector)
  if(!is.null(x)) {
    # Ensure fp_col is a column name of x
    column <- dplyr::enquo(fp_col) %>% dplyr::quo_name()
    if(!column %in% names(x)) {
      stop("The value in fp_col does not refer to a column in x.")
    }
    files <- x %>%
      dplyr::filter(stringr::str_detect({{ fp_col }}, supported_formats)) %>%
      dplyr::select({{ fp_col }}) %>%
      dplyr::pull()
  } else {
    files <- list.files(audio_dir, pattern = supported_formats, full.names = TRUE)
  }

  # Loop through each audio file and run through AP
  doParallel::registerDoParallel()
  foreach::foreach(file = files) %dopar% {

    message("Processing ", file)

    file_name <- basename(file)

    # New folders for results
    suppressWarnings(file_specific_output_directory <- normalizePath(file.path(output_dir, file_name)))
    dir.create(file_specific_output_directory, recursive = TRUE)

    # Prepare command
    command <- sprintf('audio2csv "%s" "Towsey.Acoustic.yml" "%s" "-p"',
                       file,
                       file_specific_output_directory)

    # Execute the command
    system2(path_to_ap, command)

  }

  doParallel::stopImplicitCluster()

}

