#' Prepare media and data for upload to WildTrax
#'
#' @section `wt_make_aru_tasks`
#'
#' @description `wt_make_aru_tasks` uses a `wt_audio_scanner` input tibble to create a task template to upload to a WildTrax project. `wt_kaleido_tags` takes the output from Wildlife Acoustics'
#' Kaleidoscope software () in order to upload the hits as tags to a project for verification and publication.  Learn more in `vignette("linking-media-to-wildtrax")`.
#'
#' @param input Character; An input `wt_audio_scanner` tibble. If not a `wt_audio_scanner` tibble, the data must contain at minimum the location, recording_date_time and file_path as column headers.
#' @param output Character; Path where the output task csv file will be stored
#' @param task_method Character; Method type of the task. Options are 1SPM, 1SPT and None. See Methods in WildTrax for more details.
#' @param task_length Numeric; Task length in seconds. Must be between 1 - 1800 and can be up to two decimal places.
#'
#' @import dplyr tidyr readr pipeR stringr lubridate tibble
#' @importFrom lubridate ymd_hms with_tz
#' @export
#'
#' @examples
#' \dontrun{
#' wt_make_tasks(input = my_audio_tibble, output = tasks.csv)
#' }
#'
#' @return A csv formatted as a WildTrax task template
#'
#'

wt_make_aru_tasks <- function(input, output, task_method = c("1SPM","1SPT","None"), task_length) {

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
  #{if (im_feeling_lucky = T) sample_frac(runif(1,0,1), replace = F)}

  if (!is.null(tasks)) {
    message("Converted list of recordings to WildTrax tasks. Go to your WildTrax organization > Recordings Tab > Manage > Upload Recordings.
        Then go to your WildTrax project > Manage > Upload Tasks to upload the csv of tasks.")
  }

  return(write.csv(tasks, output, row.names = F))
}

#' @section `wt_kaleido_tags`
#' Takes the classifier output from Wildlife Acoustics Kaleidoscope and converts them into a WildTrax tag template for upload
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
#' wt_kaleido_tags(input = input.csv, output = tags.csv, tz = "", freq_bump = T)
#' }
#'
#' @return A csv formatted as a WildTrax tag template

wt_kaleido_tags <- function (input, output, tz, freq_bump = T) {

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

#' Takes the classifier output from Wildlife Acoustics Songscope and converts them into a WildTrax tag template for upload
#'
#' @param input Character; The path to the input csv
#' @param output Character; Path where the output file will be stored
#' @param species_code Character;
#' @param vocalization_type Character;
#' @param task_length Numeric;
#'
#' @import dplyr tidyr readr pipeR stringr lubridate tibble
#' @export
#'
#' @examples
#' \dontrun{
#' wt_songscope_tags(input = input.csv, output = tags.csv, species_code, vocalization_type = "Call", task_length = 180)
#' }
#'
#' @return A csv formatted as a WildTrax tag template

wt_songscope_tags <- function (input, output, species_code, vocalization_type, task_length) {

  #Check to see if the input exists and reading it in
  if (file.exists(input)) {
    in_tbl <- readr::read_table(input, col_names = F)
  } else {
    stop ("File cannot be found")
  }

  #Cleaning things up for the tag template
  in_tbl_wtd <<- in_tbl %>%
    rename("file_path" = 1) %>%
    rename("startTime" = 2) %>%
    rename("tagLength" = 3) %>%
    rename("level" = 4) %>%
    rename("quality" = 5) %>%
    rename("score" = 6) %>%
    rename("recognizer" = 7) %>%
    rename("comments"= 8) %>%
    mutate(file_name = tools::file_path_sans_ext(basename(file_path))) %>%
    tidyr::separate(file_name, into = c("location", "recording_date_time"), sep = "(?:_0\\+1_|_|__0__|__1__)", extra = "merge", remove = F) %>%
    mutate(startTime = as.numeric(startTime)) %>%
    tibble::add_column(method = "USPM", .after = "recording_date_time") %>%
    tibble::add_column(taskLength = task_length, .after = "method") %>%
    tibble::add_column(transcriber = "Not Assigned", .after = "taskLength") %>%
    tibble::add_column(species = species_code, .after = "transcriber") %>%
    dplyr::group_by(location, recording_date_time, taskLength, species) %>%
    dplyr::mutate(speciesIndividualNumber = row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(vocalization = vocalization_type) %>%
    tibble::add_column(abundance = 1, .after= "vocalization") %>%
    relocate(startTime, .after = abundance) %>%
    relocate(tagLength, .after = startTime) %>%
    tibble::add_column(minFreq = "", .after= "tagLength") %>%
    tibble::add_column(maxFreq = "", .after= "minFreq") %>%
    tibble::add_column(internal_tag_id = "", .after = "maxFreq") %>%
    select(location, recording_date_time, method, taskLength, transcriber, species, speciesIndividualNumber, vocalization, abundance, startTime, tagLength, minFreq, maxFreq, internal_tag_id, quality, score)

  #Write the file
  return(list(in_tbl_wtd, write.csv(in_tbl_wtd, file = output, row.names = F)))

  print("Converted to WildTrax tags. Go to your WildTrax project > Manage > Upload Tags.")

}


