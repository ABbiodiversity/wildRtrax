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
#' wt_kaleido_tags(input = input_csv, output = tags_csv, tz = "", freq_bump = T)
#' }
#'
#' @return A csv formatted as a WildTrax tag template

input <- "/users/alexandremacphail/desktop/tsp/SML183_id.csv"

wt_kaleido_tags <- function (input, output, freq_bump = T) {

  if (file.exists(input)) {
    in_tbl <- readr::read_csv(input, col_names = TRUE, na = c("", "NA"), col_types = cols())
  } else {
    stop ("File cannot be found")
  }

  in_tbl_wtd <- in_tbl %>%
    dplyr::select(INDIR, `IN FILE`, DURATION, OFFSET, Dur, DATE, TIME, `AUTO ID*`, Fmin, Fmax) %>%
    tidyr::separate(`IN FILE`, into = c("location", "recording_date_time"), sep = "(?:_0\\+1_|_|__0__|__1__)", extra = "merge", remove = F) %>%
    dplyr::select(-(DATE:TIME)) %>%
    dplyr::relocate(location) %>%
    dplyr::relocate(recording_date_time, .after = location) %>%
    dplyr::mutate(recording_date_time = stringr::str_remove(recording_date_time,'.+?(?:__)')) %>%
    # Create date/time fields
    dplyr::mutate(recording_date_time = lubridate::with_tz(lubridate::ymd_hms(recording_date_time), tzone = tz, roll = TRUE)) %>%
    dplyr::rename("taskLength" = 5,
           "startTime" = 6,
           "tagLength" = 7,
           "species" = 8,
           "minFreq" = 9,
           "maxFreq" = 10) %>%
    dplyr::select(-(INDIR:`IN FILE`)) %>%
    dplyr::mutate(species = case_when(species == "NoID" ~ "UBAT",
                               species == "H_freq_Bat" ~ "HighF",
                               species == "L_freq_Bat" ~ "LowF",
                               TRUE ~ species),
           startTime = dplyr::case_when(startTime == 0 ~ 0.1, TRUE ~ startTime)) %>%
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
    dplyr::mutate(minFreq = dplyr::case_when(freq_bump == TRUE ~ minFreq - 10000, TRUE ~ minFreq),
           maxFreq = dplyr::case_when(freq_bump == TRUE ~ maxFreq + 10000, TRUE ~ maxFreq)) %>%
    dplyr::relocate(taskLength, .after = method) %>%
    dplyr::relocate(startTime, .after = abundance) %>%
    dplyr::relocate(tagLength, .after = startTime) %>%
    dplyr::relocate(minFreq, .after = tagLength) %>%
    dplyr::relocate(maxFreq, .after = minFreq) %>%
    dplyr::relocate(internal_tag_id, .after = maxFreq)

  return(write.csv(in_tbl_wtd, file = output, row.names = F))

  print("Converted to WildTrax tags. Go to your WildTrax project > Manage > Upload Tags.")

}
