#' Takes the classifier output from Wildlife Acoustics Kaleidoscope and converts them into a WildTrax tag template for upload
#'
#' @param input Character; The path to the input csv
#' @param output Character; Where the output file will be stored
#' @param tz Character; Assigns a timezone to the recording files. Use `OlsonNames()` to get a list of valid names.
#' @param freq_bump Boolean; Set to TRUE to add a buffer to the frequency values exported from Kaleidoscope. Helpful for getting more context around a signal in species verification
#'
#' @import dplyr tidyr readr pipeR
#' @export
#'
#' @examples
#' \dontrun{
#' wt_kaleido_tags(input = input_csv, output = tags_csv, freq_bump = T)
#' }
#'
#' @return A csv formatted as a WildTrax tag template

input <- "/users/alexandremacphail/desktop/tsp/SML183_id.csv"

wt_kaleido_tags <- function (input, output, freq_bump = T) {

  if (file.exists(input)) {
    in_tbl <- read_csv(input, col_names = TRUE, na = c("", "NA"), col_types = cols())
  } else {
    stop ("File cannot be found")
  }

  in_tbl_wtd <- in_tbl %>%
    select(INDIR, `IN FILE`, DURATION, OFFSET, Dur, DATE, TIME, `AUTO ID*`, Fmin, Fmax) %>%
    separate(`IN FILE`, into = c("location", "recording_date_time"), sep = "(?:_0\\+1_|_|__0__|__1__)", extra = "merge", remove = F) %>%
    select(-(DATE:TIME)) %>%
    relocate(location) %>%
    relocate(recording_date_time, .after = location) %>%
    dplyr::mutate(recording_date_time = str_remove(recording_date_time,'.+?(?:__)')) %>%
    # Create date/time fields
    dplyr::mutate(recording_date_time = lubridate::force_tz(lubridate::ymd_hms(recording_date_time), tzone = "US/Mountain", roll = TRUE)) %>%
    rename("taskLength" = 5,
           "startTime" = 6,
           "tagLength" = 7,
           "species" = 8,
           "minFreq" = 9,
           "maxFreq" = 10) %>%
    select(-(INDIR:`IN FILE`)) %>%
    mutate(species = case_when(species == "NoID" ~ "UBAT",
                               species == "H_freq_Bat" ~ "HighF",
                               species == "L_freq_Bat" ~ "LowF",
                               TRUE ~ species),
           startTime = case_when(startTime == 0 ~ 0.1, TRUE ~ startTime)) %>%
    add_column(method = "1SPT", .after = "recording_date_time") %>%
    add_column(transcriber = "Not Assigned", .after = "taskLength") %>%
    group_by(location, recording_date_time, taskLength, species) %>%
    mutate(speciesIndividualNumber = row_number()) %>%
    ungroup() %>%
    add_column(vocalization = "", .after = "speciesIndividualNumber") %>%
    add_column(abundance = 1, .after= "vocalization") %>%
    mutate(vocalization = case_when(species == "Noise" ~ "Non-vocal", TRUE ~ "Call")) %>%
    add_column(internal_tag_id = "", .after = "maxFreq") %>%
    mutate(recording_date_time = as.character(recording_date_time)) %>%
    rowwise() %>%
    mutate(tagLength = case_when(tagLength > taskLength ~ taskLength, TRUE ~ tagLength)) %>%
    mutate(tagLength = case_when(is.na(tagLength) ~ taskLength - startTime, TRUE ~ tagLength),
           minFreq = case_when(is.na(minFreq) ~ 12000, TRUE ~ minFreq * 1000),
           maxFreq = case_when(is.na(maxFreq) ~ 96000, TRUE ~ maxFreq * 1000)) %>%
    ungroup() %>%
    mutate_at(vars(taskLength,minFreq,maxFreq), ~round(.,2)) %>%
    mutate(minFreq = case_when(freq_bump == TRUE ~ minFreq - 10000, TRUE ~ minFreq),
           maxFreq = case_when(freq_bump == TRUE ~ maxFreq + 10000, TRUE ~ maxFreq)) %>%
    relocate(taskLength, .after = method) %>%
    relocate(startTime, .after = abundance) %>%
    relocate(tagLength, .after = startTime) %>%
    relocate(minFreq, .after = tagLength) %>%
    relocate(maxFreq, .after = minFreq) %>%
    relocate(internal_tag_id, .after = maxFreq)

  return(write.csv(in_tbl_wtd, file = output, row.names = F))

  print("Converted to WildTrax tags. Go to your WildTrax project > Manage > Upload Tags.")

}
