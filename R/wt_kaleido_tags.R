#' Takes the classifier output from Wildlife Acoustics Kaleidoscope and converts them into a WildTrax tag template for upload
#'
#' @param input Character; The path to the input csv
#' @param output Character; Where the output file will be stored
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


wt_kaleido_tags <- function (input, output, freq_bump = T) {

  if (file.exists(input)) {
    in_tbl <- read_csv(input, col_names = TRUE, na = c("", "NA"), col_types = cols())
  } else {
    stop ("File cannot be found")
  }

  in_tbl_wtd <- in_tbl %>%
    select(INDIR, `IN FILE`, DURATION, OFFSET, Dur, DATE, TIME, `AUTO ID*`, Fmin, Fmax) %>%
    mutate(location = basename(dirname(file_path))) %>% ####ASSUMES WILDTRAX SOURCE FILE STRUCTURE MAY NEED TO UPDATE
    arrange(file_path) %>%
    select(-(INDIR:`IN FILE`)) %>%
    relocate(file_path) %>%
    mutate(recordingDate = as.POSIXct(paste(DATE, TIME), format = "%Y-%m-%d %H:%M:%S", tz = "US/Mountain")) %>% ####FIX TO TIMEZONE SPECIFIC
    select(-(DATE:TIME)) %>%
    relocate(location) %>%
    relocate(recordingDate, .after = location) %>%
    select(-file_path) %>%
    rename("taskLength" = 3,
           "startTime" = 4,
           "tagLength" = 5,
           "species" = 6,
           "minFreq" = 7,
           "maxFreq" = 8) %>%
    mutate(species = case_when(species == "NoID" ~ "UBAT",
                               species == "H_freq_Bat" ~ "HighF",
                               species == "L_freq_Bat" ~ "LowF",
                               TRUE ~ species),
           startTime = case_when(startTime == 0 ~ 0.1, TRUE ~ startTime)) %>%
    add_column(method = "1SPT", .after = "recordingDate") %>%
    add_column(transcriber = "Not Assigned", .after = "taskLength") %>%
    group_by(location, recordingDate, taskLength, species) %>%
    mutate(speciesIndividualNumber = row_number()) %>%
    ungroup() %>%
    add_column(vocalization = "", .after = "speciesIndividualNumber") %>%
    add_column(abundance = 1, .after= "vocalization") %>%
    mutate(vocalization = case_when(species == "Noise" ~ "Non-vocal", TRUE ~ "Call")) %>%
    add_column(internal_tag_id = "", .after = "maxFreq") %>%
    mutate(recordingDate = as.character(recordingDate)) %>%
    rowwise() %>%
    mutate(tagLength = case_when(tagLength > taskLength ~ taskLength, TRUE ~ tagLength)) %>%
    mutate(tagLength = case_when(is.na(tagLength) ~ taskLength - startTime, TRUE ~ tagLength),
           minFreq = case_when(is.na(minFreq) ~ 12000, TRUE ~ minFreq * 1000),
           maxFreq = case_when(is.na(maxFreq) ~ 96000, TRUE ~ maxFreq * 1000)) %>%
    ungroup() %>%
    mutate_at(vars(taskLength,minFreq,maxFreq), ~round(.,2)) %>%
    {if (freq_bump == TRUE) mutate(., min_Freq = minFreq - 10000, maxFreq + 10000)} %>%
    relocate(taskLength, .after = method) %>%
    relocate(startTime, .after = abundance) %>%
    relocate(tagLength, .after = startTime) %>%
    relocate(minFreq, .after = tagLength) %>%
    relocate(maxFreq, .after = minFreq) %>%
    relocate(internal_tag_id, .after = maxFreq) %>>%
    "Converted to WildTrax tags. Go to your WildTrax project > Manage > Upload Tags."

  return(write.csv(in_tbl_wtd, file = output, row.names = F))

}
