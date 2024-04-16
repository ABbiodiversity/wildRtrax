#' Functions for handling classifier reports
#'
#'
#'

wt_evaluate_classifier <- function(data, resolution = "recording", remove_species = TRUE,  species = NULL, thresholds = c(10, 99)){

  #Check if the data object is in the right format
  if(class(data)!="list" | length(data)!=2 | str_sub(names(data)[1], -18, -1)!="birdnet_report.csv" | str_sub(names(data)[2], -15, -1)!="main_report.csv"){
    stop("The input for the `wt_evaluate_classifier()` function should be the output of the `wt_download_report()` function with the argument `reports=c('main', 'birdnet')`")
  }

  #Check if the project has the correct transcription method for evaluation method chosen
  method <- data[[2]]$task_method[1]
  if(method=="NONE"){
    stop("The `wt_evaluate_classifier()` function only works on recordings processed with the '1SPT' or '1SPM' methods")
  }
  if(method=="1SPT" & resolution=="minute"){
    stop("You can only evaluate at the minute resolution for recordings that have been processed with the '1SPM' method")
  }

  #Get the classifier report and filter species as requested
  if(remove_species==TRUE){
    class <- data[[1]] |>
      dplyr::filter(is_species_allowed_in_project==TRUE)
  } else {
    class <- data[[1]]
  }

  #Summarize the classifier report to the requested resolution
  if(resolution=="minute"){
    detections <- class |>
      mutate(minute = ifelse(start_s==0, 1, ceiling(start_s/60))) |>
      group_by(project_id, location_id, recording_id, species_code, minute) |>
      summarize(confidence = max(confidence), .groups="keep") |>
      ungroup() |>
      mutate(classifier = 1)
  }

  if(resolution=="recording"){
    detections <- class |>
      group_by(project_id, location_id, recording_id, species_code) |>
      summarize(confidence = max(confidence),  .groups="keep") |>
      ungroup() |>
      mutate(classifier = 1)
  }

  #Tidy up the main report
  if(resolution=="minute"){
    main <- suppressMessages(wt_tidy_species(data[[2]], remove=c("mammal", "amphibian", "abiotic", "insect", "human", "unknown"))) |>
      mutate(minute = ifelse(start_s==0, 1, ceiling(start_s/60))) |>
      dplyr::select(project_id, location_id, recording_id, species_code, minute) |>
      unique() |>
      mutate(human = 1)
  }

  if(resolution=="recording"){
    main <- suppressMessages(wt_tidy_species(data[[2]], remove=c("mammal", "amphibian", "abiotic", "insect", "human", "unknown"))) |>
      dplyr::select(project_id, location_id, recording_id, species_code) |>
      unique() |>
      mutate(human = 1)
  }

  #Join together
  both <- full_join(detections, main, by=c("project_id", "location_id", "recording_id", "species_code")) |>
    mutate(human = ifelse(is.na(human), 0, 1),
           classifier = ifelse(is.na(classifier), 0, 1),
           tp = ifelse(classifier==1 & human==1, 1, 0),
           fp = ifelse(classifier==1 & human==0, 1, 0),
           fn = ifelse(classifier==0 & human==1, 1, 0))

  #Filter to just species of interest if requested
  if(!is.null(species)){
    both <- dplyr::filter(both, species_code %in% species)
  }

  #Total number of human detections
  human_total <- sum(both$human, na.rm=TRUE)

  #Make threshold vector
  threshold <- seq(thresholds[1], thresholds[2], 1)

  #Calculate metrics
  prf <- do.call(rbind, lapply(X=threshold, FUN=wt_calculate_prf, data=both, human_total=human_total))

  #return metrics
  return(prf)

}

#'
#'
#'

wt_calculate_prf <- function(threshold, data, human_total){

  #Summarize
  data_thresholded <- dplyr::filter(data, confidence >= threshold) |>
    summarize(precision = sum(tp)/(sum(tp) + sum(fp)),
              recall = sum(tp)/human_total) |>
    mutate(fscore = (2*precision*recall)/(precision + recall),
           threshold = threshold)

}

#'
#'
#'
#'

wt_get_threshold <- function(data){

  #Filter to highest Fscore
  highest_fscore <- eval |>
    mutate(fscore = round(fscore, 2)) |>
    dplyr::filter(fscore==max(fscore))

  #Take highest threshold of highest Fscore
  return(max(highest_fscore$threshold))

}

#'
#'
#'
#'
#'
#'

wt_classifier_detections <- function(data, threshold, remove_species = TRUE, species=NULL){

  #Take out species that aren't allowed
  if(remove_species==TRUE){
    data <- data |>
      dplyr::filter(is_species_allowed_in_project==TRUE)
  }

  #Filter by threshold
  detections <- data |>
    dplyr::filter(confidence > threshold)

  #Filter to just species of interest if requested
  if(!is.null(species)){
    detections <- dplyr::filter(detections, species_code %in% species)
  }

  return(detections)

}

#'
#'

wt_additional_species <- function(data, remove_species = TRUE, threshold = 50, resolution="project"){

  #Check if the data object is in the right format
  if(class(data)!="list" | length(data)!=2 | str_sub(names(data)[1], -18, -1)!="birdnet_report.csv" | str_sub(names(data)[2], -15, -1)!="main_report.csv"){
    stop("The input for the `wt_evaluate_classifier()` function should be the output of the `wt_download_report()` function with the argument `reports=c('main', 'birdnet')`")
  }

  #Get the classifier report and filter species as requested
  if(remove_species==TRUE){
    class <- data[[1]] |>
      dplyr::filter(is_species_allowed_in_project==TRUE)
  } else {
    class <- data[[1]]
  }

  #Summarize the reports and put together at the desired resolution
  if(resolution=="recording"){

    #Classifier report
    detections <- class |>
      dplyr::filter(confidence >= threshold) |>
      group_by(project_id, location_id, recording_id, species_code) |>
      summarize(confidence = max(confidence),  .groups="keep") |>
      ungroup()

    #Main report
    main <- suppressMessages(wt_tidy_species(data[[2]], remove=c("mammal", "amphibian", "abiotic", "insect", "human", "unknown"))) |>
      dplyr::select(project_id, location_id, recording_id, species_code) |>
      unique()

    #Put together
    new <- anti_join(detections, main, by=c("project_id", "location_id", "recording_id", "species_code")) |>
      left_join(class, by=c("project_id", "location_id", "recording_id", "species_code", "confidence"), multiple="all") |>
      group_by(project_id, location_id, recording_id, species_code, confidence) |>
      sample_n(1) |>
      ungroup()

  }

  if(resolution=="location"){

    #Classifier report
    detections <- class |>
      dplyr::filter(confidence >= threshold) |>
      group_by(project_id, location_id, species_code) |>
      summarize(confidence = max(confidence),  .groups="keep") |>
      ungroup()

    #Main report
    main <- suppressMessages(wt_tidy_species(data[[2]], remove=c("mammal", "amphibian", "abiotic", "insect", "human", "unknown"))) |>
      dplyr::select(project_id, location_id, species_code) |>
      unique()

    #Put together
    new <- anti_join(detections, main, by=c("project_id", "location_id", "species_code")) |>
      left_join(class, by=c("project_id", "location_id", "species_code", "confidence"), multiple="all") |>
      group_by(project_id, location_id, species_code, confidence) |>
      sample_n(1) |>
      ungroup()

  }

  if(resolution=="project"){

    #Classifier report
    detections <- class |>
      dplyr::filter(confidence >= threshold) |>
      group_by(project_id, species_code) |>
      summarize(confidence = max(confidence),  .groups="keep") |>
      ungroup()

    #Main report
    main <- suppressMessages(wt_tidy_species(data[[2]], remove=c("mammal", "amphibian", "abiotic", "insect", "human", "unknown"))) |>
      dplyr::select(project_id, species_code) |>
      unique()

    #Put together
    new <- anti_join(detections, main, by=c("project_id", "species_code")) |>
      left_join(class, by=c("project_id", "species_code", "confidence"), multiple="all") |>
      group_by(project_id, species_code, confidence) |>
      sample_n(1) |>
      ungroup()

  }



  return(new)

}

#'Function takes a BirdNET report and a Tasks list (from the same project) and creates a dataframe of WildTrax tags that can be uploaded to the project
#'
#'

wt_format_detections <- function(BirdNET_report, Tasks_report, output = NULL, universal_threshold = ""){

  #Define the fields required in the reports to create WildTrax tags
  req_cols_tasks <- c("location", "recordingDate", "method", "taskLength", "status", "transcriber", "internal_task_id")
  req_cols_tags <- c("location","recording_date_time","species_code","start_s","end_s")

  #Tell the user if any reports are missing fields for the creation of WildTrax tags
  task_prep <- Tasks_report

  if (!all(req_cols_tasks %in% names(task_prep))){
    stop("Tasks report is missing required columns")
  }

  tag_prep <- BirdNET_report

  if (!all(req_cols_tags %in% names(tag_prep))){
    stop("BirdNET report is missing required columns")
  }

  #Format date and time in Tasks report
  tasks <- Tasks_report %>%
    dplyr::mutate(recordingDate = as.POSIXct(recordingDate))

  #Format the BirdNET report data to match the Tasks report and create necessary fields
  selections <- BirdNET_report %>%
    dplyr::rename(recordingDate = recording_date_time) %>%
    dplyr::mutate(recordingDate = as.POSIXct(recordingDate)) %>%
    dplyr::rename(species = species_code) %>%
    dplyr::rename(startTime = start_s) %>%
    dplyr::mutate(tagLength = (end_s - startTime)) %>%
    tibble::add_column(speciesIndividualNumber = "") %>%
    tibble::add_column(vocalization = "") %>%
    tibble::add_column(abundance = "") %>%
    tibble::add_column(minFreq = "") %>%
    tibble::add_column(maxFreq = "") %>%
    tibble::add_column(speciesIndividualComment = "") %>%
    tibble::add_column(internal_tag_id = "")

  #Merge the Tasks report and BirdNET report into one dataset, and remove unwanted data to generate a WildTrax tags dataset
  tags <- selections %>%
    dplyr::left_join(tasks, by = c("recordingDate" = "recordingDate", "location" = "location")) %>%
    dplyr::filter(confidence >= universal_threshold) %>%
    dplyr::filter(is_species_allowed_in_project == "t") %>%
    dplyr::select(c(location, recordingDate, method, taskLength, transcriber, species, speciesIndividualNumber, vocalization, abundance, startTime, tagLength, minFreq, maxFreq, speciesIndividualComment, internal_tag_id))

  return(tags)
}
