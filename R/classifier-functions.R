#' Evaluate a classifier
#'
#' @description Calculates precision, recall, and F-score of BirdNET for a requested sequence of thresholds. You can request the metrics at the minute level for recordings that are processed with the species per minute method (1SPM). You can also exclude species that are not allowed in the project from the BirdNET results before evaluation.
#'
#' @param data Output from the `wt_download_report()` function when you request the `main` and `birdnet` reports
#' @param resolution Character; either "recording" to summarize at the entire recording level or "minute" to summarize the minute level if the `task_method` is "1SPM"
#' @param remove_species Logical; indicates whether species that are not allowed in the WildTrax project should be removed from the BirdNET report
#' @param species Character; optional subset of species to calculate metrics for (e.g., species = c("OVEN", "OSLF", "BOCH"))
#' @param thresholds Numeric; start and end of sequence of score thresholds at which to calculate performance metrics
#'
#' @import dplyr
#' @export
#'
#' @examples
#' \dontrun{
#' data <- wt_download_report(project_id = 1144, sensor_id = "ARU", reports = c("main", "birdnet"), weather_cols = FALSE)
#'
#' eval <- wt_evaluate_classifier(data, resolution = "recording", remove_species = TRUE, thresholds = c(10, 99))
#' }
#'
#' @return A tibble containing columsn for precision, recall, and F-score for each of the requested thresholds.

wt_evaluate_classifier <- function(data, resolution = "recording", remove_species = TRUE,  species = NULL, thresholds = c(10, 99)){

  #Check if the data object is in the right format
  if(!inherits(data, "list") | length(data)!=2 | str_sub(names(data)[1], -18, -1)!="birdnet_report.csv" | str_sub(names(data)[2], -15, -1)!="main_report.csv"){
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

#' Internal evaluation function
#'
#' @description Internal function to calculate precision, recall, and F-score for a given score threshold.
#'
#' @param data Output from the `wt_download_report()` function when you request the `main` and `birdnet` reports
#' @param threshold A single numeric value for score threshold
#' @param human_total The total number of detections in the gold standard, typically from human listening data (e.g., the main report)
#'
#' @import dplyr
#' @export
#'
#' @return A vector of precision, recall, F-score, and threshold

wt_calculate_prf <- function(threshold, data, human_total){

  #Summarize
  data_thresholded <- dplyr::filter(data, confidence >= threshold) |>
    summarize(precision = sum(tp)/(sum(tp) + sum(fp)),
              recall = sum(tp)/human_total) |>
    mutate(fscore = (2*precision*recall)/(precision + recall),
           threshold = threshold)

}

#' Identify optimal threshold
#'
#' @description Retrieves the score threshold that maximizes F-score, which is a tradeoff between precision and recall.
#'
#' @param data Tibble output from the `wt_evaluate_classifier()` function.
#'
#' @import dplyr
#' @export
#'
#' @examples
#' \dontrun{
#' data <- wt_download_report(project_id = 1144, sensor_id = "ARU", reports = c("main", "birdnet"), weather_cols = FALSE)
#'
#' eval <- wt_evaluate_classifier(data, resolution = "recording", remove_species = TRUE, thresholds = c(10, 99))
#'
#'threshold_use <- wt_get_threshold(eval) |> print()
#' }
#'
#' @return A single numeric value

wt_get_threshold <- function(data){

  #Filter to highest Fscore
  highest_fscore <- data |>
    mutate(fscore = round(fscore, 2)) |>
    dplyr::filter(fscore==max(fscore))

  #Take highest threshold of highest Fscore
  return(max(highest_fscore$threshold))

}

#' Convert to detections
#'
#' @description Converts and filters the `birdnet` report from score probabilities to detections using a specified score threshold. You can exclude species that are not allowed in the project from the BirdNET results before evaluation.
#'
#' @param data The `birdnet` report from the `wt_download_report()` function
#' @param threshold Numeric; the desired score threshold
#' @param remove_species Logical; indicates whether species that are not allowed in the WildTrax project should be removed from the BirdNET report
#' @param species Character; optional subset of species to calculate metrics for (e.g., species = c("OVEN", "OSLF", "BOCH"))
#'
#' @import dplyr
#' @export
#'
#' @examples
#' \dontrun{
#' data <- wt_download_report(project_id = 1144, sensor_id = "ARU", reports = c("main", "birdnet"), weather_cols = FALSE)
#'
#' eval <- wt_evaluate_classifier(data, resolution = "recording", remove_species = TRUE, thresholds = c(10, 99))
#'
#'threshold_use <- wt_get_threshold(eval) |> print()
#'
#'birdnet <- data[[1]]
#'
#'detections <- wt_classifier_detections(birdnet, threshold = threshold_use, remove_species = TRUE)
#' }
#'
#' @return A tibble with the same fields as the `birdnet` report that contains only detections above the specified threshold.

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

#' Find new species
#'
#' @description Check for species reported by BirdNET that the human listeners did not detect in our project.
#'
#' @param data Output from the `wt_download_report()` function when you request the `main` and `birdnet` reports
#' @param remove_species Logical; indicates whether species that are not allowed in the WildTrax project should be removed from the BirdNET report
#' @param threshold Numeric; the desired score threshold
#' @param resolution Character; either "recording" to identify any new species for each recording or "location" to identify new species for each location
#'
#' @import dplyr
#' @export
#'
#' @examples
#' \dontrun{
#' data <- wt_download_report(project_id = 1144, sensor_id = "ARU", reports = c("main", "birdnet"), weather_cols = FALSE)
#'
#'new <- wt_additional_species(data, remove_species = TRUE, threshold = 80, resolution="location")
#' }
#'
#' @return A tibble with the same fields as the `birdnet` report with the highest scoring detection for each new species detection in each recording.

wt_additional_species <- function(data, remove_species = TRUE, threshold = 50, resolution="project"){

  #Check if the data object is in the right format
  if(!inherits(data, "list") | length(data)!=2 | str_sub(names(data)[1], -18, -1)!="birdnet_report.csv" | str_sub(names(data)[2], -15, -1)!="main_report.csv"){
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
