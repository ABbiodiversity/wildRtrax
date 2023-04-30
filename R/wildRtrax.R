#' wildRtrax: A series of advanced user functions for environmental sensor data management, analytics and exploration.
#'
#' @author Alex MacPhail \email{agmacpha@ualberta.ca} Marcus Becker \email{mabecker89@ualberta.ca} Elly Knight \email{ecknight@ualberta.ca}
#' @docType package
#' @name wildRtrax
"_PACKAGE"

# Define global variables
utils::globalVariables(c("%>>%", ".", "name", "size_Mb", "file_path", "file_ext", "file_name", "recording_date_time",
                         "data", "sample_rate", "samples", "%dopar%", "detection_models", "prob", "info", "channels",
                         "packageVersion", "sensorId", "r", "unzip", "fullNm", "status", "time", "time_lag",
                         "new_detection", "detection", "end_time_s", "start_time_s", "date_detected", ":=",
                         "common_name", "field_of_view", "number_individuals", "project", "scientific_name",
                         "start_time", "end_time", "start_date", "end_date", "max_animals", "detections", "counts",
                         "presence", "tasks", "report", "|>", "cam", "aru", "pc",
                         "dat", "tag_start", "tag_clip_url", "species", "filetype",
                         "abundance", "species_code", "user_id",
                         "occur", "recording_date", "visit",
                         "method", "proj4string<-",
                         "species_class", "species_common_name", "individual_appearance_order", "tag_start_s",
                         "vocalization", "species_Comments", "is_verified", "organization", "project_name",
                         "organization", "project_name", "method", "observer",
                         "observer_id"))
