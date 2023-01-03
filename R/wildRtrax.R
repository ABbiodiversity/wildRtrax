#' wildRtrax: Am R package to facilitate full-cycle environmental sensor data work flows.
#'
#' @author Alex MacPhail \email{agmacpha@ualberta.ca} Marcus Becker \email{mabecker89@ualberta.ca} Elly Knight \email{ecknight@ualberta.ca}
#' @docType package
#' @name wildRtrax
"_PACKAGE"

# Define global variables
utils::globalVariables(c("%>>%", ".", "name", "size_Mb", "file_path", "file_ext", "file_name", "recording_date_time",
                         "data", "sample_rate", "samples", "%dopar%", "detection_models", "prob", "info", "channels",
                         "packageVersion", "sensorId", "r", "unzip", "fullNm", "status", "time", "time_lag",
                         "new_detection", "detection", "end_time_s", "start_time_s"))
