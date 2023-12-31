#' wildRtrax: A series of advanced user functions for environmental sensor data management and an
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
                         "dat", "tag_start", "tag_clip_url", "species", "filetype","method","proj4string","unsafe","wac_info",
                         "file_type","length_seconds","input","species_code","recording_date","occur","visit","list_all","FileName",
                         "ResultMinute","ResultStartSeconds","SegmentDurationSeconds","RankOrder","ZeroSignal","index_variable",
                         "ggplot","aes","index_value","geom_boxplot","scale_fill_viridis_d","theme_bw","facet_wrap","theme","guides","guide_legend",
                         "xlab","ylab","ggtitle","INDIR","IN FILE","DURATION","OFFSET","Dur","DATE","TIME","AUTO ID*","Fmin","Fmax",
                         "ALFL", "Quality", "RDA1", "RDA2", "Score", "Unknown", "YRWA", "abiotic_codes", "abundance",
                         "arrow", "detection_order", "distance", "distance_to", "geom_hline", "geom_point",
                         "geom_segment", "geom_smooth", "geom_text", "geom_vline", "geometry",
                         "individual_order", "indvs", "internal_tag_id", "is_verified", "labs",
                         "latitude", "location.x", "location.y", "location_from", "longitude", "maxFreq",
                         "max_tag_freq", "minFreq", "min_tag_freq", "observer", "observer_id", "organization",
                         "proj4string", "project_name", "recordingDate", "scale_colour_viridis_d",
                         "speciesIndividualNumber", "species_class", "species_comments",
                         "species_common_name", "startTime", "stat_ellipse", "tagLength", "tag_duration_s",
                         "tag_start_s", "taskLength", "total", "transcriber", "unique_times", "unit",
                         "vocalization", "write.csv", "x", "ylim", "aru_task_status",
                         "clip_url","detection_time","individual_count",
                         "individual_order",
                         "my_tasks",
                         "observer_user_id",
                         "pred",
                         "project_id",
                         "species_individual_comments",
                         "tag_is_verified",
                         "tag_spectrogram_url",
                         "task_method",
                         "wt_spp_table","<<-","clip_type","output_filename","image_date_time",
                         "unlist", "PC", "sens","organizationId","organizationName","spectrogram_url","longer","start_times"))
