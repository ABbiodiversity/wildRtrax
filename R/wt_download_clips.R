#' Download clips of WildTrax annotations.
#'
#' @description This function downloads annotations from WildTrax either as sound files or spectrograms. Using this function requires obtaining the urls for the objects using the `wt_download_report` function.
#'
#' @param data WildTrax tag report from the `wt_download_report` function.
#' @param filepath Character; folder to download objects into.
#' @param object Character; the format requested for the clips ("soundfile", or "spectrogram").
#' @param quiet Logical; suppress download details for each object.
#' @import dplyr
#' @importFrom purrr map2
#' @export
#'
#' @examples
#' \dontrun{
#' dat.tag <- wt_download_clips(dat, "data/clips", "soundfile", quiet=TRUE)
#' }
#' @return A dataframe identical to input but including ID and filepath for each downloaded object.

wt_download_clips <- function(data, filepath, object="soundfile", quiet=FALSE){

  #Check if object type is specified
  if(missing(object)){
    stop("Please specify an object type using the 'object' argument as either 'soundfile' or 'spectogram'")
  }

  #check if filepath folder exists, make it if not
  if(dir.exists(filepath)==FALSE){
    dir.create(filepath)
    print(paste0("Created directory ", filepath))
  }

  #To download the clip file
  if(object=="soundfile"){

    #do some wrangling
    path <- dat %>%
      arrange(project, location, date, tag_start) %>%
      mutate(id = row_number(),
             filetype = ifelse(str_sub(tag_clip_url, -3, -1)=="mp3", "mp3", "flac"),
             outpath = file.path(filepath, paste0(species, "_", id, ".", filetype))) %>%
      dplyr::select(-filetype)

    #download clips
    if(quiet==FALSE){
      purrr::map2(.x = path$tag_clip_url, .y = path$outpath,  .f = ~ download.file(.x, .y))
    }
    if(quiet==TRUE){
      purrr::map2(.x = path$tag_clip_url, .y = path$outpath,  .f = ~ download.file(.x, .y, quiet=TRUE))
    }

  }

  #To download the clip spectrogram
  if(object=="spectrogram"){

    #do some wrangling
    path <- dat %>%
      arrange(project, location, date, tag_start) %>%
      mutate(id = row_number(),
             outpath = file.path(filepath, paste0(species, "_", id, ".jpeg")))

    #download spectros
    if(quiet==FALSE){
      purrr::map2(.x = path$tag_spectro_url, .y = path$outpath,  .f = ~ download.file(.x, .y))
    }
    if(quiet==TRUE)
      purrr::map2(.x = path$tag_spectro_url, .y = path$outpath,  .f = ~ download.file(.x, .y, quiet=TRUE))
  }

  return(path)

}
