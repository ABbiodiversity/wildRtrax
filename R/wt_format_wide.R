#' Convert to a wide survey by species dataframe.
#'
#' @description This function converts a long-formatted report into a wide survey by species dataframe of abundance values. This function is best preceded by the`wt_tidy_species` and `wt_replace_tmtt` functions  to ensure 'TMTT' and amphibian calling index values are not converted to zeros.
#'
#' @param data WildTrax main report or tag report from the `wt_download_report` function.
#' @param sound Character; vocalization type(s) to retain ("all", "song", "call", "non-vocal"). Can be used to remove certain types of detections. Defaults to "all" (i.e., no filtering).
#' @import dplyr
#' @export
#'
#' @examples
#' \dontrun{
#' dat.clean <- wt_clean_species(dat)
#' dat.tmtt <- wt_replace_tmtt(dat.clean)
#' dat.wide <- wt_make_wide(dat.tmtt, sound="all")
#' }
#' @return A dataframe identical to input with observations of the specified groups removed.


wt_make_wide <- function(data, sound="all"){

  #Filter to first detection per individual
  summed <- dat %>%
    group_by(organization, project_name, location, recording_date, method, status, observer, species_code, species_common_name, species_class, individual_appearance_order) %>%
    mutate(first = max(tag_start_s)) %>%
    ungroup() %>%
    dplyr::filter(tag_start_s==first)

  #Remove undesired sound types
  if(!"all" %in% sound){

    #Sigh, make it title case
    sound <- str_to_title(sound)

    #Filter
    summed <- dplyr::filter(summed, vocalization %in% sound)

  }

  #Make it wide
  #TO DO: COME BACK TO THE ERROR HANDLING
  #  options(warn=-1)
  wide <- summed %>%
    mutate(abundance = as.numeric(abundance)) %>%
    pivot_wider(id_cols = organization:species_class,
                names_from = "species_code",
                values_from = "abundance",
                values_fn = sum,
                values_fill = 0,
                names_sort = TRUE)
  #  options(warn=0)

  #Warn about NAs in the data
  # if(!is.na(warnings(wide))){
  #   warning('Non-numeric values in abundance field have been converted to zeros')
  # }

  return(wide)

}
