#' Get QPAD offsets.
#'
#' @description This function formats the summary report from the `wt_download_report` function into an unmarked object for occupancy modelling. The current version only includes formatting for the ARU sensor and for single species single season models.
#'
#' @param data WildTrax main report or tag report from the `wt_download_report` function. Summary report of WildTrax observations from the `wt_download_report` function. Currently only functioning for the ARU sensor.
#' @param species Character; four-letter alpha code for the species desired for occupancy modelling.
#' @param siteCovs Optional dataframe of site covariates. Must contain a column with the same values as the location field in the data, with one row per unique value of location (i.e., one row per site).
#' @import dplyr lubridate
#' @importFrom unmarked unmarkedFrameOccu
#' @export
#'
#' @examples
#' \dontrun{
#' dat.occu <- wt_format_occupancy(dat, species="CONI", siteCovs=NULL)
#' mod <- occu(~ 1 ~ 1, dat.occu)
#' }
#' @return An object of class unmarkedFrameOccu. See `?unmarked::unmarkedFrameOccu` for details.

wt_qpad_offsets <- function(data, species = "all", version = 3, useMethod = "y", output = "together"){

  #Make prediction object
  cat("Extracting covariates for offset calculation - be patient")
  x <- make_x(dat)

  #Load QPAD estimates
  cat("\nLoading QPAD estimates: ")
  load_BAM_QPAD(version)

  #Make the species list
  if(species=="all") spp <- sort(intersect(getBAMspecieslist(), colnames(dat))) else spp <- species

  #Set up the offset loop
  cat("\nCalculating offsets")
  off <- matrix(0, nrow(x), length(spp))
  colnames(off) <- spp

  #Make the offsets
  for (i in 1:length(spp)){
    cat(spp[i], "\n")
    flush.console()
    o <- make_off(spp[i], x, useMethod=useMethod)
    off[,i] <- o$offset
  }

  #Return output as dataframe if separate output requested
  if(output=="separate"){
    return(data.frame(off))
  }

  #Put together if requested
  if(output=="together"){
    out <- cbind(dat,
                 data.frame(off) %>%
                   rename_with(.fn=~paste0(.x, ".off")))

    return(out)
  }

  cat("\nDone!")

}
