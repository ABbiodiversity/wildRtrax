#' Get QPAD offsets.
#'
#' @description This function calculates statistical offsets that account for survey-specific and species-specific variation in availability for detection and perceptibility of birds, as per 'Solymos et al. 2013. Calibrating indices of avian density from non-standardized survey data: making the most of a messy situation Methods in Ecology and Evolution, 4, 1047-1058.' This function requires download of the `QPAD` R package and should be used on the output of the `wt_format_wide` function.
#'
#' @param data Dataframe output from the `wt_format_wide` function.
#' @param species Character; species for offset calculation. Can be a list of 4-letter AOU codes (e.g., c("TEWA", "OSFL", "OVEN")) or "all" to calculate offsets for every species in the input dataframe for which offsets are available. Defaults to "all".
#' @param version Numeric; version of QPAD offsets to use (2, or 3). Defaults to 3.
#' @param together Logical; whether or not offsets should be bound to the input dataframe or returned as a separate object.
#' @import QPAD dplyr
#' @export
#'
#' @examples
#' \dontrun{
#' devtools::install_github("borealbirds/QPAD")
#'
#' dat.clean <- wt_clean_species(dat)
#' dat.tmtt <- wt_replace_tmtt(dat.clean)
#' dat.wide <- wt_make_wide(dat.tmtt, sound="all")
#' dat.qpad <- wt_qpad_offsets(dat.wide, species="all", version=3, together = TRUE)
#' }
#' @return An object of class unmarkedFrameOccu. See `?unmarked::unmarkedFrameOccu` for details.

wt_qpad_offsets <- function(data, species = "all", version = 3, together=TRUE){

  #Make prediction object
  cat("Extracting covariates for offset calculation - be patient")
  x <- .make_x(dat)

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
    o <- .make_off(spp[i], x)
    off[,i] <- o$offset
  }

  #Return output as dataframe if separate output requested
  if(together==TRUE){
    return(data.frame(off))
  }

  #Put together if requested
  if(together==FALSE){
    out <- cbind(dat,
                 data.frame(off) %>%
                   rename_with(.fn=~paste0(.x, ".off")))

    return(out)
  }

  cat("\nDone!")

}
