#' Replace 'TMTT' abundance entries with model-predicted values.
#'
#' @description This function uses a lookup table of model-predicted values to replace 'TMTT' entries in listener-processed ARU data from WildTrax. The model-predicted values were produced using estimated abundances for 'TMTT' entries in mixed effects model with a Poisson distribution and random effects for species and observer.
#'
#' @param data WildTrax main report or tag report from the `wt_download_report` function.
#' @param calc Character; method to convert model predictions to integer ("round", "ceiling", or "floor"). See `?round()` for details.
#' @import dplyr
#' @export
#'
#' @examples
#' \dontrun{
#' dat.tmtt <- replace_tmtt(dat, calc="round")
#' }
#' @return A dataframe identical to input with 'TMTT' entries in the abundance column replaced by integer values.


wt_replace_tmtt <- function(data, calc="round"){

  #load tmtt lookup table
  .tmtt <- read.csv(system.file("tmtt_predictions.csv", package="wildRtrax"))

  #wrangle to tmtts only
  dat.tmtt <- data %>%
    dplyr::filter(abundance=="TMTT")

  #replace values with mean value from bootstraps
  dat.abun <- dat.tmtt %>%
    mutate(species_code = ifelse(species_code %in% .tmtt$species_code, species_code, "species"),
           observer_id = as.integer(ifelse(observer_id %in% .tmtt$observer_id, observer_id, 0))) %>%
    data.frame() %>%
    left_join(.tmtt, by=c("species_code", "observer_id"))

  #summarize predicted values
  dat.abun$abundance <- as.integer(case_when(calc=="round" ~ round(pred),
                                   calc=="ceiling" ~ ceiling(pred),
                                   calc=="floor" ~ floor(pred)))

  #join back to data
  out <- dat.abun  %>%
    dplyr::select(colnames(dat)) %>%
    rbind(data %>%
            dplyr::filter(abundance!="TMTT"))

  #return the unmarked object
  return(out)

  #remove the lookup table
  rm(.tmtt)

}
