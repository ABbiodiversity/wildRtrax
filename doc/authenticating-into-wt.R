## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = '#>'
)

## ----setup, echo=FALSE, include=FALSE, eval=TRUE------------------------------
#Attach package
library(wildRtrax)

## ----echo=TRUE, eval=FALSE----------------------------------------------------
#  # Note that you need to use 'WT_USERNAME' and 'WT_PASSWORD'.
#  
#  Sys.setenv(WT_USERNAME = 'guest', WT_PASSWORD = 'Apple123')
#  

## ----echo=TRUE, eval=FALSE----------------------------------------------------
#  # Authenticate
#  
#  wt_auth()
#  

## ----echo=TRUE, eval=FALSE----------------------------------------------------
#  #Download the project summary you have access to
#  my_projects <- wt_get_download_summary(sensor_id = 'ARU')
#  
#  my projects
#  

## ----echo=TRUE, eval=FALSE, warning = F, message = F--------------------------
#  #Download the project report
#  my_report <- wt_download_report(project_id = 41, sensor_id = 'ARU', cols_def = F, weather_cols = T)
#  
#  my_report
#  

