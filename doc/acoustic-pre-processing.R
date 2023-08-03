## ---- include = FALSE, echo = F, include = T, warning = F, message = F--------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = '#>'
)

library(wildRtrax)
library(tidyverse)


## ---- echo=TRUE, include=TRUE, eval=T, warning = F, message = F---------------
# Scan data

if (dir.exists(".")) {
  wt_audio_scanner(path = ".", file_type = "wav", extra_cols = T)
} else {
  'Can\'\t find this directory'
}


## ---- echo=TRUE, include=T, eval=T, warning = F, message = F------------------
# Set the directory path
load("book.RData")
#save.image("book.RData")


## ---- echo=TRUE, include=TRUE, eval=T, warning = F, message = F---------------
files


## ---- echo=TRUE, include=TRUE, eval=T, warning = F, message = F---------------
library(lubridate)

files %>%
  mutate(hour = hour(recording_date_time)) %>%
  filter(julian == 176, 
         hour %in% c(4:8))


## ----echo=TRUE, include=T, eval=F, warning = F, message = F-------------------
#  #Use the wt_* tibble to execute the AP on the files
#  
#  wt_run_ap(x = my_files, output_dir = paste0(root, 'ap_outputs'), path_to_ap = '/where/you/store/AP')
#  

## ---- echo=T, include=T, eval=F, warning=F, message, prompt=T, comment=""-----
#  
#  wt_glean_ap()
#  

## ---- include=TRUE, eval=F, warning=F, message=F------------------------------
#  # Example audio file
#  signal_file <- wt_audio_scanner(path = ".", file_type = "wav", extra_cols = T)
#  
#  # Run
#  s <- wt_signal_level(path = signal_file$file_path[[1]],
#                       fmin = 0,
#                       fmax = 2000,
#                       threshold = 20,
#                       channel = 'left',
#                       aggregate = 5)
#  
#  # Return a list object, with parameters stored
#  str(s)
#  
#  # We can view the output:
#  s['output']
#  # We have eleven detections that exceeded this threshold.
#  

## ---- include = T, eval=F, warning=F, message=F-------------------------------
#  wt_make_aru_tasks(input = testff, task_method = "1SPT", task_length = 180)
#  

## ---- include = T, eval=F, warning=F, message=F, prompt="", prompt=T----------
#  wt_songscope_tags()
#  

## ---- include = T, eval=F, warning=F, message=F-------------------------------
#  wt_kaleidoscope_tags()
#  

