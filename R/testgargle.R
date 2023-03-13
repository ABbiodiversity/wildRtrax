library(tidyverse)
library(future)
library(pipeR)
library(furrr)
library(fs)

b2r <- .GlobalEnv$wt_audio_scanner(path = "/volumes/budata/cws-ont/aru/b2r", file_type = "all", tz = "EST", extra_cols = F)

napken <- .GlobalEnv$wt_audio_scanner(path = "/volumes/budata/cws-ont/aru/napken/all2", file_type = "all", tz = "EST", extra_cols = F)

bn <- bind_rows(b2r, napken)

h <- gargle::token_fetch()


wt_auth()

Sys.setenv(WT_USERNAME = "Guest")
Sys.setenv(WT_PASSWORD = "Apple123")

library(wildRtrax)

wildRtrax::wt_auth()

wildRtrax::wt_get_download_summary(sensor_id = "ARU")
