## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----Examples of reading acoustic data into R, include = TRUE, echo = TRUE, eval = FALSE----
#  root <- "/volumes/GoogleDrive/Shared drives/wildRtrax/data/example/"
#  
#  file <- paste0(root,"ABMI-0754-SW_20170301_085900.wav")
#  
#  wave_t <- tuneR::readWave(file, header = T) #True header format
#  
#  wave_f <- tuneR::readWave(file, header = F)
#  
#  list(wave_t, wave_f)
#  

## ----Accessing S4 and lists, include = TRUE, echo = TRUE, eval = FALSE--------
#  sound_length_S4 <- round((wave_f@left / wave_f@samp.rate), 2)
#  
#  #Is equivalent to:
#  sound_length_list <- wave_t$samples / wave_t$sample.rate
#  

## ---- echo = T, eval = F, warnings = FALSE, include = TRUE--------------------
#  #Plot a spectrogram
#  v <- seewave::ggspectro(tuneR::readWave(file, from = 0, to = 60, units = "seconds"), ovlp = 50) + ggplot2::geom_tile(aes(fill=amplitude)) + theme_bw()
#  

