
# wildRtrax <img src="man/figures/hex-logo-pipit.png" width="40%" align="right" />

<!-- badges: start -->

[![Travis build
status](https://travis-ci.com/ABbiodiversity/wildRtrax.svg?branch=master)](https://travis-ci.com/ABbiodiversity/wildRtrax)
<!-- badges: end -->

## Overview

The `wildRtrax` (pronounced *‘wilder tracks’*) package provides a set of
functions for environmental sensor data (autonomous recording units and
remote game cameras) management to and from
[WildTrax](https://www.wildtrax.ca/home.html).

## Installation

You can install the development version of wildRtrax directly from this
repository with:

``` r
# install.packages("remotes")
remotes::install_github("ABbiodiversity/wildRtrax")
```

## Usage

All functions begin with a **wt_*** prefix 

``` r
library(wildRtrax)
library(tidyverse)
library(vroom)

# Scan directories of files using wt_audio_scanner

test_files <- "/path/to/files"

files <- **wt_audio_scanner**("/path/to/files/", file_type = "both")

# Apply a limited amplitude filter

files_lim_amp <- files %>% 
      mutate(thresholds = future_map(.x = file_path, .f = ~ **wt_signal_level**(.x, fmin = 0, fmax = 2000, threshold = 55, aggregate = 10))) 

# Create long-duration false-colour spectrograms and generate acoustic index values using the QUT Ecoacoustics package

path_indices <- "/path/to/results"

**wt_run_ap**(test_files, output_dir = path_indices, path_to_ap = /path/to/ap)

#Extract the index values - find the files...
index_files <- fs::dir_ls(path = path_indices,
                   regexp = '*Towsey.Acoustic.Indices.csv',
                   recurse = TRUE)

#Which acoustic indices do you want to use?
index_list <-
  c(
    "Snr",
    "BackgroundNoise",
    "AcousticComplexity",
    "TemporalEntropy",
    "Ndsi",
    "ResultMinute",
    "FileName"
  )

#...then vroom together the indices you want from all the csvs

test_indices <- vroom(index_files, col_select = index_list)

# Join the index values to the wt_audio_scanner tibble
test_join <-
  files %>% inner_join(., test_indices, by = c("file_name" = "FileName")) %>%
  pivot_longer(cols = Snr:Ndsi,
               names_to = "index_variable",
               values_to = "index_value") %>%
  #Plot a graph of the indices
  ggplot(.,
         aes(x = ResultMinute, y = index_value, colour = index_variable)) +
  geom_point() +
  blanktheme +
  facet_wrap(~ index_variable, ncol = 1)
test_join
```

## Issues

To report bugs, request additional features, or get help using the
package, please file an
[issue](https://github.com/ABbiodiversity/wildRtrax/issues).
Alternatively, you can email
Marcus Becker <mabecker@ualberta.ca> or Alex MacPhail <agmacpha@ualberta.ca>.

## License

This R package is licensed under [MIT
license](https://github.com/ABbiodiversity/wildRtrax/blob/master/LICENSE)
© 2020 Marcus Becker, Alex MacPhail, and the
[ABMI](http://https://abmi.ca/home.html)
