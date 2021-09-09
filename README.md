
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
# Scan directories of files using wt_audio_scanner

files <- wt_audio_scanner("/path/to/files/", file_type = "both")


# Detect limited amplitude sounds using wt_signal_level

test_files <- "/users/alexandremacphail/desktop/wav/test2"

files <- wt_audio_scanner(test_files, file_type = "wav")

# Apply a limited amplitude filter
.f = ~ wt_signal_level(.x, fmin = 0, fmax = 2000, threshold = 55, aggregate = 10))) 
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
