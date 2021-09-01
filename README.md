
# wildRtrax <img src="man/figures/hex-logo-pipit.png" align="right" />

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/wildRtrax)](https://CRAN.R-project.org/package=wildRtrax)
[![Codecov test
coverage](https://codecov.io/gh/ABbiodiversity/wildRtrax/branch/master/graph/badge.svg)](https://codecov.io/gh/ABbiodiversity/wildRtrax?branch=master)
[![R build
status](https://github.com/ABbiodiversity/wildRtrax/workflows/R-CMD-check/badge.svg)](https://github.com/ABbiodiversity/wildRtrax/actions)
[![Travis build
status](https://travis-ci.com/ABbiodiversity/wildRtrax.svg?branch=master)](https://travis-ci.com/ABbiodiversity/wildRtrax)
<!-- badges: end -->

=============================================================

## Overview

The ``wildRtrax`` (pronounced '*wilder tracks*') package provides a set of functions for
environmental sensor data management to and from [WildTrax](https://www.wildtrax.ca/home.html). The functions currently focus on autonomous recording units (ARUs) but will soon include other types of sensor data. 

## Installation

You can install the development version of wildRtrax with:

```r
# install.packages("devtools")
devtools::install_github("ABbiodiversity/wildRtrax")
```

## Usage

All functions begin with a `wt_*` prefix for ease of use in an R environment

* `wt_audio_scannner` scans through multiple audio file types and returns standard metadata
* `wt_run_ap` runs the [QUT Ecoacoustics AnalysisPrograms](https://github.com/QutEcoacoustics/audio-analysis) software package
* `wt_signal_level` returns the time and relative sound level (RSL) in an audio file with customized settings
* `wt_prob_det` estimates the probability of detection of species, given a location is occupied, based on the audio survey length and the number of surveys conducted

## Issues

To report bugs, request additional features, or get help using the
package, please file an
[issue](https://github.com/ABbiodiversity/wildRtrax/issues).
Alternatively, you can email Alex MacPhail <agmacpha@ualberta.ca> or
Marcus Becker <mabecker@ualberta.ca>.

## License

This R package is licensed under [MIT
license](https://github.com/ABbiodiversity/wildRtrax/blob/master/LICENSE)
© 2020 Marcus Becker, Alex MacPhail, and the ABMI.
