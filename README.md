
# wildRtrax <img src="man/figures/logo.png" width="40%" align="right" />

<!-- badges: start -->

[![Travis build status](https://travis-ci.com/ABbiodiversity/wildRtrax.svg?branch=master)](https://travis-ci.com/ABbiodiversity/wildRtrax)
[![CRAN status](https://www.r-pkg.org/badges/version/wildRtrax)](https://CRAN.R-project.org/package=wildRtrax)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

<!-- badges: end -->

## Overview

`wildRtrax` (pronounced *wild-R-tracks*) is an R package containing functions to help manage and analyze environmental sensor data. It helps to simplify the entire data life cycle by offering tools for data pre-processing, wrangling, and analysis, facilitating seamless data transfer to and from [WildTrax](https://www.wildtrax.ca/home.html). With `wildRtrax`, users can effortlessly establish end-to-end workflows and ensure reproducibility in their analyses. By providing a consistent and organized framework, the package promotes transparency and integrity in research, making it effortless to share and replicate results.

## Installation

You can install `wildRtrax` directly from this repository with:

``` r
# install.packages("remotes")
remotes::install_github("ABbiodiversity/wildRtrax")
```

## Usage

All functions begin with a `wt_*` prefix. Column names and metadata align with the WildTrax infrastructure. The goal is to follow the work flow of pre-processing, linking with WildTrax, download and analysis. Some functions are built specifically for ARUs :sound:, cameras :camera: or point counts :bird:, while others are used for all sensors. :bat: denotes ultrasonic functionalities.

- Pre-process acoustic data
  - `wt_audio_scanner()` :sound:
  - `wt_run_ap()` :sound:
  - `wt_glean_ap()` :sound:
  - `wt_signal_level()` :sound:
  - `wt_chop()` :sound:
  - `wt_make_aru_tasks()` :sound:
  - `wt_songscope_tags()` :sound:
  - `wt_kaleidoscope_tags()` :sound: :bat:
- Download processed data from WildTrax
  - `wt_auth()` :camera: :sound: :bird: :bat:
  - `wt_get_download_summary()` :camera: :sound: :bird: :bat:
  - `wt_download_report()` :camera: :sound: :bird: :bat:
    - Available reports: `main, project, location, recording / image / point count, tag, megadetector, megaclassifer, birdnet`
  - `wt_get_species()` :camera: :sound: :bird: :bat:
- Analyze data
  - `wt_summarise_cam()` :camera:
  - `wt_ind_det()` :camera:
- Convenience functions
  - `wt_location_distances()` :sound: :camera: :bird: :bat:
  - `wt_tidy_species()` :sound: :camera: :bird: :bat:
  - `wt_replace_tmtt()` :sound:
  - `wt_make_wide()` :sound: :camera: :bird: :bat:
  - `wt_occupancy_format()` :sound: :bird:
  - `wt_qpad_offsets()` :sound: :bird:

## Issues

To report bugs, request additional features, or get help using the package, please file an
[issue](https://github.com/ABbiodiversity/wildRtrax/issues).

## Contributors

* `wildRtrax` is authored and created by [Alexander G. MacPhail](https://github.com/agmacpha), created and maintained by [Marcus Becker](https://github.com/mabecker89) and created by [Dr. Elly Knight](https://github.com/ecknight). The [Alberta Biodiversity Monitoring Institute](http://https://abmi.ca/home.html) provides ongoing support, development and funding for the package.
* Many thanks to [Dr. Richard Hedley](https://richardwhedley.wordpress.com/) for providing the basis for the `wt_wac_info()` internal function for *wac* file support

## License

This R package is licensed under [MIT license](https://github.com/ABbiodiversity/wildRtrax/blob/master/LICENSE)© 2023 Marcus Becker, Alex MacPhail, Elly Knight and the [Alberta Biodiversity Monitoring Institute](http://https://abmi.ca/home.html).

## Code of Conduct

Please note that `wildRtrax` is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this project, you agree to abide by its terms.
