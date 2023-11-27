
# wildRtrax <img src="man/figures/logo.png" width="40%" align="right" />

<!-- badges: start -->

[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![CRAN status](https://www.r-pkg.org/badges/version/wildRtrax)](https://CRAN.R-project.org/package=wildRtrax)
[![R-CMD-check](https://github.com/ABbiodiversity/wildRtrax/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ABbiodiversity/wildRtrax/actions/workflows/R-CMD-check.yaml)

<!-- badges: end -->

## Overview

`wildRtrax` (pronounced *wild-R-tracks*) is an R package containing functions to help manage and analyze environmental sensor data. It helps to simplify the entire data life cycle by offering tools for data pre-processing, wrangling, and analysis, facilitating seamless data transfer to and from [WildTrax](https://www.wildtrax.ca/home.html). With `wildRtrax`, users can effortlessly establish end-to-end workflows and ensure reproducibility in their analyses. By providing a consistent and organized framework, the package promotes transparency and integrity in research, making it effortless to share and replicate results.

## Installation

You can install version 1.1.0 of `wildRtrax` directly from this repository with:

``` r
# install.packages("remotes")
remotes::install_github("ABbiodiversity/wildRtrax")
```

The [development](https://github.com/ABbiodiversity/wildRtrax/tree/development) version of this package contains experimental features and recent fixes. It can be installed with: 

```r
remotes::install_github("ABbiodiversity/wildRtrax@development")
```

The development version of the package will be periodically merged and will be reflected in the [Changelogs](https://abbiodiversity.github.io/wildRtrax/news/index.html).

## Usage

All functions begin with a `wt_*` prefix. Column names and metadata align with the WildTrax infrastructure. The goal is to follow the work flow of pre-processing, linking with WildTrax, download and analysis. Some functions are built specifically for ARUs :sound:, cameras :camera: or point counts :bird:, while others are used for all sensors. :bat: denotes ultrasonic functionalities.

- Pre-process acoustic data
  - `wt_audio_scanner()` :sound: :bat:
  - `wt_run_ap()` :sound:
  - `wt_glean_ap()` :sound:
  - `wt_signal_level()` :sound:
  - `wt_chop()` :sound: :bat:
  - `wt_make_aru_tasks()` :sound: :bat:
  - `wt_songscope_tags()` :sound:
  - `wt_kaleidoscope_tags()` :sound: :bat:
- Download data from WildTrax
  - `wt_auth()` :camera: :sound: :bird: :bat:
  - `wt_get_download_summary()` :camera: :sound: :bird: :bat:
  - `wt_download_report()` :camera: :sound: :bird: :bat:
    - Available reports: `main, project, location, recording, image_set, image, tag, point_count, megadetector, megaclassifer, birdnet`
  - `wt_get_species()` :camera: :sound: :bird: :bat:
  - `wt_download_tags()` :sound: :bat:
- Analyze data
  - `wt_summarise_cam()` :camera:
  - `wt_ind_detect()` :camera:
- Convenience functions
  - `wt_location_distances()` :sound: :camera: :bird: :bat:
  - `wt_tidy_species()` :sound: :camera: :bird: :bat:
  - `wt_replace_tmtt()` :sound:
  - `wt_make_wide()` :sound: :bird: :bat:
  - `wt_format_occupancy()` :sound: :bird:
  - `wt_qpad_offsets()` :sound: :bird:

## Issues

To report bugs, request additional features, or get help using the package, please file an
[issue](https://github.com/ABbiodiversity/wildRtrax/issues).

## Contributors

* `wildRtrax` is authored, created and maintained by [Alexander G. MacPhail](https://github.com/agmacpha) and [Marcus Becker](https://github.com/mabecker89), with significant contributions from [Dr. Elly Knight](https://github.com/ecknight) and [Dr. Richard Hedley](https://richardwhedley.wordpress.com/). We encourage ongoing contributions and collaborations to improve the package into the future. The [Alberta Biodiversity Monitoring Institute](https://abmi.ca) provides ongoing support, development and funding.

## License

This R package is licensed under [MIT license](https://github.com/ABbiodiversity/wildRtrax/blob/master/LICENSE)© 2023 Marcus Becker, Alex MacPhail, Elly Knight and the [Alberta Biodiversity Monitoring Institute](https://abmi.ca).

## Code of Conduct

Please note that `wildRtrax` is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this project, you agree to abide by its terms.
