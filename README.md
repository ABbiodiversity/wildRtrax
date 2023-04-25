
# wildRtrax <img src="man/figures/hex-logo-pipit.png" width="40%" align="right" />

<!-- badges: start -->

[![Travis build
status](https://travis-ci.com/ABbiodiversity/wildRtrax.svg?branch=master)](https://travis-ci.com/ABbiodiversity/wildRtrax)
[![CRAN status](https://www.r-pkg.org/badges/version/wildRtrax)](https://CRAN.R-project.org/package=wildRtrax)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

## Overview

`wildRtrax` (pronounced *‘wilder tracks’*) is an R package to help environmental sensor data users to create full-cycle work flows from data management to analytics.

With `wildRtrax`, users can easily create end-to-end workflows for managing and analyzing environmental sensor data. The package provides a wide range of tools and functions that streamline the entire data lifecycle, from data import and cleaning to data visualization and statistical analysis.

`wildRtrax` also promotes reproducibility in data analysis by providing a consistent and organized framework for managing data and analysis workflows. This helps users to maintain transparency and integrity in their research, making it easier to share and reproduce results.

## Installation

You can install `wildRtrax` directly from this repository with:

``` r
# install.packages("remotes")
remotes::install_github("ABbiodiversity/wildRtrax")
```

Or the development version with:

``` r
remotes::install_github("ABbiodiversity/wildRtrax@development")
```

## Usage

All functions begin with a `wt_*` prefix. Column names and metadata align with the WildTrax infrastructure. The goal is to follow the work flow of pre-processing, linking with WildTrax, download and analysis. 

- Pre-process acoustic data
  - `wt_audio_scanner` :sound:
  - `wt_run_ap` :sound:
  - `wt_glean_ap` :sound:
  - `wt_signal_level` :sound:
- Organize acoustic media and metadata for upload to WildTrax
  - `wt_make_aru_tasks` :sound:
  - `wt_kaleidoscope_tags` :sound:
  - `wt_songscope_tags` :sound:
- Download processed data from WildTrax
  - `wt_auth` :camera: :sound: :bird:
  - `wt_get_download_summary` :camera: :sound: :bird:
  - `wt_download_report` :camera: :sound: :bird:
  - `wt_download_tags` :sound: :camera:
- Analyze data
  - `wt_summarise_cam` :camera:
  - `wt_ind_det` :camera:
- Convenience functions
  - `wt_chop` :sound:
  - `wt_location_distances` :sound: :camera: :bird:
  - `wt_replace_tmtt` :sound:
  - `wt_tidy_species` :sound: :camera: :bird:
  - `wt_make_wide` :sound: :camera: :bird:
  - `wt_occupancy_format` :sound: :bird:
  - `wt_qpad_offsets` :sound: :bird:

## Issues

To report bugs, request additional features, or get help using the package, please file an
[issue](https://github.com/ABbiodiversity/wildRtrax/issues).

## Contributors

* `wildRtrax` is authored and created by [Alexander G. MacPhail](https://github.com/agmacpha), created and maintained by [Marcus Becker](https://github.com/mabecker89) and created by [Dr. Elly Knight](https://github.com/ecknight). The [Alberta Biodiversity Monitoring Institute](http://https://abmi.ca/home.html) provides ongoing support, development and funding for the package.
* Many thanks to [Dr. Richard Hedley](https://richardwhedley.wordpress.com/) for providing the basis for the `wt_wac_info` internal function for *wac* file support

## License

This R package is licensed under [MIT license](https://github.com/ABbiodiversity/wildRtrax/blob/master/LICENSE)© 2020 Marcus Becker, Alex MacPhail, Elly Knight and the [Alberta Biodiversity Monitoring Institute](http://https://abmi.ca/home.html).

## Code of Conduct

Please note that `wildRtrax` is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this project, you agree to abide by its terms.
