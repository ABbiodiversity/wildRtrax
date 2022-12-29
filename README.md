
# wildRtrax <img src="man/figures/hex-logo-pipit.png" width="40%" align="right" />

<!-- badges: start -->

[![Travis build
status](https://travis-ci.com/ABbiodiversity/wildRtrax.svg?branch=master)](https://travis-ci.com/ABbiodiversity/wildRtrax)
<!-- badges: end -->

## Overview

`wildRtrax` (pronounced *‘wilder tracks’*) is an R package to help ecologists create full-cycle environmental sensor data work flows within [WildTrax](https://www.wildtrax.ca/home.html).

- Pre-process acoustic data
  - `wt_audio_scanner`
  - `wt_run_ap`
  - `wt_glean_ap`
  - `wt_signal_level`
- Upload acoustic media and metadata to WildTrax
  - `wt_make_aru_tasks`
  - `wt_kaleidoscope_tags`
  - `wt_songscope_tags`
- Download processed data from WildTrax
  - `wt_auth`
  - `wt_get_download_summary`
  - `wt_download_report`
- Analyze data
  - `wt_replace_tmtt`
  - `wt_occupancy`
  - `wt_summarise_cam`
  - `wt_ind_det`

## Installation

You can install the development version of `wildRtrax` directly from this
repository with:

``` r
# install.packages("remotes")
remotes::install_github("ABbiodiversity/wildRtrax@development")
```

## Usage

All functions begin with a `wt_*` prefix. Column names and metadata align with the WildTrax infrastructure. 

## Issues

To report bugs, request additional features, or get help using the package, please file an
[issue](https://github.com/ABbiodiversity/wildRtrax/issues).

## Contributors

* `wildRtrax` is authored and created by [Alexander G. MacPhail](), created and maintained by [Marcus Becker]() and created by [Dr. Elly Knight](). The [Alberta Biodiversity Monitoring Institute](http://https://abmi.ca/home.html) provides ongoing support, development and funding for the package.
* Many thanks to [Dr. Richard Hedley]() for providing the basis for the `wt_wac_info` internal function for *wac* file support

## License

This R package is licensed under [MIT license](https://github.com/ABbiodiversity/wildRtrax/blob/master/LICENSE)© 2020 Marcus Becker, Alex MacPhail, Elly Knight and the [Alberta Biodiversity Monitoring Institute](http://https://abmi.ca/home.html).
