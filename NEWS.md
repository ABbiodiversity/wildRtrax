# wildrtrax 1.3.0

## Major changes

* Addition of five new functions: 
  * `wt_dd_summary()` for querying data from Data Discover. See [APIs](https://abbiodiversity.github.io/wildrtrax/articles/apis.html#data-discover) for more information
  * `wt_evaluate_classifier()`, `wt_get_threshold()`, and `wt_additional_species()` for wrangling acoustic automated classification results. See [Acoustic classifiers](https://abbiodiversity.github.io/wildrtrax/articles/acoustic-classifiers.html) for more information.
  * `wt_add_grts()` to intersect locations with GRTS IDs from [NABat](https://www.nabatmonitoring.org/)
* `wt_download_tags()` now becomes `wt_download_media()` to support broader media downloads in batch from WildTrax
* Deprecated `wt_report()`

## Minor changes

* Switch to `curl::curl_download()` for media and assets
* Removed dependencies `pipeR`, `progressr`, `jsonlite`, `future`, `furrr`, `tools`, `magrittr`, `markdown`, `rmarkdown` to increase package stability but reduces speed for functions such as `wt_audio_scanner()`, `wt_run_ap()`. Moved `vembedr` to suggests for vignettes
* Switched `wt_download_report()` to POST requests
* Lowercase package name

---

# wildrtrax 1.2.0

## Major changes

* `wt_chop()` now recurses across all input files
* Moving geospatial assets to new repository to lighten package size. Asset requests are now made only through usage of `wt_qpad_offsets()`.

## Minor changes

* Improvements to APIs and acoustic convenience functions to resolve issues and PRs
* Improvements to test suite, testing dependencies, code coverage
* Addition of [Camera data wrangling vignette](https://abbiodiversity.github.io/wildrtrax/articles/camera-data-wrangling.html) and additional [tutorials](https://abbiodiversity.github.io/wildrtrax/articles/tutorials.html)

---

# wildrtrax 1.1.0

## Major changes

* `wildrtrax` now honours new WildTrax report structures. Future changes will incorporate standardized naming in syncing functions.
* Replaced geospatial functionalities from `rgdal`, `rgeos` and `maptools` with `sf`, `sp` and `terra` packages. Added functionality with the `suntools` package. Users should re-download the package by October 2023 in-line with the former package retirement: https://geocompx.org/post/2023/rgdal-retirement/.

## Minor changes

* Tweaks to [Acoustic data wrangling](https://abbiodiversity.github.io/wildrtrax/articles/acoustic-data-wrangling.html) for (#16)
* Addition of geospatial assets. Users should be warned package size is now ~40 MB.
* Moved TMTT predictions from csv to .RDS file.
* Work flow repairs to `wt_get_species()` and `wt_tidy_species()` (#21)
* Replaced `utils::read.csv()` to `readr::read_csv()` in `wt_download_report()` (#20)

---

# wildrtrax 1.0.1

* Patching API errors in `wt_download_report()`
* Adding additional articles on [Acoustic data wrangling](https://abbiodiversity.github.io/wildrtrax/articles/acoustic-data-wrangling.html)

---

# wildrtrax 1.0.0

## Major changes 

* Improvements to `wt_audio_scanner()`
  * Addition of *flac* as file type
  * Addition of `extra_cols` argument to enable faster scanning when argument is set to `FALSE`. This also deals with headerless file errors for (#2)
  * Enabled parallel file scanning; microbenchmarked base scanning at 5.6x faster on a dual-core machine
  * Moved progress bars to the `progressr` package
* Addition of `wt_glean_ap()` function to acoustic pre-processing work flow to extract desired data from a `wt_run_ap()` output
* Addition of linking functions in order to add desired media and metadata to WildTrax: `wt_make_aru_tasks()`, `wt_kaleidoscope_tags()` and `wt_songscope_tags()`
* Addition of convenience functions: `wt_location_distances()` and `wt_chop()`
* Alignment of `wt_download_report()` with column headers released in [WildTrax Phase 8](https://wildtrax.ca/phase-8-spring-2023/) to resolve (#3, #4, #5)
* Addition of additional acoustic functions to prepare data for analysis: `wt_replace_tmtt()`, `wt_make_wide()`, `wt_format_occupancy()`, `wt_qpad_offsets()`
* Addition of `wt_get_species()` to download the WildTrax species table and `wt_tidy_species()` to filter various taxa
* Addition of `wt_download_tags()` to download images, spectrograms and audio clips from tags
* Experimental testing of customizable, automated reports with `wt_report()`
* Long-form documentation available for full-cycle environmental work flows and new articles for usage of acoustic and camera data analysis functions

## Minor improvements and bug fixes

* Moved `wt_run_ap()` to `furrr::future_map` from `dopar` loop to lessen package dependencies
* Quiet console output from `wt_run_ap()` for Windows users
* Added a `NEWS.md` file to track changes to the package
* Renamed `wt_ind_det` to `wt_ind_detect()`

## Deprecated 

* `wt_prob_det()`

---

# wildrtrax 0.1.0

* Addition of base functions:
  * **Acoustic**
    * `wt_audio_scanner()`, `wt_run_ap()`, `wt_signal_level()`, `wt_prob_det()`
  * **Camera**
    * `wt_ind_det`, `wt_summarise_cam()`
  * **Authorization and Download from WildTrax**
    * `wt_auth()`, `wt_get_download_summary()`, `wt_download_report()`
