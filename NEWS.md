# wildRtrax 1.0.0 'Rose-breasted Grosbeak'

## Major changes 

* `wt_auth` is now also Google Auth0 enabled; use new argument `type = "Google"` or `type = "OAuth0"` if you use a username and password to authenticate
* Addition of `wt_glean_ap` function to acoustic pre-processing work flow to extract desired data from a `wt_run_ap` output
* Addition of linking functions in order to add desired media and metadata to WildTrax: `wt_make_aru_tasks`, `wt_kaleido_tags` and `wt_songscope_tags`
* Alignment of `wt_download_report` with column headers released in [WildTrax Phase 8]() to resolve (#4)
* Addition of acoustic analysis functions:
  * `wt_replace_tmtt`
  * `wt_occupancy`
* Long-form documentation available for full-cycle environmental work flows and new articles for usage of acoustic and camera data analysis functions

## Minor improvements and bug fixes

* Improvements to `wt_audio_scanner`
  * Addition of *flac* as file type
  * Addition of `extra_cols` argument to enable faster scanning when argument is `FALSE` and to deal with headerless files (#2)
  * Enabled parallel file scanning; microbenchmarked base scanning at 5.6x faster on a dual-core machine
  * Moved progress bars from `furrr_options` to `progressr` type bars
* Moved `wt_run_ap` to `future_map` from `dopar` loop to lessen package dependencies
* Quiet console output from `wt_run_ap` for Windows users
* Added a `NEWS.md` file to track changes to the package
* `year` column in `wt_download_report` fixed in response to (#3)

## Deprecated 

* `wt_prob_det`

# wildRtrax 0.1.0 'American Pipit'

* Addition of base functions:
  * **Acoustic**
    * `wt_audio_scanner`, `wt_run_ap`, `wt_signal_level`, `wt_prob_det`
  * **Camera**
    * `wt_ind_det`, `wt_summarise_cam`
  * **Authorization and Download from WildTrax**
    * `wt_auth`, `wt_get_download_summary`, `wt_download_report`
