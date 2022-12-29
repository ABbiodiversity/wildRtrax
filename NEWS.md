# wildRtrax 1.0.0

## Major changes 

* `wt_auth` is now Google Auth0 enabled; use newargument `type = "Google"` or `type = "OAuth0"` if you use a username and password to authenticate
* Addition of `wt_glance_ap` function to acoustic pre-processing work flow to extract desired data from a `wt_run_ap` output
* Addition of linking functions in order to add desired media and metadata to WildTrax: `wt_make_aru_tasks`, `wt_kaleido_tags` and `wt_songscope_tags`
* Addition of acoustic analysis work flows:
  * `wt_replace_tmtt`
  * `wt_occupancy`
* Long-form documentation available for full-cycle environmental work flows and new articles to acoustic and camera data analysis

## Minor improvements and bug fixes

* Improvements to `wt_audio_scanner`
  * Addition of *flac* as file type to scanner
  * Addition of `extra_cols` argument to enable faster scanning when argument is `FALSE`
  * Enabled parallel file scanning; microbenchmarked base scanning at 5.6x faster on a dual-core machine
* Moved `wt_run_ap` to `future_map` from `dopar` loop to lessen package dependencies
* Added a `NEWS.md` file to track changes to the package.

# wildRtrax 0.1.0

