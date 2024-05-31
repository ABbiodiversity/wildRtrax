library(testthat)
library(dplyr)

test_that("wt_tidy_species and related functions work as expected", {
  # Set up environment and authenticate
  Sys.setenv(WT_USERNAME = "guest", WT_PASSWORD = "Apple123")
  wt_auth(force = TRUE)

  # Download the report
  ecosys21 <- wt_download_report(685, 'ARU', 'main', FALSE)

  # Add a location with 0 birds observed
  ecosys21_mod_row <- slice(ecosys21, 1)
  ecosys21_mod_row$species_code <- "MOBA"
  ecosys21_mod_row$species_scientific_name <- NA
  ecosys21_mod_row$vocalization <- "Non-vocal"
  ecosys21_mod_row$location_id <- ecosys21_mod_row$location_id + max(ecosys21$location_id)
  ecosys21_mod <- bind_rows(ecosys21, ecosys21_mod_row)

  # All locations from input should be present in outputs unless not transcribed
  ecosys21_locs <- ecosys21_mod %>% filter(aru_task_status == "Transcribed") %>%
    distinct(location_id)

  ecosys21_tidy <- wt_tidy_species(ecosys21_mod, remove = c("mammal", "abiotic", "amphibian"), zerofill = TRUE)
  ecosys21_tmtt <- wt_replace_tmtt(ecosys21_tidy, calc = "round")
  ecosys21_wide <- wt_make_wide(ecosys21_tmtt, sound = "all")

  # Total abundance should be 0 for the modified row
  tot_birds <- ecosys21_wide %>%
    filter(location_id == ecosys21_mod_row$location_id) %>%
    rowwise() %>%
    mutate(tot_birds = sum(c_across(matches("^....$"))))
  expect_equal(nrow(tot_birds), 0)
})
