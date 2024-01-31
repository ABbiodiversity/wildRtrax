library(testthat)

################################### ARU Test suite

test_that("Authentication works correctly", {
  Sys.setenv(WT_USERNAME = "guest", WT_PASSWORD = "Apple123")
  wt_auth(force = TRUE)
  expect_true(!is.null(wt_get_download_summary(sensor_id = 'ARU')))
})

test_that("Downloading ARU report", {
  ecosys21_aru <- wt_download_report(685, 'ARU', 'main', FALSE)
  expect_true(!is.null(ecosys21_aru))
})

test_that("Downloading CAM report", {
  Sys.setenv(WT_USERNAME = "guest", WT_PASSWORD = "Apple123")
  wt_auth(force = TRUE)
  ecosys21_cam <- wt_download_report(799, 'CAM', 'main', FALSE)
  expect_true(!is.null(ecosys21_cam))
})

test_that("Downloading PC report", {
  Sys.setenv(WT_USERNAME = "guest", WT_PASSWORD = "Apple123")
  wt_auth(force = TRUE)
  lpb_pc <- wt_download_report(888, 'PC', 'main', FALSE)
  expect_true(!is.null(lpb_pc))
})

test_that("Testing a Private Project", {
  Sys.setenv(WT_USERNAME = "guest", WT_PASSWORD = "Apple123")
  wt_auth(force = TRUE)
  expect_error(wt_download_report(1373, 'ARU', 'main', FALSE))
})

test_that("Downloading ARU as PC report", {
  Sys.setenv(WT_USERNAME = "guest", WT_PASSWORD = "Apple123")
  wt_auth(force = TRUE)
  ecosys21_as_pc <- wt_download_report(685, 'PC', 'main', FALSE)
  expect_true(!is.null(ecosys21_as_pc))
})

test_that("Attempting PC as ARU report", {
  Sys.setenv(WT_USERNAME = "guest", WT_PASSWORD = "Apple123")
  wt_auth(force = TRUE)
  #TODO: should have better error message
  expect_error({
    ecosys21_as_pc <- wt_download_report(888, 'ARU', 'main', FALSE)
  })

})

# Download report to use for testing
Sys.setenv(WT_USERNAME = "guest", WT_PASSWORD = "Apple123")
wt_auth(force = TRUE)
ecosys21 <- wt_download_report(685, 'ARU', 'main', FALSE)

test_that("Tidying species", {
  ecosys21_tidy <- wt_tidy_species(ecosys21, remove = c("mammals", "abiotic", "amphibians"), zerofill = T, "ARU")
  expect_true(nrow(ecosys21_tidy) < nrow(ecosys21))
})

test_that("Replacing TMTT", {
  ecosys21_tidy <- wt_tidy_species(ecosys21, remove = c("mammals", "abiotic", "amphibians"), zerofill = T, "ARU")
  ecosys21_tmtt <- wt_replace_tmtt(ecosys21_tidy, calc = "round") %>%
    select(individual_count) %>%
    distinct()
  expect_true(!('TMTT' %in% ecosys21_tmtt))
})

test_that('Making wide', {
  ecosys21_tidy <- wt_tidy_species(ecosys21, remove = c("mammals", "abiotic", "amphibians"), zerofill = T, "ARU")
  ecosys21_tmtt <- wt_replace_tmtt(ecosys21_tidy, calc = "round")
  ecosys21_wide <- wt_make_wide(ecosys21_tmtt, sound = "all", sensor = 'ARU')
  expect_true(ncol(ecosys21_wide) > ncol(ecosys21_tmtt))
})

test_that('Zerofill and tmtt are respected after wide', {
  # add a location with 0 birds observed
  ecosys21_mod_row <- slice(ecosys21, 1)
  ecosys21_mod_row$species_code <- "MOBA"
  ecosys21_mod_row$species_scientific_name <- NA
  ecosys21_mod_row$vocalization <- "Non-vocal"
  ecosys21_mod_row$location_id <- ecosys21_mod_row$location_id+max(ecosys21$location_id)
  ecosys21_mod <- bind_rows(ecosys21, ecosys21_mod_row)

  # all locations from input should be present in outputs unless not transcribed
  ecosys21_locs <- ecosys21_mod %>% filter(aru_task_status == "Transcribed") %>%
    distinct(location_id)

  ecosys21_tidy <- wt_tidy_species(ecosys21_mod, remove = c("mammals", "abiotic", "amphibians"), zerofill = T, "ARU")
  ecosys21_locs %>% anti_join(ecosys21_tidy, by = "location_id") %>%
    pull(location_id) %>%
    expect_length(0)

  ecosys21_tmtt <- wt_replace_tmtt(ecosys21_tidy, calc = "round")
  ecosys21_locs %>% anti_join(ecosys21_tmtt, by = "location_id") %>%
    pull(location_id) %>%
    expect_length(0)

  ecosys21_wide <- wt_make_wide(ecosys21_tmtt, sound = "all", sensor = 'ARU')
  ecosys21_locs %>% anti_join(ecosys21_wide, by = "location_id") %>%
    pull(location_id) %>%
    expect_length(0)

  # total abundance should be 0
  filter(ecosys21_wide, location_id == ecosys21_mod_row$location_id) %>%
    rowwise() %>%
    mutate(tot_birds = sum(c_across(matches("^....$")))) %>%
    pull(tot_birds) %>%
    expect_equal(0)

})

test_that('Getting QPAD offsets', {
  ecosys21_tidy <- wt_tidy_species(ecosys21, remove = c("mammals", "abiotic", "amphibians"), zerofill = T, "ARU")
  ecosys21_tmtt <- wt_replace_tmtt(ecosys21_tidy, calc = "round")
  ecosys21_wide <- wt_make_wide(ecosys21_tmtt, sound = "all", sensor = 'ARU')
  ecosys21_qpad <- wt_qpad_offsets(ecosys21_wide, species = "all", version = 3, together = F, sensor = 'ARU')
  expect_condition(ncol(ecosys21_qpad) == 1)
})

test_that('Occupancy formatting', {
  ecosys21_tidy <- wt_tidy_species(ecosys21, remove = c("mammals", "abiotic", "amphibians"), zerofill = T, "ARU")
  ecosys21_tmtt <- wt_replace_tmtt(ecosys21_tidy, calc = "round")
  occu <- wt_format_occupancy(ecosys21_tmtt, species = "OVEN")
  expect_s4_class(occu, 'unmarkedFrameOccu')
})

##wt_audio_scanner
##wt_run_ap
##wt_glean_ap
##wt_chop
##wt_location_distances

################################### Camera Test suite

##wt_summarise_cam
##wt_ind_detect

################################### Point count test suite

test_that('QPAD for PC', {
  pc_data <- wt_download_report(2016, 'PC', 'main', FALSE)
  pc_tidy <- wt_tidy_species(pc_data, remove = c("mammals", "abiotic", "amphibians"), zerofill = T, sensor = 'PC')
  pc_wide <- wt_make_wide(pc_tidy, sound = "all", sensor = 'PC')
  pc_qpad <- wt_qpad_offsets(pc_wide, species = "OVEN", version = 3, together = F, sensor = 'PC')
  expect_true(ncol(pc_qpad) == 1)
})

test_that('Occupancy for PC', {
  pc_data <- wt_download_report(2016, 'PC', 'main', FALSE)
  pc_tidy <- wt_tidy_species(pc_data, remove = c("mammals", "abiotic", "amphibians"), zerofill = T, sensor = 'PC')
  occu <- wt_format_occupancy(pc_tidy, species = "OVEN")
  expect_condition(class(occu)[1] == 'unmarkedFrameOccu')
})


test_that('Error for TMTT', {
  pc_data <- wt_download_report(2016, 'PC', 'main', FALSE)
  pc_tidy <- wt_tidy_species(pc_data, remove = c("mammals", "abiotic", "amphibians"), zerofill = T, sensor = 'PC')
  expect_error(wt_replace_tmtt(pc_tidy, calc = 'round'))
})
