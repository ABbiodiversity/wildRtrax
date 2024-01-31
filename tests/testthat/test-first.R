library(testthat)

################################### ARU Test suite

test_that("Authentication works correctly", {
  Sys.setenv(WT_USERNAME = "guest", WT_PASSWORD = "Apple123")
  wt_auth(force = TRUE)
  expect_true(!is.null(wt_get_download_summary(sensor_id = 'ARU')))
})

test_that("Downloading ARU report", {
  ecosys21_aru <- wt_download_report(685, 'ARU', 'main', FALSE)
  expect_true(!is.null(ecosys21))
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
  ecosys21_tidy <- wt_tidy_species(ecosys21_aru, remove = c("mammals", "abiotic", "amphibians"), zerofill = T, "ARU")
  ecosys21_tmtt <- wt_replace_tmtt(ecosys21_tidy, calc = "round")
  ecosys21_wide <- wt_make_wide(ecosys21_tmtt, sound = "all", sensor = 'ARU')
  expect_true(ncol(ecosys21_wide) > ncol(ecosys21_tmtt))
})

test_that('Getting QPAD offsets', {
  ecosys21_tidy <- wt_tidy_species(ecosys21, remove = c("mammals", "abiotic", "amphibians"), zerofill = T, "ARU")
  ecosys21_tmtt <- wt_replace_tmtt(ecosys21_tidy, calc = "round")
  ecosys21_wide <- wt_make_wide(ecosys21_tmtt, sound = "all", sensor = 'ARU')
  ecosys21_qpad <- wt_qpad_offsets(ecosys21_wide, species = "all", version = 3, together = F, sensor = 'ARU')
  expect_condition(ncol(ecosys21_qpad) == 1)
})

wt_make_wide(ecosys21_tmtt, sound = "all", sensor = 'ARU')

test_that('Occupancy formatting', {
  Sys.setenv(WT_USERNAME = "guest", WT_PASSWORD = "Apple123")
  wt_auth(force = TRUE)
  ecosys21 <- wt_download_report(685, 'ARU', 'main', FALSE)
  ecosys21_tidy <- wt_tidy_species(ecosys21, remove = c("mammals", "abiotic", "amphibians"), zerofill = T, "ARU")
  ecosys21_tmtt <- wt_replace_tmtt(ecosys21_tidy, calc = "round")
  occu <- wt_format_occupancy(ecosys21_tmtt, species = "OVEN")
  expect_condition(class(occu)[1] == 'unmarkedFrameOccu')
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
