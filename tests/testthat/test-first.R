library(testthat)
library(wildRtrax)

################################### ARU Test suite

test_that("Authentication works correctly", {
  Sys.setenv(WT_USERNAME = "guest", WT_PASSWORD = "Apple123")
  wt_auth(force = TRUE)
  expect_true(!is.null(wt_get_download_summary(sensor_id = 'ARU')))
  })

test_that("Downloading ARU report", {
  ecosys21_aru <- wt_download_report(605, 'ARU', 'main', FALSE)
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
  ecosys21_as_pc <- wt_download_report(605, 'PC', 'main', FALSE)
  expect_true(!is.null(ecosys21_as_pc))
})

test_that("Attempting PC as ARU report", {
  Sys.setenv(WT_USERNAME = "guest", WT_PASSWORD = "Apple123")
  wt_auth(force = TRUE)
  ecosys21_as_pc <- wt_download_report(888, 'ARU', 'main', FALSE)
  expect_true(!is.null(ecosys21_as_pc))
})

test_that("Tidying species", {
  Sys.setenv(WT_USERNAME = "guest", WT_PASSWORD = "Apple123")
  wt_auth(force = TRUE)
  ecosys21 <- wt_download_report(605, 'ARU', 'main', FALSE)
  ecosys21_tidy <- wt_tidy_species(ecosys21, remove = c("mammal", "abiotic", "amphibians"), zerofill = T)
  expect_true(nrow(ecosys21_tidy) < nrow(ecosys21))
})

test_that("Replacing TMTT", {
  Sys.setenv(WT_USERNAME = "guest", WT_PASSWORD = "Apple123")
  wt_auth(force = TRUE)
  ecosys21 <- wt_download_report(605, 'ARU', 'main', FALSE)
  ecosys21_tidy <- wt_tidy_species(ecosys21, remove = c("mammal", "abiotic", "amphibians"), zerofill = T)
  ecosys21_tmtt <- wt_replace_tmtt(ecosys21_tidy, calc = "round") %>%
    select(individual_count) %>%
    distinct()
  expect_true(!('TMTT' %in% ecosys21_tmtt))
})

test_that('Making wide', {
  Sys.setenv(WT_USERNAME = "guest", WT_PASSWORD = "Apple123")
  wt_auth(force = TRUE)
  ecosys21 <- wt_download_report(605, 'ARU', 'main', FALSE)
  ecosys21_tidy <- wt_tidy_species(ecosys21, remove = c("mammal", "abiotic", "amphibians"), zerofill = T)
  ecosys21_tmtt <- wt_replace_tmtt(ecosys21_tidy, calc = "round")
  ecosys21_wide <- wt_make_wide(ecosys21_tmtt, sound = "all", sensor = 'ARU')
  expect_true(ncol(ecosys21_wide) > ncol(ecosys21_tmtt))
})

test_that('Getting QPAD offsets', {
  Sys.setenv(WT_USERNAME = "guest", WT_PASSWORD = "Apple123")
  wt_auth(force = TRUE)
  ecosys21 <- wt_download_report(605, 'ARU', 'main', FALSE)
  ecosys21_tidy <- wt_tidy_species(ecosys21, remove = c("mammal", "abiotic", "amphibians"), zerofill = T)
  ecosys21_tmtt <- wt_replace_tmtt(ecosys21_tidy, calc = "round")
  ecosys21_wide <- wt_make_wide(ecosys21_tmtt, sound = "all", sensor = 'ARU')
  ecosys21_qpad <- wt_qpad_offsets(ecosys21_wide, species = "all", version = 3, together = F, sensor = 'ARU')
  expect_condition(ncol(ecosys21_qpad) == 1)
})


test_that('Occupancy formatting', {
  Sys.setenv(WT_USERNAME = "guest", WT_PASSWORD = "Apple123")
  wt_auth(force = TRUE)
  ecosys21 <- wt_download_report(605, 'ARU', 'main', FALSE)
  ecosys21_tidy <- wt_tidy_species(ecosys21, remove = c("mammal", "abiotic", "amphibians"), zerofill = T)
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
