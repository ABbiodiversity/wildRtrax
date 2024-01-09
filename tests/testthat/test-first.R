library(testthat)
library(wildRtrax)

################################### ARU Test suite

test_that("Authentication works correctly", {
  Sys.setenv(WT_USERNAME = "guest", WT_PASSWORD = "Apple123")
  wt_auth(force = TRUE)
  expect_true(!is.null(wt_get_download_summary(sensor_id = 'ARU')))
  })

test_that("Downloading report", {
  ecosys21 <- wt_download_report(605, 'ARU', 'main', FALSE)
  expect_true(!is.null(ecosys21))
})

test_that("Tidying species", {
  ecosys21_tidy <- wt_tidy_species(ecosys21, remove = c("mammal", "abiotic", "amphibians"), zerofill = T)
  expect_true(nrow(ecosys21_tidy) < nrow(ecosys21))
})

test_that("Replacing TMTT", {
  ecosys21_tmtt <- wt_replace_tmtt(ecosys21_tidy, calc = "round") %>%
    select(individual_count) %>%
    distinct()
  expect_condition(!('TMTT' %in% ecosys21_tmtt))
})

test_that('Making ecosys21 wide', {
  ecosys21_wide <- wt_make_wide(ecosys21_tmtt, sound = "all", sensor = 'ARU')
  expect_condition(ncol(ecosys21_wide) > ncol(ecosys21_tmtt))
})

test_that('Getting QPAD offsets', {
  ecosys21_qpad <- wt_qpad_offsets(ecosys21_wide, species = "all", version = 3, together = T, sensor = 'ARU')
  expect_condition(ncol(ecosys21_qpad) == 1)
})



test_that('Occupancy formatting', {
  mpb_occu <- wt_format_occupancy(ecosys21_tmtt, species = "OVEN")
  expect_condition(class(mpb_occu)[1] == 'unmarkedFrameOccu')
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

test_that("Authentication works correctly", {
  Sys.setenv(WT_USERNAME = "guest", WT_PASSWORD = "Apple123")
  wt_auth(force = TRUE)
  expect_true(!is.null(wt_get_download_summary(sensor_id = 'PC')))
})

test_that("Downloading report", {
  mpb <- wt_download_report(605, 'PC', 'main', FALSE)
  expect_true(!is.null(mpb))
})

test_that("Tidying species", {
  mpb_tidy <- wt_tidy_species(mpb, remove = c("mammal", "abiotic", "amphibians"))
  expect_true(nrow(mpb_tidy) < nrow(mpb))
})
