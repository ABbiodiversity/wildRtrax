library(testthat)
library(wildRtrax)

################################### ARU Test suite

test_that("Authentication works correctly", {
  Sys.setenv(WT_USERNAME = "guest", WT_PASSWORD = "Apple123")
  wt_auth(force = TRUE)
  expect_true(!is.null(wt_get_download_summary(sensor_id = 'ARU')))
  })

test_that("Downloading report", {
  mpb <- wt_download_report(605, 'ARU', 'main', FALSE)
  expect_true(!is.null(mpb))
})

test_that("Tidying species", {
  mpb_tidy <- wt_tidy_species(mpb, remove = c("mammal", "abiotic", "amphibians"))
  expect_true(nrow(mpb_tidy) < nrow(mpb))
})

test_that("Replacing TMTT", {
  mpb_tmtt <- wt_replace_tmtt(mpb_tidy) %>%
    select(individual_count) %>%
    distinct()
  expect_condition(!('TMTT' %in% mpb_tmtt))
})

test_that('Making data wide', {
  mpb_wide <- wt_make_wide(mpb_tmtt, sound = "all")
  expect_condition(ncol(mpb_wide) > ncol(mpb_tmtt))
})

test_that('Getting QPAD offsets', {
  mpb_qpad <- wt_qpad_offsets(mpb_tmtt, species = "OVEN", version = 3, together = F, sensor = 'ARU')
  expect_condition(ncol(mpb_qpad) == 1)
})

test_that('Occupancy formatting', {
  mpb_occu <- wt_format_occupancy(mpb_tmtt, species = "OVEN")
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
