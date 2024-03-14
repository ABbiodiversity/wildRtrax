library(testthat)

Sys.setenv(WT_USERNAME = "guest", WT_PASSWORD = "Apple123")
wt_auth(force = TRUE)

################################### ARU Test suite

test_that("Authentication works correctly", {
  expect_true(!is.null(wt_get_download_summary(sensor_id = 'ARU')))
  })

test_that("Downloading ARU report", {
  cypress_hills_aru <- wt_download_report(620, 'ARU', 'main', FALSE)
  expect_true(!is.null(cypress_hills))
})

test_that("Testing a Private Project", {
  expect_error(wt_download_report(1373, 'ARU', 'main', FALSE))
})

test_that("Downloading ARU as PC report", {
  cypress_hills_as_pc <- wt_download_report(620, 'PC', 'main', FALSE)
  expect_true(!is.null(cypress_hills_as_pc))
})

test_that("Attempting PC as ARU report", {
  expect_error(wt_download_report(887, 'ARU', 'main', FALSE))
})

test_that("Tidying species zero-filling true", {
  cypress_hills <- wt_download_report(620, 'ARU', 'main', FALSE)
  cypress_hills_tidy <- wt_tidy_species(cypress_hills, remove = c("mammal", "abiotic", "amphibian","unknown"), zerofill = T, sensor = "ARU")
  expect_true(nrow(cypress_hills_tidy) < nrow(cypress_hills))
})

test_that("Tidying species zero-filling false", {
  cypress_hills <- wt_download_report(620, 'ARU', 'main', FALSE)
  cypress_hills_tidy_f <- wt_tidy_species(cypress_hills, remove = c("mammal", "abiotic", "amphibian","unknown"), zerofill = F, sensor = "ARU")
  expect_true(nrow(cypress_hills_tidy_f) < nrow(cypress_hills))
})

test_that("Replacing TMTT", {
  cypress_hills <- wt_download_report(620, 'ARU', 'main', FALSE)
  cypress_hills_tidy <- wt_tidy_species(cypress_hills, remove = c("mammal", "abiotic", "amphibian", "unknown"), zerofill = T, sensor = "ARU")
  cypress_hills_tmtt <- wt_replace_tmtt(cypress_hills_tidy, calc = "round") |>
    select(individual_count) |>
    distinct()
  expect_true(!('TMTT' %in% cypress_hills_tmtt))

})

test_that('Making wide', {
  cypress_hills_aru <- wt_download_report(620, 'ARU', 'main', FALSE)
  cypress_hills_tidy <- wt_tidy_species(ecypress_hills_aru, remove = c("mammal", "abiotic", "amphibian", "unknown"), zerofill = T, sensor = "ARU")
  cypress_hills_tmtt <- wt_replace_tmtt(cypress_hills_tidy, calc = "round")
  cypress_hills_wide <- wt_make_wide(cypress_hills_tmtt, sound = "all", sensor = 'ARU')
  expect_true(ncol(cypress_hills_wide) > ncol(cypress_hills_tmtt))
})

test_that('Getting QPAD offsets', {
  cypress_hills <- wt_download_report(620, 'ARU', 'main', FALSE)
  cypress_hills_tidy <- wt_tidy_species(cypress_hills, remove = c("mammal", "abiotic", "amphibian"), zerofill = T, sensor = "ARU")
  cypress_hills_tmtt <- wt_replace_tmtt(cypress_hills_tidy, calc = "round")
  cypress_hills_wide <- wt_make_wide(cypress_hills_tmtt, sound = "all", sensor = 'ARU')
  cypress_hills_qpad <- wt_qpad_offsets(cypress_hills_wide, species = "all", version = 3, together = F, sensor = 'ARU')
  expect_true(ncol(cypress_hills_qpad) > 1)
})

test_that('Occupancy formatting', {
  cypress_hills <- wt_download_report(620, 'ARU', 'main', FALSE)
  cypress_hills_tidy <- wt_tidy_species(cypress_hills, remove = c("mammal", "abiotic", "amphibian"), zerofill = T, sensor = "ARU")
  cypress_hills_tmtt <- wt_replace_tmtt(cypress_hills_tidy, calc = "round")
  occu <- wt_format_occupancy(cypress_hills_tmtt, species = "OVEN")
  expect_true(class(occu)[1] == 'unmarkedFrameOccu')
})

##Pre-processing
##wt_audio_scanner
##wt_run_ap
##wt_glean_ap
##wt_chop
##wt_location_distances

