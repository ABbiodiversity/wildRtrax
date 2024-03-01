library(testthat)

################################### ARU Test suite

test_that("Authentication works correctly", {
  Sys.setenv(WT_USERNAME = "guest", WT_PASSWORD = "Apple123")
  wt_auth(force = TRUE)
  expect_true(!is.null(wt_get_download_summary(sensor_id = 'ARU')))
  })

test_that("Downloading ARU report", {
  cypress_hills_aru <- wt_download_report(620, 'ARU', 'main', FALSE)
  expect_true(!is.null(cypress_hills))
})

test_that("Downloading CAM report", {
  Sys.setenv(WT_USERNAME = "guest", WT_PASSWORD = "Apple123")
  wt_auth(force = TRUE)
  abmi_amph_cam <- wt_download_report(391, 'CAM', 'main', FALSE)
  expect_true(!is.null(abmi_amph_cam))
})

test_that("Downloading PC report", {
  Sys.setenv(WT_USERNAME = "guest", WT_PASSWORD = "Apple123")
  wt_auth(force = TRUE)
  lpb_pc <- wt_download_report(887, 'PC', 'main', FALSE)
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
  cypress_hills_as_pc <- wt_download_report(620, 'PC', 'main', FALSE)
  expect_true(!is.null(cypress_hills_as_pc))
})

test_that("Attempting PC as ARU report", {
  Sys.setenv(WT_USERNAME = "guest", WT_PASSWORD = "Apple123")
  wt_auth(force = TRUE)
  expect_error(wt_download_report(887, 'ARU', 'main', FALSE))
})

test_that("Tidying species zero-filling true", {
  Sys.setenv(WT_USERNAME = "guest", WT_PASSWORD = "Apple123")
  wt_auth(force = TRUE)
  cypress_hills <- wt_download_report(620, 'ARU', 'main', FALSE)
  cypress_hills_tidy <- wt_tidy_species(cypress_hills, remove = c("mammal", "abiotic", "amphibian","unknown"), zerofill = T)
  expect_true(nrow(cypress_hills_tidy) < nrow(cypress_hills))
})

test_that("Tidying species zero-filling false", {
  Sys.setenv(WT_USERNAME = "guest", WT_PASSWORD = "Apple123")
  wt_auth(force = TRUE)
  cypress_hills <- wt_download_report(620, 'ARU', 'main', FALSE)
  cypress_hills_tidy_f <- wt_tidy_species(cypress_hills, remove = c("mammal", "abiotic", "amphibian","unknown"), zerofill = F)
  expect_true(nrow(cypress_hills_tidy_f) < nrow(cypress_hills))
})

test_that("Replacing TMTT", {
  Sys.setenv(WT_USERNAME = "guest", WT_PASSWORD = "Apple123")
  wt_auth(force = TRUE)
  cypress_hills <- wt_download_report(620, 'ARU', 'main', FALSE)
  cypress_hills_tidy <- wt_tidy_species(cypress_hills, remove = c("mammal", "abiotic", "amphibian"), zerofill = T, "ARU")
  cypress_hills_tmtt <- wt_replace_tmtt(cypress_hills_tidy, calc = "round") %>%
    select(individual_count) %>%
    distinct()
  expect_true(!('TMTT' %in% cypress_hills_tmtt))

})

test_that('Making wide', {
  Sys.setenv(WT_USERNAME = "guest", WT_PASSWORD = "Apple123")
  wt_auth(force = TRUE)
  cypress_hills_aru <- wt_download_report(620, 'ARU', 'main', FALSE)
  cypress_hills_tidy <- wt_tidy_species(ecypress_hills_aru, remove = c("mammal", "abiotic", "amphibian"), zerofill = T, "ARU")
  cypress_hills_tmtt <- wt_replace_tmtt(cypress_hills_tidy, calc = "round")
  cypress_hills_wide <- wt_make_wide(cypress_hills_tmtt, sound = "all", sensor = 'ARU')
  expect_true(ncol(cypress_hills_wide) > ncol(cypress_hills_tmtt))
})

test_that('Getting QPAD offsets', {
  Sys.setenv(WT_USERNAME = "guest", WT_PASSWORD = "Apple123")
  wt_auth(force = TRUE)
  cypress_hills <- wt_download_report(620, 'ARU', 'main', FALSE)
  cypress_hills_tidy <- wt_tidy_species(cypress_hills, remove = c("mammal", "abiotic", "amphibian"), zerofill = T, "ARU")
  cypress_hills_tmtt <- wt_replace_tmtt(cypress_hills_tidy, calc = "round")
  cypress_hills_wide <- wt_make_wide(cypress_hills_tmtt, sound = "all", sensor = 'ARU')
  cypress_hills_qpad <- wt_qpad_offsets(cypress_hills_wide, species = "all", version = 3, together = F, sensor = 'ARU')
  expect_true(ncol(cypress_hills_qpad) > 1)
})

test_that('Occupancy formatting', {
  Sys.setenv(WT_USERNAME = "guest", WT_PASSWORD = "Apple123")
  wt_auth(force = TRUE)
  cypress_hills <- wt_download_report(620, 'ARU', 'main', FALSE)
  cypress_hills_tidy <- wt_tidy_species(cypress_hills, remove = c("mammal", "abiotic", "amphibian"), zerofill = T, "ARU")
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

################################### Camera Test suite

##wt_summarise_cam
##wt_ind_detect

################################### Point count test suite

test_that('QPAD for PC together false', {
  pc_data <- wt_download_report(887, 'PC', 'main', FALSE)
  pc_tidy <- wt_tidy_species(pc_data, remove = c("unknown"), zerofill = T, sensor = 'PC')
  pc_wide <- wt_make_wide(pc_tidy, sound = "all", sensor = 'PC')
  pc_qpad <- wt_qpad_offsets(pc_wide, species = "OVEN", version = 3, together = F, sensor = 'PC')
  expect_true(ncol(pc_wide) > ncol(pc_qpad))
})

test_that('QPAD for PC together true', {
  pc_data <- wt_download_report(887, 'PC', 'main', FALSE)
  pc_tidy <- wt_tidy_species(pc_data, remove = c("unknown"), zerofill = T, sensor = 'PC')
  pc_wide <- wt_make_wide(pc_tidy, sound = "all", sensor = 'PC')
  pc_qpad <- wt_qpad_offsets(pc_wide, species = "OVEN", version = 3, together = T, sensor = 'PC')
  expect_true(ncol(pc_qpad) > ncol(pc_wide))
})

test_that('QPAD for PC zerofill false together false', {
  pc_data <- wt_download_report(887, 'PC', 'main', FALSE)
  pc_tidy <- wt_tidy_species(pc_data, remove = c("unknown"), zerofill = F, sensor = 'PC')
  pc_wide <- wt_make_wide(pc_tidy, sound = "all", sensor = 'PC')
  pc_qpad <- wt_qpad_offsets(pc_wide, species = "OVEN", version = 3, together = F, sensor = 'PC')
  expect_true(ncol(pc_wide) > ncol(pc_qpad))
})

test_that('QPAD for PC zetrofill false', {
  pc_data <- wt_download_report(887, 'PC', 'main', FALSE)
  pc_tidy <- wt_tidy_species(pc_data, remove = c("unknown"), zerofill = F, sensor = 'PC')
  pc_wide <- wt_make_wide(pc_tidy, sound = "all", sensor = 'PC')
  pc_qpad <- wt_qpad_offsets(pc_wide, species = "OVEN", version = 3, together = T, sensor = 'PC')
  expect_true(ncol(pc_qpad) > ncol(pc_wide))
})

test_that('Occupancy for PC', {
  pc_data <- wt_download_report(887, 'PC', 'main', FALSE)
  pc_tidy <- wt_tidy_species(pc_data, remove = c("unknown"), zerofill = T, sensor = 'PC')
  occu <- wt_format_occupancy(pc_tidy, species = "OVEN")
  expect_condition(class(occu)[1] == 'unmarkedFrameOccu')
})


test_that('Error for TMTT', {
  pc_data <- wt_download_report(887, 'PC', 'main', FALSE)
  pc_tidy <- wt_tidy_species(pc_data, remove = c("unknown"), zerofill = T, sensor = 'PC')
  expect_error(wt_replace_tmtt(pc_tidy, calc = 'round'))
})
