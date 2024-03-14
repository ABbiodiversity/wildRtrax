library(testthat)

Sys.setenv(WT_USERNAME = "guest", WT_PASSWORD = "Apple123")
wt_auth(force = TRUE)

################################### Point count test suite

test_that("Downloading PC report", {
  Sys.setenv(WT_USERNAME = "guest", WT_PASSWORD = "Apple123")
  wt_auth(force = TRUE)
  lpb_pc <- wt_download_report(887, 'PC', 'main', FALSE)
  expect_true(!is.null(lpb_pc))
})

test_that("Downloading ARU as PC report", {
  cypress_hills_as_pc <- wt_download_report(620, 'PC', 'main', FALSE)
  expect_true(!is.null(cypress_hills_as_pc))
})

test_that("Attempting PC as ARU report", {
  expect_error(wt_download_report(887, 'ARU', 'main', FALSE))
})


test_that('QPAD for PC together false', {
  pc_data <- wt_download_report(887, 'PC', 'main', FALSE)
  pc_tidy <- wt_tidy_species(pc_data, remove = c("unknown"), zerofill = T)
  pc_wide <- wt_make_wide(pc_tidy, sound = "all")
  pc_qpad <- wt_qpad_offsets(pc_wide, species = "all", version = 3, together = F)
  expect_true(ncol(pc_wide) > ncol(pc_qpad))
})

test_that('QPAD for PC together true', {
  pc_data <- wt_download_report(887, 'PC', 'main', FALSE)
  pc_tidy <- wt_tidy_species(pc_data, remove = c("unknown"), zerofill = T)
  pc_wide <- wt_make_wide(pc_tidy, sound = "all")
  pc_qpad <- wt_qpad_offsets(pc_wide, species = "OVEN", version = 3, together = T)
  expect_true(ncol(pc_qpad) > ncol(pc_wide))
})

test_that('QPAD for PC zerofill false together false', {
  pc_data <- wt_download_report(887, 'PC', 'main', FALSE)
  pc_tidy <- wt_tidy_species(pc_data, remove = c("unknown"), zerofill = F)
  pc_wide <- wt_make_wide(pc_tidy, sound = "all")
  pc_qpad <- wt_qpad_offsets(pc_wide, species = "OVEN", version = 3, together = F)
  expect_true(ncol(pc_wide) > ncol(pc_qpad))
})

test_that('QPAD for PC zetrofill false', {
  pc_data <- wt_download_report(887, 'PC', 'main', FALSE)
  pc_tidy <- wt_tidy_species(pc_data, remove = c("unknown"), zerofill = F)
  pc_wide <- wt_make_wide(pc_tidy, sound = "all")
  pc_qpad <- wt_qpad_offsets(pc_wide, species = "OVEN", version = 3, together = T)
  expect_true(ncol(pc_qpad) > ncol(pc_wide))
})

test_that('Occupancy for PC', {
  pc_data <- wt_download_report(887, 'PC', 'main', FALSE)
  pc_tidy <- wt_tidy_species(pc_data, remove = c("unknown"), zerofill = T)
  pc_occu <- wt_format_occupancy(pc_tidy, species = "all")
  expect_condition(class(pc_occu)[1] == 'unmarkedFrameOccu')
})


test_that('Error for TMTT', {
  pc_data <- wt_download_report(887, 'PC', 'main', FALSE)
  pc_tidy <- wt_tidy_species(pc_data, remove = c("unknown"), zerofill = T)
  expect_error(wt_replace_tmtt(pc_tidy, calc = 'round'))
})
