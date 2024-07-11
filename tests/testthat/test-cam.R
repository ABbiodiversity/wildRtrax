library(testthat)

Sys.setenv(WT_USERNAME = "guest", WT_PASSWORD = "Apple123")
wt_auth(force = TRUE)

################################### Camera Test suite

test_that("Downloading CAM report", {
  abmi_amph_cam <- wt_download_report(391, 'CAM', c('megadetector'), FALSE)
  expect_true(!is.null(abmi_amph_cam))
})

test_that("Downloading CAM report", {
  suppressMessages(abmi_amph_cam <- wt_download_report(391, 'CAM', c('tag','image_set'), FALSE))
  expect_true(length(abmi_amph_cam) == 2)
})

# Test other reports
test_that("Downloading CAM report", {
  abmi_amph_cam <- wt_download_report(391, 'CAM', c('project','location', 'image_set','image_report','megaclassifier','megadetector'), FALSE)
  expect_true(!is.null(abmi_amph_cam))
})


test_that("Individual detections", {
  abmi_amph_cam <- wt_download_report(391, 'CAM', 'main', FALSE)
  ind_detections <- wt_ind_detect(abmi_amph_cam, threshold = 60, units = "minutes", datetime_col = image_date_time, remove_human = TRUE, remove_domestic = TRUE)
  expect_true(nrow(abmi_amph_cam) > nrow(ind_detections))

})

# test_that("Individual detections", {
#   abmi_amph_cam <- wt_download_report(391, 'CAM', 'main', FALSE)
#   ind_detections <- wt_ind_detect(abmi_amph_cam, threshold = 10, units = "minutes", datetime_col = image_date_time, remove_human = TRUE, remove_domestic = TRUE)
#   summary <- wt_summarise_cam(detect_data = ind_detections, raw_data = abmi_amph_cam, time_interval = "day", variable = "detections", output_format = "wide")
#   expect_true(ncol(summary) > ncol(ind_detections))
# })
